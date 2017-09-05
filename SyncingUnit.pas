unit SyncingUnit;

interface

uses
  // System includings
  System.SysUtils, System.Generics.Collections,
  // API includings
  Winapi.Windows, Shellapi;

type

  TCopyCallBack = function( TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: int64;
                            StreamNumber, CallbackReason: Dword;
                            SourceFile, DestinationFile: THandle; Data: Pointer): DWord;

  TSyncher = class;
  

  TSyncher = class
  type

    TSyncherItem = class
    private
      FParent: TSyncherItem;
      FName: string;
      FPath: string;
      FFullPath: string;
      FIsFile: boolean;
      FSubTree: Tlist<TSyncherItem>;

      constructor Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
      constructor CreateRoot(const Path: string);

      function GetTreeCount: integer;
      function GetChild(Index: integer): TSyncherItem;
      function CollectChildren: TList<TSyncherItem>;
    public
      property Name: string read FName;
      property Directory: string read FPath;
      property IsFile: boolean read FIsFile;
      property Parent: TSyncherItem read FParent;
      function FullPath: string;

      property SubTreeCount: integer read GetTreeCount;
      property Child[Index: integer]: TSyncherItem read GetChild; default;
      function GetSubTreeArray: TArray<TSyncherItem>;

      function FileName: string;

      destructor Destroy;
    end;

  private
    procedure DeleteExtras(MainItem, SyncItem: TSyncherItem; UseRecycleBin: boolean);
    procedure CopyNew(MainItem, SyncItem: TSyncherItem);

    function DeleteFile(SI: TSyncherItem; UseRecycleBin: boolean): integer;
    function CopyFile(MI, PSI: TSyncherItem): integer;

    class function Compare(const Item1, Item2: TSyncherItem): boolean;
  public
    function AddBackSlash(const S: String): string;

    function SyncFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
  end;

  TSyncherItem = TSyncher.TSyncherItem;

var
  Syncher: TSyncher;

implementation

{ TSyncher }

class function TSyncher.Compare(const Item1, Item2: TSyncherItem): boolean;
begin
  result := (Item1.Name = Item2.Name) and (Item1.IsFile = Item2.IsFile);
end;

function TSyncher.AddBackSlash(const S: String): string;
begin
  Result := S;
  if S <> '' then
  begin
    if S[length(S)] <> '\' then
      Result := S + '\';
  end
  else
    Result := '\';
end;

function TSyncher.DeleteFile(SI: TSyncherItem; UseRecycleBin: boolean): integer;
var
  FileOp: TSHFileOpStruct;
begin
  if integer(GetFileAttributes(PChar(SI.FFullPath))) = -1 then
  begin
    result := 0;
    exit;
  end;

  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(SI.FFullPath + #0);
  FileOP.fAnyOperationsAborted := false;

  if UseRecycleBin then
    FileOp.fFlags := FOF_NO_UI
  else
    FileOp.fFlags := FOF_ALLOWUNDO or FOF_NO_UI;

  Result := SHFileOperation(FileOp);
end;

function TSyncher.CopyFile(MI, PSI: TSyncherItem): integer;
var
  FileOp: TSHFileOpStruct;
begin
  if integer(GetFileAttributes(PChar(MI.FFullPath))) = -1 then
  begin
    result := 0;
    exit;
  end;

  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.wFunc := FO_COPY;
  FileOp.pFrom := PChar(MI.FFullPath + #0);
  FileOP.pTo := PChar(PSI.FFullPath + #0);
  FileOP.fAnyOperationsAborted := false;
  FileOp.fFlags := FOF_NO_UI;

  Result := SHFileOperation(FileOp);

  TSyncherItem.Create(PSI, MI.Name, MI.IsFile);
end;

procedure TSyncher.DeleteExtras(MainItem, SyncItem: TSyncherItem; UseRecycleBin: boolean);
var
  i, j: integer;
  NeedDelete: boolean;
  SI: TSyncherItem;
begin
  for i := SyncItem.SubTreeCount - 1 downto 0 do
  begin
    SI := SyncItem[i];
    NeedDelete := true;

    for j := 0 to MainItem.SubTreeCount - 1 do
      if Compare(SI, MainItem[j]) then
      begin
        if not SI.IsFile then
          DeleteExtras(MainItem[j], SI, UseRecycleBin);
        NeedDelete := false;
        break;
      end;

    if NeedDelete then
    begin
      DeleteFile(SI, UseRecycleBin);
      SI.Destroy;
    end;
  end;
end;

procedure TSyncher.CopyNew(MainItem, SyncItem: TSyncherItem);
var
  i, j: Integer;
  MI: TSyncherItem;
  NeedCopy: boolean;
begin
  for i := 0 to MainItem.SubTreeCount - 1 do
  begin
    MI := MainItem[i];
    NeedCopy := true;

    for j := 0 to SyncItem.SubTreeCount - 1 do
      if Compare(SyncItem[j], MI) then
      begin
        if not MI.IsFile then
          CopyNew(MI, SyncItem[j]);
        NeedCopy := false;
        break;
      end;

    if NeedCopy then
      CopyFile(MI, SyncItem);
  end;
end;

function TSyncher.SyncFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
var
  MainItem, SyncItem: TSyncherItem;
begin
  MainItem := TSyncherItem.CreateRoot(AMainPath);
  SyncItem := TSyncherItem.CreateRoot(APath);

  DeleteExtras(MainItem, SyncItem, ARecycleBin);
  CopyNew(MainItem, SyncItem);

  MainItem.Destroy;
  SyncItem.Destroy;
end;

{ TSyncher.TSyncherItem }

function TSyncher.TSyncherItem.CollectChildren: TList<TSyncherItem>;
var
  i: integer;
  FindRec: TSearchRec;
  SearchPath: string;
begin
  SearchPath := FFullPath + '*.*';

  result := TList<TSyncherItem>.Create;
  i := FindFirst(SearchPath, faAnyFile or faDirectory, FindRec);
  try
    while i = 0 do
    begin
      if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
        result.Add(TSyncherItem.Create(Self, FindRec.Name, FindRec.Attr <> faDirectory));
      i := FindNext(FindRec);
    end;
  finally
    System.SysUtils.FindClose(FindRec);
  end;
end;

constructor TSyncher.TSyncherItem.Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
begin
  FParent := Parent;
  FName := Name;
  FFullPath := Parent.FFullPath + FName;
  FPath := Parent.FPath + FName;
  FIsFile := IsFile;

  if FIsFile then
    FSubTree := nil
  else
  begin
    FFullPath := FFullPath + '\';
    FPath := FPath + '\';
    FSubTree := CollectChildren;
  end;
end;

constructor TSyncher.TSyncherItem.CreateRoot(const Path: string);
begin
  FParent := nil;
  FName := '';
  FFullPath := Path;
  FPath := '\';
  FIsFile := false;
  FSubTree := CollectChildren;
end;

destructor TSyncher.TSyncherItem.Destroy;
var
  i: integer;
begin
  if FParent <> nil then
    FParent.FSubTree.Remove(Self);

  if FIsFile then
    exit;

  for i := FSubTree.Count - 1 downto 0 do
    FSubTree[i].Destroy;
  FSubTree.Destroy;
end;

function TSyncher.TSyncherItem.GetTreeCount: integer;
begin
  if FIsFile then
    result := -1
  else
    result := FSubTree.Count;
end;

function TSyncher.TSyncherItem.GetChild(Index: integer): TSyncherItem;
begin
  if FIsFile then
    result := nil
  else
    result := FSubTree[Index];
end;

function TSyncher.TSyncherItem.GetSubTreeArray: TArray<TSyncherItem>;
begin
  if FIsFile then
    result := nil
  else
    result := FSubTree.ToArray;
end;

function TSyncher.TSyncherItem.FileName: string;
begin
  result := FPath + FName;
end;

function TSyncher.TSyncherItem.FullPath: string;
begin
  result := FPath + '\' + FName;
end;

initialization

Syncher := TSyncher.Create;

finalization

Syncher.Destroy;

end.
