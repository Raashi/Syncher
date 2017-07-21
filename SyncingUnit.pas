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
      FRoot: boolean;
      FParent: TSyncherItem;
      FName: string;
      FDirectory: string;
      FIsFile: boolean;
      FSubTree: Tlist<TSyncherItem>;

      constructor Create(Parent: TSyncherItem; const Name, Directory: string; const IsFile: boolean);
      constructor CreateRoot(const Path: string);

      function GetTreeCount: integer;
      function GetItem(Index: integer): TSyncherItem;
    public
      property Name: string read FName;
      property Directory: string read FDirectory;
      property IsFile: boolean read FIsFile;

      property SubTreeCount: integer read GetTreeCount;
      property Item[Index: integer]: TSyncherItem read GetItem; default;
      function GetSubTreeArray: TArray<TSyncherItem>;

      function FileName: string;

      class function Compare(Item1, Item2: TSyncherItem): boolean;

      destructor Delete;
      destructor Destroy;
    end;
    
  strict private
    constructor Create;
    destructor Destroy;
  private
    function ExtractParentDir(Dir: string): string;
    function ExtractNameDir(Dir: string): string;
    
    function GetAllItems(Path: string): Tlist<TSyncherItem>;
    procedure DeleteExtras(MainItem, SyncItem: TSyncherItem; RecycleBin: boolean);
    function DeleteFile(SI: TSyncherItem; RecycleBin: boolean): boolean;
  public
    function AddBackSlash(const S: String): string;

    function SyncFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
  end;

  TSyncherItem = TSyncher.TSyncherItem;

var
  Syncher: TSyncher;

implementation

{ TSyncher }

function TSyncher.AddBackSlash(const S: String): string;
begin
  Result := S;
  if S <> '' then
  begin
    If S[length(S)] <> '\' then
      Result := S + '\';
  end
  else
    Result := '\';
end;

constructor TSyncher.Create;
begin
end;

destructor TSyncher.Destroy;
begin
  inherited;
end;

function TSyncher.ExtractNameDir(Dir: string): string;
begin
  Dir := Dir.Substring(1, Length(Dir) - 1);
  result := Dir.Substring(Dir.LastIndexOf('\')) + '\';
end;

function TSyncher.ExtractParentDir(Dir: string): string;
begin
  Dir := Dir.Substring(1, Length(Dir) - 1);
  result := Dir.Substring(1, Dir.LastIndexOf('\'));
end;

function TSyncher.DeleteFile(SI: TSyncherItem; RecycleBin: boolean): boolean;
var 
  FileOp: TSHFileOpStruct;
begin
  if integer(GetFileAttributes(PChar(SI.FileName))) = -1 then
  begin
    result := false;
    exit;
  end;

  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(SI.FileName);

  if RecycleBin then
    FileOp.fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION
  else
    FileOp.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;

  Result := SHFileOperation(FileOp) = 0;
end;

procedure TSyncher.DeleteExtras(MainItem, SyncItem: TSyncherItem; RecycleBin: boolean);
var
  i, j: integer;
  DoNotDelete: boolean;
  si: TSyncherItem;
begin
  for i := 0 to SyncItem.SubTreeCount - 1 do
  begin
    si := SyncItem[i];

    DoNotDelete := false;
    for j := 0 to MainItem.SubTreeCount - 1 do
      if TSyncherItem.Compare(si, MainItem[j]) then
      begin
        if not si.IsFile then
          DeleteExtras(MainItem[j], si, RecycleBin);
        DoNotDelete := true;
        break;
      end;

    if not DoNotDelete then
       
  end;
end;

function TSyncher.GetAllItems(Path: string): Tlist<TSyncherItem>;
var
  i: integer;
  FindRec: TSearchRec;
  SearchPath: string;
begin
  SearchPath := Path + '*.*';

  result := TList<TSyncherItem>.Create;
  i := FindFirst(SearchPath, faAnyFile or faDirectory, FindRec);
  try
    while i = 0 do
    begin
      if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
        result.Add(TSyncherItem.Create(FindRec.Name, Path, FindRec.Attr <> faDirectory));
      i := FindNext(FindRec);
    end;
  finally
    System.SysUtils.FindClose(FindRec);
  end;
end;

function TSyncher.SyncFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
var
  MainItem, SyncItem: TSyncherItem;
begin
  MainItem := TSyncherItem.CreateRoot(AMainPath);
  SyncItem := TSyncherItem.CreateRoot(APath);

  DeleteExtras(MainItem, SyncItem, ARecycleBin);
end;

{ TSyncher.TSyncherItem }

class function TSyncher.TSyncherItem.Compare(Item1, Item2: TSyncherItem): boolean;
begin
  result := Item1.Name = Item2.Name;
end;

constructor TSyncher.TSyncherItem.Create(Parent: TSyncherItem; const Name, Directory: string; const IsFile: boolean);
begin
  FRoot := false;
  FParent := Parent;
  FName := Name;
  FDirectory := Directory;
  FIsFile := IsFile;

  if not FIsFile then
  begin
    FName := Syncher.AddBackSlash(FName);
    FSubTree := Syncher.GetAllItems(FDirectory + FName);    
  end
  else
    FSubTree := nil;
end;

constructor TSyncher.TSyncherItem.CreateRoot(const Path: string);
begin
  FRoot := true;
  FParent := nil;
  FName := Syncher.ExtractNameDir(Path);
  FDirectory := Syncher.ExtractParentDir(Path);
  FIsFile := false;
  FSubTree := Syncher.GetAllItems(Path);
end;

destructor TSyncher.TSyncherItem.Destroy;
var
  si: TSyncherItem;
begin
  if not FIsFile then
  begin
    for si in FSubTree do
      si.Destroy;
    FSubTree.Destroy;
  end;

  inherited;
end;

function TSyncher.TSyncherItem.GetTreeCount: integer;
begin
  if FIsFile then
    result := -1
  else
    result := FSubTree.Count;
end;

function TSyncher.TSyncherItem.GetItem(Index: integer): TSyncherItem;
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
  result := FDirectory + FName;
end;

initialization

Syncher := TSyncher.Create;

finalization

Syncher.Destroy;

end.
