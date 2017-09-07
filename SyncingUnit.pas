unit SyncingUnit;

interface

uses
  // System includings
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults, System.Classes,
  // API includings
  Winapi.Windows, Shellapi;

type

  TCopyCallBack = function( TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: int64;
                            StreamNumber, CallbackReason: Dword;
                            SourceFile, DestinationFile: THandle; Data: Pointer): DWord;

  TAnalyzisReport = TStringList;

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

      FBytes: int64;
      FFilesCount: integer;
      FFoldersCount: integer;

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

      function SubTree: TArray<TSyncherItem>;
      property SubTreeCount: integer read GetTreeCount;
      property Child[Index: integer]: TSyncherItem read GetChild; default;

      function FileName: string;

      destructor Destroy; override;
    end;

  TSynchRelations = (srBothMain, srMainSecondary);
  TConflictSolving = (csShowConflict, csUseLatest, csUseOldest);

  TSynchingOptions = record
    SynchRelations: TSynchRelations;
    MainItem: integer;
    ConflictSolving: TConflictSolving;
  end;

  TConflictPreferedVariant = (cpvNotSolved, cpvFirst, cpvSecond, cpvBoth);

  TConflict = record
    FirstItem: TSyncherItem;
    FirstUpdate: TDateTime;
    SecondItem: TSyncherItem;
    SecondUpdate: TDateTime;
    PreferedVariant: TConflictPreferedVariant;

    constructor Create(FI: TSyncherItem; FU: TDateTime; SI: TSyncherItem; SU: TDateTime);
  end;

  TAnalyzedResult = class
    FilesToDelete: TList<TSyncherItem>;
    FilesToCopy: TList<TSyncherItem>;
    Conflicts: TList<TConflict>;

    function ToText: TStringList;

    constructor Create;
    destructor Destroy;
  end;

  private
    FItem1: TSyncherItem;
    FItem2: TSyncherItem;
  
    procedure AnalyzeMainSecondary(Main, Secondary: TSyncherItem; Options: TSynchingOptions; const Result: TAnalyzedResult);

    procedure DeleteExtras(MainItem, SyncItem: TSyncherItem; UseRecycleBin: boolean);
    procedure CopyNew(MainItem, SyncItem: TSyncherItem);
    function DeleteFile(SI: TSyncherItem; UseRecycleBin: boolean): integer;
    function CopyFile(MI, PSI: TSyncherItem): integer;

    procedure NewAnalyzeRequest(const Path1, Path2: string);

    class function Compare(const Item1, Item2: TSyncherItem): boolean;
    class function CompareData(const Item1, Item2: TSyncherItem): boolean;
  public
    function AddBackSlash(const S: String): string;
    function RemoveBackSlash(const S: string): string;

    function AnalyzeCopying(FirstPath, SecondPath: string; Options: TSynchingOptions): TAnalyzisReport;

    constructor Create;
    destructor Destroy; override;
  end;

  TSyncherItem = TSyncher.TSyncherItem;
  TSynchingOptions = TSyncher.TSynchingOptions;
  TAnalyzedResult = TSyncher.TAnalyzedResult;

var
  Syncher: TSyncher;

implementation

procedure InitLinks(var Main, Secondary: TSyncherItem; const F, S: TSyncherItem; MainItem: integer);
begin
  if (MainItem <> 1) and (MainItem <> 2) then
    raise Exception.Create('Неверное значение MainItem: ' + MainItem.ToString);

  if MainItem = 1 then
  begin
    Main := F;
    Secondary := S;
  end
  else
  begin
    Main := S;
    Secondary := F;
  end;
end;

{ TSyncher }

constructor TSyncher.Create;
begin
  FItem1 := nil;
  FItem2 := nil;
end;

destructor TSyncher.Destroy;
begin
  if FItem1 <> nil then
    FItem1.Destroy;
  if FItem2 <> nil then
    FItem2.Destroy;

  inherited;
end;

procedure TSyncher.NewAnalyzeRequest(const Path1, Path2: string);
begin
  if FItem1 <> nil then
  begin
    if FItem1.FFullPath = AddBackSlash(Path1) then
      
  end;
  // TODO: make it with gotos;
end;

function TSyncher.AnalyzeCopying(FirstPath, SecondPath: string; Options: TSynchingOptions): TAnalyzisReport;
var
  Main, Secondary: TSyncherItem;
  Analyze: TAnalyzedResult;
begin
  // TODO: stupid. Make good cheking
  if FItem1 <> nil then
    FItem1.Destroy;
  if FItem2 <> nil then
    FItem2.Destroy;
  
  FItem1 := TSyncherItem.CreateRoot(FirstPath);
  FItem2 := TSyncherItem.CreateRoot(SecondPath);

  Analyze := TAnalyzedResult.Create;
  case Options.SynchRelations of
    srBothMain:
    begin
      // TODO: add last-update checking? Maybe in TSyncherItem's constructor? (Check if conflicts solved by user)
    end;
    srMainSecondary:
    begin
      InitLinks(Main, Secondary, FItem1, FItem2, Options.MainItem);
      AnalyzeMainSecondary(Main, Secondary, Options, Analyze);
    end;
  end;

  result := Analyze.ToText;
  Analyze.Destroy;
end;

procedure TSyncher.AnalyzeMainSecondary(Main, Secondary: TSyncherItem; Options: TSynchingOptions; const Result: TAnalyzedResult);
var
  si1, si2: TSyncherItem;
  DeleteFilesList, CopyFilesList: TList<TSyncherItem>;
  si: TObject;
begin
  CopyFilesList := TList<TSyncherItem>.Create;
  CopyFilesList.AddRange(Main.SubTree);
  DeleteFilesList := TList<TSyncherItem>.Create;
  DeleteFilesList.AddRange(Secondary.SubTree);

  // analyzing
  for si1 in Main.SubTree do
    for si2 in Secondary.SubTree do
      if Compare(si1, si2) then
      begin
        if si1.FIsFile then
        begin
          if not CompareData(si1, si2) then
          begin  
            // new to change old file with the new one
            result.FilesToCopy.Add(si1);
            result.FilesToDelete.Add(si2);
          end;
          // else doing nothing - file is actual or content didn't checked because of huge size
        end
        else
          // get information about tree if it's a folder
          AnalyzeMainSecondary(si1, si2, Options, Result);

        // remove equal items - information about them was gathered above
        CopyFilesList.Remove(si1);
        DeleteFilesList.Remove(si2);
        break;
      end;
  // check for new files in Main and files to Delete in Secondary
  for si1 in CopyFilesList do
    result.FilesToCopy.Add(si1);
  for si2 in DeleteFilesList do
    result.FilesToDelete.Add(si2);
  
  CopyFilesList.Destroy;
  DeleteFilesList.Destroy;
end;

class function TSyncher.Compare(const Item1, Item2: TSyncherItem): boolean;
begin
  result := (Item1.Name = Item2.Name) and (Item1.IsFile = Item2.IsFile);
end;

class function TSyncher.CompareData(const Item1, Item2: TSyncherItem): boolean;
begin
  // TODO: check hash-sum(?) in order to detect conflicts
  result := true;
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

function TSyncher.RemoveBackSlash(const S: string): string;
begin
  if S[S.Length] <> '\' then
    result := S
  else
    result := S.Substring(0, S.Length - 1);
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

{ TSyncher.TSyncherItem }

constructor TSyncher.TSyncherItem.Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
var
  hFile: integer;
begin
  FParent := Parent;
  FName := Name;
  FFullPath := Parent.FFullPath + FName;
  FPath := Parent.FPath + FName;
  FIsFile := IsFile;

  if FIsFile then
  begin
    FSubTree := nil;
    hFile := FileOpen(FFullPath, fmOpenRead);
    FBytes := GetFileSize(hFile, nil);
    FFilesCount := -1;
    FFoldersCount := -1;
    FileClose(hFile);
  end
  else
  begin
    FFullPath := FFullPath + '\';
    FPath := FPath + '\';
    FBytes := 0;
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

  inherited;
end;

function TSyncher.TSyncherItem.CollectChildren: TList<TSyncherItem>;
var
  i: integer;
  si: TSyncherItem;
  FindRec: TSearchRec;
  SearchPath: string;
begin
  SearchPath := FFullPath + '*.*';
  FFilesCount := 0;
  FFoldersCount := 0;

  result := TList<TSyncherItem>.Create;
  i := FindFirst(SearchPath, faAnyFile or faDirectory, FindRec);
  try
    while i = 0 do
    begin
      if (FindRec.Name <> '.') and (FindRec.Name <> '..') then
      begin
        si := TSyncherItem.Create(Self, FindRec.Name, FindRec.Attr <> faDirectory);
        // TODO: test this formula
        FBytes := FBytes + si.FBytes;
        if si.FIsFile then
          inc(FFilesCount)
        else
          FFoldersCount := FFoldersCount + 1 + si.FFoldersCount;
        result.Add(si);
      end;
      i := FindNext(FindRec);
    end;
  finally
    System.SysUtils.FindClose(FindRec);
  end;

  // DONE: test this sorting
  result.Sort(TComparer<TSyncherItem>.Construct(
    function (const L, R: TSyncherItem): integer
    begin
      if L.FIsFile <> R.FIsFile then
      begin
        if L.FIsFile then
          result := 1
        else
          result := -1;
      end
      else
      begin
        if L.FName < R.FName then
          result := 1
        else if L.FName = R.FName then
          result := 0
        else
          result := -1;
      end;
    end));
end;

function TSyncher.TSyncherItem.GetTreeCount: integer;
begin
  if FIsFile then
    result := -1
  else
    result := FSubTree.Count;
end;

function TSyncher.TSyncherItem.SubTree: TArray<TSyncherItem>;
begin
  result := nil;
  if not FIsFile then
    result := FSubTree.ToArray;
end;

function TSyncher.TSyncherItem.GetChild(Index: integer): TSyncherItem;
begin
  if FIsFile then
    result := nil
  else
    result := FSubTree[Index];
end;

function TSyncher.TSyncherItem.FileName: string;
begin
  result := FPath + FName;
end;

{ TSyncher.TConflict }

constructor TSyncher.TConflict.Create(FI: TSyncherItem; FU: TDateTime; SI: TSyncherItem; SU: TDateTime);
begin
  FirstItem := FI;
  SecondItem := SI;
  FirstUpdate := FU;
  SecondUpdate := SU;
  PreferedVariant := cpvNotSolved;
end;

{ TSyncher.TAnalyzedResult }

constructor TSyncher.TAnalyzedResult.Create;
begin
  FilesToDelete := TList<TSyncherItem>.Create;
  FilesToCopy := TList<TSyncherItem>.Create;
  Conflicts := TList<TConflict>.Create;
end;

destructor TSyncher.TAnalyzedResult.Destroy;
begin
  FilesToDelete.Destroy;
  FilesToCopy.Destroy;
  Conflicts.Destroy;

  inherited;
end;

function TSyncher.TAnalyzedResult.ToText: TStringList;
var
  si: TSyncherItem;
begin
  result := TStringList.Create;
  result.Add('File to Copy');
  for si in FilesToCopy do
    result.Add(si.FFullPath + ' ' + si.FBytes.ToString);
  result.Add('File to Delete');
  for si in FilesToDelete do
    result.Add(si.FFullPath);
end;

initialization

Syncher := TSyncher.Create;

finalization

Syncher.Destroy;

end.
