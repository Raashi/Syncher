unit SynchingUnit;

interface

uses
  // System includings
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults, System.Classes,
  // API includings
  Winapi.Windows, Shellapi,
  // Project includings
  SyncherItemUnit, CRCHash;

type

  TCopyCallBack = function( TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: int64;
                            StreamNumber, CallbackReason: Dword;
                            SourceFile, DestinationFile: THandle; Data: Pointer): DWord;

  TAnalyzisReport = TStringList;

  TSyncher = class
  type

    TSynchRelations = (srBothMain, srMainSecondary);
    TConflictSolving = (csShowConflict, csUseLatest, csUseOldest);
    TConflictPreferedVariant = (cpvNotSolved, cpvFirst, cpvSecond, cpvBoth);

    TSynchingOptions = record
      SynchRelations: TSynchRelations;
      ConflictSolving: TConflictSolving;

      constructor Create(SynchRelations: TSynchRelations; ConflictSolving: TConflictSolving); overload;
      constructor Create(Options: TSynchingOptions); overload;
    end;

    TConflict = record
      FirstItem: TSyncherItem;
      FirstUpdate: TDateTime;
      SecondItem: TSyncherItem;
      SecondUpdate: TDateTime;
      PreferedVariant: TConflictPreferedVariant;

      constructor Create(FI: TSyncherItem; FU: TDateTime; SI: TSyncherItem; SU: TDateTime);
    end;

    TAnalyzisResult = class
      FilesToDelete: TList<TSyncherItem>;
      FilesToCopyToFolder1: TList<TSyncherItem>;
      FilesToCopyToFolder2: TList<TSyncherItem>;
      Conflicts: TList<TConflict>;

      function ToText: TStringList;

      constructor Create;
      destructor Destroy; override;
    end;

  private
    FItem1: TSyncherItem;
    FItem2: TSyncherItem;

    FAnalyzed: boolean;
    FLastAnalyzis: TAnalyzisResult;
    FLastOptions: TSynchingOptions;
  
    procedure AnalyzeMainSecondary(Main, Secondary: TSyncherItem);

    procedure DeleteExtras(MainItem, SyncItem: TSyncherItem; UseRecycleBin: boolean);
    procedure CopyNew(MainItem, SyncItem: TSyncherItem);
    function DeleteFile(SI: TSyncherItem; UseRecycleBin: boolean): integer;
    function CopyFile(MI, PSI: TSyncherItem): integer;

    class function Compare(const Item1, Item2: TSyncherItem): boolean;
    class function CompareData(const Item1, Item2: TSyncherItem): boolean;
  public
    property Analyzed: boolean write FAnalyzed;

    function AddBackSlash(const S: String): string;
    function RemoveBackSlash(const S: string): string;

    function AnalyzeSynching(FirstPath, SecondPath: string; Options: TSynchingOptions): TAnalyzisReport;
    procedure Synch;

    constructor Create;
    destructor Destroy; override;
  end;

  TSynchRelations = TSyncher.TSynchRelations;
  TSynchingOptions = TSyncher.TSynchingOptions;
  TAnalyzedResult = TSyncher.TAnalyzisResult;

var
  Syncher: TSyncher;

implementation

const
  MB = 1048576;
  HashMaxFileSize = MB * 5;
  Separator = '-------------------------------------------------------';

{ TSyncher }

constructor TSyncher.Create;
begin
  FItem1 := nil;
  FItem2 := nil;

  FAnalyzed := false;
  FLastAnalyzis := nil;
end;

destructor TSyncher.Destroy;
begin
  FreeAndNil(Fitem1);
  FreeAndNil(Fitem2);

  FreeAndNil(FLastAnalyzis);

  inherited;
end;

function TSyncher.AnalyzeSynching(FirstPath, SecondPath: string; Options: TSynchingOptions): TAnalyzisReport;
begin
  FreeAndNil(Fitem1);
  FreeAndNil(Fitem2);
  FreeAndNil(FLastAnalyzis);

  FItem1 := TSyncherItem.CreateRoot(FirstPath);
  FItem2 := TSyncherItem.CreateRoot(SecondPath);
  FLastAnalyzis := TAnalyzisResult.Create;
  FLastOptions := TSynchingOptions.Create(Options);

  case Options.SynchRelations of
    srBothMain:
    begin
      // TODO: add last-update checking? Maybe in TSyncherItem's constructor? (Check if conflicts solved by user)
    end;
    srMainSecondary:
      AnalyzeMainSecondary(FItem1, FItem2);
  end;

  result := FLastAnalyzis.ToText;
end;

procedure TSyncher.AnalyzeMainSecondary(Main, Secondary: TSyncherItem);
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
        if si1.IsFile then
        begin
          if not CompareData(si1, si2) then
          begin  
            // new to change old file with the new one
            FLastAnalyzis.FilesToCopyToFolder2.Add(si1);
            FLastAnalyzis.FilesToDelete.Add(si2);
          end;
          // else doing nothing - file is actual or content didn't checked because of huge size
        end
        else
          // get information about tree if it's a folder
          AnalyzeMainSecondary(si1, si2);

        // remove equal items - information about them was gathered above
        CopyFilesList.Remove(si1);
        DeleteFilesList.Remove(si2);
        break;
      end;
  // check for new files in Main and files to Delete in Secondary
  for si1 in CopyFilesList do
    FLastAnalyzis.FilesToCopyToFolder2.Add(si1);
  for si2 in DeleteFilesList do
    FLastAnalyzis.FilesToDelete.Add(si2);
  
  CopyFilesList.Destroy;
  DeleteFilesList.Destroy;
end;

procedure TSyncher.Synch;
begin

end;

class function TSyncher.Compare(const Item1, Item2: TSyncherItem): boolean;
begin
  result := (Item1.Name = Item2.Name) and (Item1.IsFile = Item2.IsFile);
end;

class function TSyncher.CompareData(const Item1, Item2: TSyncherItem): boolean;
begin
  result := Item1.Bytes = Item2.Bytes;
  if (Item1.Bytes < HashMaxFileSize) and (Item2.Bytes < HashMaxFileSize) and result then
    result := GetFileCRC(Item1.FullPath) = GetFileCRC(Item2.FullPath);
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
  if integer(GetFileAttributes(PChar(SI.FullPath))) = -1 then
  begin
    result := 0;
    exit;
  end;

  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.wFunc := FO_DELETE;
  FileOp.pFrom := PChar(SI.FullPath + #0);
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
  if integer(GetFileAttributes(PChar(MI.FullPath))) = -1 then
  begin
    result := 0;
    exit;
  end;

  ZeroMemory(@FileOp, SizeOf(FileOp));
  FileOp.wFunc := FO_COPY;
  FileOp.pFrom := PChar(MI.FullPath + #0);
  FileOP.pTo := PChar(PSI.FullPath + #0);
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

constructor TSyncher.TAnalyzisResult.Create;
begin
  FilesToDelete := TList<TSyncherItem>.Create;
  FilesToCopyToFolder1 := TList<TSyncherItem>.Create;
  FilesToCopyToFolder2 := TList<TSyncherItem>.Create;
  Conflicts := TList<TConflict>.Create;
end;

destructor TSyncher.TAnalyzisResult.Destroy;
begin
  FilesToDelete.Destroy;
  FilesToCopyToFolder1.Destroy;
  FilesToCopyToFolder2.Destroy;
  Conflicts.Destroy;

  inherited;
end;

function TSyncher.TAnalyzisResult.ToText: TStringList;
var
  si: TSyncherItem;
begin
  result := TStringList.Create;

  result.Add('File to Copy To ' + Syncher.FItem2.FullPath + sLineBreak + Separator);
  for si in FilesToCopyToFolder2 do
    result.Add(si.FullPath + ' ' + si.Bytes.ToString);
  result.Add(Separator);

  result.Add(sLineBreak + 'File to Copy To ' + Syncher.FItem1.FullPath + sLineBreak + Separator);
  for si in FilesToCopyToFolder1 do
    result.Add(si.FullPath + ' ' + si.Bytes.ToString);
  result.Add(Separator);

  result.Add(sLineBreak + 'File to Delete' + sLineBreak + Separator);
  for si in FilesToDelete do
    result.Add(si.FullPath);
  result.Add(Separator);
end;

{ TSyncher.TSynchingOptions }

constructor TSyncher.TSynchingOptions.Create(SynchRelations: TSynchRelations; ConflictSolving: TConflictSolving);
begin
  Self.SynchRelations := SynchRelations;
  Self.ConflictSolving := ConflictSolving;
end;

constructor TSyncher.TSynchingOptions.Create(Options: TSynchingOptions);
begin
  SynchRelations := Options.SynchRelations;
  ConflictSolving := Options.ConflictSolving;
end;

initialization

Syncher := TSyncher.Create;

finalization

Syncher.Destroy;

end.
