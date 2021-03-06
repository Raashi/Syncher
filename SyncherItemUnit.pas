unit SyncherItemUnit;

interface

uses
  // System includings
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  // API includings
  Winapi.Windows;

type

  TSyncherItem = class
  private
    FParent: TSyncherItem;
    FName: string;
    FRelativePath: string;
    FFullPath: string;
    FIsFile: boolean;
    FSubTree: Tlist<TSyncherItem>;

    FBytes: int64;
    FFilesCount: integer;
    FFoldersCount: integer;
    FLastModifiedDate: TDateTime;

    function GetTreeCount: integer;
    function GetChild(Index: integer): TSyncherItem;
    function CollectChildren: TList<TSyncherItem>;
  public
    property Parent: TSyncherItem read FParent;
    property Name: string read FName;
    property FullPath: string read FFullPath;
    property RelativePath: string read FRelativePath;
    property IsFile: boolean read FIsFile;

    property Bytes: Int64 read FBytes;
    property FilesCount: integer read FFilesCount;
    property FoldersCount: integer read FFoldersCount;
    property LastModifiedTime: TDateTime read FLastModifiedDate;

    function SubTree: TArray<TSyncherItem>;
    property SubTreeCount: integer read GetTreeCount;
    property Child[Index: integer]: TSyncherItem read GetChild; default;

    constructor Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
    constructor CreateRoot(const Path: string);
    destructor Destroy; override;

    class function RefreshAndGet(Root: TSyncherItem; RelativePath: string): TSyncherItem;
    class procedure PutItemInList(SI: TSyncherItem; List: TList<TSyncherItem>);
  end;

function AddBackSlash(const S: String): string;
function RemoveBackSlash(const S: string): string;

implementation

function AddBackSlash(const S: String): string;
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

function RemoveBackSlash(const S: string): string;
begin
  if S[S.Length] <> '\' then
    result := S
  else
    result := S.Substring(0, S.Length - 1);
end;

{ TSyncherItem }

constructor TSyncherItem.Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
var
  hFile: integer;
begin
  FParent := Parent;
  FName := Name;
  FFullPath := Parent.FFullPath + FName;
  FRelativePath := Parent.FRelativePath + FName;
  FIsFile := IsFile;

  if integer(GetFileAttributes(PChar(FFullPath))) = -1 then
    raise Exception.Create('File: "' + FFullPath + '" doesn''t exit!');

  if FIsFile then
  begin
    FSubTree := nil;
    hFile := FileOpen(FFullPath, fmOpenRead);
    FBytes := GetFileSize(hFile, nil);
    FFilesCount := -1;
    FFoldersCount := -1;
    FileClose(hFile);
    FileAge(FFullPath, FLastModifiedDate);
  end
  else
  begin
    FFullPath := FFullPath + '\';
    FRelativePath := FRelativePath + '\';
    FBytes := 0;
    FSubTree := CollectChildren;
  end;
end;

constructor TSyncherItem.CreateRoot(const Path: string);
begin
  FParent := nil;
  FName := '';
  FFullPath := Path;
  FRelativePath := '\';
  FIsFile := false;
  FSubTree := CollectChildren;
end;

destructor TSyncherItem.Destroy;
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

function TSyncherItem.CollectChildren: TList<TSyncherItem>;
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

function TSyncherItem.GetTreeCount: integer;
begin
  if FIsFile then
    result := -1
  else
    result := FSubTree.Count;
end;

function TSyncherItem.SubTree: TArray<TSyncherItem>;
begin
  result := nil;
  if not FIsFile then
    result := FSubTree.ToArray;
end;

function TSyncherItem.GetChild(Index: integer): TSyncherItem;
begin
  if FIsFile then
    result := nil
  else
    result := FSubTree[Index];
end;

class procedure TSyncherItem.PutItemInList(SI: TSyncherItem; List: TList<TSyncherItem>);
var
  item: TSyncherItem;
begin
  if SI.IsFile or (SI.FFilesCount = 0) then
    List.Add(SI)
  else if SI.FFilesCount > 0 then
    for item in SI.SubTree do
      PutItemInList(item, List);
end;

class function TSyncherItem.RefreshAndGet(Root: TSyncherItem; RelativePath: string): TSyncherItem;
var
  arr: TArray<string>;
  i: integer;
  si, s_si: TSyncherItem;
  FolderNotFound: boolean;
begin
  if Root.Parent <> nil then
    raise Exception.Create('Item isn''t root!');

  arr := RelativePath.Split(['\']);
  s_si := Root;
  si := nil;
  for i := 0 to Length(arr) - 1 do
  begin
    FolderNotFound := true;

    for si in s_si.FSubTree do
      if si.FName = arr[i] then
      begin
        FolderNotFound := false;
        break;
      end;

    if FolderNotFound then
      s_si := si
    else
    begin
      si := TSyncherItem.Create(s_si, arr[i], i = Length(arr) - 1);
      s_si := si;
    end;
  end;

  result := s_si;
end;

end.
