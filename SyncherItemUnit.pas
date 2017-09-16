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
    property Path: string read FRelativePath;
    property FullPath: string read FFullPath;
    property RelativePath: string read FRelativePath;
    property IsFile: boolean read FIsFile;

    property Bytes: Int64 read FBytes;
    property FilesCount: integer read FFilesCount;
    property FoldersCount: integer read FFoldersCount;

    function SubTree: TArray<TSyncherItem>;
    property SubTreeCount: integer read GetTreeCount;
    property Child[Index: integer]: TSyncherItem read GetChild; default;

    constructor Create(Parent: TSyncherItem; const Name: string; const IsFile: boolean);
    constructor CreateRoot(const Path: string);
    destructor Destroy; override;
  end;

implementation

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

end.
