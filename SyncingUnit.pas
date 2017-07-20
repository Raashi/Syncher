unit SyncingUnit;

interface

uses
  // System includings
  System.SysUtils, System.Generics.Collections,
  // API includings
  Winapi.Windows;

type

  TCopyCallBack = function( TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: int64;
                            StreamNumber, CallbackReason: Dword;
                            SourceFile, DestinationFile: THandle; Data: Pointer): DWord;

  TSyncher = class
  private

  public
    function GetAllItems(APath: string): TList<string>;

    function SynchFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
  end;

implementation

{ TSyncher }

function TSyncher.GetAllItems(APath: string): TList<string>;
var
  i: integer;
  FindRec: TSearchRec;
begin
  result := TList<string>.Create;
  i := FindFirst(APath, 0 , FindRec);
  try
    while i <> 0 do
    begin
      result.Add(FindRec.Name);
      i := FindNext(FindRec);
    end;
  finally
    System.SysUtils.FindClose(FindRec);
  end;
end;

function TSyncher.SynchFolder(AMainPath, APath: string; ARecycleBin: boolean; ARoutine: TCopyCallBack = nil): integer;
begin

end;

end.
