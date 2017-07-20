unit FileCopierU;
(***************************************************************
  Author Despatcher (Timbo) 2011
****************************************************************)
interface

uses
  Windows, Messages, SysUtils, Classes, VCL.Controls, VCL.Stdctrls, strUtils, VCL.ComCtrls, ShellApi, Math;

Type
  TFolderOp = (foCopy, foCount, foSize);
  TCopyCallBack = function( TotalFileSize, TotalBytesTransferred, StreamSize, StreamBytesTransferred: int64;
                            StreamNumber, CallbackReason: Dword;
                            SourceFile, DestinationFile: THandle; Data: Pointer): DWord;

  TFileCopier = class(TPersistent)
  private
    fCopyCount: Integer;
    fFileCount: Integer;
    fFileSize: Int64;
    fCallBack: TCopyCallBack;
     function DoFolderFiles(const ASourcePath, ATargetPath: string; const Op: TFolderOp): Int64;
     function DoFolderTree(const ASourcePath, ATargetPath: string; const Op: TFolderOp): Int64;
  public
     constructor Create; virtual;
     function AddBackSlash(const S: String): string;
     function DoFiles(const ASourcePath, ATargetPath: string; const Op: TFolderOp): Int64;
     property CallBack: TCopyCallBack read fCallBack write fCallBack;
     property CopyCount: Integer read fCopyCount;
     property FileCount: Integer read fFileCount;
     property FileSize: Int64 read fFileSize;
  end;

implementation

{ TFileCopier }

function TFileCopier.AddBackSlash(const S: String): string;
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

function TFileCopier.DoFiles(const ASourcePath, ATargetPath: string;
  const Op: TFolderOp): Int64;
begin
  case Op of
   foCopy: fCopyCount := 0;
   foCount: fFileCount := 0;
   foSize: fFileSize:= 0;
  end;
  Result := DoFolderTree(ASourcePath, ATargetPath, Op);
end;

constructor TFileCopier.Create;
begin
  inherited;
  CallBack := nil;
end;

function TFileCopier.DoFolderFiles( const ASourcePath, ATargetPath: string;
                                    const Op: TFolderOp): Int64;
// Return -1: failed/error x: count of to or count of copied or Size of all files
// Root paths must exist
var
  StrName,
  MySearchPath,
  MyTargetPath,
  MySourcePath: string;
  FindRec: TSearchRec;
  i: Integer;
  Cancelled: Boolean;
  Attributes: WIN32_FILE_ATTRIBUTE_DATA;
begin
  Result := 0;
  Cancelled := False;
  MyTargetPath := AddBackSlash(ATargetPath);
  MySourcePath := AddBackSlash(ASourcePath);
  MySearchPath := AddBackSlash(ASourcePath) + '*.*';
  i := FindFirst(MySearchPath, 0 , FindRec);
  try
    while (i = 0) and (Result <> -1) do
    begin
      try
        case op of
          foCopy: begin
            StrName := MySourcePath + FindRec.Name;
            if CopyFileEx(PWideChar(StrName), PWideChar(MyTargetPath + FindRec.Name), @fCallBack, nil, @Cancelled, COPY_FILE_FAIL_IF_EXISTS) then
            begin
              inc(Result);
              inc(fCopyCount);
            end
            else
              Result := -1;
          end;
          foCount:
          begin
            Inc(Result);
            Inc(fFileCount);
          end;
          foSize:
          begin
            Result := Result + FindRec.Size;
            fFileSize := fFileSize + FindRec.Size;
          end;
        end; // case
        except
          Result := -1;
        end;
      i := FindNext(FindRec);
    end;
  finally
    FindClose(FindRec);
  end;

end;

function TFileCopier.DoFolderTree( const ASourcePath, ATargetPath: string;
                                     const Op: TFolderOp): Int64;
// Return -1: failed/error x: count of to or count of copied or Size of all files
// Root paths must exist
// Recursive
var
  FindRec: TSearchRec;
  StrName, StrExt,
  MySearchPath,
  MyTargetPath,
  MySourcePath: string;
  InterimResult :Int64;
  i: Integer;
begin
  Result := 0;
  // Find Folders
  MySearchPath := AddBackSlash(ASourcePath) + '*.*';
  MySourcePath := AddBackSlash(ASourcePath);
  MyTargetPath := AddBackSlash(ATargetPath);
  i := FindFirst(MySearchPath, faDirectory , FindRec);
  try
    while (i = 0) and (Result <> -1) do
    begin
      StrName := FindRec.Name;
      if (Bool(FindRec.Attr and faDirectory)) and (StrName <> '.') and (StrName <> '..') then
      begin
        try
          case op of
           foCopy:
             if CreateDir(MyTargetPath + StrName) then
              begin
                InterimResult := DoFolderTree(MySourcePath + StrName, MyTargetPath + StrName, Op);
                if InterimResult <> -1 then
                begin
                  Result := Result + InterimResult;
                  fCopyCount := Result;
                end
                else
                  Result := -1;
              end; // foCopy
           foCount, foSize:
           begin
             InterimResult := DoFolderTree(MySourcePath + StrName, MyTargetPath + StrName, Op);
             if InterimResult <> -1 then
               Result := Result + InterimResult
             else
               Result := -1;  // or result, -1 easier to read
           end; // foCount, foSize
          end; // case
        except
          Result := -1;
        end;
      end;
      i := FindNext(FindRec);
    end;
  finally
    FindClose(FindRec);
  end;
  if Result <> -1 then
  case op of
   foCopy:
    begin
     InterimResult := DoFolderFiles( AddBackSlash(ASourcePath), AddBackSlash(ATargetPath), Op);
     if InterimResult <> -1 then
     begin
       Result := Result + InterimResult;
       fCopyCount := Result;
     end
     else
       Result := InterimResult;
    end;
   foCount:
   begin
     InterimResult := DoFolderFiles(AddBackSlash(ASourcePath), AddBackSlash(ATargetPath), Op);
     if InterimResult <> -1 then
     begin
       Result := Result + InterimResult;
       fFileCount := Result;
     end
     else
       Result := InterimResult;
   end; // foCount
   foSize:
   begin
     InterimResult := DoFolderFiles(AddBackSlash(ASourcePath), AddBackSlash(ATargetPath), Op);
     if InterimResult <> -1 then
     begin
       Result := Result + InterimResult;
       fFileSize := Result;
     end
     else
       Result := InterimResult;
   end; // foSize
  end; // case
end;


end.
