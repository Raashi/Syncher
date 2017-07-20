unit MainUnit;

interface

uses
  // System includings
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.DateUtils,
  // FMX includings
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  // Project includings
  IdHashMessageDigest, idHash, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TMainForm = class(TForm)
    HashButton: TButton;
    FileOpenEdit: TEdit;
    FileOpenButton: TButton;
    OpenDialog: TOpenDialog;
    procedure FileOpenButtonClick(Sender: TObject);
    procedure HashButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FileOpenButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FileOpenEdit.Text := OpenDialog.FileName;
end;

procedure TMainForm.HashButtonClick(Sender: TObject);
var
  idmd5: TIdHashMessageDigest5;
  fs: TFileStream;
  hash: T4x4LongWordRecord;
  h: string;
  sec: integer;
  t: TDateTime;
begin
  h := '';
  idmd5 := TIdHashMessageDigest5.Create;
  t := Now;
  fs := TFileStream.Create(FileOpenEdit.Text, fmOpenRead OR fmShareDenyWrite);
  try
    h := idmd5.HashBytesAsHex(idmd5.HashStream(fs));
  finally
    fs.Free;
    idmd5.Free;
  end;
  sec := MillisecondsBetween(t, Now);
  ShowMessage(sec.ToString + ' ' + h);
end;

end.
