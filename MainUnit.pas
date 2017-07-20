unit MainUnit;

interface

uses
  // System includings
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.DateUtils, System.Generics.Collections,
  // FMX includings
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo,
  // Project includings
  SyncingUnit;

type
  TMainForm = class(TForm)
    TestButton: TButton;
    FileOpenEdit: TEdit;
    FileOpenButton: TButton;
    OpenDialog: TOpenDialog;
    TestMemo: TMemo;
    procedure FileOpenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  Syncher: TSyncher;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Syncher := TSyncher.Create;

  //OpenDialog.Options := [TOpenOption.ofdoPickFolders];
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Syncher.Destroy;
end;

procedure TMainForm.FileOpenButtonClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    FileOpenEdit.Text := OpenDialog.FileName;

  OpenDialog.OptionsEx
end;

procedure TMainForm.TestButtonClick(Sender: TObject);
var
  s: string;
  l: TList<string>;
begin
  TestMemo.Lines.Clear;
  l := Syncher.GetAllItems(FileOpenEdit.Text);
  for s in l do
    TestMemo.Lines.Add(s);
  l.Destroy;
end;

end.
