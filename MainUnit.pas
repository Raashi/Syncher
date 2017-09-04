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
    TestMemo: TMemo;
    SyncingFoldersButton: TButton;
    SyncingFoldersMemo: TMemo;
    MainDirectoryLabel: TLabel;
    SyncingFoldersLabel: TLabel;
    RecycleCheckBox: TCheckBox;
    procedure FileOpenButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure SyncingFoldersButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.SyncingFoldersButtonClick(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Выберите папку для синхронизации', '', Directory) then
    SyncingFoldersMemo.Lines.Add(Syncher.AddBackSlash(Directory));
end;

procedure TMainForm.FileOpenButtonClick(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Выберите главную папку', '', Directory) then
    FileOpenEdit.Text := Syncher.AddBackSlash(Directory);
end;

procedure TMainForm.TestButtonClick(Sender: TObject);
begin
  Syncher.SyncFolder(FileOpenEdit.Text, SyncingFoldersMemo.Lines[0], RecycleCheckBox.IsPressed);
end;

end.
