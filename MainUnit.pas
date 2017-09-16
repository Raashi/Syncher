unit MainUnit;

interface

uses
  // System includings
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.DateUtils, System.Generics.Collections,
  // FMX includings
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit, FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox,
  FMX.Memo,
  // Project includings
  SynchingUnit;

type
  TMainForm = class(TForm)
    TestButton: TButton;
    FileOpenEdit1: TEdit;
    FileOpenButton1: TButton;
    TestMemo: TMemo;
    FileOpenButton2: TButton;
    FileOpenLabel1: TLabel;
    FileOpenLabel2: TLabel;
    RecycleCheckBox: TCheckBox;
    FileOpenEdit2: TEdit;
    SwapButton: TSpeedButton;
    MainFolderCheckBox: TCheckBox;
    procedure FileOpenButton1Click(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure FileOpenButton2Click(Sender: TObject);
    procedure FileOpenEdit1Change(Sender: TObject);
    procedure SwapButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FileOpenButton2Click(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Выберите папку для синхронизации', '', Directory) then
    FileOpenEdit2.Text := Syncher.AddBackSlash(Directory);
end;

procedure TMainForm.FileOpenEdit1Change(Sender: TObject);
begin
  Syncher.Analyzed := false;
end;

procedure TMainForm.SwapButtonClick(Sender: TObject);
var
  s: string;
begin
  s := FileOpenEdit1.Text;
  FileOpenButton1.Text := FileOpenEdit2.Text;
  FileOpenEdit2.Text := s;
end;

procedure TMainForm.FileOpenButton1Click(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Выберите первую папку', '', Directory) then
    FileOpenEdit1.Text := Syncher.AddBackSlash(Directory);
end;

procedure TMainForm.TestButtonClick(Sender: TObject);
var
  Options: TSynchingOptions;
  Report: TStringList;
begin
  Options := TSynchingOptions.Create(TSynchRelations(MainFolderCheckBox.IsChecked.ToInteger), csShowConflict);

  Report := Syncher.AnalyzeSynching(FileOpenEdit1.Text, FileOpenEdit2.Text, Options);
  TestMemo.Lines.AddStrings(Report);
  Report.Destroy;
end;

end.
