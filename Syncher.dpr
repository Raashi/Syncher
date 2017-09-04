program Syncher;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  FileCopierU in 'FileCopierU.pas',
  SyncingUnit in 'SyncingUnit.pas';

{$R *.res}

begin
{IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
