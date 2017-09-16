program Syncher;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  SynchingUnit in 'SynchingUnit.pas',
  CRCHash in 'CRCHash.pas',
  SyncherItemUnit in 'SyncherItemUnit.pas';

{$R *.res}

begin
{IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
