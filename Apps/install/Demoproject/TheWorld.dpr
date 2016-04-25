program TheWorld;

uses
  Forms,
  FMain in 'FMain.pas' {frmWorld},
  uWorld in 'uWorld.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWorld, frmWorld);
  Application.Run;
end.
