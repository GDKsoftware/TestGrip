unit WindmillPP.CmdLine;

interface

uses
  Runner.Interfaces,
  Output.Interfaces,
  Input.Params.Interfaces,
  Feeder.Interfaces;

type
  TWindmillPPCmdLine = class(TInterfacedObject, IWindmillPP)
  private
    FProjectFilepath: string;
    FOutput: IOutput;
    FInputParams: IInputParams;

    procedure RunOnUnit(const Filepath: string);
    procedure RunOnProject;
    procedure ProcessCommandLineParameters;
  public
    constructor Create(const InputParams: IInputParams; const Output: IOutput);

    procedure Go;
  end;

implementation

uses
  System.SysUtils,
  Feeder.Project,
  Runner.Default;

constructor TWindmillPPCmdLine.Create(const InputParams: IInputParams; const Output: IOutput);
begin
  FOutput := Output;
  FInputParams := InputParams;
end;

procedure TWindmillPPCmdLine.ProcessCommandLineParameters;
begin
  if FInputParams.Count >= 1 then
  begin
    FProjectFilepath := FInputParams.Str(1);

    if not FileExists(FProjectFilePath) then
    begin
      FOutput.Info('Please supply an Existing .dproj file');
      Abort;
    end;
  end
  else
  begin
    FOutput.Info('Please supply a .dproj file');
    Abort;
  end;
end;

procedure TWindmillPPCmdLine.Go;
begin
  ProcessCommandLineParameters;
  RunOnProject;
end;

procedure TWindmillPPCmdLine.RunOnProject;
var
  FileFeeder: IFeeder;
begin
  if FProjectFilepath.EndsWith('.pas', True) then
  begin
    RunOnUnit(FProjectFilepath);
  end
  else
  begin
    FileFeeder := TFeederProject.Create(FProjectFilepath);

    while not FileFeeder.Eof do
    begin
      RunOnUnit(FileFeeder.NextFile);
    end;
  end;
end;

procedure TWindmillPPCmdLine.RunOnUnit(const Filepath: string);
var
  Runner: IRunner;
begin
  if not FileExists(FilePath) then
  begin
    FOutput.Warning(Filepath, 0, 'Unit doesn''t exist, but is added to dproject file');
  end
  else
  begin
    Runner := TRunnerDefault.Create(FOutput);
    Runner.Execute(Filepath);
  end;
end;

end.
