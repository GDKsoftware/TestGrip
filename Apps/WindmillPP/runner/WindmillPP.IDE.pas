unit WindmillPP.IDE;

interface

uses
  Runner.Interfaces, Output.Interfaces, Feeder.Interfaces;

type
  TWindmillPPIDE = class(TInterfacedObject, IWindMillPP)
  private
    FOutput: IOutput;
  public
    constructor Create(const Output: IOutput);

    procedure Go;
  end;

implementation

uses
  ToolsAPI, Feeder.Project, Runner.Default, System.SysUtils, System.IOUtils;

constructor TWindmillPPIDE.Create(const Output: IOutput);
begin
  inherited Create;

  FOutput := Output;
end;

procedure TWindmillPPIDE.Go;
var
  IDEProject: IOTAProject;
  Feeder: IFeeder;
  Runner: IRunner;
  Filepath: string;
  ProjectFilepath: string;
begin
  Runner := TRunnerDefault.Create(FOutput);

  IDEProject := ToolsAPI.GetActiveProject;
  if Assigned(IDEProject) then
  begin
    ProjectFilepath := TPath.GetFullPath(IDEProject.FileName);

    Feeder := TFeederProject.Create(ProjectFilepath);
    while not Feeder.Eof do
    begin
      Filepath := Feeder.NextFile;

      Runner.Execute(Filepath);
    end;
  end;
end;

end.
