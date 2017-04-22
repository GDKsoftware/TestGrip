unit Output.CmdLine;

interface

uses
  Output.Interfaces;

type
  TOutputCmdLine = class(TInterfacedObject, IOutput)
  private
    FWarningAsError: Boolean;
    FWarningCount: Integer;
    FErrorCount: Integer;
    FInfoCount: Integer;
  public
    procedure SetWarningAsError(const Yes: Boolean);
    procedure Warning(const Filepath: string; const Linenumber: Integer; const Warning: string);
    procedure Error(const Filepath: string; const Linenumber: Integer; const Error: string);
    procedure Info(const Info: string);
    function WarningCount: Integer;
    function ErrorCount: Integer;
    function InfoCount: Integer;
  end;

implementation

uses
  System.SysUtils;

procedure TOutputCmdLine.SetWarningAsError(const Yes: Boolean);
begin
  FWarningAsError := Yes;
end;

procedure TOutputCmdLine.Warning(const Filepath: string; const Linenumber: Integer; const Warning: string);
begin
  if FWarningAsError then
  begin
    Error(Filepath, Linenumber, Warning);
  end
  else
  begin
    WriteLn(ErrOutput, '[WindmillPP Warning] ' + ExtractFileName(FilePath) + '(' + Linenumber.ToString + '): ' + Warning);
    Inc(FWarningCount);
  end;
end;

procedure TOutputCmdLine.Error(const Filepath: string; const Linenumber: Integer; const Error: string);
begin
  WriteLn(ErrOutput, '[WindmillPP Error] ' + ExtractFileName(FilePath) + '(' + Linenumber.ToString + '): ' + Error);
  Inc(FErrorCount);
end;

procedure TOutputCmdLine.Info(const Info: string);
begin
  WriteLn(Info);
  Inc(FInfoCount);
end;

function TOutputCmdLine.WarningCount: Integer;
begin
  Result := FWarningCount;
end;

function TOutputCmdLine.ErrorCount: Integer;
begin
  Result := FErrorCount;
end;

function TOutputCmdLine.InfoCount: Integer;
begin
  Result := FInfoCount;
end;

end.
