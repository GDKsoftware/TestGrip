unit Input.Params.CmdLine;

interface

uses
  Input.Params.Interfaces;

type
  TInputParamsCmdLine = class(TInterfacedObject, IInputParams)
  public
    function Count: Integer;
    function Str(const Index: Integer): string;
  end;

implementation

function TInputParamsCmdLine.Count: Integer;
begin
  Result := ParamCount;
end;

function TInputParamsCmdLine.Str(const Index: Integer): string;
begin
  Result := ParamStr(Index);
end;

end.
