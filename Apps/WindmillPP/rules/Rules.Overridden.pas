unit Rules.Overridden;

interface

uses
  Rules.Interfaces,
  Output.Interfaces,
  System.Classes,
  uPascalDefs;

type
  TRulesOverridden = class(TInterfacedObject, IRulesOnMethod)
  private
    FOutput: IOutput;
  public
    constructor Create(const Output: IOutput);

    procedure Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils;

constructor TRulesOverridden.Create(const Output: IOutput);
begin
  FOutput := Output;
end;

procedure TRulesOverridden.Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
var
  Idx: Integer;
  ExpectInherited: Boolean;
  ExpectResult: Boolean;
  ContainsInherited: Boolean;
  AssignedToResult: Boolean;
begin
  ExpectInherited := Method.IsVirtualOverride;
  if ExpectInherited then
  begin
    ExpectResult := (Method.Functype <> 'void') and (Method.Functype <> 'constructor') and (Method.Functype <> 'destructor');
    ContainsInherited := False;
    AssignedToResult := False;

    for Idx := 0 to Lines.Count - 1 do
    begin
      ContainsInherited := ContainsText(Lines[Idx], 'inherited');

      if ContainsInherited and ExpectResult then
        AssignedToResult := ContainsText(Lines[Idx], ':=');

      if ContainsInherited then
        Break;
    end;

    if not ContainsInherited then
    begin
      FOutput.Warning(Filepath, Method.LineNumber + 1, 'inherited missing from overridden method');
    end;

    if ContainsInherited and ExpectResult and not AssignedToResult then
    begin
      FOutput.Warning(Filepath, Method.LineNumber + Idx + 1, 'inherited result not assigned to anything');
    end;
  end;
end;

end.
