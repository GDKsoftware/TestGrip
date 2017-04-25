unit Rules.Method.All;

interface

uses
  uPascalDefs,
  System.Classes,
  Rules.Interfaces,
  Output.Interfaces;

type
  TRulesMethodAll = class(TInterfacedObject, IRulesOnMethod)
  private
    FOutput: IOutput;
  public
    constructor Create(const Output: IOutput);

    procedure Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
  end;

implementation

uses
  Rules.Without, Rules.Overridden, Rules.QueryStrings;

constructor TRulesMethodAll.Create(const Output: IOutput);
begin
  FOutput := Output;
end;

procedure TRulesMethodAll.Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
var
  CurrentRule: IRulesOnMethod;
begin
  // first one...
  CurrentRule := TRulesWithout.Create(FOutput);
  CurrentRule.Process(Filepath, ClassDef, Method, Lines);

  // next...
  CurrentRule := TRulesOverridden.Create(FOutput);
  CurrentRule.Process(Filepath, ClassDef, Method, Lines);

  // ..
  CurrentRule := TRulesQueryStrings.Create(FOutput);
  CurrentRule.Process(Filepath, ClassDef, Method, Lines);
end;

end.
