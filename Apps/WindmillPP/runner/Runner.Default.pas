unit Runner.Default;

interface

uses
  Runner.Interfaces,
  Output.Interfaces,
  System.Classes,
  uPascalDefs,
  uUnitParser,
  uUnitLines;

type
  TRunnerDefault = class(TInterfacedObject, IRunner)
  private
    FOutput: IOutput;
    FFilepath: string;
    FUnitParser: TUnitParser;
    FSourcecode: TFileStream;
    FUnitLines: TUnitLines;

    procedure InitParser;
    procedure ExecuteForMethod(const Method: TMethodDefinition);
  public
    constructor Create(const Output: IOutput);
    destructor Destroy; override;

    procedure Execute(const Filepath: string);
  end;

implementation

uses
  System.SysUtils,
  System.Contnrs,
  Rules.Interfaces,
  Rules.Method.All;

constructor TRunnerDefault.Create(const Output: IOutput);
begin
  FOutput := Output;
end;

destructor TRunnerDefault.Destroy;
begin
  FreeAndNil(FSourcecode);
end;

procedure TRunnerDefault.InitParser;
begin
  FreeAndNil(FSourcecode);
  FSourcecode := TFileStream.Create(FFilepath, fmOpenRead or fmShareDenyWrite);

  FUnitParser := TUnitParser.Create;
  FUnitParser.ParseUnit(FSourcecode, True, False, False, True, True);

  FUnitLines := TUnitLines.Create(FSourceCode, FUnitParser);
end;

procedure TRunnerDefault.ExecuteForMethod(const Method: TMethodDefinition);
var
  Rule: IRulesOnMethod;
  Lines: TStrings;
  ClassIdx: Integer;
  ClassDef: TClassDefinition;
begin
  Lines := FUnitLines.GetLinesForMethod(Method);
  try
    Rule := TRulesMethodAll.Create(FOutput);

    ClassIdx := FUnitParser.InterfaceClassList.IndexOfName(Method.InClass);

    if ClassIdx <> -1 then
      ClassDef := TClassDefinition(FUnitParser.InterfaceClassList.Objects[ClassIdx])
    else
      ClassDef := nil;

    Rule.Process(FFilepath, ClassDef, Method, Lines)
  finally
    Lines.Free;
  end;
end;

procedure TRunnerDefault.Execute(const Filepath: string);
var
  Idx: Integer;
  Method: TMethodDefinition;
  ClName: string;
  Parents: string;
begin
  FFilepath := Filepath;

  InitParser;

  for Idx := 0 to FUnitParser.MethodList.Count - 1 do
  begin
    Method := TMethodDefinition(FUnitParser.MethodList[Idx]);

    ClName := Method.InClass;
    Parents := FUnitParser.InterfaceClassList.Values[ClName];

    ExecuteForMethod(Method);
  end;
end;

end.
