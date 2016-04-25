unit uUnitTestGenBase;

interface

uses
  uPascalFileGen, uUnitTestGenIntf, uTestDefs,
  Classes;

type
  TUnitTestClassFileGenBase = class(TPascalFileGen, IUnitTestClassFileGen)
  protected
    FUnitTestClassName: string;
    FSetupCode: string;
    FTests: TList;

    function GetReturnType(const lstFunctionRef: TList; const aFunction: TInputFunction): string;

    function GetUnitTestClassName: string;
    procedure SetUnitTestClassName(const Value: string);

    function GetSetupCode: string;
    procedure SetSetupCode(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GenerateTest(const aClass: TInputTestClass; const aFunction: TInputFunction; const aTest: TInputTest; const sTestClassName: string; const lstFunctionRef: TList = nil); virtual; abstract;
    function GetTemplateCode: string; virtual; abstract;

    property TestList: TList
      read FTests;
  end;

implementation

uses
  Contnrs, SysUtils, uPascalDefs;

{ TUnitTestClassFileGenBase }

constructor TUnitTestClassFileGenBase.Create;
begin
  inherited;

  FTests := TObjectList.Create(True);

  FUnitTestClassName := c_inhclassname;
  FSetupCode := '';
end;

destructor TUnitTestClassFileGenBase.Destroy;
begin
  FreeAndNil(FTests);

  inherited;
end;

function TUnitTestClassFileGenBase.GetSetupCode: string;
begin
  Result := FSetupCode;
end;

function TUnitTestClassFileGenBase.GetUnitTestClassName: string;
begin
  Result := FUnitTestClassName;
end;

procedure TUnitTestClassFileGenBase.SetSetupCode(const Value: string);
begin
  FSetupCode := Value;
end;

procedure TUnitTestClassFileGenBase.SetUnitTestClassName(const Value: string);
begin
  FUnitTestClassName := Value;
end;

function TUnitTestClassFileGenBase.GetReturnType(const lstFunctionRef: TList; const aFunction: TInputFunction): string;
var
  aMethodDef: TMethodDefinition;
  i, c: Integer;
begin
  Result := '';

  if Assigned(lstFunctionRef) then
  begin
    c := lstFunctionRef.Count - 1;
    for i := 0 to c do
    begin
      aMethodDef := TMethodDefinition(lstFunctionRef[i]);
      if SameText(aMethodDef.DefMethodName, aFunction.MethodName) then
      begin
        Result := aMethodDef.FuncType;
      end;
    end;
  end;
end;

end.
