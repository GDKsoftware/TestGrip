unit uTestDefs;

interface

uses
  Classes, uPascalDefs, Contnrs;

type
  TInputConstant = class;
  TInputParam = class;
  TInputTest = class;
  TInputFunction = class;
  TInputTestClass = class;
  TInputCommonClass = class;
  TXMLTagObject = class;

  TXMLTagObject = class(TObject)
  protected
    FDestroyed: boolean;
  public

    function ToXmlString: string; virtual; abstract;
    function ToResultXmlString: string; virtual;

    constructor Create; virtual;
    destructor Destroy; override;

  end;

  // Base class with recurring fields
  TInputCommonClass = class(TXMLTagObject)
  protected
  public
    Vars        : string;
    Defines     : string;
    Description : string;
    InitCode    : string;

    procedure CloneFrom( const anObj: TInputCommonClass ); virtual;

    function ToXmlString: string; override;
  end;

  // Class contains one global defined constant
  TInputConstant = class(TXMLTagObject)
  protected
  public
    ConstantName  : string;
    ConstantValue : string;

    procedure CloneFrom( const aSource: TInputConstant ); virtual;

    function ToXmlString: string; override;
  end;

  // Class contains one function parameter
  TInputParam = class(TXMLTagObject)
  protected
  public
    ParamName  : string;
    ParamValue : string;
    ParamType  : string;

    IsComplexType: boolean;

    procedure CloneFrom( const aSource: TInputParam );

    function ToXmlString: string; override;
  end;

  // Class contains one evalution statement
  TInputImplier = class(TXMLTagObject)
  protected
  public
    Eval : string;

    TestResult : boolean;
    TestResultExpected: string;
    TestResultWas: string;

    constructor Create; override;
    destructor Destroy; override;

    procedure CloneFrom( const aSource: TInputImplier );

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  // Class defines one testscenario
  TInputTest = class(TInputCommonClass)
  protected
    FParent: TInputFunction;
    FTestResultString  : string;

    FTestResultEqualsExpected: string;
    FTestResultEqualsWas: string;

    procedure SetTestResultString( const s: string );

    function EqualsNodeToXml: string;
    function EqualsTestResultNodeToXml: string;
  public
    DisplayName : string;
    ParamList   : TObjectList;
    Equals      : variant;
    EqualsNot   : boolean;
    PreImpliesCode: string;
    Implies     : TObjectList;

    TestResult        : boolean;
    EqualsTestResult  : boolean;

    property TestResult_EqualsExpected: string
      read FTestResultEqualsExpected;
    property TestResult_EqualsWas: string
      read FTestResultEqualsWas;

    property TestResultString: string
      read FTestResultString write SetTestResultString;

    property Parent: TInputFunction
      read FParent;

    constructor Create( const aParent: TInputFunction = nil );
    destructor Destroy; override;

    procedure CloneFrom( const aTest: TInputCommonClass ); override;
    procedure CopyAllImpliersFrom( const aTest: TInputTest );
    function CopyImpliesFrom(const aImplier: TInputImplier): TInputImplier;
    procedure CopyAllParamsFrom( const aTest: TInputTest );

    procedure ClearInputParams;
    procedure ClearImplies;
    procedure ClearImpliesTestResults;

    function AddInputParam(const ParamName: string; const ParamValue: string; const ParamType: string ): TInputParam;
    function GetInputParam(const ParamName: string; var InputParam: TInputParam): boolean;
    function AddImplies(const Eval: string): TInputImplier;
    function GetInputImplier(const Eval: string; var InputImplier: TInputImplier): boolean;

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  TInputIndependentTest = class(TInputTest)
  protected
  public

  end;

  TInputIndependentTests = class(TXMLTagObject)
  protected
    FList: TObjectList;
  public

  end;

  TInputSpecialCheck = class(TXMLTagObject)
  protected
    FCheckType: string;
    FCheckEquals: string;

    FTestResultEqualsExpected: string;
    FTestResultEqualsWas: string;

    FTestResult: boolean;
  public
    property TestResult: boolean
      read FTestResult write FTestResult;

    property CheckType: string
      read FCheckType write FCheckType;
    property CheckEquals: string
      read FCheckEquals write FCheckEquals;

    property TestResult_EqualsExpected: string
      read FTestResultEqualsExpected;
    property TestResult_EqualsWas: string
      read FTestResultEqualsWas;

    procedure RegisterTestResult(const sExpected, sActual: string);

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  TInputUrlTest = class(TInputIndependentTest)
  protected
    FSpecialChecks: TObjectList;
    FUrl: string;
    FMethod: string;
  public
    property Url: string
      read FUrl write FUrl;
    property Method: string
      read FMethod write FMethod;

    property SpecialChecks: TObjectList
      read FSpecialChecks;

    procedure RegisterEqualsTestResult(const sExpected, sActual: string);

    function AddSpecialCheck(const sCheckType: string; const sCheckEquals: string): TInputSpecialCheck;

    procedure CloneFrom( const aTest: TInputCommonClass ); override;

    constructor Create( const aParent: TInputIndependentTests = nil );
    destructor Destroy; override;

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  // Class defines tests per function
  TInputFunction = class(TInputCommonClass)
  protected
    FParent: TInputTestClass;
    FCachedType: string;
    FTotalTestResult: TStrings;
    FAllTestPassed: boolean;
    FCompileResult : TStrings;
    FCompileCommand: TStrings;
    FRunLog: TStrings;
    FSignatureHint: string;


    function GetTestCount: integer;
    function GetTestResult: boolean;

  public
    MethodName    : string;
    Compiled      : boolean;
    TestList      : TObjectList;

    UseCustomSetupCode: boolean;
    CustomSetupCode: string;

    property CachedType: string
      read FCachedType write FCachedType;

    property TotalTestResult: TStrings
      read FTotalTestResult;
    property RunLog: TStrings
      read FRunLog;
    property CompileResult: TStrings
      read FCompileResult;
    property CompileCommand: TStrings
      read FCompileCommand;

    property TestResult: boolean
      read GetTestResult;

    property TestCount: integer
      read GetTestCount;
    property Parent: TInputTestClass
      read FParent;
    property SignatureHint: string
      read FSignatureHint write FSignatureHint;

    function GuessSignature: string;

    constructor Create(const aParent: TInputTestClass = nil);
    destructor Destroy; override;

    procedure CloneFrom( const ASourceFunction : TInputFunction );
    function AddInputTest(const TestName: string): TInputTest;
    function GetInputTest(const TestName: string; var InputTest: TInputTest): boolean;

    procedure MarkTestResult( iIndex: integer; bOk: boolean );
    procedure MarkTestResultString( iIndex: integer; sTestResult: string);

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  // Class containing functions with tests defined
  TInputTestClass = class(TInputCommonClass)
  protected
  public
    Name        : string;
    ClassName   : string;
    Constants   : TObjectList;
    FunctionList: TObjectList;

    ExtraUses   : string;

    UseCustomSetupCode: boolean;
    CustomSetupCode: string;

    constructor Create; override;
    destructor Destroy; override;

    procedure CloneFrom( const aClass: TInputTestClass );

    function AddInputFunction(const FunctionName: string): TInputFunction; overload;
    function AddInputFunction(const mdef: TMethodDefinition): TInputFunction; overload;
    function GetInputFunction(const FunctionName: string; var InputFunction: TInputFunction): boolean; overload;
    function GetInputFunction(const mdef: TMethodDefinition; var InputFunction: TInputFunction): boolean; overload;

    function GetOrAddInputFunction(const AMethodDefinition: TMethodDefinition): TInputFunction;

    function ToXmlString: string; override;
    function ToResultXmlString: string; override;
  end;

  procedure SaveTestResultFile( const sResultFilename: string; const sSourceFileName: string; const aClass: TInputTestClass; const sTotalTestTime: string = '' );

implementation

uses
  uXmlFuncs, uConst, Variants, SysUtils, StrUtils, uCommonFunctions, uUTF8Functions{$ifdef VER150}, uD7Functions{$endif};

{ TInputTest }

function TInputTest.AddImplies(const Eval: string): TInputImplier;
begin
  result := TInputImplier.Create;
  result.Eval := Eval;
  Implies.Add(result);
end;

function TInputTest.AddInputParam(const ParamName,
  ParamValue, ParamType: string): TInputParam;
begin
  result := TInputParam.Create;
  result.ParamName := ParamName;
  if Pos(#13#10, ParamValue) = 0 then
  begin
    result.ParamValue := ReplaceStr(ParamValue, #10, #13#10);
  end
  else
  begin
    result.ParamValue := ParamValue;
  end;
  result.ParamType := ParamType;
  ParamList.Add(result);
end;

procedure TInputTest.ClearImplies;
begin
  Implies.Clear;
end;

procedure TInputTest.ClearImpliesTestResults;
var
  i, c: integer;
  implier: TInputImplier;
begin
  c := Implies.Count - 1;
  for i := 0 to c do
  begin
    implier := TInputImplier(Implies[i]);

    implier.TestResult := True;
    implier.TestResultExpected := '';
    implier.TestResultWas := '';
  end;
end;

procedure TInputTest.ClearInputParams;
begin
  ParamList.Clear;
end;

procedure TInputTest.CloneFrom( const aTest: TInputCommonClass );
var
  aParam: TInputParam;
begin

  inherited CloneFrom(aTest);

  if aTest is TInputTest then
  begin
    DisplayName       := TInputTest(aTest).DisplayName;
    Equals            := TInputTest(aTest).Equals;
    EqualsNot         := TInputTest(aTest).EqualsNot;
    TestResult        := TInputTest(aTest).TestResult;
    EqualsTestResult  := TInputTest(aTest).EqualsTestResult;
    PreImpliesCode    := TInputTest(aTest).PreImpliesCode;
    FTestResultString := TInputTest(aTest).TestResultString; // niet SetTestResultString uitvoeren hier

    CopyAllParamsFrom( TInputTest(aTest) );
  end;

end;

procedure TInputTest.CopyAllImpliersFrom(const aTest: TInputTest);
var
  i, c: integer;
  implier: TInputImplier;
begin
  c := aTest.Implies.Count - 1;
  for i := 0 to c do
  begin
    implier := TInputImplier(aTest.Implies[i]);
    if Assigned(implier) then
    begin
      Self.CopyImpliesFrom(implier);
    end;
  end;
end;

procedure TInputTest.CopyAllParamsFrom(const aTest: TInputTest);
var
  i, c: integer;
  param: TInputParam;
begin
  c := aTest.ParamList.Count - 1;
  for i := 0 to c do
  begin
    param := TInputParam(aTest.ParamList[i]);
    if Assigned(param) then
    begin
      Self.AddInputParam( param.ParamName, param.ParamValue, param.ParamType );
    end;
  end;
end;

function TInputTest.CopyImpliesFrom(
  const aImplier: TInputImplier): TInputImplier;
begin
  Result := TInputImplier.Create;
  Result.CloneFrom( aImplier );
  Implies.Add(Result);
end;

constructor TInputTest.Create(const aParent: TInputFunction);
begin
  inherited Create;

  ParamList := TObjectList.Create(True);
  Implies := TObjectList.Create(True);

  Equals := Null;
  EqualsNot := False;

  TestResult := False;
  EqualsTestResult := False;

  FParent := aParent;
end;

destructor TInputTest.Destroy;
begin
  Implies.Free;
  ParamList.Free;

  inherited;
end;

function TInputTest.EqualsNodeToXml: string;
var
  sEquals: string;
begin
  sEquals := '';
  if not VarIsNull(Equals) then
  begin
    sEquals := Equals;
  end;

  Result := '';
  if sEquals = '' then
  begin
    if EqualsNot then
    begin
      Result := Result + '<' + nodenameEquals + ' not="true" />';
    end
    else
    begin
      Result := Result + '<' + nodenameEquals + ' not="false" />';
    end;
  end
  else
  begin
    if TCommonStringFunctions.ContainsNonVisibleOrCRLFCharacters(sEquals) then  // eigenlijk bij alles wat < 48 en > 127 is (of iets dergelijks)
    begin
      sEquals := TBase64.Encode(sEquals);

      if EqualsNot then
      begin
        Result := Result + '<' + nodenameEquals + ' not="true" xmlns:dt="urn:schemas-microsoft-com:datatypes" dt:dt="binary.base64">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
      end
      else
      begin
        Result := Result + '<' + nodenameEquals + ' not="false" xmlns:dt="urn:schemas-microsoft-com:datatypes" dt:dt="binary.base64">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
      end;
    end
    else
    begin
      if EqualsNot then
      begin
        Result := Result + '<' + nodenameEquals + ' not="true">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
      end
      else
      begin
        Result := Result + '<' + nodenameEquals + ' not="false">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
      end;
    end;
  end;
end;

function TInputTest.EqualsTestResultNodeToXml: string;
var
  sEquals: string;
  sTestResult: string;
begin
  sTestResult := 'false';
  if TestResult then
  begin
    sTestResult := 'true';
  end;

  sEquals := '';
  if not VarIsNull(Equals) then
  begin
    sEquals := Equals;
  end;

  Result := '';
  if sEquals = '' then
  begin
    if EqualsNot then
    begin
      Result := Result + '<' + nodenameEquals + ' not="true" result="' + sTestResult + '" />';
    end
    else
    begin
      Result := Result + '<' + nodenameEquals + ' not="false" result="' + sTestResult + '" />';
    end;
  end
  else
  begin
    if EqualsNot then
    begin
      Result := Result + '<' + nodenameEquals + ' not="true" result="' + sTestResult + '">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
    end
    else
    begin
      Result := Result + '<' + nodenameEquals + ' not="false" result="' + sTestResult + '">' + XmlFuncs_Escape(sEquals) + '</' + nodenameEquals + '>';
    end;
  end;
end;

function TInputTest.GetInputImplier(const Eval: string;
  var InputImplier: TInputImplier): boolean;
var
  i, c: integer;
begin
  result := false;
  c := Implies.Count - 1;
  for i := 0 to c do
  begin
    if TInputImplier(Implies.Items[i]).Eval = Eval then
    begin
      InputImplier := TInputImplier(Implies.Items[i]);
      result := true;
      break;
    end;
  end;
end;

function TInputTest.GetInputParam(const ParamName: string;
  var InputParam: TInputParam): boolean;
var
  I: Integer;
begin
  result := false;
  for I := 0 to ParamList.count - 1 do
  begin
    if TInputParam(ParamList.Items[i]).ParamName = ParamName then
    begin
      InputParam := TInputParam(ParamList.Items[i]);
      result := true;
      break;
    end;
  end;
end;

procedure TInputTest.SetTestResultString(const s: string);
var
  p0: integer;
  p1, p2, p3: integer;
  i: integer;
  implier: TInputImplier;
begin
  FTestResultString := s;
  FTestResultEqualsExpected := '';
  FTestResultEqualsWas := '';

  p0 := Pos( 'test_implies', s );

  //'      "test_equals, expected: <9> but was: <8>"' +
  if Pos( 'test_equals', s ) <> 0 then
  begin
    p1 := PosEx( 'expected: <', s, 1 );
    if p1 <> 0 then
    begin
      p2 := PosEx( '> but was: <', s, p1 + 11 );
      if p2 <> 0 then
      begin
        FTestResultEqualsExpected := Copy( s, p1 + 11, p2 - p1 - 11 );
        p3 := TCommonStringFunctions.RPos( '>', s );
        if p3 <> 0 then
        begin
          FTestResultEqualsWas := Copy( s, p2 + 12, p3 - p2 - 12 );
        end;
      end;
    end;
  end
  else if p0 <> 0 then
  begin

    Inc(p0, 13);
    p1 := PosEx(',',s,p0);
    i := StrToIntDef( Copy(s, p0, p1 - p0), -1 );
    if i <> -1 then
    begin
      if i <= self.Implies.Count then
      begin
        implier := TInputImplier(Self.Implies[i - 1]);

        p1 := PosEx( 'expected: <', s, 1 );
        if p1 <> 0 then
        begin
          p2 := PosEx( '> but was: <', s, p1 + 11 );
          if p2 <> 0 then
          begin
            implier.TestResult := False;

            implier.TestResultExpected := Copy( s, p1 + 11, p2 - p1 - 11 );
            p3 := TCommonStringFunctions.RPos( '>', s );
            if p3 <> 0 then
            begin
              implier.TestResultWas := Copy( s, p2 + 12, p3 - p2 - 12 );
            end;
          end;
        end;
      end;
    end;

  end;

end;

function TInputTest.ToResultXmlString: string;
var
  i, c: integer;
  sTestResultOk: string;
begin
  if TestResult then
  begin
    sTestResultOk := 'true';
  end
  else
  begin
    sTestResultOk := 'false';
  end;

  Result :=
    '<' + nodenameTestScenario + ' displayname="' + XmlFuncs_Escape(DisplayName) + '" result="' + XmlFuncs_Escape(sTestResultOk) + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDesc, Description) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenamePreImpliesCode, PreImpliesCode) + #13#10;

  if ParamList.Count <> 0 then
  begin
    Result := Result + '<' +  nodenameParamsGroup + '>' + #13#10;
    c := ParamList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputParam(ParamList[i]).ToResultXmlString;
    end;
    Result := Result + '</' + nodenameParamsGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameParamsGroup + ' />' + #13#10;
  end;

  Result := Result + EqualsTestResultNodeToXml + #13#10;

  if Implies.Count <> 0 then
  begin
    Result := Result + '<' + nodenameImpliesGroup + '>' + #13#10;
    c := Implies.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputImplier(Implies[i]).ToResultXmlString;
    end;
    Result := Result + '</' + nodenameImpliesGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameImpliesGroup + ' />' + #13#10;
  end;

  Result := Result +
    '</' + nodenameTestScenario + '>' + #13#10;
end;

function TInputTest.ToXmlString: string;
var
  i, c: integer;
begin
  Result :=
    '<' + nodenameTestScenario + ' displayname="' + XmlFuncs_Escape(DisplayName) + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDesc, Description) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenamePreImpliesCode, PreImpliesCode) + #13#10;

  if ParamList.Count <> 0 then
  begin
    Result := Result + '<' +  nodenameParamsGroup + '>' + #13#10;
    c := ParamList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputParam(ParamList[i]).ToXMLString;
    end;
    Result := Result + '</' + nodenameParamsGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameParamsGroup + ' />' + #13#10;
  end;

  Result := Result + EqualsNodeToXml + #13#10;

  if Implies.Count <> 0 then
  begin
    Result := Result + '<' + nodenameImpliesGroup + '>' + #13#10;
    c := Implies.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputImplier(Implies[i]).ToXMLString;
    end;
    Result := Result + '</' + nodenameImpliesGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameImpliesGroup + ' />' + #13#10;
  end;

  Result := Result +
    '</' + nodenameTestScenario + '>' + #13#10;
end;

{ TInputTestClass }

function TInputTestClass.AddInputFunction(
  const FunctionName: string): TInputFunction;
begin
  result := TInputFunction.Create(Self);
  result.MethodName := FunctionName;
  FunctionList.Add(result);

end;

function TInputTestClass.AddInputFunction(
  const mdef: TMethodDefinition): TInputFunction;
begin
  result := TInputFunction.Create(Self);
  result.MethodName := mdef.DefMethodName;
  result.SignatureHint := mdef.Signature;
  FunctionList.Add(result);
end;

procedure TInputTestClass.CloneFrom(const aClass: TInputTestClass);
var
  i, c: integer;
  c1, c2: TInputConstant;
begin
  Name := aClass.Name;
  ClassName := aClass.ClassName;

  c := Constants.Count - 1;
  for i := 0 to c do
  begin
    c1 := TInputConstant(aClass.Constants[i]);
    c2 := TInputConstant.Create;
    c2.CloneFrom(c1);

  	Constants.Add( c2 );
  end;

  // function wordt later gekopieerd
//  c := FunctionList.Count - 1;
//  for i := 0 to c do
//  begin
//    m1 := aClass.FunctionList[i];
//    m2 := TInputFunction.Create;
//    m2.CloneFrom(m1);
//
//  	FunctionList.Add( m2 );
//  end;

  CustomSetupCode := aClass.CustomSetupCode;
  UseCustomSetupCode := aClass.UseCustomSetupCode;

  ExtraUses := aClass.ExtraUses;
end;

constructor TInputTestClass.Create;
begin
  inherited;

  FunctionList := TObjectList.Create(True);
  Constants    := TObjectList.Create(True);

  UseCustomSetupCode := False;
  CustomSetupCode := '';

  ExtraUses := '';
end;

destructor TInputTestClass.Destroy;
begin
  FunctionList.Free;
  Constants.Free;

  inherited;
end;

function TInputTestClass.GetInputFunction(const mdef: TMethodDefinition;
  var InputFunction: TInputFunction): boolean;
var
  i, c: integer;
  aFunc: TInputFunction;
begin
  Result := False;

  c := FunctionList.Count - 1;
  for i := 0 to c do
  begin
    aFunc := TInputFunction(FunctionList.Items[i]);

    if Pos(aFunc.GuessSignature, mdef.signature) <> 0 then
    begin
      result := True;
      InputFunction := aFunc;
    end;
  end;
end;

function TInputTestClass.GetOrAddInputFunction(const AMethodDefinition: TMethodDefinition): TInputFunction;
begin
  if not GetInputFunction(AMethodDefinition, Result) then
  begin
    Result := AddInputFunction(AMethodDefinition);
  end;
end;

function TInputTestClass.GetInputFunction(const FunctionName: string;
  var InputFunction: TInputFunction): boolean;
var
  I: Integer;
  LocalInputFunction: TInputFunction;
begin
  result := false;
  for I := 0 to FunctionList.Count - 1 do
  begin
    LocalInputFunction := TInputFunction(FunctionList.Items[i]);
    if LocalInputFunction.MethodName = FunctionName then
    begin
      result := true;
      InputFunction := LocalInputFunction;
    end;          
  end;
end;

function TInputTestClass.ToResultXmlString: string;
var
  i, c: integer;
begin
  Result := '<' + nodenameClass + ' name="' + XmlFuncs_Escape(ClassName) + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  if UseCustomSetupCode then
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'true') + #13#10;
  end
  else
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'false') + #13#10;
  end;
  Result := Result + XmlFuncs_QuickNode(nodenameCustomSetupCode, CustomSetupCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameExtraUses, ExtraUses) + #13#10;

  if Constants.Count <> 0 then
  begin
    Result := Result + '<variables>' + #13#10;
    c := Constants.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputConstant(Constants[i]).ToResultXmlString;
    end;
    Result := Result + '</variables>' + #13#10;
  end
  else
  begin
    Result := Result + '<variables />' + #13#10;
  end;

  if FunctionList.Count <> 0 then
  begin
    c := FunctionList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputFunction(FunctionList[i]).ToResultXmlString;
    end;
  end;

  Result := Result + '</' + nodenameClass + '>' + #13#10;
end;

function TInputTestClass.ToXmlString: string;
var
  i, c: integer;
begin
  Result := '<' + nodenameClass + ' name="' + XmlFuncs_Escape(ClassName) + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  if UseCustomSetupCode then
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'true') + #13#10;
  end
  else
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'false') + #13#10;
  end;
  Result := Result + XmlFuncs_QuickNode(nodenameCustomSetupCode, CustomSetupCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameExtraUses, ExtraUses) + #13#10;

  if Constants.Count <> 0 then
  begin
    Result := Result + '<variables>' + #13#10;
    c := Constants.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputConstant(Constants[i]).ToXmlString;
    end;
    Result := Result + '</variables>' + #13#10;
  end
  else
  begin
    Result := Result + '<variables />' + #13#10;
  end;


  if FunctionList.Count <> 0 then
  begin
    c := FunctionList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputFunction(FunctionList[i]).ToXmlString;
    end;
  end;

  Result := Result + '</' + nodenameClass + '>' + #13#10;
end;

{ TInputFunction }

function TInputFunction.AddInputTest(const TestName: string): TInputTest;
begin
  result := TInputTest.Create(Self);
  Result.DisplayName := TestName;
  TestList.Add(result);
end;

procedure TInputFunction.CloneFrom(const ASourceFunction: TInputFunction);
begin
  MethodName      := ASourceFunction.MethodName;
  Compiled        := ASourceFunction.Compiled;
  CustomSetupCode := ASourceFunction.CustomSetupCode;
  CachedType      := ASourceFunction.CachedType;

  // geen tests clonen, worden later toegevoegd

  TotalTestResult.Assign( ASourceFunction.TotalTestResult );
  CompileResult.Assign( ASourceFunction.CompileResult );
end;

constructor TInputFunction.Create(const aParent: TInputTestClass);
begin
  inherited Create;

  TestList := TObjectList.Create(True);
  FParent := aParent;
  FTotalTestResult := TStringlist.Create;
  FCompileResult := TStringlist.Create;
  FCompileCommand := TStringList.Create;
  FAllTestPassed := false;

  FRunLog := TStringList.Create;

  FSignatureHint := '';

  UseCustomSetupCode := False;
  CustomSetupCode := '';
end;

destructor TInputFunction.Destroy;
begin
  FCompileCommand.free;
  FRunLog.Free;
  TestList.Free;
  FTotalTestResult.Free;
  FCompileResult.Free;

  inherited;
end;

function TInputFunction.GetInputTest(const TestName: string;
  var InputTest: TInputTest): boolean;
var
  LocalInputTest: TInputTest;
  i: Integer;
begin
  result := false;
  for i := 0 to TestList.Count - 1 do
  begin
    LocalInputTest := TInputTest(TestList.Items[i]);
    if LocalInputTest.DisplayName = TestName then
    begin
      InputTest := LocalInputTest;
      result := true;
      break;
    end;
  end;

end;

function TInputFunction.GuessSignature: string;
var
  i, c: integer;
  aTest: TInputTest;
  aParam: TInputParam;
begin
  Result := '';

  if FSignatureHint <> '' then
  begin
    Result := FSignatureHint;
    Exit;
  end
  else
  begin
    if TestCount >= 1 then
    begin
      aTest := TInputTest(TestList[0]);

      c := aTest.ParamList.Count - 1;
      for i := 0 to c do
      begin
        if i <> 0 then
        begin
          Result := Result + ',';
        end;

        aParam := TInputParam(aTest.ParamList[i]);
        Result := Result + aParam.ParamType;
      end;
    end;
  end;

  // cachedtype werkt niet altijd correct, niet meenemen in signiture
  if Assigned(FParent) and (FParent.ClassName <> '') then
  begin
    Result := FParent.ClassName + '.' + MethodName + '(' + Result +  ')';
  end
  else
  begin
    Result := MethodName + '(' + Result +  ')';
  end;

  Result := LowerCase(Result);
end;

function TInputFunction.GetTestCount: integer;
begin
  if Assigned(TestList) then
  begin
    Result := TestList.Count;
  end
  else
  begin
    Result := 0;
  end;
end;

function TInputFunction.GetTestResult: boolean;
var
  i, c: integer;
  obj: TInputTest;
  bOneFailed: boolean;
begin
  Result := true;
  bOneFailed := False;

  c := TestList.Count - 1;
  for i := 0 to c do
  begin
  	obj := TInputTest(TestList[i]);
    if not obj.TestResult then
    begin
      bOneFailed := True;
    end;
  end;

  if bOneFailed then
  begin
    Result := False;
  end;
end;

procedure TInputFunction.MarkTestResult(iIndex: integer; bOk: boolean);
var
  test: TInputTest;
begin
  if (iIndex > 0) and (iIndex <= TestList.Count) then
  begin
    test := TInputTest(TestList[iIndex - 1]);

    if Assigned(test) then
    begin
      test.TestResult := bOk;
      if not bOk then
        FAllTestPassed := false;
    end;
  end;
end;

procedure TInputFunction.MarkTestResultString(iIndex: integer;
  sTestResult: string);
var
  test: TInputTest;
begin
  if (iIndex > 0) and (iIndex <= TestList.Count) then
  begin
    test := TInputTest(TestList[iIndex - 1]);

    if Assigned(test) then
    begin
      test.TestResultString := sTestResult;
      FTotalTestResult.Add(sTestResult);
    end;
  end;

end;

function TInputFunction.ToResultXmlString: string;
var
  i, c: integer;
  sTestResultOk: string;
begin
  sTestResultOk := '';
  if GetTestResult then
  begin
    sTestResultOk := 'true';
  end
  else
  begin
    sTestResultOk := 'false';
  end;

  Result := '<' + nodenameFunction + ' name="' + MethodName + '" type="' + FCachedType + '" result="' + sTestResultOk + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  if UseCustomSetupCode then
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'true') + #13#10;
  end
  else
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'false') + #13#10;
  end;
  Result := Result + XmlFuncs_QuickNode(nodenameCustomSetupCode, CustomSetupCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;


  if TestList.Count <> 0 then
  begin
    Result := Result + '<' + nodenameTestGroup + '>' + #13#10;

    c := TestList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputTest(TestList[i]).ToResultXmlString;
    end;

    Result := Result + '</' + nodenameTestGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameTestGroup + ' />' + #13#10;
  end;

  Result := Result + '</' + nodenameFunction + '>' + #13#10;
end;

function TInputFunction.ToXmlString: string;
var
  i, c: integer;
begin
  Result := '<' + nodenameFunction + ' name="' + MethodName + '" type="' + FCachedType + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDefines, Defines) + #13#10;
  if UseCustomSetupCode then
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'true') + #13#10;
  end
  else
  begin
    Result := Result + XmlFuncs_QuickNode(nodenameUseCustomSetupCode, 'false') + #13#10;
  end;
  Result := Result + XmlFuncs_QuickNode(nodenameCustomSetupCode, CustomSetupCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameInitCode, InitCode) + #13#10;
  Result := Result + XmlFuncs_QuickNode(nodenameVars, Vars) + #13#10;


  if TestList.Count <> 0 then
  begin
    Result := Result + '<' + nodenameTestGroup + '>' + #13#10;

    c := TestList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputTest(TestList[i]).ToXmlString;
    end;

    Result := Result + '</' + nodenameTestGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameTestGroup + ' />' + #13#10;
  end;

  Result := Result + '</' + nodenameFunction + '>' + #13#10;
end;




{ TInputImplier }

procedure TInputImplier.CloneFrom(const aSource: TInputImplier);
begin
  TestResult := aSource.TestResult;
  TestResultExpected := aSource.TestResultExpected;
  TestResultWas := aSource.TestResultWas;
  Eval := aSource.Eval;
end;

constructor TInputImplier.Create;
begin
  inherited;

  TestResult := False;
  Eval := '';
  TestResultExpected := '';
  TestResultWas := '';
end;

destructor TInputImplier.Destroy;
begin

  inherited;
end;

function TInputImplier.ToResultXmlString: string;
var
  sTestResultOk: string;
begin
  sTestResultOk := '';
  if TestResult then
  begin
    sTestResultOk := 'true';
  end
  else
  begin
    sTestResultOk := 'false';
  end;

  if Eval <> '' then
  begin
    Result :=
      '<' + nodenameEval + ' result="' + sTestResultOk + '">' +
      HtmlEntities( Eval ) +
      '</'+ nodenameEval + '>' + #13#10;
  end;
end;

function TInputImplier.ToXmlString: string;
begin
  Result := XmlFuncs_QuickNode(nodenameEval, Eval) + #13#10;
end;

{ TInputCommonClass }

procedure TInputCommonClass.CloneFrom(const anObj: TInputCommonClass);
begin
  Vars        := anObj.Vars;
  Defines     := anObj.Defines;
  Description := anObj.Description;
  InitCode    := anObj.InitCode;
end;

function TInputCommonClass.ToXmlString: string;
begin
  Result := '';
end;

{ TInputConstant }

procedure TInputConstant.CloneFrom(const aSource: TInputConstant);
begin
  ConstantName := aSource.ConstantName;
  ConstantValue := aSource.ConstantValue;
end;

function TInputConstant.ToXmlString: string;
begin
  if ConstantValue <> '' then
  begin
    Result := '<var name="' + XmlFuncs_Escape(ConstantName) + '">' + XmlFuncs_Escape(ConstantValue) + '</var>' + #13#10;
  end
  else
  begin
    Result := '<var />' + #13#10;
  end;
end;

{ TInputParam }

procedure TInputParam.CloneFrom(const aSource: TInputParam);
begin
  ParamName     := aSource.ParamName;
  ParamValue    := aSource.ParamValue;
  ParamType     := aSource.ParamType;
  IsComplexType := aSource.IsComplexType;
end;

function TInputParam.ToXmlString: string;
begin
  if ParamValue <> '' then
  begin
    Result := '<' + nodenameParamGroup + ' type="' + XmlFuncs_Escape(ParamType) + '" name="' + XmlFuncs_Escape(ParamName) +  '">' + XmlFuncs_Escape(ParamValue) + '</' + nodenameParamGroup + '>' + #13#10;
  end
  else
  begin
    Result := '<' + nodenameParamGroup + ' type="' + XmlFuncs_Escape(ParamType) + '" name="' + XmlFuncs_Escape(ParamName) +  '" />' + #13#10;
  end;
end;

{ TXMLTagObject }

constructor TXMLTagObject.Create;
begin
  FDestroyed := False;
end;

destructor TXMLTagObject.Destroy;
begin
  Assert( not FDestroyed, 'Error! ' + ClassName + ' already destroyed' );

  FDestroyed := True;

  inherited;
end;

function TXMLTagObject.ToResultXmlString: string;
begin
  Result := ToXmlString;
end;

//-----

procedure SaveTestResultFile( const sResultFilename: string; const sSourceFileName: string; const aClass: TInputTestClass; const sTotalTestTime: string = '' );
var
  xml: TStringlist;
begin
  xml := TStringlist.Create;
  try

    xml.Add( '<?xml version="1.0" encoding="utf-8"?>' );
    xml.Add( '<?xml-stylesheet href="output.xsl" type="text/xsl"?>' );
    xml.Add( '<IntegratedUnitTest>' );
    xml.Add( '<info>' );
    xml.Add( XmlFuncs_QuickNode('file', sSourceFileName ) );
    xml.Add( XmlFuncs_QuickNode('time', XmlFuncs_DateTimeStamp(Now) ) );
    xml.Add( XmlFuncs_QuickNode('testtime', sTotalTestTime ) );
    xml.Add( '</info>' );

    if Assigned(aClass) then
    begin
      xml.Text := xml.Text + aClass.ToResultXmlString;
    end;

    xml.Add( '</IntegratedUnitTest>' );

    {$ifdef VER180}
    xml.Text := AnsiToUtf8(xml.Text);
    xml.SaveToFile(sResultFilename);
    {$else}
    {$ifdef VER150}
    xml.Text := AnsiToUtf8(xml.Text);
    xml.SaveToFile(sResultFilename);
    {$else}
    xml.SaveToFile(sResultFilename, uUTF8Functions.GetUTF8WithoutBOM);
    {$endif}
    {$endif}
  finally
    xml.Free;
  end;
end;


{ TInputUrlTest }

function TInputUrlTest.AddSpecialCheck(const sCheckType, sCheckEquals: string): TInputSpecialCheck;
begin
  Result := TInputSpecialCheck.Create;
  Result.FCheckType := sCheckType;
  Result.FCheckEquals := sCheckEquals;

  FSpecialChecks.Add(Result);
end;

procedure TInputUrlTest.CloneFrom(const aTest: TInputCommonClass);
var
  i, c: integer;
  oCheck: TInputSpecialCheck;
begin
  inherited;

  if aTest is TInputUrlTest then
  begin
    Url := TInputUrlTest(aTest).Url;
    Method := TInputUrlTest(aTest).Method;

    c := TInputUrlTest(aTest).SpecialChecks.Count - 1;
    for i := 0 to c do
    begin
      oCheck := TInputSpecialCheck(TInputUrlTest(aTest).SpecialChecks[i]);

      AddSpecialCheck(oCheck.CheckType, oCheck.CheckEquals);
    end;
  end;
end;

constructor TInputUrlTest.Create(const aParent: TInputIndependentTests);
begin
  inherited Create;

  FSpecialChecks := TObjectList.Create(True);
end;

destructor TInputUrlTest.Destroy;
begin
  FSpecialChecks.Free;

  inherited;
end;

procedure TInputUrlTest.RegisterEqualsTestResult(const sExpected,
  sActual: string);
begin
  FTestResultEqualsExpected := sExpected;
  FTestResultEqualsWas := sActual;

  // TestResult hier niet opslaan, is afhankelijke van meer dingen
end;

function TInputUrlTest.ToResultXmlString: string;
begin
  Result := ToXmlString;
end;

function TInputUrlTest.ToXmlString: string;
var
  i, c: integer;
begin
  Result :=
    '<' + nodenameUrlTest + ' displayname="' + XmlFuncs_Escape(DisplayName) + '" url="'+ XmlFuncs_Escape(FUrl) +'" method="' + XmlFuncs_Escape(FMethod) + '">' + #13#10;

  Result := Result + XmlFuncs_QuickNode(nodenameDesc, Description) + #13#10;

  if ParamList.Count <> 0 then
  begin
    Result := Result + '<' +  nodenameParamsGroup + '>' + #13#10;
    c := ParamList.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputParam(ParamList[i]).ToXMLString;
    end;
    Result := Result + '</' + nodenameParamsGroup + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameParamsGroup + ' />' + #13#10;
  end;

  Result := Result + EqualsNodeToXml + #13#10;

  if FSpecialChecks.Count <> 0 then
  begin
    Result := Result + '<' + nodenameSpecialchecks + '>' + #13#10;
    c := FSpecialChecks.Count - 1;
    for i := 0 to c do
    begin
      Result := Result + TInputSpecialCheck(FSpecialChecks[i]).ToXMLString;
    end;
    Result := Result + '</' + nodenameSpecialchecks + '>' + #13#10;
  end
  else
  begin
    Result := Result + '<' + nodenameSpecialchecks + ' />' + #13#10;
  end;

  Result := Result +
    '</' + nodenameUrlTest + '>' + #13#10;
end;

{ TInputSpecialCheck }

procedure TInputSpecialCheck.RegisterTestResult(const sExpected,
  sActual: string);
begin
  FTestResultEqualsExpected := sExpected;
  FTestResultEqualsWas := sActual;

  FTestResult := SameStr(sExpected, sActual);
end;

function TInputSpecialCheck.ToResultXmlString: string;
var
  sEquals: string;
begin
  if TCommonStringFunctions.ContainsNonVisibleOrCRLFCharacters(FCheckEquals) then  // eigenlijk bij alles wat < 48 en > 127 is (of iets dergelijks)
  begin
    sEquals := TBase64.Encode(FCheckEquals);

    Result := Result +
      '<' + nodenameCheck + ' type="' + XmlFuncs_Escape(FCheckType) + '" xmlns:dt="urn:schemas-microsoft-com:datatypes" dt:dt="binary.base64">';
  end
  else
  begin
    sEquals := FCheckEquals;

    Result := Result +
      '<' + nodenameCheck + ' type="' + XmlFuncs_Escape(FCheckType) + '">';
  end;

  Result := Result +
    XmlFuncs_Escape(sEquals) +
    '</' + nodenameCheck + '>' + #13#10;
end;

function TInputSpecialCheck.ToXmlString: string;
var
  sEquals: string;
begin
  Result := '';

  if TCommonStringFunctions.ContainsNonVisibleOrCRLFCharacters(FCheckEquals) then  // eigenlijk bij alles wat < 48 en > 127 is (of iets dergelijks)
  begin
    sEquals := TBase64.Encode(FCheckEquals);

    Result := '<' + nodenameCheck + ' type="' + XmlFuncs_Escape(FCheckType) + '" xmlns:dt="urn:schemas-microsoft-com:datatypes" dt:dt="binary.base64">';
  end
  else
  begin
    sEquals := FCheckEquals;

    Result := '<' + nodenameCheck + ' type="' + XmlFuncs_Escape(FCheckType) + '">';
  end;

  Result := Result +
    XmlFuncs_Escape(sEquals) +
    '</' + nodenameCheck + '>' + #13#10;
end;

end.

