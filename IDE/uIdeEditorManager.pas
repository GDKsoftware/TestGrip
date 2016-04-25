unit uIdeEditorManager;

interface

uses
  SysUtils, Contnrs, uTestDefs, uInputParser, classes;

type
  TPascalFile = class
    PascalFileName: string;
    TestFileName: string;
    TestClassList: TList;

  private

    function Get(Index: Integer): TInputTestClass;
    procedure Put(Index: Integer; const Value: TInputTestClass);
  public
    function AddTestClass(const ClassName: string): TInputTestClass;
    function GetInputTestClass(const ClassName: string; var InputTestClass: TInputTestClass): boolean;
    function GetOrAddInputTestClass(const AClassname: string): TInputTestClass;

    procedure SaveToFile(aFilename: string);
    procedure LoadFromFile(aFilename: string);

    property Items[Index: Integer]: TInputTestClass read Get write Put; default;

    constructor Create;
    destructor Destroy; override;
  end;

  TIdeEditorManager = class
    PascalFileList: TObjectList;
  private
    function LoadTestFile(PascalFilename: string; var PascalFile: TPascalFile): boolean;
  public
    function AddManagedPascalFile(const PascalFile: string; CreateIfNotExists: boolean): TPascalFile;
    function GetCurrentPascalFile(const PascalFile: string; var TestClass: TPascalFile): boolean;

    function GetOrAddPascalFile(const AUnit: string): TPascalFile;

    constructor Create;
    destructor Destroy; override;

  end;

implementation

uses
  uUTF8Functions;

{ TIdeEditorManager }

function TIdeEditorManager.AddManagedPascalFile(const PascalFile: string; CreateIfNotExists: boolean): TPascalFile;
var
  TestFile: TPascalFile;
  TestFileExists: boolean;
  TestFileName: string;
begin
  result := nil;
  if GetCurrentPascalFile(PascalFile,TestFile) then
  begin
    result := TestFile;
    exit;
  end;

  TestFileName := StringReplace(PascalFile,'.pas','.test',[rfIgnoreCase]);

  TestFileExists := FileExists(TestFilename);
  if TestFileExists then // Check if testfile exists
  begin
    TestFile := TPascalFile.Create;
    if LoadTestFile(TestFilename,TestFile) then
    begin
      TestFile.PascalFileName := PascalFile;
      TestFile.TestFileName := TestFileName;

      PascalFileList.Add(TestFile);

      result := TestFile;
    end
    else
      TestFileExists := false;
  end;

  if (not TestFileExists) and CreateIfNotExists then
  begin
    // Create new testfile
    TestFile := TPascalFile.Create;
    TestFile.PascalFileName := PascalFile;
    TestFile.TestFileName := TestFileName;

    // Save dummy file
    TestFile.SaveToFile(TestFileName);

    // Add to local list
    PascalFileList.Add(TestFile);

    result := TestFile;
  end;


end;

constructor TIdeEditorManager.Create;
begin
  inherited;
  PascalFileList := TObjectList.Create;
end;

destructor TIdeEditorManager.Destroy;
begin
  PascalFilelist.Free;
  inherited;
end;

function TIdeEditorManager.GetCurrentPascalFile(
  const PascalFile: string; var TestClass: TPascalFile): boolean;
var
  TestFile: TPascalFile;
  i: integer;
begin
  result := false;
  // check if unit is added before
  for I := 0 to PascalFileList.count -1 do
  begin
    TestFile := TPascalFile(PascalFileList.Items[i]);
    if TestFile.PascalFileName = PascalFile then
    begin
      TestClass := TestFile;
      result := true;
    end;
  end;
end;

function TIdeEditorManager.GetOrAddPascalFile(const AUnit: string): TPascalFile;
begin
  if not GetCurrentPascalFile(AUnit, Result) then
  begin
    Result := AddManagedPascalFile(AUnit, true);
  end;
end;

function TIdeEditorManager.LoadTestFile(PascalFilename: string; var PascalFile: TPascalFile): boolean;
var
  InputParser: TInputParser;
begin
  result := false;

  // load .test file
  InputParser := TInputParser.Create(PascalFilename, PascalFile.TestClassList);
  try
    try
      result := InputParser.ParseTestFile;
    except
      on e:exception do
        raise Exception.Create(e.message);
    end;
  finally
    InputParser.Free;
  end;
end;

{ TPascalFile }

function TPascalFile.AddTestClass(const ClassName: string): TInputTestClass;
begin
  result := TInputTestClass.Create;

  result.Name := ClassName;
  result.ClassName := ClassName;
  TestClassList.Add(result);
end;

constructor TPascalFile.Create;
begin
  TestClassList := TList.Create;
end;

destructor TPascalFile.Destroy;
begin
  TestClassList.Free;
  inherited;
end;

function TPascalFile.Get(Index: Integer): TInputTestClass;
begin
  result := TInputTestClass(TestClassList.Items[Index]);
end;

function TPascalFile.GetInputTestClass(const ClassName: string;
  var InputTestClass: TInputTestClass): boolean;
var
  I: Integer;
  oInputClass: TInputTestClass;
begin
  result := false;
  for I := 0 to TestClassList.Count -1 do
  begin
    oInputClass := TInputTestClass(TestClassList.Items[i]);
    if oInputClass.ClassName = ClassName then
    begin
      result := true;
      InputTestClass := oInputClass;
    end;
  end;
end;

function TPascalFile.GetOrAddInputTestClass(const AClassname: string): TInputTestClass;
begin
  if not GetInputTestClass(AClassname, Result) then
  begin
    Result := AddTestClass(AClassname);
  end;
end;

procedure TPascalFile.LoadFromFile(aFilename: string);
begin
  // ???
end;

procedure TPascalFile.Put(Index: Integer; const Value: TInputTestClass);
begin
  TestClassList.Items[index] := Value;
end;

procedure TPascalFile.SaveToFile(aFilename: string);
var
  XmlString: TStringlist;
  InputTestClass: TInputTestClass;
  I: Integer;
begin
  XMLString := TStringlist.Create;
  try
    // Header
    XMLString.Delimiter := #0;
    XMLString.Text := '<?xml version="1.0" encoding="utf-8"?>' + #13#10 +
      '<IntegratedUnitTest>' + #13#10;

    for I := 0 to TestClassList.Count -1 do
    begin
      InputTestClass := TInputTestClass(TestClassList.Items[i]);
      XMLString.Text := XMLString.Text + InputTestClass.ToXmlString;
    end;

    // Footer
    XMLString.Text := XMLString.Text + '</IntegratedUnitTest>';

    {$ifdef VER180}
    XMLString.Text := AnsiToUtf8(XMLString.Text);
    XMLString.SaveToFile(aFilename);
    {$else}
    {$ifdef VER150}
    XMLString.Text := AnsiToUtf8(XMLString.Text);
    XMLString.SaveToFile(aFilename);
    {$else}
    XMLString.SaveToFile(aFilename, uUTF8Functions.GetUTF8WithoutBOM);
    {$endif}
    {$endif}
    // todo parse file
  finally
    XMLString.Free;
  end;
end;

end.
