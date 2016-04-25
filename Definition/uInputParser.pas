unit uInputParser;

interface

uses Classes, Dialogs, XMLDoc, XMLIntf, uTestDefs;

type
  // Class containing a list with tests defined for specified classes
  TInputParser = class(TObject)
  private
    FSourceFile : string;
    FTestFile   : string;

    FIndependentTests: TList;
    FInputClassList: TList;

  protected
    procedure HandleError(const sErrorMessage : string);

    procedure parseNodeFunction(xmlFunctionNode : IXMLNode; const functionList : TList; const aParent: TInputTestClass);
    procedure parseNodeTestGroup(pXmlNode : IXMLNode; const testList : TList; const aParent: TInputFunction);
    procedure parseCommonNode(pXmlNode : IXMLNode; const pInputObject : TInputCommonClass);
    procedure parseTestNode(pXmlNode: IXMLNode; const testList : TList; const aParent: TInputFunction);
    procedure parseUrlTestNode(pXmlNode: IXMLNode; const aTestObj: TInputUrlTest);
    procedure parseParamNode(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure parseParamsNode(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure parsePreImpliesCodeNode(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure parseEqualsNode(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure parseImpliesGroup(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure parseImpliesNode(pXmlNode: IXMLNode; const aTest: TInputTest);
    procedure ParseClassCreateCodeNode(pXmlNode: IXMLNode; const pInputObject: TInputCommonClass);
    procedure ParseTestCreateCodeNode(pXmlNode: IXMLNode; const aTest: TInputTest);

    procedure parseSpecialChecksNode(pXmlNode: IXMLNode; const aTest: TInputUrlTest);

    procedure ParseCustomSetupCodeNode(pXmlNode: IXMLNode; const aFunc: TInputFunction); overload;
    procedure ParseCustomSetupCodeNode(pXmlNode: IXMLNode; const aClass: TInputTestClass); overload;
  public
    constructor Create(const sSourceFile: string; const InputClassList: TList; const IndependentTests: TList = nil);
    destructor Destroy; override;

    function ParseTestFile: boolean;

    property TestFile: string read FTestFile;
    property SourceFile: string read FSourceFile;


  end;

implementation

uses
  SysUtils, uCommonFunctions, Variants, uConst, uUTF8Functions;

{ TInputParser }

constructor TInputParser.Create(const sSourceFile: string; const InputClassList: TList; const IndependentTests: TList);
begin
  FSourceFile := sSourceFile;
  FTestFile := FSourceFile;

  FIndependentTests := IndependentTests;

  FInputClassList := InputClassList;
end;

destructor TInputParser.Destroy;
begin
  inherited;
end;

procedure TInputParser.HandleError(const sErrorMessage : string);
begin
  raise Exception.Create(sErrorMessage);
end;

procedure TInputParser.ParseClassCreateCodeNode(pXmlNode: IXMLNode;
  const pInputObject: TInputCommonClass);
begin
  if not VarIsNull(pXmlNode.NodeValue) then
  begin
    pInputObject.InitCode := '' + pXmlNode.NodeValue + #13#10 + pInputObject.InitCode;
  end;
end;

procedure TInputParser.parseCommonNode(pXmlNode: IXMLNode; const pInputObject: TInputCommonClass);
var
  sNodeName : string;

begin
  { Parse common nodes into input objects }
  sNodeName := pXmlNode.NodeName;

  if (sNodeName = nodenameDesc) and (not VarIsNull(pXmlNode.NodeValue)) then
    pInputObject.Description := pXmlNode.NodeValue
  else if (sNodeName = nodenameInitCode) and (not VarIsNull(pXmlNode.NodeValue))  then
    pInputObject.InitCode := pXmlNode.NodeValue
  else if (sNodeName = nodenameVars) and (not VarIsNull(pXmlNode.NodeValue))  then
    pInputObject.Vars := pXmlNode.NodeValue
  else if (sNodeName = nodenameDefines) and (not VarIsNull(pXmlNode.NodeValue)) then
    pInputObject.Defines := pXmlNode.NodeValue;
end;

procedure TInputParser.ParseCustomSetupCodeNode(pXmlNode: IXMLNode;
  const aFunc: TInputFunction);
var
  sNodeName : string;
begin
  sNodeName := pXmlNode.NodeName;

  if (sNodeName = nodenameUseCustomSetupCode) and (not VarIsNull(pXmlNode.NodeValue)) then
  begin
    aFunc.UseCustomSetupCode := SameText(pXmlNode.NodeValue, 'true');
  end
  else if (sNodeName = nodenameCustomSetupCode) and (not VarIsNull(pXmlNode.NodeValue)) then
  begin
    aFunc.CustomSetupCode := pXmlNode.NodeValue;
  end;
end;

procedure TInputParser.ParseCustomSetupCodeNode(pXmlNode: IXMLNode;
  const aClass: TInputTestClass);
var
  sNodeName : string;
begin
  sNodeName := pXmlNode.NodeName;

  if (sNodeName = nodenameUseCustomSetupCode) and (not VarIsNull(pXmlNode.NodeValue)) then
  begin
    aClass.UseCustomSetupCode := SameText(pXmlNode.NodeValue, 'true');
  end
  else if (sNodeName = nodenameCustomSetupCode) and (not VarIsNull(pXmlNode.NodeValue)) then
  begin
    aClass.CustomSetupCode := pXmlNode.NodeValue;
  end;
end;

procedure TInputParser.parseEqualsNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
begin
  if not VarIsNull(pXmlNode) then
  begin
    if pXmlNode.HasAttribute('dt:dt') and SameText(pXmlNode.Attributes['dt:dt'], 'binary.base64') then
    begin
      // nodetyped value moeten we gebruiken voor automatische conversie, maar die hebben we niet in delplhi blijkbaar???
      aTest.Equals := TBase64.Decode(pXmlNode.NodeValue);
    end
    else
    begin
      aTest.Equals := pXmlNode.NodeValue;
    end;
  end;

  if pXmlNode.HasAttribute('not') then
  begin
    aTest.EqualsNot := SameText(pXmlNode.Attributes['not'], 'true');
  end;
end;

procedure TInputParser.parseImpliesGroup(pXmlNode: IXMLNode;
  const aTest: TInputTest);
var
  nodeList : IXMLNodeList;
  xmlNode  : IXMLNode;
  nIndex   : integer;
begin
  nodeList := pXmlNode.GetChildNodes;
  for nIndex := 0 to nodeList.Count - 1 do
  begin
    xmlNode := nodeList.Get(nIndex);
    if (xmlNode.NodeName = nodenameEval) then
    begin
      parseImpliesNode(xmlNode, aTest);
    end;
  end;
end;

procedure TInputParser.parseImpliesNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
begin
  if not VarIsNull(pXmlNode) then
  begin
    if not VarIsNull(pXmlNode.NodeValue) and not VarIsEmpty(pXmlNode.NodeValue) then
    begin
      aTest.AddImplies(pXmlNode.NodeValue);
    end;
  end;
end;

procedure TInputParser.parseNodeFunction(xmlFunctionNode: IXMLNode; const functionList : TList; const aParent: TInputTestClass);
var
  objInputFunction : TInputFunction;
  nodeList : IXMLNodeList;
  xmlNode  : IXMLNode;
  nIndex   : integer;
  sNodeName: string;
begin
  { Parse node "function" into corresponding class }

  // Create instance of class TInpputFunction
  objInputFunction := TInputFunction.Create(aParent);
  objInputFunction.MethodName := xmlFunctionNode.Attributes['name'];
  if xmlFunctionNode.HasAttribute('type') then
  begin
    objInputFunction.CachedType := xmlFunctionNode.Attributes['type'];
  end;

  // Read the childnodes to get the defined tests
  nodeList := xmlFunctionNode.GetChildNodes;
  for nIndex := 0 to nodeList.Count - 1 do
  begin
    xmlNode := nodeList.Get(nIndex);
    sNodeName := xmlNode.NodeName;
    if (sNodeName = nodenameTestGroup) then
      parseNodeTestGroup(xmlNode, objInputFunction.TestList, objInputFunction)
    else
      parseCommonNode(xmlNode, objInputFunction);
  end;
  functionList.Add(objInputFunction);
end;

procedure TInputParser.parseNodeTestGroup(pXmlNode: IXMLNode; const testList: TList; const aParent: TInputFunction);
var
  nodeList : IXMLNodeList;
  xmlNode  : IXMLNode;
  nIndex   : integer;
  objCommonInput : TInputCommonClass;
begin
  objCommonInput := TInputCommonClass.Create;
  try
    nodeList := pXmlNode.GetChildNodes;
    for nIndex := 0 to nodeList.Count - 1 do
    begin
      xmlNode := nodeList.Get(nIndex);
      if (xmlNode.NodeName = nodenameTestScenario) then
      begin
        parseTestNode(xmlNOde, testlist, aParent);
      end
      else
      begin
        parseCommonNode(xmlNode, objCommonInput);
      end;
    end;
  finally
    objCommonInput.Free;
  end;
end;

procedure TInputParser.parseUrlTestNode(pXmlNode: IXMLNode;
  const aTestObj: TInputUrlTest);
var
  i, c: integer;
  subnode: IXMLNode;
begin
  aTestObj.DisplayName := pXmlNode.Attributes['displayname'];
  aTestObj.Url    := pXmlNode.Attributes['url'];
  aTestObj.Method := pXmlNode.Attributes['method'];

  c := pXmlNode.ChildNodes.Count - 1;
  for i := 0 to c do
  begin
    subnode := pXmlNode.ChildNodes[i];

    if subnode.NodeName = nodenameEquals then
    begin
      parseEqualsNode(subnode, aTestObj);
    end
    else if subnode.NodeName = nodenameParamsGroup then
    begin
      parseParamsNode(subnode, aTestObj);
    end
    else if subnode.NodeName = nodenameImpliesGroup then
    begin
      // implies requires a programming/scripting language, we dont have that yet for urltests
    end
    else if subnode.NodeName = nodenameSpecialChecks then
    begin
      parseSpecialChecksNode(subnode, aTestObj);
    end;
  end;

end;

procedure TInputParser.parseParamNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
var
  sName: string;
  sType: string;
  sValue: string;
begin
  sName := pXmlNode.Attributes['name'];

  sValue := '';
  if pXmlNode.NodeValue <> Null then
  begin
    sValue := '' + pXmlNode.NodeValue;
  end;

  sType := 'Variant';
  if pXmlNode.HasAttribute('type') then
  begin
    if (pXmlNode.Attributes['type'] <> '') then
      sType := pXmlNode.Attributes['type'];
  end;

  aTest.AddInputParam( sName, sValue, sType );
end;

procedure TInputParser.parseParamsNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
var
  nodeList : IXMLNodeList;
  xmlNode  : IXMLNode;
  nIndex   : integer;
begin
  nodeList := pXmlNode.GetChildNodes;
  for nIndex := 0 to nodeList.Count - 1 do
  begin
    xmlNode := nodeList.Get(nIndex);
    if (xmlNode.NodeName = nodenameParamGroup) then
    begin
      parseParamNode(xmlNode, aTest);
    end;
  end;
end;

procedure TInputParser.parsePreImpliesCodeNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
var
  sValue: string;
begin
  sValue := '';

  if pXmlNode.NodeValue <> Null then
  begin
    sValue := '' + pXmlNode.NodeValue;
  end;

  aTest.PreImpliesCode := sValue;
end;

procedure TInputParser.parseSpecialChecksNode(pXmlNode: IXMLNode;
  const aTest: TInputUrlTest);
var
  i, c: integer;
  subnode: IXMLNode;
begin
  c := pXmlNode.ChildNodes.Count - 1;
  for i := 0 to c do
  begin
    subnode := pXmlNode.ChildNodes[i];
    if subnode.NodeName = nodenameCheck then
    begin
      if subnode.HasAttribute('dt:dt') and SameText(subnode.Attributes['dt:dt'], 'binary.base64') then
      begin
        aTest.AddSpecialCheck( subnode.Attributes['type'], TBase64.Decode(subnode.NodeValue) );
      end
      else
      begin
        aTest.AddSpecialCheck( subnode.Attributes['type'], subnode.NodeValue );
      end;
    end;
  end;
end;

procedure TInputParser.ParseTestCreateCodeNode(pXmlNode: IXMLNode;
  const aTest: TInputTest);
begin
  //
end;

function TInputParser.ParseTestFile: boolean;
var
  xmlDoc: IXMLDocument;
  xmlRootList: IXMLNodeList;
  xmlRootNode: IXMLNode;
  xmlNodes: IXMLNodeList;
  xmlNode: IXMLNode;
  i,c,d,nodecount: integer;

  InputFile: TFileStream;
  objInputClass : TInputTestClass;
  sNodeName : string;
  objIndependentTest: TInputIndependentTest;
begin
  result := false;

  // Check testfile exists
  if not FileExists(FTestFile) then
  begin
    HandleError('Testfile not found (' + FTestFile + ')');
    Exit;
  end;

  // Read file and parse XML content
  InputFile := TFileStream.Create(FTestFile, fmOpenRead);
  try
    if InputFile.Size = 0 then
    begin
      result := false;
      DeleteFile(FTestFile);
      exit;
    end;

    xmlDoc := TXMLDocument.Create(nil);
    try
      // Read XML from file
      xmlDoc.LoadFromStream( InputFile, xetUTF_8 );

      xmlDoc.active := True;

      result := true;
      // Get the root node (root node = "IntegratedUnitTest")
      xmlRootList := xmlDoc.ChildNodes;
      xmlRootNode := xmlRootList.FindNode(nodenameRoot);
      if (xmlRootNode = nil) then
      begin
        HandleError('Not a valid testfile');
        Exit;
      end;

      // Get the root node of the CLASS definitions
      xmlRootList := xmlRootNode.ChildNodes;
      c := xmlRootList.Count - 1;
      for nodecount := 0 to c do
      begin
        xmlRootNode := xmlRootlist.Get(nodecount);//xmlRootList. FindNode(nodenameClass);
        if (xmlRootNode = nil) then
        begin
          HandleError('Not a valid testfile');
          Exit;
        end;

        if Assigned(FInputClassList) and (xmlRootNode.NodeName = nodenameClass) then
        begin

          // Create instance of TInputTestClass
          objInputClass := TInputTestClass.Create;
          objInputClass.Name         := xmlRootNode.Attributes['name'];
          objInputClass.ClassName    := objInputClass.Name;

          // Loop through child nodes
          xmlNodes := xmlRootNode.ChildNodes;
          d := xmlNodes.Count -1;
          for i := 0 to d do
          begin
            xmlNode := xmlNodes.Get(i);

            sNodeName := xmlNode.NodeName;
            if (sNodeName = nodenameFunction) then
            begin
              parseNodeFunction(xmlNode, objInputClass.FunctionList, objInputClass)
            end
            else if (sNodeName = nodenameExtraUses) then
            begin
              if not VarIsNull(xmlNode.NodeValue) then
              begin
                objInputClass.ExtraUses := xmlNode.NodeValue;
              end;
            end     
            else
            begin
              ParseCustomSetupCodeNode( xmlNode, objInputClass );
              parseCommonNode(xmlNode, objInputClass);
            end;
          end;

          FInputClassList.Add( objInputClass );
        end
        else if Assigned(FIndependentTests) and (xmlRootNode.NodeName = nodenameIndependenttests) then
        begin
          xmlNodes := xmlRootNode.ChildNodes;
          d := xmlNodes.Count - 1;
          for i := 0 to d do
          begin
            xmlNode := xmlNodes.Get(i);

            sNodeName := xmlNode.NodeName;
            if (sNodeName = nodenameUrlTest) then
            begin
              objIndependentTest := TInputUrlTest.Create;
              parseUrlTestNode(xmlNode, objIndependentTest as TInputUrlTest);
              FIndependentTests.Add(objIndependentTest);
            end
            else
            begin
              // other test types
            end;
          end;
        end;
      end;
    finally

    end;

  finally
    InputFile.Free;
  end;

end;

procedure TInputParser.parseTestNode(pXmlNode: IXMLNode; const testList: TList; const aParent: TInputFunction);
var
  nodeList : IXMLNodeList;
  xmlNode  : IXMLNode;
  nIndex   : integer;
  objInputTest   : TInputTest;
begin
  { parse node with testscenario }

  // Get the common and test nodes
  objInputTest   := TInputTest.Create(aParent);

  if pXmlNode.HasAttribute(nodenameDisplayName) then
    objInputTest.DisplayName := pXmlNode.Attributes[nodenameDisplayName];

  nodeList := pXmlNode.GetChildNodes;
  for nIndex := 0 to nodeList.Count - 1 do
  begin
    xmlNode := nodeList.Get(nIndex);
    if (xmlNode.NodeName = nodenameParamsGroup) then
    begin
      parseParamsNode(xmlNode, objInputTest);
    end
    else if (xmlNode.NodeName = nodenameEquals) then
    begin
      parseEqualsNode(xmlNode, objInputTest);
    end
    else if (xmlNode.NodeName = nodenameImpliesGroup) then
    begin
      parseImpliesGroup(xmlNode,objInputTest)
    end
    else if (xmlNode.NodeName = nodenamePreImpliesCode) then
    begin
      parsePreImpliesCodeNode(xmlNode, objInputTest);
    end
    else
    begin
      parseCommonNode(xmlNode, objInputTest);
    end;
    
  end;

  testList.Add(objInputTest);
end;

end.
