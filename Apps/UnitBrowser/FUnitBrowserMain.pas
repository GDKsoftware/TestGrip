unit FUnitBrowserMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ShellAPI, uUnitParser, uPascalDefs,
  Contnrs;

type
  TFrmUnitBrowserMain = class(TForm)
    treeMain: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FParser: TUnitParser;

    FClassNodeList: TStringList;
    procedure AddMethodToTree(const methoddef: TMethodDefinition);
    procedure AddMethodsToTree(const AMethods: TObjectList);
    procedure AddPropertiesToTree(const AProperties: TObjectList);
    procedure AddVariablesToTree(const AVariables: TObjectList);
    procedure AddClassesToTree(const AClasses: TStrings);
    function AddClassNode(const AClassNodeName: string; const AClassDef: TClassDefinition): TTreeNode;
  public
    { Public declarations }
    procedure AcceptFiles( var msg : TMessage ); message WM_DROPFILES;

    procedure LoadUnit( const sFilename: string );

    procedure RefreshTree;
  end;

var
  FrmUnitBrowserMain: TFrmUnitBrowserMain;

implementation

{$R *.dfm}

const
  C_APPTITLE = 'UnitBrowser v1.0';

  C_ClassAnnotationsNodeTreeIdx = 0;
  C_ClassVariableNodeTreeIdx = 1;
  C_ClassPropertyNodeTreeIdx = 2;
  C_ClassMethodNodeTreeIdx = 3;

procedure TFrmUnitBrowserMain.AcceptFiles( var msg : TMessage );
const
  cnMaxFileNameLen = 255;
var
  i,
  nCount     : integer;
  acFileName : array [0..cnMaxFileNameLen] of char;
begin
  // find out how many files we're accepting
  nCount := DragQueryFile( msg.WParam,
                           $FFFFFFFF,
                           acFileName,
                           cnMaxFileNameLen );

  // query Windows one at a time for the file name
  for i := 0 to nCount-1 do
  begin
    DragQueryFile( msg.WParam, i,
                   acFileName, cnMaxFileNameLen );

    // do your thing with the acFileName
    LoadUnit( acFilename );
  end;

  // let Windows know that you're done
  DragFinish( msg.WParam );
end;

procedure TFrmUnitBrowserMain.FormCreate(Sender: TObject);
begin
  FClassNodeList := TStringList.Create;

  FParser := nil;

  DragAcceptFiles( Handle, True );

  Self.Caption := C_APPTITLE + ' - ' + 'Drag a .pas file to this window';
end;

procedure TFrmUnitBrowserMain.FormDestroy(Sender: TObject);
begin
  treeMain.Items.Clear;

  FreeAndNil(FParser);

  FClassNodeList.Free;
end;

procedure TFrmUnitBrowserMain.LoadUnit(const sFilename: string);
var
  s: TFileStream;
begin
  treeMain.Items.Clear;
  FClassNodeList.Clear;

  FreeAndNil(FParser);
  FParser := TUnitParser.Create;

  Self.Caption := C_APPTITLE + ' - ' + ExtractFileName(sFilename);

  s := TFileStream.Create( sFilename, fmOpenRead );
  try
    FParser.ParseUnit( s, true, true, SameText(ExtractFileExt(sFilename), '.dpk'), true );
  finally
    s.Free;
  end;

  RefreshTree;
end;

procedure TFrmUnitBrowserMain.RefreshTree;
begin
  AddClassNode('global', nil);
  AddClassesToTree(FParser.InterfaceClassList);

  AddVariablesToTree(FParser.VariableList);
  AddPropertiesToTree(FParser.PropertyList);

  AddMethodsToTree(FParser.MethodList);
  AddMethodsToTree(FParser.InterfaceMethodList);

  treeMain.FullExpand;
end;

function TFrmUnitBrowserMain.AddClassNode(const AClassNodeName: string; const AClassDef: TClassDefinition): TTreeNode;
var
  Annotation: string;
begin
  Result := treeMain.Items.AddChild(nil, AClassNodeName);
  treeMain.Items.AddChild(Result, 'Annotations');
  treeMain.Items.AddChild(Result, 'Variables');
  treeMain.Items.AddChild(Result, 'Properties');
  treeMain.Items.AddChild(Result, 'Methods');
  FClassNodeList.AddObject(AClassNodeName, Result);

  if Assigned(AClassDef) then
  begin
    for Annotation in AClassDef.Annotations do
    begin
      treeMain.Items.AddChild(Result.Item[C_ClassAnnotationsNodeTreeIdx], Annotation);
    end;
  end;
end;

procedure TFrmUnitBrowserMain.AddClassesToTree(const AClasses: TStrings);
var
  i, c: Integer;
  ClassNodeName: string;
  li: TTreeNode;
begin
  c := AClasses.Count - 1;
  for i := 0 to c do
  begin
    ClassNodeName := AClasses.Names[i];

    AddClassNode(ClassNodeName, TClassDefinition(AClasses.Objects[i]));
  end;
end;

procedure TFrmUnitBrowserMain.AddMethodsToTree(const AMethods: TObjectList);
var
  i, c: Integer;
begin
  c := AMethods.Count - 1;
  for i := 0 to c do
  begin
    AddMethodToTree(TMethodDefinition(AMethods[i]));
  end;
end;

procedure TFrmUnitBrowserMain.AddMethodToTree(const methoddef: TMethodDefinition);
var
  ClassIdx: Integer;
  ClassNodeName: string;
  ClassNode, MethodNode: TTreeNode;
  Annotations: string;
  Annotation: string;
begin
  if methoddef.InClass <> '' then
  begin
    ClassIdx := FClassNodeList.IndexOf(methoddef.InClass);
    if ClassIdx <> -1 then
    begin
      ClassNode := TTreeNode(FClassNodeList.Objects[ClassIdx]);
    end
    else
    begin
      ClassNodeName := methoddef.InClass;
      ClassNode := AddClassNode(ClassNodeName, nil);
    end;
  end
  else
  begin
    ClassNode := TTreeNode(FClassNodeList.Objects[0]);
  end;

  MethodNode := ClassNode.Item[C_ClassMethodNodeTreeIdx];

  Annotations := '';
  for Annotation in methoddef.Annotations do
  begin
    Annotations := Annotations + '[' + Annotation + ']';
  end;

  treeMain.Items.AddChild(MethodNode, methoddef.DefMethodName + '(' + methoddef.ParamsAndTypes + '): ' + methoddef.Functype + Trim(' ' + Annotations));
end;

procedure TFrmUnitBrowserMain.AddPropertiesToTree(const AProperties: TObjectList);
var
  i, c: Integer;
  propdef: TPropertyDefinition;
  li: TTreeNode;
  j: Integer;
  subli: TTreeNode;
begin
  c := AProperties.Count - 1;
  for i := 0 to c do
  begin
    propdef := TPropertyDefinition(AProperties[i]);

    if propdef.InClass <> '' then
    begin
      j := FClassNodeList.IndexOf(propdef.InClass);
      li := TTreeNode(FClassNodeList.Objects[j]);
    end
    else
    begin
      li := TTreeNode(FClassNodeList.Objects[0]);
    end;

    subli := li.Item[C_ClassPropertyNodeTreeIdx];

    treeMain.Items.AddChild(subli, propdef.PropertyName + ': ' + propdef.PropertyType);
  end;
end;

procedure TFrmUnitBrowserMain.AddVariablesToTree(const AVariables: TObjectList);
var
  i, c: integer;
  li: TTreeNode;
  j: Integer;
  vardef: TVariableDefinition;
  subli: TTreeNode;
begin
  c := AVariables.Count - 1;
  for i := 0 to c do
  begin
    vardef := TVariableDefinition(AVariables[i]);

    if vardef.InClass <> '' then
    begin
      j := FClassNodeList.IndexOf(vardef.InClass);
      if j <> -1 then
      begin
        li := TTreeNode(FClassNodeList.Objects[j]);
      end
      else
      begin
        raise Exception.Create('Class not found (' + vardef.InClass + ')');
      end;
    end
    else
    begin
      li := TTreeNode(FClassNodeList.Objects[0]);
    end;

    subli := li.Item[C_ClassVariableNodeTreeIdx];

    treeMain.Items.AddChild(subli, vardef.PropertyName + ': ' + vardef.PropertyType);
  end;
end;

end.
