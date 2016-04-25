unit FUnitBrowserMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ComCtrls, ShellAPI, uUnitParser, uPascalDefs;

type
  TFrmUnitBrowserMain = class(TForm)
    treeMain: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FParser: TUnitParser;

    FClassNodeList: TStringList;
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

uses
  Contnrs;

const
  C_APPTITLE = 'UnitBrowser v1.0';

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
var
  i, c: integer;
  j: integer;
  li: TTreeNode;
  subli: TTreeNode;
  propdef: TPropertyDefinition;
  vardef: TVariableDefinition;
  methoddef: TMethodDefinition;
  s: string;
begin
  s := 'global';
  li := treeMain.Items.AddChild(nil, s);
  treeMain.Items.AddChild(li,'Variables');
  treeMain.Items.AddChild(li,'Properties');
  treeMain.Items.AddChild(li,'Methods');
  FClassNodeList.AddObject( s, li );

  c := FParser.InterfaceClassList.Count - 1;
  for i := 0 to c do
  begin
    s := FParser.InterfaceClassList.Names[i];
    li := treeMain.Items.AddChild(nil, s);

    treeMain.Items.AddChild(li,'Variables');
    treeMain.Items.AddChild(li,'Properties');
    treeMain.Items.AddChild(li,'Methods');

    FClassNodeList.AddObject( s, li );
  end;

  c := FParser.VariableList.Count - 1;
  for i := 0 to c do
  begin
    vardef := TVariableDefinition(FParser.VariableList[i]);

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

    subli := li.Item[0];

    treeMain.Items.AddChild(subli, vardef.PropertyName + ': ' + vardef.PropertyType);
  end;

  c := FParser.PropertyList.Count - 1;
  for i := 0 to c do
  begin
    propdef := TPropertyDefinition(FParser.PropertyList[i]);

    if propdef.InClass <> '' then
    begin
      j := FClassNodeList.IndexOf(propdef.InClass);
      li := TTreeNode(FClassNodeList.Objects[j]);
    end
    else
    begin
      li := TTreeNode(FClassNodeList.Objects[0]);
    end;

    subli := li.Item[1];

    treeMain.Items.AddChild(subli, propdef.PropertyName + ': ' + propdef.PropertyType);
  end;

  c := FParser.MethodList.Count - 1;
  for i := 0 to c do
  begin
    methoddef := TMethodDefinition(FParser.MethodList[i]);

    if methoddef.InClass <> '' then
    begin
      j := FClassNodeList.IndexOf(methoddef.InClass);
      if j <> -1 then
      begin
        li := TTreeNode(FClassNodeList.Objects[j]);
      end
      else
      begin
        s := methoddef.InClass;
        li := treeMain.Items.AddChild(nil, s);
        treeMain.Items.AddChild(li,'Variables');
        treeMain.Items.AddChild(li,'Properties');
        treeMain.Items.AddChild(li,'Methods');
        FClassNodeList.AddObject( s, li );
      end;
    end
    else
    begin
      li := TTreeNode(FClassNodeList.Objects[0]);
    end;

    subli := li.Item[2];
    treeMain.Items.AddChild(subli, methoddef.DefMethodName + '(' + methoddef.ParamsAndTypes + '): ' + methoddef.Functype);
  end;

  treeMain.FullExpand;
end;

end.
