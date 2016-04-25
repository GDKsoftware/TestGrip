unit FMultitestEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Grids, uPascalDefs, uTestDefs,
  ActnList,  ActnMan, ToolWin, ImgList, Contnrs,
  FDetailsEdit, XPStyleActnCtrls,
  uUnitParser, System.Actions, ImageList;

type
  TfrmTestsMatrixEdit = class(TForm)
    pnlButtons: TPanel;
    pnlMain: TPanel;
    pgMain: TPageControl;
    tbsFunction: TTabSheet;
    mmInitcodeFunction: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    edDefineFunction: TEdit;
    tbsClass: TTabSheet;
    mmInitCodeClass: TMemo;
    Label3: TLabel;
    Label2: TLabel;
    edDefinesClass: TEdit;
    tbsTests: TTabSheet;
    gridTests: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ActionManager1: TActionManager;
    acAddTest: TAction;
    imglstMain: TImageList;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    acDelete: TAction;
    acDetails: TAction;
    btnCancel: TButton;
    btnSave: TButton;
    IconList: TImageList;
    PanelTop: TPanel;
    shapeHeader: TShape;
    lblClassText: TLabel;
    lblClass: TLabel;
    lblMethodText: TLabel;
    lblMethod: TLabel;
    lcResultType: TLabel;
    lblResultType: TLabel;
    mmSetupCodeClass: TMemo;
    Label1: TLabel;
    procedure acAddTestExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridTestsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure acDetailsExecute(Sender: TObject);
  private
    { Private declarations }
    FCurrentMethodDef: TMethodDefinition;
    FCurrentClass: TInputTestClass;
    FCurrentFunc: TInputFunction;

    FPlaceHolderFunc: TInputFunction;

    FSelectedRow: integer;
    FSelectedCol: integer;

    FResultType: string;

    function GetParamIndex(const sParamName: string): integer;

    procedure InitGridColumns;

    procedure InitClassFunctionFields;
    procedure SaveClassFunctionFields;

  public
    { Public declarations }
    procedure LoadFunction( const aMethodDef: TMethodDefinition; const aClass: TInputTestClass; const aFunction: TInputFunction; const oUnitParser: TUnitParser );
    procedure SaveTests;

  end;

implementation

{$R *.dfm}

uses
  Math;

{ TfrmTestsMatrixEdit }

procedure TfrmTestsMatrixEdit.acAddTestExecute(Sender: TObject);
begin
  gridTests.SetFocus;

  gridTests.RowCount := gridTests.RowCount + 1;
  gridTests.Objects[0,gridTests.RowCount - 1] := FPlaceHolderFunc.AddInputTest('t0');
  gridTests.Row := gridTests.RowCount - 1;
  gridTests.Col := 0;
end;

procedure TfrmTestsMatrixEdit.acDetailsExecute(Sender: TObject);
var
  frmDetails: TfrmDetailsEdit;
begin
  frmDetails := TfrmDetailsEdit.Create(nil);
  try
    {$ifndef VER150}
    frmDetails.PopupParent := Self;
    frmDetails.PopupMode := pmAuto;
    {$endif}

    frmDetails.SetData( gridTests.Cells[FSelectedCol,FSelectedRow] );

    if frmDetails.ShowModal = mrOk then
    begin
      gridTests.Cells[FSelectedCol, FSelectedRow] := frmDetails.GetData;
    end;
  finally
    frmDetails.Free;
  end;
end;

procedure TfrmTestsMatrixEdit.LoadFunction(
  const aMethodDef: TMethodDefinition; const aClass: TInputTestClass;
  const aFunction: TInputFunction; const oUnitParser: TUnitParser);
var
  i, c: integer;
  t: TInputTest;
  j, d: integer;
  k: integer;
  param: TInputParam;
  b: boolean;
begin
  FCurrentMethodDef := aMethodDef;

  FCurrentFunc := aFunction;
  FCurrentClass := aClass;

  lblClass.Caption := aClass.ClassName;
  lblMethod.Caption := aFunction.MethodName;

  if Assigned(FCurrentMethodDef) then
  begin
    aFunction.CachedType := FCurrentMethodDef.Functype;
  end;

  FResultType := aFunction.CachedType;

  if ( (FResultType <> 'void') and (FResultType <> '') )  then
  begin
    lblResultType.Caption := FResultType;
    lblMethodText.Caption := 'Function:';
  end
  else
  begin
    // Method type is procedure
    lblResultType.Visible := False;
    lcResultType.Visible  := False;

    lblMethodText.Caption := 'Procedure:';
    FResultType           := '';
  end;

  InitGridColumns;

  if aFunction.InitCode = '' then
  begin
    if FCurrentMethodDef.InClass <> '' then
    begin
      if assigned(oUnitParser) then
      begin
        aFunction.InitCode   := 'TestObj := ' + oUnitParser.GetCreateCodeFromClass(FCurrentMethodDef.InClass) + ';';
      end
      else
      begin
        aFunction.InitCode := 'TestObj := ' + FCurrentMethodDef.InClass + '.Create();';
      end;
    end
    else
    begin
      aFunction.InitCode := 'TestObj := TInhTest.Create();';
    end;
  end;

  InitClassFunctionFields;

  if FCurrentFunc.TestList.Count <> 0 then
  begin
    c := FCurrentFunc.TestList.Count - 1;

    gridTests.RowCount := Max(c + 2, 2);

    for i := 0 to c do
    begin
      t := TInputTest(FCurrentFunc.TestList[i]);

      gridTests.Objects[0,i + 1] := t;

      d := t.ParamList.Count - 1;
      for j := 0 to d do
      begin
        param := TInputParam(t.ParamList[j]);

        k := GetParamIndex(param.ParamName);
        if k <> -1 then
        begin
          gridTests.Cells[k,i+1] := param.ParamValue;
        end;
      end;

      if FResultType <> '' then
      begin
        gridTests.Cells[gridTests.ColCount - 1,i+1] := '' + VarToStr(t.Equals);
      end;
    end;
  end
  else
  begin
    // eerste test
    gridTests.Objects[0,gridTests.RowCount - 1] := TInputTest.Create(FPlaceHolderFunc);
  end;

  gridTestsSelectCell( gridTests, 0, 1, b);
end;

procedure TfrmTestsMatrixEdit.SaveClassFunctionFields;
begin
  FCurrentClass.CustomSetupCode := mmSetupCodeClass.Text;

  FCurrentFunc.InitCode := mmInitcodeFunction.Text;
  FCurrentClass.InitCode := mmInitCodeClass.Text;

  FCurrentFunc.Defines := edDefineFunction.Text;
  FCurrentClass.Defines := edDefinesClass.Text;
end;

procedure TfrmTestsMatrixEdit.SaveTests;
var
  i, c: integer;
  j, d: integer;
  tempt: TInputTest;
  newt: TInputTest;
  sParamName: string;
  p: integer;
begin
  c := gridTests.RowCount - 1;
  for i := 1 to c do
  begin
    tempt := TInputTest(gridTests.Objects[0,i]);
    if tempt.Parent = FPlaceHolderFunc then
    begin
      newt := FCurrentFunc.AddInputTest('t' + IntToStr(i));
    end
    else
    begin
      newt := tempt;
    end;

    newt.ClearInputParams;

    d := gridTests.ColCount - 1;
    if FResultType <> '' then
    begin
      Dec(d);
    end;

    for j := 0 to d do
    begin
      sParamName := gridTests.Cells[j,0];
      if sParamName <> '' then
      begin
        p := Pos(' (', sParamname);
        sParamName := Copy(sParamName, 1, p - 1);
        newt.AddInputParam( sParamName, gridTests.Cells[j,i], FCurrentMethodDef.ParamTypes[j] )
      end;
    end;

    if FResultType <> '' then
    begin
      newt.Equals := gridTests.Cells[d + 1, i];
    end;
  end;

  SaveClassFunctionFields;
end;

procedure TfrmTestsMatrixEdit.FormCreate(Sender: TObject);
begin
  FCurrentMethodDef := nil;
  FCurrentClass := nil;
  FCurrentFunc := nil;
  FPlaceHolderFunc := TInputFunction.Create(nil);
end;

procedure TfrmTestsMatrixEdit.FormDestroy(Sender: TObject);
begin
  FPlaceHolderFunc.Free;
end;

function TfrmTestsMatrixEdit.GetParamIndex(const sParamName: string): integer;
var
  i, c: integer;
begin
  Result := -1;

  c := FCurrentMethodDef.Parameters.Count - 1;
  for i := 0 to c do
  begin
    if SameText(sParamname, FCurrentMethodDef.Parameters[i]) then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TfrmTestsMatrixEdit.gridTestsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  sParamType: string;
  bCanShowDetails: boolean;
begin
  bCanShowDetails := False;
  FSelectedRow := ARow;
  FSelectedCol := ACol;

  if (ARow <> 0) then
  begin
    if ACol = gridTests.ColCount - 1 then
    begin
      if SameText(FCurrentMethodDef.Functype, 'string') or SameText(FCurrentMethodDef.Functype, 'ansistring') or SameText(FCurrentMethodDef.Functype, 'widestring') then
      begin
        bCanShowDetails := True;
      end;
    end
    else
    begin
      sParamType := FCurrentMethodDef.ParamTypes[ACol];

      if SameText(sParamType, 'string') then
      begin
        bCanShowDetails := True;
      end;
    end;

    CanSelect := True;
  end;

  acDetails.Enabled := bCanShowDetails;
end;

procedure TfrmTestsMatrixEdit.InitClassFunctionFields;
begin
  mmSetupCodeClass.Text := FCurrentClass.CustomSetupCode;

  mmInitcodeFunction.Text := FCurrentFunc.InitCode;
  mmInitCodeClass.Text := FCurrentClass.InitCode;

  edDefineFunction.Text := FCurrentFunc.Defines;
  edDefinesClass.Text := FCurrentClass.Defines;
end;

procedure TfrmTestsMatrixEdit.InitGridColumns;
var
  i, c: integer;
  iTotalWidth: integer;
begin
  gridTests.RowCount := 2;

  c := FCurrentMethodDef.Parameters.Count;

  iTotalWidth := 0;

  if FResultType <> '' then
  begin
    gridTests.ColCount := c + 1;
  end
  else
  begin
    gridTests.ColCount := c;
  end;

  c := FCurrentMethodDef.Parameters.Count - 1;
  for i := 0 to c do
  begin
    gridTests.ColWidths[i] := 100;
    if SameText(FCurrentMethodDef.ParamTypes[i],'string') then
    begin
      gridTests.ColWidths[i] := 200;
    end;

    iTotalWidth := iTotalWidth + gridTests.ColWidths[i];

    gridTests.Cells[i,0] := FCurrentMethodDef.Parameters[i] + ' (' + FCurrentMethodDef.ParamTypes[i] + ')';
  end;

  if FResultType <> '' then
  begin
    if (gridTests.ClientWidth - iTotalWidth - 25) < 100 then
    begin
      // eigenlijk de rest van de kolommen ook kleiner maken nu...
      gridTests.ColWidths[c+1] := 100;
    end
    else
    begin
      gridTests.ColWidths[c+1] := gridTests.ClientWidth - iTotalWidth - 25;
    end;

    gridTests.Cells[c+1,0] := 'equals';
  end;

  // eerste rij leegmaken
  c := gridTests.ColCount - 1;
  for i := 0 to c do
  begin
    gridTests.Cells[i,1] := '';
  end;
end;

end.
