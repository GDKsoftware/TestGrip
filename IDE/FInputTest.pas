unit FInputTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uPascalDefs, StdCtrls, ComCtrls, Grids, uTestDefs,
  ExtCtrls, Menus, uUnitParser, GDCSynEdit, GDCSynMemo,
  GDCSynEditHighlighter, GDCSynHighlighterPas, ImgList,
  GDCSynCompletionProposal, Contnrs,
  uProjectParser
  {$ifdef DESIGNTIME}, SynCompletionProposal, SynEditHighlighter, SynHighlighterPas,
  SynMemo, SynEdit{$endif};

type
  TfrmInputTest = class(TForm)
    panelBottom: TPanel;
    btnCancel: TButton;
    btnSave: TButton;
    btnSaveAndAdd: TButton;
    PanelTop: TPanel;
    SynHighlighter: TSynPasSyn;
    PanelTestContent: TPanel;
    PanelParameters: TPanel;
    lblParamText: TLabel;
    lblParamDesc: TLabel;
    gridParams: TStringGrid;
    PanelEquals: TPanel;
    lblEqualsDesc: TLabel;
    lblEqualsText: TLabel;
    edEquals: TEdit;
    chkEqualsNot: TCheckbox;
    panelImplies: TPanel;
    lblHelpImplies: TLabel;
    lblImplies: TLabel;
    lblTestName: TLabel;
    edTestName: TEdit;
    lblClassText: TLabel;
    lblClass: TLabel;
    lblMethodText: TLabel;
    lblMethod: TLabel;
    lcResultType: TLabel;
    lblResultType: TLabel;
    shapeHeader: TShape;
    IconList: TImageList;
    SynCompletion: TSynCompletionProposal;
    SynCompletion2: TSynCompletionProposal;
    PanelTabAdvanced: TPanel;
    PanelAdvancedSettingsCaption: TPanel;
    shapeLineBelowAdvBtns: TShape;
    shapeAboveAdvBtns: TShape;
    btnShowAdvancedSettings: TButton;
    btnCodeView: TButton;
    pgMain: TPageControl;
    tabClass: TTabSheet;
    lblSetupClassSet: TLabel;
    lblSetupDescClassSet: TLabel;
    lblInitClassSet: TLabel;
    lblInitDescClassSet: TLabel;
    lblDefinesClass: TLabel;
    lblDefinesDescClass: TLabel;
    mmSetupcodeClass: TSynMemo;
    mmInitCodeClass: TSynMemo;
    edDefinesClass: TEdit;
    tabFunction: TTabSheet;
    lblInitFunctionSet: TLabel;
    lblInitDescFunctionSet: TLabel;
    lblDefinesFunctionSet: TLabel;
    lblDefinesDescFunctionSet: TLabel;
    lblSetupDescFunctionSet: TLabel;
    lblSetupFunctionSet: TLabel;
    mmInitcodeFunction: TSynMemo;
    edDefineFunction: TEdit;
    mmSetupcodeFunction: TSynMemo;
    tabTestSettings: TTabSheet;
    lblTestSettingsInit: TLabel;
    lblTestSettInitDesc: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    edDefines: TEdit;
    TabVariables: TTabSheet;
    Label2: TLabel;
    Label3: TLabel;
    mmClassVars: TSynMemo;
    Label4: TLabel;
    Label5: TLabel;
    mmFunctionVars: TSynMemo;
    Label6: TLabel;
    Label7: TLabel;
    mmTestVars: TSynMemo;
    Shape1: TShape;
    btnExtraUses: TButton;
    mmImplies: TSynEdit;
    mmInitCode: TSynEdit;
    Label9: TLabel;
    Label10: TLabel;
    mmPreImpliesCode: TSynEdit;
    lblFillLatestTestEqualsResult: TLabel;
    SynCompletion3: TSynCompletionProposal;
    TabSheet1: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    mmDescription: TSynMemo;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveAndAddClick(Sender: TObject);
    procedure edTestNameKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure PanelAdvancedSettingsCaptionClick(Sender: TObject);
    procedure btnShowAdvancedSettingsClick(Sender: TObject);
    procedure btnCodeViewClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExtraUsesClick(Sender: TObject);
    procedure SynCompletionExecute(Kind: SynCompletionType; Sender: TObject;
      var CurrentInput: string; var x, y: Integer; var CanExecute: Boolean);
    procedure lblFillLatestTestEqualsResultClick(Sender: TObject);
    procedure btnEqualsZoomInClick(Sender: TObject);
    procedure onTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    FUnitParser: TUnitParser;
    FDprParser: TUnitParser;
    FDprojParser: TProjectParser;

    FFunctionDef: TMethodDefinition;
    FLoadedTest: TInputTest;
    FTestUnitName: string;
    FCurrentPascalFile: TObject;

    FExtraUses: string;

    FLiveTestClass: TInputTestClass;
    FLiveTestFunction: TInputFunction;
    FLiveTest: TInputTest;

    FShowAdvancedSettings: Boolean;
    FFullWindowHeight: Integer;
    FHeightAboveAdvanced: Integer;
    FHeightBelowAdvanced: Integer;

    FHasParams: Boolean;
    FResultType: string;

    FNormalCompletionLines: TStrings;
    FNormalCompletionItems: TStrings;

    FCurrentClassFieldsLines: TStrings;
    FCurrentClassFieldsItems: TStrings;

    FFunctionDefSelfCreated: Boolean;

    FOnSave: TNotifyEvent;

    procedure CreateLiveTestClass;
    procedure SetFunctionType;
    procedure ShowCodeForm;

    function Validate: Boolean;
    procedure ShowAdvancedSettings();

    procedure LoadSynCompletion;
    procedure LoadClassFieldsSuggestions;

    function GetCompletionWordBase(const anEdit: TCustomSynEdit;
      startpos, endpos: TBufferCoord): string;

    procedure ZoomEqualsValue;
  public
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property LoadedTest: TInputTest read FLoadedTest;
    property CurrentPascalFile: TObject read FCurrentPascalFile
      write FCurrentPascalFile;

    function GetDefines: string;
    procedure GetParamList(var ParamList: TObjectList);
    procedure GetImplies(var Implies: TObjectList);
    function GetEquals: Variant;
    function GetInitCode: string;

    procedure LoadProjectFiles(const aDprFile: string);
    procedure LoadTest(const aInputTest: TInputTest; const sUnit: string;
      const MethodDefinition: TMethodDefinition = nil;
      iTestIndex: Integer = -1);
    procedure SaveToTest(const aInputTest: TInputTest);
    procedure SetFormView();
  end;

implementation

uses
  uCommonFunctions, StrUtils, uTestGen, FShowTestCode, FDetailsEdit,
  uD7Functions, uIDEGarbageCollector;

const
  PARAMCOL_NAME = 0;
  PARAMCOL_TYPE = 1;
  PARAMCOL_VALUE = 2;
  PANELHEIGHT_ADV_SETTINGS = 351;

{$R *.dfm}
  { TfrmInputTest }

procedure TfrmInputTest.btnCodeViewClick(Sender: TObject);
begin
  ShowCodeForm;
end;

procedure TfrmInputTest.btnEqualsZoomInClick(Sender: TObject);
begin
  ZoomEqualsValue
end;

procedure TfrmInputTest.btnExtraUsesClick(Sender: TObject);
var
  frmExtraUsesInput: TfrmDetailsEdit;
begin
  frmExtraUsesInput := TfrmDetailsEdit.Create(nil);
  try
    frmExtraUsesInput.Caption := 'Extra includes';
    frmExtraUsesInput.Width := 300;
    frmExtraUsesInput.Height := 300;
    frmExtraUsesInput.SetData(FExtraUses);

    if frmExtraUsesInput.ShowModal = mrOk then
    begin
      FExtraUses := frmExtraUsesInput.GetData;
    end;
  finally
    frmExtraUsesInput.Free;
  end;
end;

procedure TfrmInputTest.btnSaveAndAddClick(Sender: TObject);
begin
  if not Validate then
  begin
    ModalResult := mrNone;
  end;

  Hide;
end;

procedure TfrmInputTest.btnSaveClick(Sender: TObject);
begin
  if not Validate then
  begin
    ModalResult := mrNone;
  end
  else
  begin
    if Assigned(FOnSave) then
    begin
      FOnSave(Self);
    end;

    Hide;
  end;
end;

procedure TfrmInputTest.btnCancelClick(Sender: TObject);
begin
  if Assigned(FOnSave) then
  begin
    FOnSave(Self);
  end;
  Hide;
end;

procedure TfrmInputTest.btnShowAdvancedSettingsClick(Sender: TObject);
begin
  ShowAdvancedSettings;
end;

procedure TfrmInputTest.edTestNameKeyPress(Sender: TObject; var Key: Char);
begin
  Key := TCommonStringFunctions.OnlyNormalChars(Key);
end;

procedure TfrmInputTest.FormCreate(Sender: TObject);
begin
  FNormalCompletionLines := TStringList.Create;
  FNormalCompletionItems := TStringList.Create;
  FCurrentClassFieldsLines := TStringList.Create;
  FCurrentClassFieldsItems := TStringList.Create;

  FLiveTestClass := nil;
  FLiveTestFunction := nil;
  FLiveTest := nil;

  FDprParser := TUnitParser.Create;
  FDprojParser := TProjectParser.Create;

  FFunctionDefSelfCreated := False;

  FUnitParser := TUnitParser.Create;

  mmImplies.Options := mmImplies.Options - [eoScrollPastEol];
  mmSetupcodeClass.Options := mmSetupcodeClass.Options - [eoScrollPastEol];
  mmInitCodeClass.Options := mmInitCodeClass.Options - [eoScrollPastEol];
  mmInitcodeFunction.Options := mmInitcodeFunction.Options - [eoScrollPastEol];
  mmClassVars.Options := mmClassVars.Options - [eoScrollPastEol];
  mmFunctionVars.Options := mmFunctionVars.Options - [eoScrollPastEol];
  mmTestVars.Options := mmTestVars.Options - [eoScrollPastEol];
  mmInitCode.Options := mmInitCode.Options - [eoScrollPastEol];
  mmPreImpliesCode.Options := mmPreImpliesCode.Options - [eoScrollPastEol];
  mmDescription.Options := mmDescription.Options - [eoScrollPastEol];
{$IFDEF VER230}
(*
  btnSave.Images := IconList;
  btnSave.ImageIndex := 1;

  btnCancel.Images := IconList;
  btnCancel.ImageIndex := 0;
*)
{$ENDIF}
end;

procedure TfrmInputTest.FormDestroy(Sender: TObject);
begin
  IDEGarbageCollector.Remove(Self);

  FUnitParser.Free;
  FDprojParser.Free;
  FDprParser.Free;

  if FFunctionDefSelfCreated then
  begin
    FFunctionDef.Free;
  end;

  FCurrentClassFieldsItems.Free;
  FCurrentClassFieldsLines.Free;

  FNormalCompletionLines.Free;
  FNormalCompletionItems.Free;

  FLiveTestClass.Free;
end;

procedure TfrmInputTest.FormShow(Sender: TObject);
begin
  FHeightAboveAdvanced := (Height - ClientHeight) + PanelTop.Height +
    panelImplies.Top + panelImplies.Height;

  FHeightBelowAdvanced := PanelAdvancedSettingsCaption.Height +
    panelBottom.Height;

  FFullWindowHeight := FHeightAboveAdvanced + PANELHEIGHT_ADV_SETTINGS +
    FHeightBelowAdvanced;

  FShowAdvancedSettings := True;
  ShowAdvancedSettings;

  SynCompletion.InsertList.Assign(FNormalCompletionLines);
  SynCompletion.ItemList.Assign(FNormalCompletionItems);

  SynCompletion2.InsertList.Assign(FNormalCompletionLines);
  SynCompletion2.ItemList.Assign(FNormalCompletionItems);

  SynCompletion3.InsertList.Assign(FNormalCompletionLines);
  SynCompletion3.ItemList.Assign(FNormalCompletionItems);
end;

function TfrmInputTest.GetDefines: string;
begin
  Result := edDefines.Text;
end;

function TfrmInputTest.GetEquals: Variant;
begin
  Result := edEquals.Text;
end;

procedure TfrmInputTest.GetImplies(var Implies: TObjectList);
var
  i, c: Integer;
  Implier: TInputImplier;
begin
  c := mmImplies.Lines.Count - 1;
  for i := 0 to c do
  begin
    if Trim(mmImplies.Lines.Strings[i]) <> '' then
    begin
      Implier := TInputImplier.Create;
      Implier.Eval := mmImplies.Lines.Strings[i];
      Implies.Add(Implier);
    end;
  end;
end;

function TfrmInputTest.GetInitCode: string;
begin
  Result := mmInitCode.Text;
end;

procedure TfrmInputTest.GetParamList(var ParamList: TObjectList);
var
  nRow: Integer;
  Param: TInputParam;
begin
  for nRow := 1 to gridParams.RowCount - 1 do // from 1 not 0
  begin
    Param := TInputParam.Create;
    Param.ParamName := gridParams.Cells[PARAMCOL_NAME, nRow];
    Param.ParamValue := gridParams.Cells[PARAMCOL_VALUE, nRow];
    Param.ParamType := gridParams.Cells[PARAMCOL_TYPE, nRow];
    ParamList.Add(Param);
  end;
end;

procedure TfrmInputTest.lblFillLatestTestEqualsResultClick(Sender: TObject);
begin
  edEquals.Text := FLoadedTest.TestResult_EqualsWas;
end;

function TfrmInputTest.GetCompletionWordBase(const anEdit: TCustomSynEdit;
  startpos, endpos: TBufferCoord): string;
var
  i: Integer;
begin
  Result := '';

  for i := startpos.Line to endpos.Line do
  begin
    if (i = startpos.Line) and (i = endpos.Line) then
    begin
      Result := Result + Copy(anEdit.Lines[i - 1], startpos.Char,
        endpos.Char - startpos.Char);
    end
    else if (i = startpos.Line) then
    begin
      Result := Result + Copy(anEdit.Lines[i - 1], startpos.Char);
    end
    else if (i = endpos.Line) then
    begin
      Result := Result + Copy(anEdit.Lines[i - 1], 1, endpos.Char);
    end
    else
    begin
      Result := Result + anEdit.Lines[i - 1];
    end;
  end;
end;

procedure TfrmInputTest.LoadClassFieldsSuggestions;
var
  i, c: Integer;
  stream: TFileStream;
  prop: TPropertyDefinition;
  vardef: TVariableDefinition;
  meth: TMethodDefinition;
  sClassName: string;
  sTestSignature: string;
  s: string;
begin
  stream := TFileStream.Create(FTestUnitName, fmOpenRead);
  try
    FUnitParser.ParseUnit(stream, True, False, False, True);
  finally
    stream.Free;
  end;

  sClassName := FLoadedTest.Parent.Parent.Name;

  FCurrentClassFieldsLines.Clear;
  FCurrentClassFieldsItems.Clear;

  if Length(sClassName) > Length(SynCompletion.Columns[0].BiggestWord) then
  begin
    SynCompletion.Columns[0].BiggestWord := sClassName;
    SynCompletion2.Columns[0].BiggestWord := sClassName;
    SynCompletion3.Columns[0].BiggestWord := sClassName;
  end;

  c := FUnitParser.VariableList.Count - 1;
  for i := 0 to c do
  begin
    vardef := TVariableDefinition(FUnitParser.VariableList[i]);

    if SameText(vardef.InClass, sClassName) then
    begin
      FCurrentClassFieldsLines.Add(vardef.PropertyName);

      if Length(vardef.PropertyName) >
        Length(SynCompletion.Columns[1].BiggestWord) then
      begin
        SynCompletion.Columns[1].BiggestWord := vardef.PropertyName;
        SynCompletion2.Columns[1].BiggestWord := vardef.PropertyName;
        SynCompletion3.Columns[1].BiggestWord := vardef.PropertyName;
      end;

      FCurrentClassFieldsItems.Add(sClassName + ' \column{}' +
        vardef.PropertyName + '\column{}' + vardef.PropertyType);
    end;
  end;

  c := FUnitParser.PropertyList.Count - 1;
  for i := 0 to c do
  begin
    prop := TPropertyDefinition(FUnitParser.PropertyList[i]);

    if SameText(prop.InClass, sClassName) then
    begin
      FCurrentClassFieldsLines.Add(prop.PropertyName);

      if Length(prop.PropertyName) > Length(SynCompletion.Columns[1].BiggestWord)
      then
      begin
        SynCompletion.Columns[1].BiggestWord := prop.PropertyName;
        SynCompletion2.Columns[1].BiggestWord := prop.PropertyName;
        SynCompletion3.Columns[1].BiggestWord := prop.PropertyName;
      end;

      FCurrentClassFieldsItems.Add(sClassName + ' \column{}' + prop.PropertyName
        + '\column{}' + prop.PropertyType);
    end;
  end;

  sTestSignature := FLoadedTest.Parent.GuessSignature;

  c := FUnitParser.MethodList.Count - 1;
  for i := 0 to c do
  begin
    meth := TMethodDefinition(FUnitParser.MethodList[i]);

    if SameText(meth.InClass, sClassName) then
    begin
      if not Assigned(FFunctionDef) then
      begin
        if Pos(sTestSignature, meth.Signature) <> 0 then
        begin
          FFunctionDef := TMethodDefinition.Create(meth.RawMethod, False, csUnknown);
          FFunctionDefSelfCreated := True;
        end;
      end;

      FCurrentClassFieldsLines.Add(meth.DefMethodName);

      s := meth.DefMethodName + '(' + meth.RawParamDefinition + ')';
      if Length(s) > Length(SynCompletion.Columns[1].BiggestWord) then
      begin
        SynCompletion.Columns[1].BiggestWord := s;
        SynCompletion2.Columns[1].BiggestWord := s;
        SynCompletion3.Columns[1].BiggestWord := s;
      end;

      FCurrentClassFieldsItems.Add(sClassName + ' \column{}' + s + '\column{}' +
        meth.Functype);
    end;
  end;
end;

procedure TfrmInputTest.LoadProjectFiles(const aDprFile: string);
var
  stream: TFileStream;
  bIsDpk: Boolean;
begin
  if aDprFile <> '' then
  begin
    stream := TFileStream.Create(aDprFile, fmOpenRead);
    try
      bIsDpk := SameText(ExtractFileExt(aDprFile), '.dpk');
      FDprParser.ParseUnit(stream, True, True, bIsDpk, False);
    finally
      stream.Free;
    end;

    FDprojParser.ParseDProj(ChangeFileExt(aDprFile, '.dproj'), True);
  end;
end;

procedure TfrmInputTest.LoadSynCompletion;
begin
  FNormalCompletionLines.Clear;
  FNormalCompletionLines.Add('TestObj');
  if (FResultType <> '') then
  begin
    FNormalCompletionLines.Add('testResult');
  end;

  FNormalCompletionItems.Clear;
  FNormalCompletionItems.Add(FLoadedTest.Parent.Parent.Name +
    ' \column{}TestObj\column{}Inherited test object');
  if (FResultType <> '') then
  begin
    FNormalCompletionItems.Add(FResultType +
      ' \column{}testResult\column{}Contains function result');
  end;
end;

procedure TfrmInputTest.LoadTest(const aInputTest: TInputTest;
  const sUnit: string; const MethodDefinition: TMethodDefinition;
  iTestIndex: Integer);
var
  i, c: Integer;
  Param: TInputParam;
  j, d: Integer;
  testmdef: TMethodDefinition;
begin
  FLoadedTest := aInputTest;
  FTestUnitName := sUnit;

  if FTestUnitName <> '' then
  begin
    LoadClassFieldsSuggestions;
  end;

  SetFunctionType;

  if Assigned(MethodDefinition) and (iTestIndex <> -1) then
  begin
    aInputTest.DisplayName := 't_' + MethodDefinition.DefMethodName + '_' +
      IntToStr(iTestIndex);

    if MethodDefinition.InClass <> '' then
    begin
      if Assigned(FUnitParser) then
      begin
        aInputTest.InitCode := 'TestObj := ' +
          FUnitParser.GetCreateCodeFromClass(MethodDefinition.InClass) + ';';
      end
      else
      begin
        aInputTest.InitCode := 'TestObj := ' + MethodDefinition.InClass +
          '.Create();';
      end;
    end
    else
    begin
      aInputTest.InitCode := 'TestObj := TInhTest.Create();';
    end;

    if (aInputTest.Parent.Parent.CustomSetupCode = '') and (aInputTest.Parent.TestCount <= 1) then
    begin
      aInputTest.Parent.Parent.CustomSetupCode :=
        Trim(FDprParser.InitCode.Text);
      aInputTest.Parent.Parent.UseCustomSetupCode :=
        (aInputTest.Parent.Parent.CustomSetupCode <> '');
    end;

    if (aInputTest.Parent.Parent.Defines = '') and ((aInputTest.Parent.Parent.FunctionList.Count <= 1) and (aInputTest.Parent.TestCount <= 1)) then
    begin
      aInputTest.Parent.Parent.Defines := FDprojParser.Defines.DelimitedText;
    end;
  end;

  lblClass.Caption := aInputTest.Parent.Parent.ClassName;
  lblMethod.Caption := aInputTest.Parent.MethodName;
  edTestName.Text := aInputTest.DisplayName;

  mmDescription.Text := aInputTest.Description;

  // Class definities
  mmSetupcodeClass.Lines.Text := aInputTest.Parent.Parent.CustomSetupCode;
  mmInitCodeClass.Lines.Text := aInputTest.Parent.Parent.InitCode;
  edDefinesClass.Text := aInputTest.Parent.Parent.Defines;
  mmClassVars.Lines.Text := aInputTest.Parent.Parent.Vars;

  FExtraUses := aInputTest.Parent.Parent.ExtraUses;

  // Function Test definities
  mmSetupcodeFunction.Lines.Text := aInputTest.Parent.CustomSetupCode;
  mmInitcodeFunction.Lines.Text := aInputTest.Parent.InitCode;
  edDefineFunction.Text := aInputTest.Parent.Defines;
  mmFunctionVars.Lines.Text := aInputTest.Parent.Vars;

  // Test definities
  mmInitCode.Text := aInputTest.InitCode;
  edDefines.Text := aInputTest.Defines;
  mmTestVars.Lines.Text := aInputTest.Vars;

  mmPreImpliesCode.Text := aInputTest.PreImpliesCode;


  if not Assigned(FFunctionDef) and Assigned(FUnitParser) then
  begin
    testmdef := TMethodDefinition.Create(aInputTest.Parent.GuessSignature, true, csUnknown);
    try
      FFunctionDef := FUnitParser.FindClosestMethodDefinition(testmdef);
    finally
      testmdef.Free;
    end;
  end;

  lblMethodText.Font.Color := clBlack;
  lblMethodText.Hint := '';
  if Assigned(FFunctionDef) then
  begin
    if Assigned(FUnitParser) then
    begin
      if not FUnitParser.IsNonPrivate(FFunctionDef.Signature) then
      begin
        lblMethod.Caption := lblMethod.Caption + ' (Warning: private method cannot be tested)';
        lblMethod.Hint := 'Warning: private method cannot be tested';
        lblMethod.Font.Color := clRed;
      end;
    end;

    FHasParams := (FFunctionDef.Parameters.Count > 0);
    if (FHasParams) then
    begin
      gridParams.RowCount := FFunctionDef.Parameters.Count + 1;

      gridParams.Cells[PARAMCOL_NAME, 0] := 'Name';
      gridParams.Cells[PARAMCOL_VALUE, 0] := 'Value';
      gridParams.Cells[PARAMCOL_TYPE, 0] := 'Type';

      c := FFunctionDef.Parameters.Count - 1;
      for i := 0 to c do
      begin
        gridParams.Cells[PARAMCOL_NAME, i + 1] := FFunctionDef.Parameters[i];
        gridParams.Cells[PARAMCOL_TYPE, i + 1] := FFunctionDef.ParamTypes[i];

        d := aInputTest.ParamList.Count - 1;
        for j := 0 to d do
        begin
          Param := TInputParam(aInputTest.ParamList.Items[j]);
          if SameText(Param.ParamName, FFunctionDef.Parameters[i]) then
          begin
            if VarIsNull(Param.ParamValue) then
            begin
              gridParams.Cells[PARAMCOL_VALUE, i + 1] := 'nil';
            end
            else
            begin
              gridParams.Cells[PARAMCOL_VALUE, i + 1] := Param.ParamValue;
            end;
          end;
        end;
      end;
    end;
  end
  else
  begin
    FHasParams := (aInputTest.ParamList.Count > 0);
    if (FHasParams) then
    begin
      gridParams.RowCount := aInputTest.ParamList.Count + 1;

      gridParams.Cells[PARAMCOL_NAME, 0] := 'Name';
      gridParams.Cells[PARAMCOL_VALUE, 0] := 'Value';
      gridParams.Cells[PARAMCOL_TYPE, 0] := 'Type';

      for i := 0 to aInputTest.ParamList.Count - 1 do
      begin
        Param := TInputParam(aInputTest.ParamList.Items[i]);
        gridParams.Cells[PARAMCOL_NAME, i + 1] := Param.ParamName;
        gridParams.Cells[PARAMCOL_TYPE, i + 1] := Param.ParamType;
        if VarIsNull(Param.ParamValue) then
        begin
          gridParams.Cells[PARAMCOL_VALUE, i + 1] := 'nil';
        end
        else
        begin
          gridParams.Cells[PARAMCOL_VALUE, i + 1] := Param.ParamValue;
        end;
      end;
    end;
  end;

  if VarIsNull(aInputTest.Equals) then
  begin
    edEquals.Text := '';
  end
  else
  begin
    edEquals.Text := aInputTest.Equals;
  end;

  chkEqualsNot.Visible := True;
  if Assigned(FFunctionDef) then
  begin
    if SameText(FFunctionDef.Functype, 'boolean') then
    begin
      chkEqualsNot.Visible := False;
      aInputTest.EqualsNot := False;
    end;
  end;

  chkEqualsNot.Checked := aInputTest.EqualsNot;

  if (aInputTest.TestResult_EqualsExpected <> '') or
    (aInputTest.TestResult_EqualsWas <> '') then
  begin
    edEquals.ShowHint := True;
    edEquals.Hint := 'In your last testrun this function returned: "' +
      aInputTest.TestResult_EqualsWas + '"';

    lblFillLatestTestEqualsResult.Visible := True;
  end
  else
  begin
    edEquals.ShowHint := False;
    edEquals.Hint := '';

    lblFillLatestTestEqualsResult.Visible := False;
  end;

  mmImplies.Lines.Clear;
  for i := 0 to aInputTest.Implies.Count - 1 do
  begin
    mmImplies.Lines.Add(TInputImplier(aInputTest.Implies[i]).Eval);
  end;

  // Create testclass for live code view
  CreateLiveTestClass;
end;

procedure TfrmInputTest.onTextKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender is TWinControl then
  begin
    if (Shift = [ssCtrl]) then
    begin
      if (Key = 86) then    // ctrl V
      begin
        SendMessage(TWinControl(Sender).Handle, WM_Paste, 0, 0);
      end
      else if (Key = 67) then    // ctrl C
      begin
        SendMessage(TWinControl(Sender).Handle, WM_Copy, 0, 0);
      end
      else if (Key = 88) then    // ctrl X
      begin
        SendMessage(TWinControl(Sender).Handle, WM_Cut, 0, 0);
      end
    end;
  end;
end;

procedure TfrmInputTest.PanelAdvancedSettingsCaptionClick(Sender: TObject);
begin
  ShowAdvancedSettings();
end;

procedure TfrmInputTest.SaveToTest(const aInputTest: TInputTest);
var
  ParamList: TObjectList;
  Implies: TObjectList;
  i, c: Integer;
  aParam: TInputParam;
begin

  aInputTest.DisplayName := edTestName.Text;
  aInputTest.Description := mmDescription.Text;

  // Class definities
  aInputTest.Parent.Parent.CustomSetupCode := mmSetupcodeClass.Lines.Text;
  aInputTest.Parent.Parent.UseCustomSetupCode :=
    (Trim(aInputTest.Parent.Parent.CustomSetupCode) <> '');
  aInputTest.Parent.Parent.InitCode := mmInitCodeClass.Lines.Text;
  aInputTest.Parent.Parent.Defines := edDefinesClass.Text;
  aInputTest.Parent.Parent.Vars := mmClassVars.Lines.Text;

  aInputTest.Parent.Parent.ExtraUses := FExtraUses;

  // Function Test definities
  aInputTest.Parent.CustomSetupCode := mmSetupcodeFunction.Lines.Text;
  aInputTest.Parent.UseCustomSetupCode :=
    (Trim(aInputTest.Parent.CustomSetupCode) <> '');
  aInputTest.Parent.InitCode := mmInitcodeFunction.Lines.Text;
  aInputTest.Parent.Defines := edDefineFunction.Text;
  aInputTest.Parent.Vars := mmFunctionVars.Lines.Text;

  // Test definities
  aInputTest.Defines := edDefines.Text;
  aInputTest.InitCode := GetInitCode;
  aInputTest.Vars := mmTestVars.Lines.Text;

  aInputTest.PreImpliesCode := mmPreImpliesCode.Text;

  // loop through paramlist
  if (FHasParams) then
  begin
    ParamList := TObjectList.Create(True);
    try
      GetParamList(ParamList);

      // clear existing params and re-add everything
      aInputTest.ClearInputParams;

      c := ParamList.Count - 1;
      for i := 0 to c do
      begin
        aParam := TInputParam(ParamList.Items[i]);
        aInputTest.AddInputParam(aParam.ParamName, aParam.ParamValue,
          aParam.ParamType);
      end;
    finally
      ParamList.Free;
    end;
  end;

  aInputTest.Equals := GetEquals;
  aInputTest.EqualsNot := chkEqualsNot.Checked;

  Implies := TObjectList.Create(True);
  try
    // clear existing implies first so we won't get duplicates
    aInputTest.ClearImplies;

    GetImplies(Implies);

    c := Implies.Count - 1;
    for i := 0 to c do
    begin
      aInputTest.CopyImpliesFrom(TInputImplier(Implies[i]));
    end;
  finally
    Implies.Free;
  end;
end;

procedure TfrmInputTest.SetFormView;
begin
  // Weergave instellingen uitvoeren
  if Assigned(FLoadedTest) then
  begin
    FResultType := FLoadedTest.Parent.CachedType;
    if ((FResultType <> 'void') and (FResultType <> '')) then
    begin
      lblResultType.Caption := FResultType;
      lblMethodText.Caption := 'Function:';
    end
    else
    begin
      // Method type is procedure
      lblResultType.Visible := False;
      lcResultType.Visible := False;

      lblMethodText.Caption := 'Procedure:';
      FResultType := '';

      PanelEquals.Visible := False;
    end;

    // Method has no parameters
    if (not FHasParams) then
      PanelParameters.Visible := False
  end;
  LoadSynCompletion;

  pgMain.ActivePage := tabTestSettings;
end;

procedure TfrmInputTest.ShowCodeForm;
var
  frmShowTestcode: TfrmShowTestcode;
begin
  // Sla de gemaakte wijzigingen op in het LiveObject
  SaveToTest(FLiveTest);
  // Open scherm om code te tonen
  frmShowTestcode := TfrmShowTestcode.Create(nil);
  try
    frmShowTestcode.LoadTestCode(FLiveTest, FTestUnitName);
    frmShowTestcode.ShowModal;
  finally
    frmShowTestcode.Free;
  end;
end;

procedure TfrmInputTest.SynCompletionExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);
var
  currentpos: TBufferCoord;
  newpos1, newpos2: TBufferCoord;
  endpos: TBufferCoord;
  s: string;
  edit: TCustomSynEdit;
begin
  if Sender is TSynCompletionProposal then
  begin
    edit := TSynCompletionProposal(Sender).Editor;
  end
  else
  begin
    Exit;
  end;

  currentpos := edit.CaretXY;
  endpos := edit.WordEndEx(currentpos);

  newpos1 := edit.WordStartEx(currentpos);
  Dec(newpos1.Char);
  newpos2 := edit.WordStartEx(newpos1);

  s := GetCompletionWordBase(edit, newpos2, endpos);
  if edit = mmImplies then
  begin
    if SameText(s, 'testobj.') then
    begin
      SynCompletion.InsertList.Assign(FCurrentClassFieldsLines);
      SynCompletion.ItemList.Assign(FCurrentClassFieldsItems);
    end
    else
    begin
      SynCompletion.InsertList.Assign(FNormalCompletionLines);
      SynCompletion.ItemList.Assign(FNormalCompletionItems);
    end;
  end
  else if edit = mmInitCode then
  begin
    if SameText(s, 'testobj.') then
    begin
      SynCompletion2.InsertList.Assign(FCurrentClassFieldsLines);
      SynCompletion2.ItemList.Assign(FCurrentClassFieldsItems);
    end
    else
    begin
      SynCompletion2.InsertList.Assign(FNormalCompletionLines);
      SynCompletion2.ItemList.Assign(FNormalCompletionItems);
    end;
  end
  else if edit = mmPreImpliesCode then
  begin
    if SameText(s, 'testobj.') then
    begin
      SynCompletion3.InsertList.Assign(FCurrentClassFieldsLines);
      SynCompletion3.ItemList.Assign(FCurrentClassFieldsItems);
    end
    else
    begin
      SynCompletion3.InsertList.Assign(FNormalCompletionLines);
      SynCompletion3.ItemList.Assign(FNormalCompletionItems);
    end;
  end;
end;

procedure TfrmInputTest.SetFunctionType;
begin
  if Assigned(FFunctionDef) and Assigned(FLoadedTest) then
  begin
    FLoadedTest.Parent.CachedType := FFunctionDef.Functype;
  end;
end;

procedure TfrmInputTest.ShowAdvancedSettings;
begin
  FShowAdvancedSettings := not FShowAdvancedSettings;

  PanelTabAdvanced.Visible := FShowAdvancedSettings;
  if (FShowAdvancedSettings) then
  begin
    Width := 948;
    btnShowAdvancedSettings.Caption := 'Close advanced settings';
  end
  else
  begin
    Width := 700;
    btnShowAdvancedSettings.Caption := 'Show advanced settings';
  end;

end;

procedure TfrmInputTest.CreateLiveTestClass;
begin
  // TestObject aanmaken voor het live bijhouden van wijzigen, zodat de code hiervan getoond kan worden
  FLiveTestClass := TInputTestClass.Create;
  FLiveTestClass.CloneFrom(FLoadedTest.Parent.Parent);
  FLiveTestFunction := TInputFunction.Create(FLiveTestClass);
  FLiveTestFunction.CloneFrom(FLoadedTest.Parent);
  FLiveTest := TInputTest.Create(FLiveTestFunction);
  FLiveTest.CloneFrom(FLoadedTest);
  // De lijst met Tests bijwerken, zodat alleen de huidige test erin staat
  FLiveTestClass.FunctionList.Add(FLiveTestFunction);
  FLiveTestFunction.TestList.Add(FLiveTest);
end;

function TfrmInputTest.Validate: Boolean;
var
  i, c: Integer;
begin
  Result := True;

  // check params
  c := gridParams.RowCount - 1;
  for i := 1 to c do // from 1 not 0
  begin
    if gridParams.Cells[PARAMCOL_NAME, i] <> '' then
    begin
      if not SameText(gridParams.Cells[PARAMCOL_TYPE, i], 'string') and
        not SameText(gridParams.Cells[PARAMCOL_TYPE, i], 'ansistring') and
        not SameText(gridParams.Cells[PARAMCOL_TYPE, i], 'widestring') and
        (gridParams.Cells[PARAMCOL_VALUE, i] = '') then
      begin
        Result := False;

        MessageBox(Self.Handle, 'Parameter values missing', 'Test definition',
          MB_OK or MB_ICONERROR);

        Exit;
      end;
    end;
  end;

  // check equals if a function
  if Assigned(FFunctionDef) then
  begin
    if not SameText(FFunctionDef.Functype, 'void') and
      not SameText(FFunctionDef.Functype, 'string') and
      not SameText(FFunctionDef.Functype, 'ansistring') and
      not SameText(FFunctionDef.Functype, 'widestring') then
    begin
      if edEquals.Text = '' then
      begin
        Result := False;

        MessageBox(Self.Handle, 'Equals value missing', 'Test definition',
          MB_OK or MB_ICONERROR);

        Exit;
      end;
    end;
  end;

end;

procedure TfrmInputTest.ZoomEqualsValue;
var
  frmDetails: TfrmDetailsEdit;
begin
  frmDetails := TfrmDetailsEdit.Create(nil);
  try
{$IFNDEF VER150}
    frmDetails.PopupParent := Self;
    frmDetails.PopupMode := pmAuto;
{$ENDIF}
    frmDetails.SetData(edEquals.Text);

    if frmDetails.ShowModal = mrOk then
    begin
      edEquals.Text := frmDetails.GetData;
    end;
  finally
    frmDetails.Free;
  end;
end;

end.
