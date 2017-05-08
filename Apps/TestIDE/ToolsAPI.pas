unit ToolsAPI;

interface

uses
  VCL.Forms,
  VCL.Menus,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Controls,
  System.Classes,
  System.Generics.Collections,
  GDCSynEdit, GDCSynMemo, GDCSynEditHighlighter, GDCSynHighlighterPas, GDCSynCompletionProposal;

type
  TBindingType = (btPartial);
  TKeyBindingResult = (krHandled);

  IOTAKeyContext = interface(IInterface)
    ['{A93B95B0-F3A9-446E-A7A6-9AB8151AE832}']

  end;

  TKeyBindingProc = procedure(const Context: IOTAKeyContext; KeyCode: TShortcut; var BindingResult: TKeyBindingResult) of object;

  IOTAKeyBindingServices = interface(IInterface)
    ['{F66A0A81-BF84-4439-B6BF-AE6D94F2BC5B}']

    procedure AddKeyBinding(const AKeys: array of Word; const AKeyProc: TKeyBindingProc; const AContext: Pointer; const AFlags: Integer = 0; const AKeyboard: string = ''; const AMenuItemName: string = '');
  end;

  IOTAKeyboardBinding = interface(IInterface)
    ['{B9CCDAB4-9FFD-4F21-AB12-5189F2148AA0}']

    function GetBindingType: TBindingType;
    function GetDisplayName: string;
    function GetName: string;
    procedure BindKeyboard(const BindingServices: IOTAKeyBindingServices);
  end;

  IOTAProject = interface(IInterface)
    ['{C827FC68-0CDC-4B57-AC45-52CD38ED483D}']

    function ProjectRootPath: string;
    function FileName: string;
    procedure GetCompleteFileList(const AList: TStrings);
  end;

  IOTAProjectGroup = interface
    ['{89865242-0666-4BC3-A3DF-2AF32BEA9783}']

    function ActiveProject: IOTAProject;
    procedure AddExistingProject(const AProjectFile: string);
  end;

  TNotifierObject = class(TInterfacedPersistent)
  public

  end;

  TOTAEditPos = record
    Line: Integer;
    Col: Integer;
  end;

  TOTACharPos = record
    Pos: Integer;
  end;

  IOTANotifier = interface
    ['{3224AC3C-67F1-4FBB-A9DF-48061965D7AA}']
  end;

  IOTAIDENotifier = interface
    ['{FF9F23A5-B622-4DE1-837C-D1EF46DBE95E}']
  end;

  TOTAFileNotification = class

  end;

  IOTAModule = interface
    ['{11965AAD-550B-459B-A189-7C6AE0473B18}']

  end;

  IOTAModuleServices = interface
    ['{5F3FAB50-1F9E-4C96-96BC-709C9F40DA28}']

    function ModuleCount: Integer;
    function Modules: TList<IOTAModule>;
  end;

  IOTAServices = interface
    ['{229E095A-BDC7-44B7-ACEC-77A7E7CEEBD5}']

    function AddNotifier(const ANotifier: IOTAIDENotifier): Integer;
    procedure RemoveNotifier(const AIndex: Integer);
  end;

  IOTAEditReader = interface
    ['{A172EF01-B781-4741-BCD3-5C6461DB3CA7}']

    function GetText(const APos: Integer; const ABuffer: PAnsiChar; const ABufferLength: Integer): Integer;
  end;

  IOTAEditBuffer = interface
    ['{FF3B8803-DB47-4C1B-98AA-95A4F23C3732}']

    function CreateReader: IOTAEditReader;
    function FileName: string;
    function Modified: Boolean;
  end;

  INTAEditWindow = interface
    ['{1DE2CDA0-9DCB-403B-A9FE-B77BA0DB8131}']

    function Form: TCustomForm;
  end;

  INTAServices = interface
    ['{F71073D3-2576-4499-89EF-26BFA6A7FDD8}']

    function MainMenu: TMainMenu;
  end;

  IOTAEditPosition = interface
    ['{7D88D8AD-1EA4-4D92-8C97-ADF94BDB5712}']

    function GoToLine(const ALineNumber: Integer): Boolean;
  end;

  IOTAEditView = interface
    ['{66A67FBD-2DE3-42C0-8C98-E0D627736329}']

    function Buffer: IOTAEditBuffer;
    function CursorPos: TOTAEditPos;

    procedure ConvertPos(const AFirstParam: Boolean; const ASecondParam: TOTAEditPos; var AThirdParam: TOTACharPos);
    function CharPosToPos(const APos: TOTACharPos): Integer;

    function GetEditWindow: INTAEditWindow;

    function Position: IOTAEditPosition;

    procedure Center(const ARow: Integer; const ACol: Integer);
  end;

  TOTAEditReader = class(TInterfacedPersistent, IOTAEditReader)
  protected
    FStream: TStringStream;
  public
    constructor Create(const ALines: TStrings);
    destructor Destroy; override;

    function GetText(const APos: Integer; const ABuffer: PAnsiChar; const ABufferLength: Integer): Integer;
  end;

  TOTAEditBuffer = class(TInterfacedPersistent, IOTAEditBuffer)
  protected
    FTab: TTabSheet;
    FFilename: string;

    function Editor: TSynEdit;
  public
    constructor Create(const ATab: TTabSheet; const AFilename: string);

    function CreateReader: IOTAEditReader;
    function FileName: string;
    function Modified: Boolean;
  end;

  TINTAEditWindow = class(TInterfacedPersistent, INTAEditWindow)
  protected
    FForm: TCustomForm;
  public
    constructor Create(const AForm: TCustomForm);

    function Form: TCustomForm;
  end;

  TOTAEditView = class(TInterfacedPersistent, IOTAEditView)
  protected
    FEditorTab: TTabSheet;
    FEditor: TSynEdit;
    FBuffer: TOTAEditBuffer;
  public
    constructor Create(const AEditorTab: TTabSheet);

    function Buffer: IOTAEditBuffer;
    function CursorPos: TOTAEditPos;

    procedure ConvertPos(const AFirstParam: Boolean; const ASecondParam: TOTAEditPos; var AThirdParam: TOTACharPos);
    function CharPosToPos(const APos: TOTACharPos): Integer;

    function GetEditWindow: INTAEditWindow;

    function Position: IOTAEditPosition;

    procedure Center(const ARow: Integer; const ACol: Integer);
  end;

  IOTAEditorServices = interface
    ['{5DC9FC3E-81A7-423B-8E5A-43E567256A54}']

    function TopView: IOTAEditView;
  end;

  IOTAMessageServices = interface
    ['{02AC06BA-ABC4-40D5-98E0-56501ED05653}']

    procedure AddTitleMessage(const AText: AnsiString);
    procedure AddWideTitleMessage(const AText: string);
  end;

  IOTAKeyboardServices = interface
    ['{1AD94C27-EADA-43F2-B063-72AC3C394AF5}']

    function AddKeyboardBinding(const AKeyboardBinding: IOTAKeyboardBinding): Integer;
    procedure RemoveKeyboardBinding(const AIndex: Integer);
  end;

  ICustomIDEServices = interface
    ['{10A6A18D-B3A6-4017-9911-405ADBFBB673}']

    function Editors: TPageControl;
    procedure UnloadAllEditors;
    procedure OpenProjectFiles(const AProject: IOTAProject);

    function GetActiveGroup: IOTAProjectGroup;
  end;

  IOTAActionServices = interface
    ['{B86273FF-00D0-48D5-8EFB-96EF9485DC77}']

    procedure OpenProject(const AFilename: string; const ANewGroup: Boolean);
    function OpenFile(const AFilename: string): Boolean;
    procedure SaveFile(const AFilename: string);
  end;

  TOTAKeyBindingServices = class(TInterfacedPersistent, IOTAKeyBindingServices)
  public
    procedure AddKeyBinding(const AKeys: array of Word; const AKeyProc: TKeyBindingProc; const AContext: Pointer; const AFlags: Integer = 0; const AKeyboard: string = ''; const AMenuItemName: string = '');
  end;
  
  TBorlandIDEServices = class(TInterfacedPersistent, ICustomIDEServices, IOTAEditorServices, IOTAMessageServices, IOTAKeyboardServices, INTAServices, IOTAServices, IOTAActionServices)
  private
    function OpenFileInEditor(const AFilename: string): Boolean;
    function FindTab(const ATabName: string): TTabSheet;
  protected
    FActiveProjectGroup: IOTAProjectGroup;
    FOTAKeyBindingServices: IOTAKeyBindingServices;
    function TabNameForFile(const AFileName: string): string;

    procedure OpenProjectFiles(const AProject: IOTAProject);
  public
    constructor Create;
  
    function Editors: TPageControl;
    procedure UnloadAllEditors;

    function GetActiveGroup: IOTAProjectGroup;

    procedure OpenProject(const AFilename: string; const ANewGroup: Boolean);
    function OpenFile(const AFilename: string): Boolean;
    procedure SaveFile(const AFilename: string);

    function TopView: IOTAEditView;
    procedure AddTitleMessage(const AText: AnsiString);
    procedure AddWideTitleMessage(const AText: string);
    function AddKeyboardBinding(const AKeyboardBinding: IOTAKeyboardBinding): Integer;
    procedure RemoveKeyboardBinding(const AIndex: Integer);
    function MainMenu: TMainMenu;
    function AddNotifier(const ANotifier: IOTAIDENotifier): Integer;
    procedure RemoveNotifier(const AIndex: Integer);

  end;

  TOTAProject = class(TInterfacedPersistent, IOTAProject)
  protected
    FProjectDPRFile: string;
    FProjectDProjFile: string;
  public
    constructor Create(const AProjectFile: string);

    function ProjectRootPath: string;
    function FileName: string;
    procedure GetCompleteFileList(const AList: TStrings);
  end;

  TOTAProjectGroup = class(TInterfacedPersistent, IOTAProjectGroup)
  protected
    FActiveProject: IOTAProject;
  public
    constructor Create;

    function ActiveProject: IOTAProject;

    procedure ResetGroup;
    procedure AddExistingProject(const AProjectFile: string);
  end;

  function GetActiveProject: IOTAProject;

var
  BorlandIDEServices: IInterface;

implementation

uses
  System.SysUtils, uProjectParser, uCommonFunctions;

{ TBorlandIDEServices }


function TBorlandIDEServices.AddKeyboardBinding(const AKeyboardBinding: IOTAKeyboardBinding): Integer;
begin
  Result := 0;
  AKeyboardBinding.BindKeyboard(FOTAKeyBindingServices);
end;

function TBorlandIDEServices.AddNotifier(const ANotifier: IOTAIDENotifier): Integer;
begin
  Result := 0;
end;

procedure TBorlandIDEServices.AddTitleMessage(const AText: AnsiString);
begin
  AddWideTitleMessage(String(AText));
end;

procedure TBorlandIDEServices.AddWideTitleMessage(const AText: string);
var
  I, C: Integer;
  J, D: Integer;
  PanelControl: TWinControl;
begin
  if Assigned(Application.MainForm) then
  begin
    C := Application.MainForm.ControlCount - 1;
    for I := 0 to C do
    begin
      if Application.MainForm.Controls[I].Name = 'pnlMessages' then
      begin
        PanelControl := TWinControl(Application.MainForm.Controls[I]);
        D := PanelControl.ControlCount - 1;
        for J := 0 to D do
        begin
          if PanelControl.Controls[J].Name = 'edMessages' then
          begin
            TMemo(PanelControl.Controls[J]).Lines.Add(AText);
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

constructor TBorlandIDEServices.Create;
begin
  inherited Create;

  FOTAKeyBindingServices := TOTAKeyBindingServices.Create;
end;

function TBorlandIDEServices.Editors: TPageControl;
var
  I, C: Integer;
begin
  Result := nil;

  if Assigned(Application.MainForm) then
  begin
    C := Application.MainForm.ControlCount - 1;
    for I := 0 to C do
    begin
      if Application.MainForm.Controls[I] is TPageControl then
      begin
        Result := TPageControl(Application.MainForm.Controls[I]);
        Exit;
      end;
    end;
  end;
end;

function TBorlandIDEServices.GetActiveGroup: IOTAProjectGroup;
begin
  Result := FActiveProjectGroup;
end;

function TBorlandIDEServices.MainMenu: TMainMenu;
var
  I, C: Integer;
begin
  Result := nil;

  C := Application.MainForm.ComponentCount - 1;
  for I := 0 to C do
  begin
    if Application.MainForm.Components[I] is TMainMenu then
    begin
      Result := TMainMenu(Application.MainForm.Components[I]);
      Exit;
    end;
  end;
end;

function TBorlandIDEServices.OpenFile(const AFilename: string): Boolean;
begin
  if SameText(ExtractFileExt(AFilename), '.dproj') then
  begin
    FActiveProjectGroup := TOTAProjectGroup.Create;
    FActiveProjectGroup.AddExistingProject(AFilename);

    Result := True;
  end
  else
  begin
    Result := OpenFileInEditor(AFilename);
  end;
end;

procedure TBorlandIDEServices.OpenProject(const AFilename: string; const ANewGroup: Boolean);
begin
  OpenFile(AFileName);
end;

procedure TBorlandIDEServices.OpenProjectFiles(const AProject: IOTAProject);
var
  Files: TStringList;
  RootPath: string;
  PasFile: string;
begin
  Files := TStringList.Create;
  try
    AProject.GetCompleteFileList(Files);

    RootPath := AProject.ProjectRootPath;
    for PasFile in Files do
    begin
      OpenFile(RootPath + PasFile);
    end;
  finally
    Files.Free;
  end;
end;

procedure TBorlandIDEServices.RemoveKeyboardBinding(const AIndex: Integer);
begin

end;

procedure TBorlandIDEServices.RemoveNotifier(const AIndex: Integer);
begin

end;

function TBorlandIDEServices.FindTab(const ATabName: string): TTabSheet;
var
  Idx: Integer;
begin
  Result := nil;

  for Idx := 0 to Editors.PageCount -1 do
  begin
    if SameText(Editors.Pages[Idx].Caption, ATabName) then
    begin
      Result := Editors.Pages[Idx];
      Exit;
    end;
  end;
end;

function TBorlandIDEServices.OpenFileInEditor(const AFilename: string): Boolean;
var
  PC: TPageControl;
  Tab: TTabSheet;
  Editor: TSynEdit;
  Highlighter: TSynPasSyn;
begin
  Result := False;

  PC := Editors;
  if Assigned(PC) then
  begin
    Tab := FindTab(TabNameForFile(AFilename));
    if not Assigned(Tab) then
    begin
      Tab := TTabSheet.Create(Editors);
      Tab.Caption := TabNameForFile(AFilename);
      Tab.PageControl := PC;

      Editor := TSynEdit.Create(Tab);
      Editor.Parent := Tab;
      Editor.Align := TAlign.alClient;
      Editor.Options := Editor.Options + [eoTabsToSpaces, eoTabIndent, eoTrimTrailingSpaces, eoEnhanceEndKey, eoEnhanceHomeKey];

      Highlighter := TSynPasSyn.Create(Tab);
      Editor.Highlighter := Highlighter;

      Tab.Tag := NativeInt(TOTAEditBuffer.Create(Tab, AFileName));

      PC.ActivePage := Tab;

      Editor.Lines.LoadFromFile(AFilename);
    end
    else
    begin
      PC.ActivePage := Tab;
    end;

    Result := True;
  end;
end;

procedure TBorlandIDEServices.SaveFile(const AFilename: string);
var
  PC: TPageControl;
  Reader: IOTAEditReader;
  Writer: TFileStream;
  BytesRead: Integer;
  CurrentPos: Integer;
  Buffer: ansistring;
  EditBuffer: TOTAEditBuffer;
begin
  // get active tab
  PC := Editors;
  if Assigned(PC) then
  begin
    EditBuffer := TOTAEditBuffer(PC.ActivePage.Tag);
    Reader := EditBuffer.CreateReader;

    if AFilename = '' then
    begin
      // if '', save as existing filename
      Writer := TFileStream.Create(EditBuffer.FileName, fmOpenWrite);
      try
        SetLength(Buffer, 1024);
        BytesRead := 1024;
        CurrentPos := 0;
        while BytesRead = 1024 do
        begin
          BytesRead := Reader.GetText(CurrentPos, @Buffer[1], 1024);
          Writer.Write(Buffer[1], BytesRead);
          Inc(CurrentPos, BytesRead);
        end;
      finally
        Writer.Free;
      end;

    end
    else
    begin
      // otherwise save as given filename

    end;
  end;
end;

function TBorlandIDEServices.TabNameForFile(const AFileName: string): string;
begin
  Result := TCommonFileNameFunctions.RemoveFileExtension(ExtractFileName(AFilename));
end;

function TBorlandIDEServices.TopView: IOTAEditView;
var
  PC: TPageControl;
begin
  Result := nil;

  PC := Editors;
  if Assigned(PC) then
  begin
    if Assigned(PC.ActivePage) then
    begin
      Result := TOTAEditView.Create(PC.ActivePage);
    end;
  end;
end;

procedure TBorlandIDEServices.UnloadAllEditors;
var
  I, C: Integer;
  PC: TPageControl;
begin
  PC := Editors;
  if Assigned(PC) then
  begin
    C := PC.ControlCount - 1;
    for I := 0 to C do
    begin
      PC.RemoveControl(PC.Controls[0]);
    end;
  end;
end;

function GetActiveProject: IOTAProject;
var
  ActiveProjectGroup: IOTAProjectGroup;
begin
  ActiveProjectGroup := (BorlandIDEServices as ICustomIDEServices).GetActiveGroup;
  if Assigned(ActiveProjectGroup) then
  begin
    Result := ActiveProjectGroup.ActiveProject;
  end;
end;

{ TOTAProject }

constructor TOTAProject.Create(const AProjectFile: string);
begin
  inherited Create;

  FProjectDProjFile := AProjectFile;
  FProjectDPRFile := TCommonFileNameFunctions.RemoveFileExtension(FProjectDProjFile) + '.dpr';
end;

function TOTAProject.FileName: string;
begin
  Result := FProjectDPROJFile;
end;

procedure TOTAProject.GetCompleteFileList(const AList: TStrings);
var
  Parser: TProjectParser;
begin
  Parser := TProjectParser.Create;
  try
    Parser.ParseDProj(FProjectDProjFile, True);

    AList.AddStrings(Parser.Files);
  finally
    Parser.Free;
  end;
end;

function TOTAProject.ProjectRootPath: string;
begin
  Result := ExtractFilePath(FProjectDPROJFile);
end;

{ TOTAProjectGroup }

function TOTAProjectGroup.ActiveProject: IOTAProject;
begin
  Result := FActiveProject;
end;

procedure TOTAProjectGroup.AddExistingProject(const AProjectFile: string);
begin
  ResetGroup;

  if Assigned(BorlandIDEServices) then
  begin
    FActiveProject := TOTAProject.Create(AProjectFile);
    (BorlandIDEServices as ICustomIDEServices).OpenProjectFiles(FActiveProject);
  end;
end;

constructor TOTAProjectGroup.Create;
begin

end;

procedure TOTAProjectGroup.ResetGroup;
begin
  FActiveProject := nil;
  if Assigned(BorlandIDEServices) then
  begin
    (BorlandIDEServices as ICustomIDEServices).UnloadAllEditors;
  end;
end;

{ TINTAEditWindow }

constructor TINTAEditWindow.Create(const AForm: TCustomForm);
begin
  inherited Create;

  FForm := AForm;
end;

function TINTAEditWindow.Form: TCustomForm;
begin
  Result := FForm;
end;

{ TOTAEditView }

function TOTAEditView.Buffer: IOTAEditBuffer;
begin
  Result := FBuffer;
end;

procedure TOTAEditView.Center(const ARow, ACol: Integer);
begin

end;

function TOTAEditView.CharPosToPos(const APos: TOTACharPos): Integer;
begin
  Result := APos.Pos;
end;

procedure TOTAEditView.ConvertPos(const AFirstParam: Boolean; const ASecondParam: TOTAEditPos; var AThirdParam: TOTACharPos);
var
  I, C: integer;
begin
  AThirdParam.Pos := 1;

  C := ASecondParam.Line - 1;
  for I := 0 to C do
  begin
    AThirdParam.Pos := AThirdParam.Pos + Length(FEditor.Lines[I]) + 2;
  end;

  AThirdParam.Pos := AThirdParam.Pos + ASecondParam.Col;
end;

constructor TOTAEditView.Create(const AEditorTab: TTabSheet);
begin
  inherited Create;

  FEditorTab := AEditorTab;
  FEditor := TSynEdit(AEditorTab.Controls[0]);
  FBuffer := TOTAEditBuffer(FEditorTab.Tag);
end;

function TOTAEditView.CursorPos: TOTAEditPos;
begin
  Result.Line := FEditor.CaretY - 1;
  Result.Col  := FEditor.CaretX - 1;
end;

function TOTAEditView.GetEditWindow: INTAEditWindow;
begin
  Result := TINTAEditWindow.Create(Application.MainForm);
end;

function TOTAEditView.Position: IOTAEditPosition;
begin

end;

{ TOTAEditReader }

constructor TOTAEditReader.Create(const ALines: TStrings);
begin
  inherited Create;

  FStream := TStringStream.Create(ALines.Text, TEncoding.Default);
end;

destructor TOTAEditReader.Destroy;
begin
  FreeAndNil(FStream);

  inherited;
end;

function TOTAEditReader.GetText(const APos: Integer; const ABuffer: PAnsiChar; const ABufferLength: Integer): Integer;
begin
  FStream.Position := APos;

  Result := FStream.Read(ABuffer^, ABufferLength);
end;

{ TOTAEditBuffer }

constructor TOTAEditBuffer.Create(const ATab: TTabSheet; const AFilename: string);
begin
  inherited Create;

  FFilename := AFilename;
  FTab := ATab;
end;

function TOTAEditBuffer.CreateReader: IOTAEditReader;
begin
  Result := TOTAEditReader.Create(Editor.Lines);
end;

function TOTAEditBuffer.Editor: TSynEdit;
var
  I, C: Integer;
begin
  Result := nil;

  C := FTab.ControlCount - 1;
  for I := 0 to C do
  begin
    if FTab.Controls[I] is TSynEdit then
    begin
      Result := TSynEdit(FTab.Controls[I]);
      Exit;
    end;
  end;
end;

function TOTAEditBuffer.FileName: string;
begin
  Result := FFilename;
end;

function TOTAEditBuffer.Modified: Boolean;
begin
  Result := True;
end;

{ TOTAKeyBindingServices }

procedure TOTAKeyBindingServices.AddKeyBinding(const AKeys: array of Word; const AKeyProc: TKeyBindingProc;
  const AContext: Pointer; const AFlags: Integer; const AKeyboard, AMenuItemName: string);
var
  I: Integer;
begin
  for I := Low(AKeys) to High(AKeys) do
  begin
    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage('AKeys[' + IntToStr(I) + '] = ' + IntToStr(AKeys[I]));
  end;
end;

initialization
  BorlandIDEServices := TBorlandIDEServices.Create;

end.


