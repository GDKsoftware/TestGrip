unit FWindmillResults;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DockForm, Vcl.ComCtrls, Vcl.ExtCtrls,
  Runner.Interfaces, Output.Interfaces, System.Actions, Vcl.ActnList,
  Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.Buttons,
  ToolsAPI;

type
  TFrmWindmillResults = class(TDockableForm)
    tmrScan: TTimer;
    pnlMain: TPanel;
    lstWindmillResults: TTreeView;
    ToolBar1: TToolBar;
    actWindmillResults: TActionList;
    acScanProject: TAction;
    lstImages: TImageList;
    sbScanProject: TSpeedButton;
    tmrInit: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrScanTimer(Sender: TObject);
    procedure acScanProjectExecute(Sender: TObject);
    procedure lstWindmillResultsDblClick(Sender: TObject);
    procedure tmrInitTimer(Sender: TObject);
  private
    { Private declarations }
    FWindmill: IWindMillPP;
    FOutput: IOutput;

    procedure miShowWindmillResultsClick(Sender: TObject);

    procedure RemoveMenusFromIDE(const Services: INTAServices);
    procedure AddMenusToIDE(const Services: INTAServices);

    procedure FreeNodes(const ParentNode: TTreeNode); overload;
    procedure FreeNodes; overload;

    procedure JumpToFileLine(const Filepath: string; const Linenumber: Integer);
  public
    { Public declarations }

    destructor Destroy; override;
  end;

  TWindmillIdeNotifier = class(TNotifierObject, IOTANotifier, IOTAIDENotifier)
  protected
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
  end;


procedure WindmillComponentsRegister;
procedure WindmillComponentsUnregister;

procedure Register;

var
  EmbeddedFrmWindmillResults: TFrmWindmillResults;
  WindmillResultsNotifierIndex: Integer;

implementation

{$R *.dfm}

uses
  DeskUtil, Output.Treeview, WindmillPP.IDE, Menus;

procedure TWindmillIdeNotifier.AfterCompile(Succeeded: Boolean);
begin

end;

procedure TWindmillIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin

end;

procedure TWindmillIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
begin

end;

procedure RemoveWindmillNotifier;
var
  Services: IOTAServices;
begin
  if WindmillResultsNotifierIndex <> -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    if assigned(Services) then
      Services.RemoveNotifier(WindmillResultsNotifierIndex);
  end;
end;

procedure TFrmWindmillResults.acScanProjectExecute(Sender: TObject);
begin
  tmrScan.Enabled := True;
end;

procedure TFrmWindmillResults.FormCreate(Sender: TObject);
begin
  inherited;

  DeskSection := Name;
  AutoSave := true;

  FOutput := TOutputTreeview.Create(lstWindmillResults);
  tmrInit.Enabled := True;
end;

destructor TFrmWindmillResults.Destroy;
begin
  SaveStateNecessary := true;

  EmbeddedFrmWindmillResults := nil;

  inherited;
end;

procedure TFrmWindmillResults.FormShow(Sender: TObject);
begin
  tmrScan.Enabled := True;
end;

procedure TFrmWindmillResults.lstWindmillResultsDblClick(Sender: TObject);
var
  SelectedNode: TTreeNode;
  ParentNode: TTreeNode;
  InfoData: TOutputMessage;
  SourcefileData: TOutputSourcefile;
begin
  SelectedNode := lstWindmillResults.Selected;
  if Assigned(SelectedNode) then
  begin
    ParentNode := SelectedNode.Parent;
    if Assigned(ParentNode) then
    begin
      if TObject(ParentNode.Data) is TOutputSourcefile then
      begin
        SourcefileData := TOutputSourcefile(ParentNode.Data);
        if TObject(SelectedNode.Data) is TOutputMessage then
        begin
          InfoData := TOutputMessage(SelectedNode.Data);

          JumpToFileLine(SourcefileData.Filepath, InfoData.Linenumber);
        end;
      end;
    end;
  end;
end;

procedure TFrmWindmillResults.JumpToFileLine(const Filepath: string; const Linenumber: Integer);
var
  EditView: IOTAEditView;
  EditWindow: INTAEditWindow;
  EditPosition: IOTAEditPosition;
  EditorServices: IOTAEditorServices;
  ActionServices: IOTAActionServices;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;

  EditView := nil;

  ActionServices := BorlandIDEServices as IOTAActionServices;
  if ActionServices.OpenFile(Filepath) then
  begin
    EditView := EditorServices.TopView;

    EditWindow := EditView.GetEditWindow;
    EditWindow.Form.Show;
    EditWindow.Form.SetFocus;

    EditPosition := EditView.Position;
    if Assigned(EditPosition) then
    begin
      EditPosition.GotoLine(Linenumber);
    end;

    EditView.Center(Linenumber, 0);
  end;
end;

procedure TFrmWindmillResults.FreeNodes(const ParentNode: TTreeNode);
var
  Node: TTreeNode;
  Data: TObject;
  Idx: Integer;
begin
  for Idx := 0 to ParentNode.Count - 1 do
  begin
    Node := ParentNode.Item[Idx];

    Data := TObject(Node.Data);
    Data.Free;
    Node.Data := nil;
  end;
end;

procedure TFrmWindmillResults.FreeNodes;
var
  Node: TTreeNode;
  Data: TObject;
begin
  for Node in lstWindmillResults.Items do
  begin
    Data := TObject(Node.Data);
    Data.Free;
    Node.Data := nil;

    FreeNodes(Node);
  end;

  lstWindmillResults.Items.Clear;
end;

procedure TFrmWindmillResults.tmrInitTimer(Sender: TObject);
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  inta: INTAServices;
begin
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    tmrInit.Enabled := False;

    inta := (BorlandIDEServices as INTAServices);

    RemoveMenusFromIDE(inta);
    AddMenusToIDE(inta);
  end;
end;

procedure TFrmWindmillResults.tmrScanTimer(Sender: TObject);
begin
  tmrScan.Enabled := False;

  FreeNodes;

  FWindmill := TWindmillPPIDE.Create(FOutput);
  FWindmill.Go;

  lstWindmillResults.FullExpand;
end;

procedure TFrmWindmillResults.RemoveMenusFromIDE(const Services: INTAServices);
var
  c: Integer;
  i: Integer;
begin
  c := Services.MainMenu.Items.Count - 1;
  for i := 0 to c do
  begin
    if SameText('Windmill', Services.MainMenu.Items[i].Hint) then
    begin
      // remove existing menu, referes to old memory (should also be removed on uninstall, but how?)
      Services.MainMenu.Items.Delete(i);
      break;
    end;
  end;
end;

procedure TFrmWindmillResults.AddMenusToIDE(const Services: INTAServices);
var
  miWindmill: TMenuItem;
  miWindmillScan: TMenuItem;
  miShowWindmillResults: TMenuItem;
begin
  miWindmill := TMenuItem.Create(Services.MainMenu);
  miWindmill.Caption := 'Windmill';
  miWindmill.Hint := 'Windmill';

  miShowWindmillResults := TMenuItem.Create(miWindmill);
  miShowWindmillResults.Caption := '&Show';
  miShowWindmillResults.Enabled := True;
  miShowWindmillResults.OnClick := miShowWindmillResultsClick;
  miWindmill.Add(miShowWindmillResults);

  miWindmillScan := TMenuItem.Create(miWindmill);
  miWindmillScan.Action := acScanProject;
  acScanProject.Enabled := True;
  miWindmill.Add(miWindmillScan);

  Services.MainMenu.Items.Add(miWindmill);
end;

procedure TFrmWindmillResults.miShowWindmillResultsClick(Sender: TObject);
begin
  EmbeddedFrmWindmillResults.Visible := True;
  try
    EmbeddedFrmWindmillResults.SetFocus;
  except
  end;
end;

procedure WindmillComponentsRegister;
var
  Services: IOTAServices;
begin
  // Create the form
  if EmbeddedFrmWindmillResults = nil then
  begin
    EmbeddedFrmWindmillResults := TFrmWindmillResults.Create(Application);
    if EmbeddedFrmWindmillResults.Floating then
    begin
      EmbeddedFrmWindmillResults.Show;
    end
    else
    begin
      EmbeddedFrmWindmillResults.ForceShow;
      FocusWindow(EmbeddedFrmWindmillResults);
    end;

    // Register to save position with the IDE
    RegisterDesktopFormClass(TFrmWindmillResults, 'EmbeddedFrmWindmillResults', EmbeddedFrmWindmillResults.Name);
    if (@RegisterFieldAddress <> nil) then
      RegisterFieldAddress(EmbeddedFrmWindmillResults.Name, @EmbeddedFrmWindmillResults);

    Services := BorlandIDEServices as IOTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    WindmillResultsNotifierIndex := Services.AddNotifier(TWindmillIdeNotifier.Create);
  end;
end;

procedure WindmillComponentsUnregister;
begin
  RemoveWindmillNotifier;

  if @UnregisterFieldAddress <> nil then
    UnregisterFieldAddress(@EmbeddedFrmWindmillResults);

  FreeAndNil(EmbeddedFrmWindmillResults);
end;


procedure Register;
begin
  WindmillComponentsRegister;
end;

initialization
  WindmillResultsNotifierIndex := -1;

finalization
  WindmillComponentsUnregister;
end.
