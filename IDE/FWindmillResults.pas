unit FWindmillResults;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DockForm, Vcl.ComCtrls, Vcl.ExtCtrls,
  Runner.Interfaces, Output.Interfaces, System.Actions, Vcl.ActnList,
  Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.Buttons;

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
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tmrScanTimer(Sender: TObject);
    procedure acScanProjectExecute(Sender: TObject);
    procedure lstWindmillResultsDblClick(Sender: TObject);
  private
    { Private declarations }
    FWindmill: IWindMillPP;
    FOutput: IOutput;

    procedure FreeNodes(const ParentNode: TTreeNode); overload;
    procedure FreeNodes; overload;

    procedure JumpToFileLine(const Filepath: string; const Linenumber: Integer);
  public
    { Public declarations }
  end;

procedure WindmillResultsComponentsRegister;
procedure Register;

var
  EmbeddedFrmWindmillResults: TFrmWindmillResults;
  WindmillResultsNotifierIndex: Integer;

implementation

{$R *.dfm}

uses
  ToolsAPI, DeskUtil, Output.Treeview, WindmillPP.IDE;

procedure WindmillResultsComponentsRegister;
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
  end;

  // Register the IDE Notifier
//  Services := BorlandIDEServices as IOTAServices;
//  Assert(Assigned(Services), 'IOTAServices not available');
//  WindmillResultsNotifierIndex := Services.AddNotifier(TIdeNotifier.Create);
//
//  iKeyBindingIndex := (BorlandIDEServices As IOTAKeyboardServices).AddKeyboardBinding(TKeyboardBinding.Create);
end;

procedure Register;
begin
  WindmillResultsComponentsRegister;
end;

procedure TFrmWindmillResults.acScanProjectExecute(Sender: TObject);
begin
  tmrScan.Enabled := True;
end;

procedure TFrmWindmillResults.FormCreate(Sender: TObject);
begin
  inherited;

  FOutput := TOutputTreeview.Create(lstWindmillResults);
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

procedure TFrmWindmillResults.tmrScanTimer(Sender: TObject);
begin
  tmrScan.Enabled := False;

  FreeNodes;

  FWindmill := TWindmillPPIDE.Create(FOutput);
  FWindmill.Go;

  lstWindmillResults.FullExpand;
end;

end.
