unit FIDEMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FPropertyEditor, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus, Vcl.ComCtrls,
  System.Actions, Vcl.ActnList, Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan;

type
  TfrmIDEMain = class(TForm)
    pnlTestgripContainer: TPanel;
    MainMenu1: TMainMenu;
    pnlMessages: TPanel;
    edMessages: TMemo;
    File1: TMenuItem;
    Exit1: TMenuItem;
    pcUnits: TPageControl;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    ActionManager1: TActionManager;
    acSave: TAction;
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
  private
    { Private declarations }
    FIDEPath: string;

    procedure SaveCurrentFile;
  public
    { Public declarations }
  end;

var
  frmIDEMain: TfrmIDEMain;

implementation

{$R *.dfm}

uses ToolsAPI;

procedure TfrmIDEMain.acSaveExecute(Sender: TObject);
begin
  SaveCurrentFile;
end;

procedure TfrmIDEMain.Exit1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmIDEMain.FormCreate(Sender: TObject);
begin
  FIDEPath := ExtractFilePath(ParamStr(0));

  PropertyEditorComponentsRegister;

  GDC_frmPropertyEditor.Parent := pnlTestgripContainer;
end;

procedure TfrmIDEMain.FormShow(Sender: TObject);
begin
  (BorlandIDEServices as IOTAActionServices).OpenProject(FIDEPath + '..\demoproject\TheWorld.dproj', True);
end;

procedure TfrmIDEMain.SaveCurrentFile;
begin
  (BorlandIDEServices as IOTAActionServices).SaveFile('');
end;

end.
