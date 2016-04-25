unit FUnitSearchMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFrmUnitSearchMain = class(TForm)
    edSearchUnit: TEdit;
    btnSearch: TButton;
    edProject: TEdit;
    lstSearchResults: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    btnBrowseForProjectFile: TButton;
    procedure btnSearchClick(Sender: TObject);
    procedure lstSearchResultsDblClick(Sender: TObject);
    procedure btnBrowseForProjectFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure OpenUnit(const AFullPath: string);
  public
    { Public declarations }

  end;

var
  FrmUnitSearchMain: TFrmUnitSearchMain;

implementation

{$R *.dfm}

uses
  uUsedUnitSearch,
  ClipBrd;

procedure TFrmUnitSearchMain.btnBrowseForProjectFileClick(Sender: TObject);
var
  OpenFileDialog: TOpenDialog;
begin
  OpenFileDialog := TOpenDialog.Create(nil);
  try
    OpenFileDialog.Filter := '*.dproj|*.dproj';

    if OpenFileDialog.Execute then
    begin
      edProject.Text := OpenFileDialog.FileName;
    end;
  finally
    OpenFileDialog.Free;
  end;
end;

procedure TFrmUnitSearchMain.btnSearchClick(Sender: TObject);
var
  UsedUnitSearch: TUsedUnitSearch;
begin
  UsedUnitSearch := TUsedUnitSearch.Create(edProject.Text);
  try
    lstSearchResults.Items.AddStrings(UsedUnitSearch.Search(edSearchUnit.Text));
  finally
    UsedUnitSearch.Free;
  end;
end;

procedure TFrmUnitSearchMain.FormCreate(Sender: TObject);
begin
  if ParamCount >= 1 then
  begin
    edProject.Text := ParamStr(1);

    if ParamCount >= 2 then
    begin
      edSearchUnit.Text := ParamStr(2);
    end;
  end;
end;

procedure TFrmUnitSearchMain.lstSearchResultsDblClick(Sender: TObject);
var
  SelectedIdx: Integer;
  UnitNode: TUnitNode;
begin
  SelectedIdx := lstSearchResults.ItemIndex;
  if SelectedIdx <> -1 then
  begin
    UnitNode := TUnitNode(lstSearchResults.Items.Objects[SelectedIdx]);
    if Assigned(UnitNode) and (UnitNode.Path <> '') then
    begin
      OpenUnit(UnitNode.Path);
    end
    else
    begin
      OpenUnit(lstSearchResults.Items[SelectedIdx]);
    end;
  end;
end;

procedure TFrmUnitSearchMain.OpenUnit(const AFullPath: string);
var
  Clippy: TClipboard;
begin
  Clippy :=  TClipboard.Create;
  try
    Clippy.Open;
    Clippy.SetTextBuf(PChar(AFullPath));
    Clippy.Close;
  finally
    Clippy.Free;
  end;
end;

end.
