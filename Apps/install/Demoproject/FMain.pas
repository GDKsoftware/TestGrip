unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, DBClient, StdCtrls, uWorld;

type
  TfrmWorld = class(TForm)
    lbObjects: TListBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edPersonName: TEdit;
    edLength: TEdit;
    edWeight: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnAddPerson: TButton;
    Label5: TLabel;
    edTreeName: TEdit;
    btnAddTree: TButton;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddPersonClick(Sender: TObject);
    procedure btnAddTreeClick(Sender: TObject);
  private
    FWorld : TWorld;
  public
    { Public declarations }
  end;

var
  frmWorld: TfrmWorld;

implementation

{$R *.dfm}


procedure TfrmWorld.btnAddPersonClick(Sender: TObject);
var
  aPerson: TPerson;
begin
  aPerson := TPerson.Create;
  aPerson.Name   := edPersonName.Text;
  aPerson.Length := StrToIntDef(edLength.Text,0);
  aPerson.Weight := StrToIntDef(edWeight.Text,0);
  FWorld.AddWorldObject(aPerson);

  lbObjects.AddItem('Person ' + aPerson.Name,aPerson);
end;

procedure TfrmWorld.btnAddTreeClick(Sender: TObject);
var
  aPerson: TPerson;
begin
  aPerson := TPerson.Create;
  aPerson.Name   := edTreeName.Text;
  FWorld.AddWorldObject(aPerson);

  lbObjects.AddItem('Tree ' + aPerson.Name,aPerson);
end;

procedure TfrmWorld.FormCreate(Sender: TObject);
begin
  FWorld := TWorld.Create;
end;

procedure TfrmWorld.FormDestroy(Sender: TObject);
begin
  FWorld.Free;
end;

end.
 := TWorld.Create;
end;

procedure TfrmWorld.FormDestroy(Sender: TObject);
begin
  FWorld.Free;
end;

end.
