unit FDetailsEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ActnList, ActnMan,
  ImgList, XPStyleActnCtrls;

type
  TfrmDetailsEdit = class(TForm)
    pnlMain: TPanel;
    pnlButtons: TPanel;
    memData: TMemo;
    btnOk: TButton;
    btnCancel: TButton;
    ActionManager1: TActionManager;
    acSave: TAction;
    asCancel: TAction;
    IconList: TImageList;
    procedure acSaveExecute(Sender: TObject);
    procedure asCancelExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure SetData( const sData: string );
    function GetData: string;
  end;

implementation

{$R *.dfm}

{ TfrmDetailsEdit }

procedure TfrmDetailsEdit.acSaveExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmDetailsEdit.asCancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmDetailsEdit.GetData: string;
begin
  Result := memData.Text;
end;

procedure TfrmDetailsEdit.SetData(const sData: string);
begin
  memData.Text := sData;
end;

end.
