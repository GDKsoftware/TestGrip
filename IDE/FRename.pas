unit FRename;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ImgList,
  ActnList, XPStyleActnCtrls, ActnMan, System.Actions, System.ImageList;

type
  TfrmRename = class(TForm)
    IconList: TImageList;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    ActionManager1: TActionManager;
    acSave: TAction;
    asCancel: TAction;
    edName: TEdit;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }

    procedure SetData( const s: string );
    function GetData: string;
  public
    { Public declarations }
    property Data: string
      read GetData write SetData;

  end;

implementation

{$R *.dfm}

{ TfrmRename }

procedure TfrmRename.FormActivate(Sender: TObject);
begin
  try
    edName.SetFocus;
  except
  end;
end;

function TfrmRename.GetData: string;
begin
  Result := edName.Text;
end;

procedure TfrmRename.SetData(const s: string);
begin
  edName.Text := s;

  Self.Caption := 'Rename "' + s + '"...';
end;

end.
