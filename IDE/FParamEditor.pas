unit FParamEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmParamEditor = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    mmVars: TMemo;
    lblVariable: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmParamEditor: TfrmParamEditor;

implementation

{$R *.dfm}

end.
