unit FWarnings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmWarnings = class(TForm)
    lstWarnings: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWarnings: TfrmWarnings;

implementation

{$R *.dfm}

end.
