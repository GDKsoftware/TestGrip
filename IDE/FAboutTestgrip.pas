unit FAboutTestgrip;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TFrmAboutTestgrip = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BitBtn1: TBitBtn;
    Label5: TLabel;
    lblLicenseInfo: TLabel;
    Image1: TImage;
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  uCommonFunctions, uTestgripUIFuncs, uConst, ShellAPI;

procedure TFrmAboutTestgrip.Label4Click(Sender: TObject);
begin
  TCommonExecutionFunctions.OpenURL( 'http://www.gdcsoftware.com/' );
end;

end.
