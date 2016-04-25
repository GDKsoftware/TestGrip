unit uTestgripUIFuncs;

interface

uses
  Windows;

procedure TestgripShowInfo( const sMessage: string );
procedure TestgripShowError( const sMessage: string );

implementation

procedure TestgripShowInfo( const sMessage: string );
begin
  MessageBox(0, PChar(sMessage), PChar('GDC Testgrip'), MB_OK or MB_ICONINFORMATION);
end;

procedure TestgripShowError( const sMessage: string );
begin
  MessageBox(0, PChar(sMessage), PChar('GDC Testgrip'), MB_OK or MB_ICONERROR);
end;

end.
