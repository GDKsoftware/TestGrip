unit uUTF8Functions;

interface

{$ifndef VER180}{$ifndef VER150}
uses
  SysUtils;

type
    TUTF8EncodingWithoutBOM = class(TUTF8Encoding)
    protected
    public
      function GetPreamble: TBytes; override;
    end;

    function GetUTF8WithoutBOM: TEncoding;
{$endif}{$endif}

implementation

{$ifndef VER180}{$ifndef VER150}
uses
  Windows;

var
  GSingleUTF8WithoutBOM: TUTF8EncodingWithoutBOM;


function GetUTF8WithoutBOM: TEncoding;
var
  LEncoding: TEncoding;
begin
  if GSingleUTF8WithoutBOM = nil then
  begin
    LEncoding := TUTF8EncodingWithoutBOM.Create;
    if InterlockedCompareExchangePointer(Pointer(GSingleUTF8WithoutBOM), LEncoding, nil) <> nil then
      LEncoding.Free;
  end;
  Result := GSingleUTF8WithoutBOM;
end;


{ TUTF8EncodingWithoutBOM }

function TUTF8EncodingWithoutBOM.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;


initialization
finalization
  FreeAndNil(GSingleUTF8WithoutBOM);
{$endif}{$endif}

end.
