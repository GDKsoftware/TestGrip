unit uXmlFuncs;

interface

uses
  SysUtils;

type
  EXMLException = class(Exception);
  EXMLNoDate = class(EXMLException);

  TXmlObject = class(TObject)
  public
    function AsXmlString: string; virtual; abstract;
  end;


function XmlFuncs_Escape( const s: string ): string;
function XmlFuncs_QuickNode( const sName: string; const sValue: string; bOptional: boolean = False ): string; overload;
function XmlFuncs_QuickNode( const sName: string; iValue: integer; bOptional: boolean = False  ): string; overload;
function XmlFuncs_QuickNode( const sName: string; bValue: boolean; bOptional: boolean = False  ): string; overload;
function XmlFuncs_QuickNode( const sName: string; dValue: Extended; bOptional: boolean = False  ): string; overload;
function XmlFuncs_QuickNode( const sName: string; v: Variant; bOptional: boolean = False  ): string; overload;
function XmlFuncs_QuickNode_dt( const sName: string; dtValue: TDateTime; bOptional: boolean = false ): string; overload;

function XmlFuncs_NodeEncaps( const sName: string; const pXmlObject: TXmlObject; bOptional: boolean = True ): string; overload;
function XmlFuncs_NodeEncaps( const sName: string; const sValue: string ): string; overload;

function XmlFuncs_DateTimeStamp( dt: TDateTime ): string;
function XmlFuncs_DateStamp( dt: TDateTime ): string;
function XmlFuncs_DtStrToDateTime( const sDt: string ): TDateTime;

function XmlFuncs_FormatFloat(dValue: Extended): string;
function XmlFuncs_ParseFloat(const s: string; dDefault: Extended): Extended;

function HtmlEntities( const s: string ): string;
function HtmlToAnsi( const s: string ): string;

function Htmlnl2br( const s: string ): string;

function StripXml(const s: string): string;


procedure ToggleXMLEncodingHintInFile(const sFilename: string; const sFromEncoding, sToEncoding: string);

implementation

uses
  StrUtils, Variants, Controls {$ifdef VER150}, uD7Functions{$endif}, DateUtils,
  Math;

function XmlFuncs_Escape( const s: string ): string;
begin
  Result := s;
  Result := ReplaceStr(Result, '&', '&amp;');
  Result := ReplaceStr(Result, '"', '&quot;');
  Result := ReplaceStr(Result, '<', '&lt;');
  Result := ReplaceStr(Result, '>', '&gt;');
end;

function XmlFuncs_QuickNode( const sName: string; const sValue: string; bOptional: boolean ): string;
begin
  if sValue <> '' then
  begin
    Result := '<' + sName + '>' + XmlFuncs_Escape(sValue) + '</' + sName + '>';
  end
  else if not bOptional then
  begin
    Result := '<' + sName + ' />';
  end
  else
  begin
    Result := '';
  end;
end;

function XmlFuncs_NodeEncaps( const sName: string; const pXmlObject: TXmlObject; bOptional: boolean ): string;
var
  sValue: string;
begin
  if not Assigned(pXmlObject) and bOptional then
  begin
    Result := '';
  end
  else if not Assigned(pXmlObject) then
  begin
    Result := '<' + sName + ' />';
  end
  else
  begin
    sValue := pXmlObject.AsXmlString;

    if sValue <> '' then
    begin
      Result := '<' + sName + '>' + sValue + '</' + sName + '>';
    end
    else
    begin
      Result := '<' + sName + ' />';
    end;
  end;
end;

function XmlFuncs_NodeEncaps( const sName: string; const sValue: string ): string;
begin
  if sValue <> '' then
  begin
    Result := '<' + sName + '>' + sValue + '</' + sName + '>';
  end
  else
  begin
    Result := '<' + sName + ' />';
  end;
end;

function XmlFuncs_QuickNode( const sName: string; iValue: integer; bOptional: boolean ): string;
begin
  Result := XmlFuncs_QuickNode( sName, IntToStr(iValue), bOptional );
end;

function XmlFuncs_QuickNode( const sName: string; bValue: boolean; bOptional: boolean ): string;
begin
  if bValue then
  begin
    Result := XmlFuncs_QuickNode( sName, 'true', bOptional );
  end
  else
  begin
    Result := XmlFuncs_QuickNode( sName, 'false', bOptional );
  end;
end;

function XmlFuncs_FormatFloat(dValue: Extended): string;
var
  format: TFormatSettings;
begin
  if Trunc(dValue) = dValue then
  begin
    Result := IntToStr(Trunc(dValue));
  end
  else
  begin
    format.DecimalSeparator := '.';
    format.ThousandSeparator := #0;

    Result := FormatFloat('0.00', dValue, format);
  end;
end;

function XmlFuncs_ParseFloat(const s: string; dDefault: Extended): Extended;
var
  format: TFormatSettings;
begin
  format.DecimalSeparator := '.';
  format.ThousandSeparator := #0;

  Result := StrToFloatDef(s, dDefault, format);
end;

function XmlFuncs_QuickNode_dt( const sName: string; dtValue: TDateTime; bOptional: boolean ): string;
begin
  if dtValue = 0 then
  begin
    Result := XmlFuncs_QuickNode( sName, '', bOptional);
  end
  else
  begin
    Result := XmlFuncs_QuickNode( sName, XmlFuncs_DateTimeStamp(dtValue), bOptional );
  end;
end;

function XmlFuncs_QuickNode( const sName: string; dValue: Extended; bOptional: boolean ): string;
var
  format: TFormatSettings;
begin
  if Trunc(dValue) = dValue then
  begin
    Result := XmlFuncs_QuickNode( sName, IntToStr(Trunc(dValue)), bOptional);
  end
  else
  begin
    format.DecimalSeparator := '.';
    format.ThousandSeparator := #0;

    Result := XmlFuncs_QuickNode( sName, FormatFloat('0.00', dValue, format), bOptional );
  end;
end;

function XmlFuncs_QuickNode( const sName: string; v: Variant; bOptional: boolean ): string;
begin
  case VarType(v) of
    varEmpty, varNull:
    begin
      Result := XmlFuncs_QuickNode(sName, '', bOptional);
    end;
    varInteger, varShortInt, varWord, varByte, varLongWord:
    begin
      Result := XmlFuncs_QuickNode(sName, int(v), bOptional);
    end;
{$ifndef VER180}{$ifndef VER150}
    varInt64, varUInt64:
    begin
      Result := XmlFuncs_QuickNode(sName, int(v), bOptional);
    end;
{$endif}{$endif}
    varDouble, varCurrency:
    begin
      Result := XmlFuncs_QuickNode(sName, extended(v), bOptional);
    end;
    varString{$ifndef VER180}{$ifndef VER150}, varUString{$endif}{$endif}, varOleStr:
    begin
      Result := XmlFuncs_QuickNode(sName, string(v), bOptional);
    end;
    varDate:
    begin
      Result := XmlFuncs_QuickNode(sName, XmlFuncs_DateTimeStamp(TDateTime(v)), bOptional);
    end;
  else
    Result := XmlFuncs_QuickNode(sName, 'errorunknowntype', bOptional);
  end;
end;

function HtmlEntities( const s: string ): string;
begin
  Result := s;
//  Result := StringReplace( Result, '''', '&apos;', [rfReplaceAll] );
  Result := StringReplace( Result, '&', '&amp;', [rfReplaceAll] );
  Result := StringReplace( Result, '<', '&lt;', [rfReplaceAll] );
  Result := StringReplace( Result, '>', '&gt;', [rfReplaceAll] );
  Result := StringReplace( Result, '"', '&quot;', [rfReplaceAll] );
end;

function HtmlToAnsi( const s: string ): string;
begin
  Result := s;
  Result := StringReplace( Result, '&quot;', '"', [rfReplaceAll] );
  Result := StringReplace( Result, '&apos;', '''', [rfReplaceAll] );
  Result := StringReplace( Result, '&amp;', '&', [rfReplaceAll] );
  Result := StringReplace( Result, '&lt;', '<', [rfReplaceAll] );
  Result := StringReplace( Result, '&gt;', '>', [rfReplaceAll] );
end;

function Htmlnl2br( const s: string ): string;
begin
  Result := ReplaceStr( s, #13#10, '<br />' );
end;

function XmlFuncs_DateTimeStamp( dt: TDateTime ): string;
var
  fmt: TFormatSettings;
  s: string;
begin
  Result := '';

  fmt.DateSeparator := '-';
  fmt.TimeSeparator := ':';

  DateTimeToString( s, 'yyyy-mm-dd', dt,fmt );
  Result := Result + s;
  DateTimeToString( s, 'hh:nn:ss', dt, fmt );
  Result := Result + 'T' + s;
end;

function XmlFuncs_DateStamp( dt: TDateTime ): string;
var
  fmt: TFormatSettings;
  s: string;
begin
  Result := '';

  fmt.DateSeparator := '-';

  DateTimeToString( s, 'yyyy-mm-dd', dt,fmt );
  Result := Result + s;
end;

function XmlFuncs_DtStrToDateTime( const sDt: string ): TDateTime;
var
  cLen: integer;
begin
  cLen := Length(sDt);
  if cLen = 10 then
  begin
    Result := EncodeDate( StrToInt(copy(sDt,1,4)), StrToInt(copy(sDt,6,2)), StrToInt(copy(sDt,9,2)) );
  end
  else if cLen = 19 then
  begin
    Result :=
      EncodeDateTime(
        StrToInt(copy(sDt,1,4)), StrToInt(copy(sDt,6,2)), StrToInt(copy(sDt,9,2)),
        StrToInt(copy(sDt,12,2)), StrToInt(copy(sDt,15,2)), StrToInt(copy(sDt,18,2)),
        0
      );
  end
  else
  begin
    raise EXMLNoDate.Create('''' + sDt + ''' is not a date');
  end;
end;

function StripXml(const s: string): string;
var
  i1, i2: integer;
begin
  Result := '';

  i2 := 0;
  i1 := PosEx('<', s, i2 + 1);
  while (i1 <> 0) do
  begin
    Result := Result + Copy(s, i2 + 1, i1 - i2 - 1);

    i2 := PosEx('>', s, i1 + 1);
    if i2 <> 0 then
    begin
      i1 := PosEx('<', s, i2 + 1);
    end
    else
    begin
      break;
    end;
  end;
  
end;

procedure ToggleXMLEncodingHintInFile(const sFilename: string; const sFromEncoding, sToEncoding: string);
var
  f: File;
  sBuf, sFull: ansistring;
  i, r, c: integer;
  iBytesLeft: integer;
begin
  AssignFile(f, sFilename);
  Reset(f, 1);
  while not Eof(f) do
  begin
    SetLength(sBuf, 255);
    BlockRead(f, sBuf[1], 128, r);
    SetLength(sBuf, r);

    sFull := sFull + sBuf;
  end;
  CloseFile(f);

  c := Length(sFull);
  AssignFile(f, sFilename);
  ReWrite(f, 1);
  i := 0;
  while i < c do
  begin
    iBytesLeft := Math.Max(c - i, 128);
    BlockWrite(f, sBuf[1+i], iBytesLeft, r);

    if r <> iBytesLeft then
    begin
      // que?
      if r = 0 then
      begin
        raise Exception.Create('Unable to write bytes to file, file is now possibly corrupted.');
      end;
    end;

    Inc(i, r);

    sFull := sFull + sBuf;
  end;
  CloseFile(f);
end;

end.
