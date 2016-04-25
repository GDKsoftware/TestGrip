unit uD7Functions;

interface

uses
  Windows, StrUtils, SysUtils;

{$ifdef VER150}
function EndsStr(const ASubText, AText: string): Boolean;
function AnsiEndsStr(const ASubText, AText: string): Boolean;
function AnsiStrIComp(S1, S2: PAnsiChar): Integer;
function ReplaceStr(const AText, AFromText, AToText: string): string;
function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
function Copy(const s: string; iStart: integer; iCount: integer = -1): string; overload;
function StartsText(const ASubText, AText: string): Boolean;
function AnsiStartsText(const ASubText, AText: string): Boolean;
function EndsText(const ASubText, AText: string): Boolean;
function AnsiEndsText(const ASubText, AText: string): Boolean;
function ReplaceText(const AText, AFromText, AToText: string): string;
function AnsiReplaceText(const AText, AFromText, AToText: string): string;
function ContainsText(const AText, ASubText: string): Boolean;
function AnsiContainsText(const AText, ASubText: string): Boolean;
function StartsStr(const ASubText, AText: string): Boolean;
function AnsiStartsStr(const ASubText, AText: string): Boolean;
function SameStr(const S1, S2: string): Boolean;
function ContainsStr(const AText, ASubText: string): Boolean;
function AnsiContainsStr(const AText, ASubText: string): Boolean;
{$endif}

implementation


{$ifdef VER150}
function AnsiStrIComp(S1, S2: PAnsiChar): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - CSTR_EQUAL;
end;

function EndsStr(const ASubText, AText: string): Boolean;
begin
  Result := AnsiEndsStr(ASubText, AText);
end;

function AnsiEndsStr(const ASubText, AText: string): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') then
    Result := AnsiStrIComp(PChar(ASubText), PChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function ReplaceStr(const AText, AFromText, AToText: string): string;
begin
  Result := AnsiReplaceStr(AText, AFromText, AToText);
end;

function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function Copy(const s: string; iStart: integer; iCount: integer): string;
begin
  if iCount = -1 then
  begin
    Result := System.Copy(s,iStart,Length(s) - iStart + 1);
  end
  else
  begin
    Result := System.Copy(s,iStart,iCount);
  end;
end;

function StartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiStartsText(ASubText, AText);
end;

function AnsiStartsText(const ASubText, AText: string): Boolean;
var
  P: PChar;
  L, L2: Integer;
begin
  P := PChar(AText);
  L := Length(ASubText);
  L2 := Length(AText);
  if L > L2 then
    Result := False
  else
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      P, L, PChar(ASubText), L) = 2;
end;

function EndsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiEndsText(ASubText, AText);
end;

function AnsiEndsText(const ASubText, AText: string): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') then
    Result := AnsiStrIComp(PChar(ASubText), PChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function ReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := AnsiReplaceText(AText, AFromText, AToText);
end;

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function ContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiContainsText(AText, ASubText);
end;

function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(AnsiUppercase(ASubText), AnsiUppercase(AText)) > 0;
end;

function StartsStr(const ASubText, AText: string): Boolean;
begin
  Result := AnsiStartsStr(ASubText, AText);
end;

function AnsiStartsStr(const ASubText, AText: string): Boolean;
begin
  Result := AnsiSameStr(ASubText, Copy(AText, 1, Length(ASubText)));
end;

function SameStr(const S1, S2: string): Boolean;
begin
  Result := AnsiSameStr(S1, S2);
end;

function ContainsStr(const AText, ASubText: string): Boolean;
begin
  Result := AnsiContainsStr(AText, ASubText);
end;

function AnsiContainsStr(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(ASubText, AText) > 0;
end;
{$endif}

end.
