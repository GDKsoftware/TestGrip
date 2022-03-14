unit uCommonFunctions;

interface

{$I ..\Testgrip.inc}

uses
  Classes, StrUtils;

type
  TCharSet = set of AnsiChar;
  TLFType = (lfDos, lfUnix, lfMac);

  TCommonAppFunctions = class
  public
    class function GetAppPath: string;
    class function GetAllEnvVars(const Vars: TStrings): Integer;
    class function GetUniqueID: string;
    class function GetAppDataFolder: string;
    class function GetSpecialWindowsFolder(aCSIDL: integer): string;
    class function HasAppParam(const s: string): boolean;
    class function GetAppParamStartingWith(const s: string): integer;
  end;

  TCommonStringFunctions = class
  public
    class function RPos(const SubStr: string; const S: string): integer;
    class function ContainsNonVisibleOrCRLFCharacters(const s: ansistring):boolean;
    class function CountLines(const s: TStream; iTypeOfLF: TLFType; iMaxLen: integer): integer; overload;
    {$IFDEF DELPHI2009_UP}
    class function CountLines(const s: string; iTypeOfLF: TLFType = lfDos; iMaxLen: integer = -1): integer; overload;
    {$ELSE}
    class function CountLines(const s: AnsiString; iTypeOfLF: TLFType = lfDos; iMaxLen: integer = -1): integer; overload;
    {$ENDIF}
    class function RemoveDoubleQuotes(const s: string): string;
    class function IsNumeric( const s: string ): boolean;
    class function OnlyNormalChars(const Key: Char): Char;
    class function SafeString(const aString: string): string;
  end;

  TCommonFileNameFunctions = class
  public
    class function RemoveFileExtension( const s: string ): string;
  end;

  TCommonFileSystemFunctions = class
  public
    class procedure GetFilesInDirectory(const aDirectory: string; var aList: TSTringlist; const aFileExt: string='*');
    class function CleanFolder(aFolder: string; aPattern: string): integer;
  end;

  TCommonExecutionFunctions = class
  public
    class function ExecuteAndWaitAndGetStdOut(const strCommandLine : String; intVisibility: Integer; var sOut: string) : Cardinal;
    class procedure ExecuteFile(const pFile: string; const pDirectory: string);
    class procedure OpenURL(const aURL: string);
  end;

  TCommonStringSearch = class
  public
    {$IFDEF DELPHI2009_UP}
    class function FindCharacterForward( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;
    class function FindCharacterBackwards( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;
    class function FindPreviousWord(const sBuffer: string; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
    class function FindNextWord(const sBuffer: string; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
    {$ELSE}
    class function FindCharacterForward( const sBuffer: ansistring; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;
    class function FindCharacterBackwards( const sBuffer: ansistring; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;
    class function FindPreviousWord(const sBuffer: ansistring; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
    class function FindNextWord(const sBuffer: ansistring; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
    {$ENDIF}
  end;

  TKeyValueFunctions = class
  public
    class function GetKey(const aParam: string): string;
    class function GetValue(const aParam: string): string;
  end;

  TBase64 = class
  public
    {$IFDEF DELPHI2009_UP}
    class function CharsToBase64( const sInput: string ): string;
    class function Encode(const s: string; bRemovePadding: boolean = false): string;
    class function Decode(const s: string): string;
    {$ELSE}
    class function CharsToBase64( const sInput: ansistring ): ansistring;
    class function Encode(const s: ansistring; bRemovePadding: boolean = false): ansistring;
    class function Decode(const s: ansistring): ansistring;
    {$ENDIF}
  end;


  {$ifdef NEEDCHARINCSET}
  function CharInSet(const AChar: Char; const ACharSet: TCharSet): Boolean;
  {$endif}

implementation

uses
  {$IFDEF DELPHI103_UP}
  System.Character,
  {$ENDIF}
  SysUtils, Windows, Forms, ShellApi, SHFolder, uD7Functions;

const
  c_base64table = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

{ TCommonStringFunctions }
class function TCommonStringFunctions.ContainsNonVisibleOrCRLFCharacters(const s: ansistring): boolean;
var
  i, c: integer;
begin
  Result := False;
  c := Length(s);
  for i := 1 to c do
  begin
    if (s[i] < #32) or (s[i] > #126) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

class function TCommonStringFunctions.CountLines(const s: TStream; iTypeOfLF: TLFType; iMaxLen: integer): integer;
const
  c_buffersize = 2048;
var
  i, j, c, d: integer;
  iSkip: integer;
  {$IFDEF DELPHI2009_UP}
  cFirstChar: char;
  block: string;
  {$ELSE}
  cFirstChar: ansichar;
  block: ansistring;
  {$ENDIF}
  bPayAttention: boolean;
begin
  Result := 0;
  bPayAttention := False;
  iSkip := 1;
  cFirstChar := #0;
  case iTypeOfLF of
    lfDos:
    begin
      iSkip := 2;
      cFirstChar := #13;
    end;
    lfUnix:
    begin
      iSkip := 1;
      cFirstChar := #10;
    end;
    lfMac:
    begin
      iSkip := 1;
      cFirstChar := #13;
    end;
  end;
  c := iMaxLen;
  d := 0;
  SetLength(block, c_buffersize);
  j := s.Read(block, c_buffersize);
  while j > 0 do
  begin
    SetLength(block, j);
    Inc(d, j);
    if d > c then
    begin
      Dec(d, j);
      bPayAttention := True;
    end;
    i := PosEx(cFirstChar, block, 1);
    while (i <> 0) and (i <= c) do
    begin
      if bPayAttention and ((d + i) > c) then
      begin
        d := c;
        break;
      end;
      Inc(Result);
      Inc(i, iSkip);
      i := PosEx(cFirstChar, block, i);
    end;
    if d >= c then
    begin
      break;
    end;
    j := s.Read(block, c_buffersize);
  end;
end;

{$IFDEF DELPHI2009_UP}
class function TCommonStringFunctions.CountLines(const s: string; iTypeOfLF: TLFType; iMaxLen: integer): integer;
{$ELSE}
class function TCommonStringFunctions.CountLines(const s: ansistring; iTypeOfLF: TLFType; iMaxLen: integer): integer;
{$ENDIF}
var
  i, c: integer;
  iSkip: integer;
  cFirstChar: char;
begin
  Result := 0;
  iSkip := 1;
  cFirstChar := #0;
  case iTypeOfLF of
    lfDos:
    begin
      iSkip := 2;
      cFirstChar := #13;
    end;
    lfUnix:
    begin
      iSkip := 1;
      cFirstChar := #10;
    end;
    lfMac:
    begin
      iSkip := 1;
      cFirstChar := #13;
    end;
  end;
  if iMaxLen = -1 then
  begin
    c := Length(s);
  end
  else
  begin
    c := iMaxLen;
  end;
  i := PosEx(cFirstChar, s, 1);
  while (i <> 0) and (i <= c) do
  begin
    Inc(Result);
    Inc(i, iSkip);
    i := PosEx(cFirstChar, s, i);
  end;
end;

class function TCommonStringFunctions.IsNumeric(const s: string): boolean;
{$IFNDEF DELPHI103_UP}
var
  i: integer;
  r: Extended;
{$ENDIF}
begin
{$IFDEF DELPHI103_UP}
  Result := IsNumeric(s);
{$ELSE}
  Val( s, r, i );
  Result := (i = 0);
{$ENDIF}
end;

class function TCommonStringFunctions.OnlyNormalChars(const Key: Char): Char;
const
   InvalidCharacters : set of ansichar = ['\', '/', ':', '*', '?', '"', '<', '>', '|',
     ' ','@','#','$','%','^','&','`'];
begin
  if CharInSet(Key, InvalidCharacters) then
    result := #0
  else
    result := Key;
end;

class function TCommonStringFunctions.RemoveDoubleQuotes(const s: string): string;
begin
  Result := s;
  if StartsStr('"', s) then
  begin
    Result := Copy(Result,2);
  end;
  if EndsStr('"', s) then
  begin
    SetLength(Result, Length(Result) - 1);
  end;
end;

class function TCommonStringFunctions.RPos(const SubStr, S: string): integer;
var
  I: integer;
  RevSubStr: string;
  RevS: string;
begin
  RevSubStr := ReverseString(SubStr);
  RevS := ReverseString(S);
  I := pos(RevSubStr, RevS);
  if I <> 0 then
  begin
    I := (Length(RevS) + 1) - (I + Length(RevSubStr) - 1);
  end;
  Result := I;
end;

class function TCommonStringFunctions.SafeString(const aString: string): string;
begin
  result := StringReplace(aString,'/','',[rfReplaceAll]);
  result := StringReplace(result,'\','',[rfReplaceAll]);
end;

{ TBase64 }

{$IFDEF DELPHI2009_UP}
class function TBase64.CharsToBase64(const sInput: string): string;
{$ELSE}
class function TBase64.CharsToBase64(const sInput: ansistring): ansistring;
{$ENDIF}
var
  a, b, c, d: byte;
begin
  a := (byte(sInput[1]) and $FC) shr 2;
  b := (byte(sInput[1]) and $03) shl 4;
  c := (byte(sInput[2]) and $F0) shr 4;
  Inc(b, c);
  c := (byte(sInput[2]) and $0F) shl 2;
  d := (byte(sInput[3]) and $C0) shr 6;
  Inc(c, d);
  d := (byte(sInput[3]) and $3F);
  Result :=
    c_base64table[1+a] +
    c_base64table[1+b] +
    c_base64table[1+c] +
    c_base64table[1+d];
end;

//  http://www.swissdelphicenter.ch/torry/showcode.php?id=1524
{$IFDEF DELPHI2009_UP}
class function TBase64.Decode(const s: string): string;
{$ELSE}
class function TBase64.Decode(const s: ansistring): ansistring;
{$ENDIF}
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], c_base64table) - 1;
    if x >= 0 then    // ignore non base64 characters (like crlf)
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + chr(x);
      end;
    end;
  end;
end;

{$IFDEF DELPHI2009_UP}
class function TBase64.Encode(const s: string; bRemovePadding: boolean): string;
{$ELSE}
class function TBase64.Encode(const s: ansistring; bRemovePadding: boolean): ansistring;
{$ENDIF}
var
  i, j, inl, c: integer;
{$IFDEF DELPHI2009_UP}
  ic: string;
{$ELSE}
  ic: ansistring;
{$ENDIF}
begin
  Result := '';
  SetLength(ic,3);
  ic[1] := #0;
  ic[2] := #0;
  ic[3] := #0;
  i := 1;
  j := 0;
  inl := 0;
  c := Length(s);
  while i <= c do
  begin
    ic[j+1] := s[i];
    Inc(j);
    Inc(i);
    j := j mod 3;
    if j = 0 then
    begin
      Result := Result + CharsToBase64(ic);
      Inc(inl);
      if inl = 19 then
      begin
        inl := 0;
        Result := Result + #13#10;
      end;
    end;
  end;
  if j = 2 then
  begin
    ic[3] := #0;
    Result := Result + CharsToBase64(ic);
    SetLength( Result, Length(Result) - 1 );
    if not bRemovePadding then
    begin
      Result := Result + '=';
    end;
  end
  else if j = 1 then
  begin
    ic[2] := #0;
    Result := Result + CharsToBase64(ic);
    SetLength( Result, Length(Result) - 2 );
    if not bRemovePadding then
    begin
      Result := Result + '==';
    end;
  end;
end;

{ TCommonStringSearch }

{$IFDEF DELPHI2009_UP}
class function TCommonStringSearch.FindCharacterForward( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean ): integer;
{$ELSE}
class function TCommonStringSearch.FindCharacterForward( const sBuffer: ansistring; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean ): integer;
{$ENDIF}
var
  i, c: integer;
begin
  Result := 0;
  i := iStartPos;
  c := Length(sBuffer);
  while (i < c) do
  begin
    if CharInSet(sBuffer[i], aCharlist) then
    begin
      Result := i;
      if not bReadUntilLast then
      begin
        break;
      end;
    end
    else
    begin
      if bReadUntilLast and (Result <> 0) then
      begin
        break;
      end;
    end;
    Inc(i);
  end;
end;

{$IFDEF DELPHI2009_UP}
class function TCommonStringSearch.FindCharacterBackwards( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean ): integer;
{$ELSE}
class function TCommonStringSearch.FindCharacterBackwards( const sBuffer: ansistring; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean ): integer;
{$ENDIF}
var
  i: integer;
begin
  Result := 0;
  i := iStartPos;
  while (i > 0) do
  begin
    if CharInSet(sBuffer[i], aCharList) then
    begin
      Result := i;
      if not bReadUntilLast then
      begin
        break;
      end;
    end
    else
    begin
      if bReadUntilLast and (Result <> 0) then
      begin
        break;
      end;
    end;
    Dec(i);
  end;
end;

{$IFDEF DELPHI2009_UP}
class function TCommonStringSearch.FindPreviousWord(const sBuffer: string; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
{$ELSE}
class function TCommonStringSearch.FindPreviousWord(const sBuffer: ansistring; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
{$ENDIF}
begin
  Result := FindCharacterBackwards(sBuffer, iStartPos, [' ',#9,#13,#10] + aExtraCharlist, False);
  sWord := Copy(sBuffer, Result + 1, iStartPos - Result);
  if Trim(sWord) = '' then
  begin
    Result := FindCharacterBackwards(sBuffer, Result - 1, [' ',#9,#13,#10] + aExtraCharlist, False);
  end;
  sWord := Copy(sBuffer, Result + 1, iStartPos - Result - 1);
end;

{$IFDEF DELPHI2009_UP}
class function TCommonStringSearch.FindNextWord(const sBuffer: string; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
{$ELSE}
class function TCommonStringSearch.FindNextWord(const sBuffer: ansistring; iStartPos: integer; aExtraCharlist: TCharSet; var sWord: string): integer;
{$ENDIF}
var
  iStart2: integer;
begin
  Result := FindCharacterForward(sBuffer, iStartPos, [' ',#9,#13,#10] + aExtraCharlist, False);
  if Result <> 0 then
  begin
    sWord := Copy(sBuffer, iStartPos, Result - iStartPos);
  end
  else
  begin
    sWord := Copy(sBuffer, iStartPos);
  end;
  if Trim(sWord) = '' then
  begin
    iStart2 := Result + 1;
    Result := FindCharacterForward(sBuffer, iStart2, [' ',#9,#13,#10] + aExtraCharlist, False);
    if Result <> 0 then
    begin
      sWord := Copy(sBuffer, iStart2, Result - iStart2);
    end
    else
    begin
      sWord := Copy(sBuffer, iStart2);
    end;
  end;
end;

{$ifdef NEEDCHARINCSET}
function CharInSet(const AChar: Char; const ACharSet: TCharSet): Boolean;
begin
  Result := AChar in ACharSet;
end;
{$endif}

{ TKeyValueFunctions }

class function TKeyValueFunctions.GetKey(const aParam: string): string;
begin
  result := Copy(aParam,0,Pos('=',aParam)-1);
end;

class function TKeyValueFunctions.GetValue(const aParam: string): string;
begin
  result := Copy(aParam,Pos('=',aParam)+1,length(aParam));
end;

{ TCommonFileNameFunctions }

class function TCommonFileNameFunctions.RemoveFileExtension(const s: string): string;
var
  sExt: string;
begin
  sExt := ExtractFileExt(s);
  Result := Copy(s, 1, Length(s) - Length(sExt) );
end;

{ TAppFunctions }

// CC license: http://www.delphidabbler.com/articles?article=6
class function TCommonAppFunctions.GetAllEnvVars(const Vars: TStrings): Integer;
var
  PEnvVars: PChar;    // pointer to start of environment block
  PEnvEntry: PChar;   // pointer to an env string in block
begin
  // Clear the list
  if Assigned(Vars) then
    Vars.Clear;
  // Get reference to environment block for this process
  PEnvVars := GetEnvironmentStrings;
  if PEnvVars <> nil then
  begin
    // We have a block: extract strings from it
    // Env strings are #0 separated and list ends with #0#0
    PEnvEntry := PEnvVars;
    try
      while PEnvEntry^ <> #0 do
      begin
        if Assigned(Vars) then
          Vars.Add(PEnvEntry);
        Inc(PEnvEntry, StrLen(PEnvEntry) + 1);
      end;
      // Calculate length of block
      Result := (PEnvEntry - PEnvVars) + 1;
    finally
      // Dispose of the memory block
      Windows.FreeEnvironmentStrings(PEnvVars);
    end;
  end
  else
    // No block => zero length
    Result := 0;
end;

class function TCommonAppFunctions.GetAppDataFolder: string;
begin
  result := IncludeTrailingPathDelimiter(GetSpecialWindowsFolder(CSIDL_APPDATA));
end;

class function TCommonAppFunctions.GetAppParamStartingWith(const s: string): integer;
var
  i, c: integer;
begin
  Result := -1;
  c := ParamCount;
  for i := 1 to c do
  begin
    if StartsText(s, ParamStr(i)) then
    begin
      Result := i;
      break;
    end;
  end;
end;

class function TCommonAppFunctions.GetAppPath: string;
begin
  // Always with trailing backslash
  result := ExtractFilePath(ParamStr(0));
end;

class function TCommonAppFunctions.GetSpecialWindowsFolder(aCSIDL: integer): string;
const
  SHGFP_TYPE_CURRENT = 0;
var
  lPath: array [0..255] of char;
begin
  SHGetFolderPath(0, aCSIDL, 0, SHGFP_TYPE_CURRENT, @lPath[0]);
  Result := lPath;
end;

class function TCommonAppFunctions.GetUniqueID: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  result := GUIDToString(Guid);
end;

class function TCommonAppFunctions.HasAppParam(const s: string): boolean;
var
  i, c: integer;
begin
  Result := False;
  c := ParamCount;
  for i := 1 to c do
  begin
    if SameText(s, ParamStr(i)) then
    begin
      Result := True;
      break;
    end;
  end;
end;

{ TCommonExecutionFunctions }

class function TCommonExecutionFunctions.ExecuteAndWaitAndGetStdOut(const strCommandLine: String;
  intVisibility: Integer; var sOut: string): Cardinal;
const
  BUFSIZE = 8192;
var
  StartupInfo : TStartupInfo;
  ProcessInformation : TProcessInformation;
  intWaitState : DWORD;
  saAttr: TSecurityAttributes;
  hChildStd_OUT_Rd: THandle;
  hChildStd_ERR_Rd: THandle;
  hChildStd_OUT_Wr: THandle;
  hChildStd_ERR_Wr: THandle;
  dwRead: DWORD;
  bSuccess: boolean;
  {$IFDEF DELPHI2009_UP}
  chBuf: PChar;
  {$ELSE}
  chBuf: PAnsiChar;
  {$ENDIF}
  sEditableCommandLineString: string;
  pEditableCommandLineString: PChar;
begin
  Result := 0;
  // TODO -opquist: code is nog niet helemaal correct/netjes, maar werkt voor het moment
  //  zie: http://groups.google.com/group/borland.public.delphi.language.delphi.win32/browse_thread/thread/8646e723bb9c7023
  //  zie: http://support.microsoft.com/kb/190351
  FillChar(saAttr, SizeOf(TSecurityAttributes), 0);
  saAttr.nLength := SizeOf(TSecurityAttributes);
  saAttr.bInheritHandle := TRUE;
  saAttr.lpSecurityDescriptor := nil;
  CreatePipe( hChildStd_OUT_Rd, hChildStd_OUT_Wr, @saAttr, SizeOf(TSecurityAttributes));
  DuplicateHandle(GetCurrentProcess,hChildStd_OUT_Wr,
                           GetCurrentProcess,@hChildStd_ERR_Wr,0,
                           TRUE,DUPLICATE_SAME_ACCESS);
  DuplicateHandle(GetCurrentProcess,hChildStd_OUT_Rd,
                           GetCurrentProcess,@hChildStd_ERR_Rd,0,
                           TRUE,DUPLICATE_SAME_ACCESS);

  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  StartupInfo.cb := SizeOf(TStartupInfo);
  StartupInfo.wShowWindow := intVisibility;
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdOutput := hChildStd_OUT_Wr;
  StartupInfo.hStdError := hChildStd_ERR_Wr;
  StartupInfo.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  // http://msdn.microsoft.com/en-us/library/ms682425
  // "The Unicode version of this function, CreateProcessW, can modify the contents of this string.
  //  Therefore, this parameter cannot be a pointer to read-only memory (such as a const variable or a literal string).
  //  If this parameter is a constant string, the function may cause an access violation."
  sEditableCommandLineString := strCommandLine + #0#0#0#0#0#0#0#0;  // +#0 om local copy te maken van string ipv reference te gebruiken
  pEditableCommandLineString := @sEditableCommandLineString[1];
  if (CreateProcess(nil, pEditableCommandLineString, @saAttr, @saAttr, True, NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo, ProcessInformation)) then
  begin
    chBuf := AllocMem(BUFSIZE + 1);
    try
      while not Application.Terminated do
      begin
        intWaitState := WaitForSingleObject(ProcessInformation.hProcess, 0);
        if intWaitState <> WAIT_TIMEOUT then
        begin
          bSuccess := PeekNamedPipe(hChildStd_ERR_Rd, @chBuf[0], BUFSIZE, @dwRead, nil, nil );
          while bSuccess and (dwRead <> 0) and not Application.Terminated do
          begin
            dwRead := 0;
            ReadFile( hChildStd_ERR_Rd, chBuf[0], BUFSIZE, dwRead, nil );
            sOut := sOut + Copy( chBuf, 0, dwRead );
            Sleep(1);
            dwRead := 0;
            bSuccess := PeekNamedPipe(hChildStd_ERR_Rd, @chBuf[0], BUFSIZE, @dwRead, nil, nil );
            if not bSuccess then
            begin
              raise Exception.Create('' + IntToStr(GetLastError));
            end;
            Application.ProcessMessages;
          end;
          break;
        end;
        dwRead := 0;
        bSuccess := PeekNamedPipe(hChildStd_ERR_Rd, @chBuf[0], BUFSIZE, @dwRead, nil, nil );
        if bSuccess and (dwRead <> 0) then
        begin
          dwRead := 0;
          ReadFile( hChildStd_ERR_Rd, chBuf[0], BUFSIZE, dwRead, nil );
          sOut := sOut + Copy( chBuf, 0, dwRead );
          Sleep(1);
        end;
        Application.ProcessMessages;
      end;
    finally
      FreeMem(chBuf);
    end;
    Sleep(0);
    if not Application.Terminated then
    begin
      if (intWaitState = WAIT_OBJECT_0) then
      begin
        if (GetExitCodeProcess(ProcessInformation.hProcess, intWaitState)) then
        begin
          Result := intWaitState;
        end;
      end;
    end;
    CloseHandle(hChildStd_ERR_Wr);
    CloseHandle(hChildStd_ERR_Rd);
    CloseHandle(ProcessInformation.hProcess);
    CloseHandle(ProcessInformation.hThread);
  end;
end;

class procedure TCommonExecutionFunctions.ExecuteFile(const pFile, pDirectory: string);
begin
  ShellExecute(Application.Handle, PChar('Open'), PChar(pFile), PChar(''), PChar(pDirectory), SW_SHOWNORMAL)
end;

class procedure TCommonExecutionFunctions.OpenURL(const aURL: string);
begin
  ShellExecute(0, nil, PChar(aURL), '', nil, 0);
end;

{ TCommonFileSystemFunctions }

class function TCommonFileSystemFunctions.CleanFolder(aFolder, aPattern: string): integer;
var
  FileList: TStringlist;
  I: Integer;
begin
  result := 0;
  FileList := TStringlist.Create;
  try
    GetFilesInDirectory(aFolder,FileList);
    for I := 0 to FileList.Count - 1 do
    begin
      if pos(aPattern,FileList.Strings[i]) > 0 then
      begin
        if DeleteFile(PChar(FileList.Strings[i])) then
        begin
          result := result + 1;
        end;
      end;
    end;
  finally
    FileList.Free;
  end;
end;

class procedure TCommonFileSystemFunctions.GetFilesInDirectory(const aDirectory: string; var aList: TSTringlist;
  const aFileExt: string);
Var
  R : TSearchRec;
  lDirectory: string;
Begin
  lDirectory := IncludeTrailingPathDelimiter(aDirectory);
  if (FindFirst(lDirectory + '*.' + AFileExt, faAnyFile, R) = 0) then
  begin
    try
      repeat
        aList.Add(lDirectory + R.Name);
      until (FindNext(R) <> 0);
    finally
      SysUtils.FindClose(R);
    end;
  end;
end;

end.
