unit uIdeHelper;

interface

uses
  uPascalDefs;

type
  TIDEHelper = class
  public
    class function GetCurrentUnitName: string;
    class function GetActiveFunctionOrProcedure: TMethodDefinition;
    class function GetActiveClassDef: TClassDefinition;

    class function GetWordUnderCaret: string;

    class procedure AddDelphiMessage(const sText: string);

  end;

implementation

uses
  ToolsAPI,
  uCommonFunctions, StrUtils, SysUtils;


{ TIDEHelper }

class function TIDEHelper.GetActiveFunctionOrProcedure: TMethodDefinition;
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  sText: ansistring;
  cr:IOTAEditReader;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  Position: integer;
  iRead: integer;
begin
  Result := nil;

   // Get access to editor services
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  // Get access to editor of active unit
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    // Create reader buffer
    cr := EditView.Buffer.CreateReader;
    EditPos := EditView.CursorPos;
    EditView.ConvertPos(True,EditPos,CharPos);
    Position := EditView.CharPosToPos(CharPos);
    SetLength(sText,100);

    // Find current function / procedure
    while position > 0 do
    begin
      SetLength(sText, 12);
      iRead := cr.GetText(Position,PAnsiChar(sText),12);
      SetLength(sText, iRead);

      if StartsText('procedure ', string(sText)) or StartsText('function ', string(sText)) then
      begin
        SetLength(sText, 2048);
        iRead := cr.GetText( Position, PAnsiChar(sText), 2048 );
        SetLength(sText, iRead);

        result := TMethodDefinition.Create( string( sText ), False, csUnknown);
        if result.DefMethodName = '' then
        begin
          FreeAndNil(result);
        end;
        break;
      end
      else if StartsText('constructor ', string(sText)) or StartsText('destructor ', string(sText)) then
      begin
        Result := nil;
        break;
      end;

      Dec(Position);
    end;

  end;
end;

class procedure TIDEHelper.AddDelphiMessage(const sText: string);
begin
  {$ifdef VER150}
  (BorlandIDEServices as
    IOTAMessageServices).AddTitleMessage(sText);// AddTitleMessage(aText);
  {$else}
  (BorlandIDEServices as
    IOTAMessageServices).AddWideTitleMessage(sText);// AddTitleMessage(aText);
  {$endif}
end;

class function TIDEHelper.GetActiveClassDef: TClassDefinition;
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  sText: ansistring;
  cr:IOTAEditReader;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  Position: integer;
  iRead: integer;
begin
  Result := nil;

   // Get access to editor services
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  // Get access to editor of active unit
  EditView := EditorServices.TopView;

  if Assigned(EditView) then
  begin
    // Create reader buffer
    cr := EditView.Buffer.CreateReader;
    EditPos := EditView.CursorPos;
    EditView.ConvertPos(True,EditPos,CharPos);
    Position := EditView.CharPosToPos(CharPos);
    SetLength(sText,100);

    // Find current function / procedure
    while position > 0 do
    begin
      SetLength(sText, 12);
      iRead := cr.GetText(Position,PAnsiChar(sText),12);
      SetLength(sText, iRead);

      if ContainsText(string(sText), 'class') then
      begin
{
  function FindCharacterForward( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;
  function FindCharacterBackwards( const sBuffer: string; iStartPos: integer; aCharlist: TCharSet; bReadUntilLast: boolean = false ): integer;

}

        SetLength(sText, 2048);
        iRead := cr.GetText( Position, PAnsiChar(sText), 2048 );
        SetLength(sText, iRead);

        break;
      end;

      Dec(Position);
    end;
  end;
end;

class function TIDEHelper.GetCurrentUnitName: string;
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
begin
  result := '';

   // Get access to editor services
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  // Get access to editor of active unit
  EditView := EditorServices.TopView;

  if not assigned(EditView) then
    Exit;

  // Check if current file is a pascal file
  if TCommonStringFunctions.RPos('.pas',EditView.Buffer.FileName) < 1 then
    Exit;

  result := EditView.Buffer.FileName;
end;

class function TIDEHelper.GetWordUnderCaret: string;
const
  c_CodeChars = [' ',#13,#10,#9,'.',',','(',')','<','>',';',':','=',''''];
var
  EditorServices: IOTAEditorServices;
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
  CharPos: TOTACharPos;
  Position: integer;
  iFrom, iNewPos: integer;
  cr: IOTAEditReader;
  iRead: integer;
  sText: ansistring;
begin
  result := '';

   // Get access to editor services
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  // Get access to editor of active unit
  EditView := EditorServices.TopView;

  if not assigned(EditView) then
    Exit;

  EditPos := EditView.CursorPos;
  EditView.ConvertPos(True,EditPos,CharPos);
  Position := EditView.CharPosToPos(CharPos);

  cr := EditView.Buffer.CreateReader;

  SetLength(sText, 200);
  iFrom := Position - 100;
  if iFrom < 0 then
  begin
    iNewPos := Position;
    iFrom := 0;
  end
  else
  begin
    iNewPos := 100;
  end;
  iRead := cr.GetText(iFrom, PAnsiChar(sText), 200);
  SetLength(sText, iRead);

  iNewPos := TCommonStringSearch.FindCharacterBackwards(sText, iNewPos, c_CodeChars) + 1;
  TCommonStringSearch.FindNextWord(sText, iNewPos, c_CodeChars, Result);
end;

end.
