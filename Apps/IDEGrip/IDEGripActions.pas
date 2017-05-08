unit IDEGripActions;

interface

uses
  uDefinitionSearch;

type
  TIDEGripActions = class
  public
    class procedure CopyInterface;
  end;

implementation

uses
  ToolsAPI, uIDEHelper, uCodeHelpers, Clipbrd;

class procedure TIDEGripActions.CopyInterface;
var
  sProjectFile: string;
  sSearchFor: string;
  sCode: string;
  sCurrentUnitname: string;
  Search: TDefinitionSearch;
begin
  // preparation to get the right projectfile and search starting
  sProjectFile := GetActiveProject.FileName;

  Search := TDefinitionSearch.Create(sProjectFile);
  try
    // find out select class/interface name
    sSearchFor := TIDEHelper.GetWordUnderCaret;

    sCurrentUnitname := TIDEHelper.GetCurrentUnitName;

    // to clipboard
    sCode := TCodeHelperA.GetInterfaceMethodDefinitions(Search, sCurrentUnitname, sSearchFor);
    if sCode <> '' then
    begin
      clipboard.AsText := sCode;
      TIDEHelper.AddDelphiMessage('Copied interface methods for ' + sSearchFor + ' to clipboard');
    end
    else
    begin
      TIDEHelper.AddDelphiMessage('Interface methods for ' + sSearchFor + ' not found');
    end;
  finally
    Search.Free;
  end;
end;

end.
