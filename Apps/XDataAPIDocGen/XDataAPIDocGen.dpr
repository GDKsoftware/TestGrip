program XDataAPIDocGen;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Main.Generator in 'Main.Generator.pas',
  uCodeHelpers in '..\..\Core\uCodeHelpers.pas',
  uDefinitionSearch in '..\..\Core\uDefinitionSearch.pas',
  uPascalDefs in '..\..\Core\uPascalDefs.pas',
  uProjectParser in '..\..\Core\uProjectParser.pas',
  uUnitParser in '..\..\Core\uUnitParser.pas',
  uUsedUnitSearch in '..\..\Core\uUsedUnitSearch.pas',
  Service.Generator in 'Service.Generator.pas',
  uCommonFunctions in '..\..\Shared\uCommonFunctions.pas',
  uD7Functions in '..\..\Shared\uD7Functions.pas',
  uUTF8Functions in '..\..\Shared\uUTF8Functions.pas',
  uXmlFuncs in '..\..\Shared\uXmlFuncs.pas',
  Index.Generator in 'Index.Generator.pas',
  XML.Generator in 'XML.Generator.pas';

var
  ProjectFilePath: string;
  DestinationFolder: string;
  MainGenerator: TMainGenerator;
begin
  try
    if ParamCount >= 1 then
    begin
      ProjectFilePath := ParamStr(1);
      if ParamCount >= 2 then
      begin
        DestinationFolder := ParamStr(2);
      end
      else
      begin
        DestinationFolder := '.\';
      end;

      MainGenerator := TMainGenerator.Create(ProjectFilePath, DestinationFolder);
      try
        MainGenerator.Run;
      finally
        MainGenerator.Free;
      end;
    end
    else
    begin
      Writeln('Usage: XDatAPIDocGen <file.dproj> [destinationfolder]');
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
