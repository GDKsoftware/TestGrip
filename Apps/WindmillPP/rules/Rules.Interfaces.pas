unit Rules.Interfaces;

interface

uses
  System.Classes, uPascalDefs;

type
  IRulesOnMethod = interface
    ['{2A54F9DB-A1FB-4CF6-A0A2-13281EAC145D}']

    procedure Process(const Filepath: string; const Classdef: TClassDefinition; const Method: TMethodDefinition; const Lines: TStrings);
  end;

implementation

end.
