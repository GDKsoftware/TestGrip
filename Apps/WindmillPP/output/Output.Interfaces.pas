unit Output.Interfaces;

interface

type
  IOutput = interface
    ['{595C85BB-B1A7-4A8C-AD8D-BF88881FC5D4}']

    procedure SetWarningAsError(const Yes: Boolean);

    procedure Warning(const Filepath: string; const Linenumber: Integer; const Warning: string);

    procedure Error(const Filepath: string; const Linenumber: Integer; const Error: string);

    procedure Info(const Info: string);

    function WarningCount: Integer;

    function ErrorCount: Integer;

    function InfoCount: Integer;
  end;

implementation

end.
