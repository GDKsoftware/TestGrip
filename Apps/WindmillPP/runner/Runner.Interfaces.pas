unit Runner.Interfaces;

interface

type
  IRunner = interface
    ['{6C806808-9878-4D99-A36E-509FAFC18704}']

    procedure Execute(const Filepath: string);
  end;

  IWindMillPP = interface
    ['{85ED5E45-E126-4BE5-B337-468A410A1FEB}']

    procedure Go;
  end;

implementation

end.
