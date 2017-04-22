unit Feeder.Interfaces;

interface

uses
  System.SysUtils;

type
  IFeeder = interface
    ['{FAE738E3-A96D-4358-BADC-8796690C4BBB}']

    function Eof: Boolean;

    function NextFile: string;
  end;

  EEndOfFiles = class(Exception);

implementation

end.
