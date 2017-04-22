unit Input.Params.Interfaces;

interface

type
  IInputParams = interface
    ['{EDC6EDFA-B266-4637-9FFA-EDE8CA4420F2}']

    function Count: Integer;

    function Str(const Index: Integer): string;
  end;

implementation

end.
