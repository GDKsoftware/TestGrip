
{$IF CompilerVersion < 20}   // Previous than UNICODE era
  {$define NEEDCHARINCSET}
{$ENDIF}

{$IF CompilerVersion >= 20}   // Tiburon / Delphi 2009
  {$DEFINE DELPHI2009_UP}
{$IFEND}

{$IF CompilerVersion >= 33}   // Rio / Delphi 10.3
  {$DEFINE DELPHI103_UP}
{$IFEND}

