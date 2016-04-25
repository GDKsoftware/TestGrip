object frmShowTestresults: TfrmShowTestresults
  Left = 0
  Top = 0
  Caption = 'Latest testresults'
  ClientHeight = 319
  ClientWidth = 656
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgMain: TPageControl
    Left = 0
    Top = 0
    Width = 656
    Height = 282
    ActivePage = tbTestErrors
    Align = alClient
    TabOrder = 0
    OnChange = pgMainChange
    ExplicitTop = -6
    ExplicitHeight = 278
    object tbCompileErrors: TTabSheet
      Caption = 'Compilation'
      ExplicitHeight = 291
      object mmCompilation: TMemo
        Left = 0
        Top = 97
        Width = 648
        Height = 157
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 194
      end
      object mmCommand: TMemo
        Left = 0
        Top = 0
        Width = 648
        Height = 97
        Align = alTop
        ScrollBars = ssBoth
        TabOrder = 1
      end
    end
    object tbTestErrors: TTabSheet
      Caption = 'Testresult'
      ImageIndex = 1
      ExplicitHeight = 291
      object mmTestresult: TMemo
        Left = 0
        Top = 0
        Width = 648
        Height = 254
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 291
      end
    end
    object tbRunlog: TTabSheet
      Caption = 'Raw output'
      ImageIndex = 2
      ExplicitHeight = 250
      object mmRunlog: TMemo
        Left = 0
        Top = 0
        Width = 648
        Height = 254
        Align = alClient
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        ExplicitHeight = 250
      end
    end
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 282
    Width = 656
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 281
    object btnShowInCode: TButton
      Left = 4
      Top = 6
      Width = 93
      Height = 25
      Caption = 'Show in code'
      TabOrder = 0
      OnClick = btnShowInCodeClick
    end
  end
end
