object frmInputTest: TfrmInputTest
  Left = 0
  Top = 0
  Caption = 'Test details'
  ClientHeight = 549
  ClientWidth = 932
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object panelBottom: TPanel
    Left = 0
    Top = 513
    Width = 932
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 36
    TabOrder = 0
    DesignSize = (
      932
      36)
    object btnCancel: TButton
      Left = 801
      Top = 3
      Width = 121
      Height = 30
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 0
      OnClick = btnCancelClick
    end
    object btnSave: TButton
      Left = 674
      Top = 3
      Width = 121
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Save'
      ModalResult = 1
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnSaveAndAdd: TButton
      Left = -1
      Top = 3
      Width = 121
      Height = 30
      Anchors = [akTop, akRight]
      Caption = 'Save and add test'
      Default = True
      ModalResult = 1
      TabOrder = 2
      Visible = False
      OnClick = btnSaveAndAddClick
    end
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 932
    Height = 57
    Align = alTop
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 1
    DesignSize = (
      932
      57)
    object shapeHeader: TShape
      Left = 0
      Top = 0
      Width = 932
      Height = 57
      Align = alClient
      Brush.Style = bsClear
      Pen.Color = clGray
      ExplicitWidth = 700
      ExplicitHeight = 1
    end
    object lblTestName: TLabel
      Left = 9
      Top = 33
      Width = 63
      Height = 13
      Caption = 'Test name:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblClassText: TLabel
      Left = 9
      Top = 9
      Width = 32
      Height = 13
      Caption = 'Class:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblClass: TLabel
      Left = 85
      Top = 9
      Width = 171
      Height = 13
      AutoSize = False
      Caption = 'lblClass'
    end
    object lblMethodText: TLabel
      Left = 262
      Top = 9
      Width = 46
      Height = 13
      Caption = 'Method:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblMethod: TLabel
      Left = 344
      Top = 9
      Width = 555
      Height = 13
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'lblMethod'
    end
    object lcResultType: TLabel
      Left = 521
      Top = 9
      Width = 68
      Height = 13
      Caption = 'Result type:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblResultType: TLabel
      Left = 602
      Top = 9
      Width = 88
      Height = 13
      AutoSize = False
      Caption = 'lblResultType'
    end
    object edTestName: TEdit
      Left = 85
      Top = 30
      Width = 812
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnKeyPress = edTestNameKeyPress
      OnKeyUp = onTextKeyUp
    end
  end
  object PanelTestContent: TPanel
    Left = 0
    Top = 57
    Width = 406
    Height = 396
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 2
    object PanelParameters: TPanel
      Left = 0
      Top = 0
      Width = 406
      Height = 180
      Align = alTop
      BevelOuter = bvNone
      Constraints.MinHeight = 180
      ParentBackground = False
      ParentColor = True
      TabOrder = 0
      DesignSize = (
        406
        180)
      object lblParamText: TLabel
        Left = 9
        Top = 6
        Width = 67
        Height = 13
        Caption = 'Parameters'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblParamDesc: TLabel
        Left = 9
        Top = 24
        Width = 157
        Height = 13
        Caption = 'Fill in a value for each parameter'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object gridParams: TStringGrid
        Left = 9
        Top = 51
        Width = 387
        Height = 123
        Anchors = [akLeft, akTop, akRight, akBottom]
        ColCount = 3
        FixedCols = 2
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnKeyUp = onTextKeyUp
        ColWidths = (
          113
          140
          422)
      end
    end
    object PanelEquals: TPanel
      Left = 0
      Top = 180
      Width = 406
      Height = 80
      Align = alTop
      BevelOuter = bvNone
      Color = clWhite
      Constraints.MinHeight = 80
      ParentBackground = False
      TabOrder = 1
      DesignSize = (
        406
        80)
      object lblEqualsDesc: TLabel
        Left = 9
        Top = 24
        Width = 387
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Define the expected result of the function  (use '#39'Test implies'#39' ' +
          'when the result is a complex datatype)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 397
      end
      object lblEqualsText: TLabel
        Left = 9
        Top = 6
        Width = 36
        Height = 13
        Caption = 'Equals'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lblFillLatestTestEqualsResult: TLabel
        Left = 282
        Top = 6
        Width = 114
        Height = 13
        Cursor = crHandPoint
        Alignment = taRightJustify
        Anchors = [akTop, akRight]
        Caption = 'Fill with latest testresult'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = lblFillLatestTestEqualsResultClick
      end
      object edEquals: TEdit
        Left = 9
        Top = 42
        Width = 319
        Height = 26
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnKeyUp = onTextKeyUp
      end
      object btnEqualsZoomIn: TButton
        Left = 330
        Top = 42
        Width = 26
        Height = 26
        Anchors = [akTop, akRight]
        TabOrder = 2
        OnClick = btnEqualsZoomInClick
      end
      object chkEqualsNot: TCheckBox
        Left = 361
        Top = 42
        Width = 40
        Height = 26
        Anchors = [akTop, akRight]
        Caption = 'not'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
      end
    end
    object panelImplies: TPanel
      Left = 0
      Top = 260
      Width = 406
      Height = 145
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      Color = clWhite
      Constraints.MinHeight = 145
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 2
      DesignSize = (
        406
        145)
      object lblHelpImplies: TLabel
        Left = 9
        Top = 18
        Width = 439
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'Define expressions to check after running the test  (use Ctrl+Sp' +
          'ace to see some posibilties)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object lblImplies: TLabel
        Left = 9
        Top = 2
        Width = 68
        Height = 13
        Caption = 'Test implies'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object mmImplies: TSynEdit
        Left = 9
        Top = 40
        Width = 387
        Height = 90
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        TabOrder = 0
        OnKeyUp = onTextKeyUp
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Highlighter = SynHighlighter
      end
    end
  end
  object PanelTabAdvanced: TPanel
    Left = 406
    Top = 57
    Width = 526
    Height = 396
    Align = alRight
    BevelOuter = bvNone
    Color = 16378066
    Constraints.MinHeight = 350
    Constraints.MinWidth = 526
    ParentBackground = False
    TabOrder = 3
    DesignSize = (
      526
      396)
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 1
      Height = 396
      Align = alLeft
      Brush.Style = bsClear
      Pen.Color = clGray
    end
    object pgMain: TPageControl
      Left = 3
      Top = 2
      Width = 521
      Height = 392
      ActivePage = tabTestSettings
      Anchors = [akLeft, akTop, akRight, akBottom]
      BiDiMode = bdLeftToRight
      Constraints.MinHeight = 350
      ParentBiDiMode = False
      TabOrder = 0
      object tabClass: TTabSheet
        Caption = 'Class settings'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          513
          364)
        object lblSetupClassSet: TLabel
          Left = 9
          Top = 4
          Width = 63
          Height = 13
          Caption = 'Setup code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblSetupDescClassSet: TLabel
          Left = 9
          Top = 19
          Width = 275
          Height = 13
          Caption = 'Define the setup code for al tests within the current class'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblInitClassSet: TLabel
          Left = 9
          Top = 132
          Width = 71
          Height = 13
          Caption = 'Initialization'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblInitDescClassSet: TLabel
          Left = 9
          Top = 148
          Width = 358
          Height = 13
          Caption = 
            'Define the create and initialization code for al tests within th' +
            'e current class'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDefinesClass: TLabel
          Left = 9
          Top = 260
          Width = 42
          Height = 13
          Caption = 'Defines'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblDefinesDescClass: TLabel
          Left = 9
          Top = 276
          Width = 292
          Height = 13
          Caption = 'Configure the defines for al the tests within the current class'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object mmSetupcodeClass: TSynMemo
          Left = 9
          Top = 40
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
        object mmInitCodeClass: TSynMemo
          Left = 9
          Top = 169
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 1
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
        object edDefinesClass: TEdit
          Left = 9
          Top = 296
          Width = 501
          Height = 24
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          OnKeyUp = onTextKeyUp
        end
        object btnExtraUses: TButton
          Left = 393
          Top = 326
          Width = 117
          Height = 25
          Caption = 'Extra includes...'
          TabOrder = 3
          OnClick = btnExtraUsesClick
        end
      end
      object tabFunction: TTabSheet
        Caption = 'Function settings'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          513
          364)
        object lblInitFunctionSet: TLabel
          Left = 9
          Top = 132
          Width = 71
          Height = 13
          Caption = 'Initialization'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblInitDescFunctionSet: TLabel
          Left = 9
          Top = 148
          Width = 579
          Height = 13
          Caption = 
            'Define the create and initialization code for al tests within th' +
            'e current function (this code overrides the class initialization' +
            ')'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblDefinesFunctionSet: TLabel
          Left = 9
          Top = 260
          Width = 42
          Height = 13
          Caption = 'Defines'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblDefinesDescFunctionSet: TLabel
          Left = 9
          Top = 276
          Width = 500
          Height = 13
          Caption = 
            'Configure the defines for al the tests within the current functi' +
            'on (this defines override the class defines)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSetupDescFunctionSet: TLabel
          Left = 9
          Top = 19
          Width = 494
          Height = 13
          Caption = 
            'Define the setup code for al tests within the current function (' +
            'this code overrides the class setup code)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object lblSetupFunctionSet: TLabel
          Left = 9
          Top = 4
          Width = 63
          Height = 13
          Caption = 'Setup code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object mmInitcodeFunction: TSynMemo
          Left = 9
          Top = 169
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
        object edDefineFunction: TEdit
          Left = 9
          Top = 296
          Width = 501
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          OnKeyUp = onTextKeyUp
        end
        object mmSetupcodeFunction: TSynMemo
          Left = 9
          Top = 40
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 2
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
      end
      object tabTestSettings: TTabSheet
        Caption = 'Test settings'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ImageIndex = 3
        ParentFont = False
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          513
          364)
        object lblTestSettingsInit: TLabel
          Left = 9
          Top = 4
          Width = 71
          Height = 13
          Caption = 'Initialization'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lblTestSettInitDesc: TLabel
          Left = 9
          Top = 20
          Width = 591
          Height = 13
          Caption = 
            'Define the create and initialization code specific for the curre' +
            'nt test (this code overrides the class and function initializati' +
            'on)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label1: TLabel
          Left = 9
          Top = 160
          Width = 42
          Height = 13
          Caption = 'Defines'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label8: TLabel
          Left = 9
          Top = 178
          Width = 493
          Height = 13
          Caption = 
            'Configure the defines specific for the current test (this define' +
            's override the class and function defines)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 9
          Top = 236
          Width = 96
          Height = 13
          Caption = 'Pre-Implies code'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          Left = 9
          Top = 255
          Width = 256
          Height = 13
          Caption = 'Code to execute before the Impliers are being tested'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object edDefines: TEdit
          Left = 9
          Top = 196
          Width = 501
          Height = 24
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 0
          OnKeyUp = onTextKeyUp
        end
        object mmInitCode: TSynEdit
          Left = 9
          Top = 41
          Width = 501
          Height = 97
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 1
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Highlighter = SynHighlighter
        end
        object mmPreImpliesCode: TSynEdit
          Left = 9
          Top = 274
          Width = 501
          Height = 75
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 2
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Highlighter = SynHighlighter
        end
      end
      object TabVariables: TTabSheet
        Caption = 'Variable declarations'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          513
          364)
        object Label2: TLabel
          Left = 9
          Top = 4
          Width = 96
          Height = 13
          Caption = 'Class declaration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label3: TLabel
          Left = 9
          Top = 20
          Width = 501
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Define variables on class level'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 502
        end
        object Label4: TLabel
          Left = 9
          Top = 129
          Width = 115
          Height = 13
          Caption = 'Function declaration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label5: TLabel
          Left = 9
          Top = 145
          Width = 501
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Define variables on function level'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 502
        end
        object Label6: TLabel
          Left = 9
          Top = 255
          Width = 92
          Height = 13
          Caption = 'Test declaration'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label7: TLabel
          Left = 9
          Top = 271
          Width = 501
          Height = 13
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = 'Define variables on current test level'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 502
        end
        object mmClassVars: TSynMemo
          Left = 9
          Top = 41
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
        object mmFunctionVars: TSynMemo
          Left = 9
          Top = 166
          Width = 501
          Height = 80
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 1
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
        object mmTestVars: TSynMemo
          Left = 9
          Top = 292
          Width = 501
          Height = 67
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 2
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Highlighter = SynHighlighter
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
      end
      object TabSheet1: TTabSheet
        Caption = 'Extra'
        ImageIndex = 4
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        DesignSize = (
          513
          364)
        object Label11: TLabel
          Left = 9
          Top = 23
          Width = 284
          Height = 13
          Caption = 'Enter an optional description of what this test should prove'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGray
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
        object Label12: TLabel
          Left = 9
          Top = 4
          Width = 64
          Height = 13
          Caption = 'Description'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object mmDescription: TSynMemo
          Left = 9
          Top = 41
          Width = 501
          Height = 113
          Anchors = [akLeft, akTop, akRight]
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Courier New'
          Font.Style = []
          TabOrder = 0
          OnKeyUp = onTextKeyUp
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Courier New'
          Gutter.Font.Style = []
          Gutter.Width = 20
          Options = [eoAutoIndent, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces]
          RightEdge = 120
        end
      end
    end
  end
  object PanelAdvancedSettingsCaption: TPanel
    Left = 0
    Top = 453
    Width = 932
    Height = 60
    Align = alBottom
    BevelOuter = bvNone
    Constraints.MinHeight = 60
    TabOrder = 4
    DesignSize = (
      932
      60)
    object shapeLineBelowAdvBtns: TShape
      Left = 0
      Top = 59
      Width = 932
      Height = 1
      Align = alBottom
      Pen.Color = clGray
      ExplicitTop = 0
      ExplicitWidth = 700
    end
    object shapeAboveAdvBtns: TShape
      Left = 0
      Top = 0
      Width = 932
      Height = 1
      Align = alTop
      Pen.Color = clGray
      ExplicitWidth = 700
    end
    object btnShowAdvancedSettings: TButton
      Left = 4
      Top = 3
      Width = 322
      Height = 53
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Caption = 'Show advanced settings'
      TabOrder = 0
      OnClick = btnShowAdvancedSettingsClick
    end
    object btnCodeView: TButton
      Left = 604
      Top = 2
      Width = 322
      Height = 53
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Anchors = [akTop, akRight]
      Caption = 'Show code view'
      TabOrder = 1
      OnClick = btnCodeViewClick
    end
  end
  object SynHighlighter: TSynPasSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clNavy
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 216
    Top = 376
  end
  object IconList: TImageList
    Left = 144
    Top = 376
    Bitmap = {
      494C01010300BC00200110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      000000000000000000000000000000000000000000000000000000000000F7F7
      FF00B5B6CE00CECFCE0000000000000000000000000000000000F7F7FF00ADAE
      CE00CECFCE00FFFFFF000000000000000000FFFFFF00FFFFFF00DEDFDE00CECF
      CE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00CECF
      CE00CECFCE00CECFCE00C6C7C600DEDFDE00FFFFFF00FFFFFF00DEDFDE00CECF
      CE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00CECFCE00C6C7
      C60073A6840031A652007BA68C00D6D7D6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFEFFF004A38
      CE003930D6006B61B500BDBEBD000000000000000000F7F7FF005A51CE003930
      DE005A51BD00C6C7C600FFFFFF0000000000FFFFFF00737173004A494A005251
      52009C9E9C009C9E9C009C9E9C00C6C7C600CECFCE00BDBEBD008C8E8C002120
      2100212021004A494A0042414200BDBEBD00FFFFFF00737173004A494A005251
      52009C9E9C009C9E9C009C9E9C00C6C7C600CECFCE00BDBEBD008C8E8C001059
      210021C74A0018DF730018CF5A006B9673000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000EFEFFF004A38C6003928
      D6004238DE005A51DE007369B500BDBEBD00F7F7FF006B61CE005249E7004A41
      E7003930D6005A51B500C6C7C600FFFFFF00F7F7F70031303100000000000000
      0000ADAEAD005251520000000000F7F7F700E7E7E700CECFCE009C9E9C000000
      0000000000001818180031303100BDBEBD00F7F7F70031303100000000000000
      0000ADAEAD005251520000000000F7F7F700E7E7E700CECFCE009C9E9C00189E
      290063C76B0084DF9C005ACF7300219E39000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7EFFF006351CE005241D6003930
      D6004230DE004238E7007369E7007369BD007B71CE006361EF004A41EF004238
      E7003930DE003120CE005A51B500CECFCE00EFEFEF0029282900000000000000
      0000BDBEBD005A595A0000000000EFEFEF00D6D7D600C6C7C6009C9E9C000000
      0000000000002928290029282900BDBEBD00EFEFEF0029282900000000000000
      0000BDBEBD005A595A0000000000EFEFEF00D6D7D600C6C7C6009C9E9C001071
      2100F7FFF700FFFFFF00DEF7DE00428E52000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ADA6E7007361DE006359DE005A51
      DE004A38DE004238DE004238E7006B61EF006359EF004A41E7004238E7004230
      DE003928D6003120D6003120C600ADAEC600EFEFEF0018181800000000000000
      0000CECFCE005A595A0000000000EFEFEF00E7E7E700DEDFDE00BDBEBD000000
      0000000000003938390018181800BDBEBD00EFEFEF0018181800000000000000
      0000CECFCE005A595A0000000000EFEFEF00E7E7E700DEDFDE00BDBEBD000008
      0000188E310052CF730010863100ADAEAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DEDFF7007369D6007369DE006B59
      DE006351DE005249DE004230DE004238DE004238E7004238DE003930DE003930
      DE003128D6003120CE005241BD0000000000EFEFEF0010101000000000000000
      0000E7E7E700DEDFDE00C6C7C600F7F7F700F7F7F700F7F7F700D6D7D6002928
      2900292829003938390010101000BDBEBD00EFEFEF0010101000000000000000
      0000E7E7E700DEDFDE00C6C7C600F7F7F700F7F7F700F7F7F700D6D7D6002928
      2900292829003938390010101000BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DEDFF7007369CE007B71
      DE006B61DE006359DE005249DE003930D6003930DE003928D6003928D6003128
      D6003928CE005241B5000000000000000000EFEFEF0008080800000000000000
      0000000000000808080008080800080808000808080008080800000000000000
      0000000000000000000000000000BDBEBD00EFEFEF0008080800000000000000
      0000000000000808080008080800080808000808080008080800000000000000
      0000000000000000000000000000BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEDFF7008479
      D6007B71E7007369DE006359DE005249D6003128D6003120CE003120CE003928
      CE005A49B500000000000000000000000000EFEFEF00000000004A494A00D6D7
      D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7
      D600D6D7D6003130310000000000BDBEBD00EFEFEF00000000004A494A00D6D7
      D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7D600D6D7
      D600D6D7D6003130310000000000BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F7F7FF007B71
      C6008C86E7008479E7007369DE006B61DE005249D6003120CE003120CE003928
      CE006351A500BDBEBD000000000000000000EFEFEF00080808007B797B00DEDF
      DE00DEDFDE00DEDFDE00DEDFDE00DEDFDE00DEDFDE00D6D7D600D6D7D600D6D7
      D600D6D7D6006361630008080800BDBEBD00EFEFEF00080808007B797B00DEDF
      DE00DEDFDE00DEDFDE00DEDFDE00DEDFDE00DEDFDE00D6D7D600D6D7D600D6D7
      D600D6D7D6006361630008080800BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7F7FF008C79C600ADA6
      E7009C96EF00948EE7008479E7007B71E7007361DE004A41D6003120CE003120
      CE003120BD0063599C00BDBEBD0000000000EFEFEF004A494A00B5B6B500EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00E7E7
      E700DEDFDE006361630018181800BDBEBD00EFEFEF004A494A00B5B6B500EFEF
      EF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00EFEFEF00E7E7
      E700DEDFDE006361630018181800BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7F7FF009C96CE00C6C7EF00BDB6
      EF00ADAEEF00A59EEF00948EE7008C86E7008471E7007369DE004230D6003120
      CE003120CE004A30C6006B59A500CECFCE00EFEFEF006B696B00C6C7C600F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F7006B696B0021202100BDBEBD00EFEFEF006B696B00C6C7C600F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F7006B696B0021202100BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDB6E700E7E7F700D6D7F700CEC7
      F700BDBEEF00B5AEEF00A5A6EF009C96E700948EE7008479E7007369DE003120
      CE003120CE003120CE006351C600B5AEC600EFEFEF0084868400DEDFDE00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CECFCE0063616300BDBEBD00EFEFEF0084868400DEDFDE00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00CECFCE0063616300BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DED7EF00DEDFF700E7E7FF00DED7
      F700CECFF700C6BEF700C6BEEF00AD9ED600B5AEDE009C96E7008C86E7005A49
      DE003120CE004A38D6007B71C600F7EFF700EFEFEF009C9E9C00EFEFEF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E7E7E70094969400BDBEBD00EFEFEF009C9E9C00EFEFEF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00E7E7E70094969400BDBEBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D6D7EF00E7E7F700EFEF
      FF00DEDFF700DEDFF700B5AED600F7F7FF00E7DFF700C6BEE700A59EEF008C86
      E7005A51D6009486CE00EFEFF70000000000EFEFEF00B5B6B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A5A6A500C6C7C600EFEFEF00B5B6B500FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00A5A6A500C6C7C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6D7EF00EFEF
      FF00F7F7FF00C6BEE700F7F7FF000000000000000000E7DFF700D6CFEF00BDBE
      EF00B5AEDE00EFEFF7000000000000000000F7F7F7008C8E8C00D6D7D600F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F700D6D7D60084868400FFFFFF00F7F7F7008C8E8C00D6D7D600F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F700D6D7D60084868400FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000DED7
      EF00CEC7E700F7F7FF0000000000000000000000000000000000E7DFF700C6BE
      E700F7EFFF00000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00E3C3000000000000C181000000000000
      8000000000000000000000000000000000000000000000000001000000000000
      8003000000000000C007000000000000C0030000000000008001000000000000
      0000000000000000000000000000000000000000000000008001000000000000
      C183000000000000E3C700000000000000000000000000000000000000000000
      000000000000}
  end
  object SynCompletion: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 320
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsBold]
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsItalic]
      end>
    OnExecute = SynCompletionExecute
    ShortCut = 16416
    Editor = mmImplies
    Left = 296
    Top = 376
  end
  object SynCompletion2: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 320
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsBold]
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsItalic]
      end>
    OnExecute = SynCompletionExecute
    ShortCut = 16416
    Editor = mmInitCode
    Left = 296
    Top = 400
  end
  object SynCompletion3: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    Width = 320
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsBold]
      end
      item
        BiggestWord = 'CONSTRUCTOR'
        DefaultFontStyle = [fsItalic]
      end>
    OnExecute = SynCompletionExecute
    ShortCut = 16416
    Editor = mmPreImpliesCode
    Left = 296
    Top = 344
  end
end
