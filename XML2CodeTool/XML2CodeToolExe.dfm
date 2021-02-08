object FormXML2Code: TFormXML2Code
  Left = 0
  Top = 0
  Caption = 'XML2Code'
  ClientHeight = 366
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenuXML2Code
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inline FrameXML2Code: TFrameXML
    Left = 0
    Top = 0
    Width = 635
    Height = 366
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 635
    ExplicitHeight = 366
    inherited SplitterXML2Code: TSplitter
      Top = 361
      Width = 635
      ExplicitTop = 361
      ExplicitWidth = 635
    end
    inherited TreeXML: TTree
      Width = 635
      Height = 361
      ExplicitWidth = 635
      ExplicitHeight = 361
    end
  end
  object MainMenuXML2Code: TMainMenu
    Left = 152
    Top = 48
    object NewCML: TMenuItem
      Caption = #26032#24314#37197#32622#25991#20214
      ShortCut = 112
      OnClick = NewCMLClick
    end
    object OpenFileMenu: TMenuItem
      Caption = #25171#24320#37197#32622#25991#20214
      ShortCut = 113
      OnClick = OpenFileMenuClick
    end
    object SaveFileMenu: TMenuItem
      Caption = #20445#23384#37197#32622#25991#20214
      ShortCut = 114
      OnClick = SaveFileMenuClick
    end
    object ExportXMLUnitMenu: TMenuItem
      Caption = #23548#20986'XML'#20195#30721
      ShortCut = 115
      OnClick = ExportXMLUnitMenuClick
    end
    object ExportXMLTreeUnitMenu: TMenuItem
      AutoCheck = True
      Caption = #23548#20986'XMLTree'#20195#30721' '
      ShortCut = 116
      OnClick = ExportXMLTreeUnitMenuClick
    end
    object ExportClassUnitMenu: TMenuItem
      Caption = #23548#20986#31867#20195#30721
      ShortCut = 117
      OnClick = ExportClassUnitMenuClick
    end
    object CMLs2Codes: TMenuItem
      Caption = #25209#37327#29983#25104#20195#30721
      ShortCut = 118
      OnClick = CMLs2CodesClick
    end
  end
  object OpenDialogCML: TOpenDialog
    DefaultExt = 'CML'
    Filter = 'CML|*.CML'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 296
    Top = 24
  end
end
