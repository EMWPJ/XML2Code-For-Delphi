object FormTextEdit: TFormTextEdit
  Left = 0
  Top = 0
  Caption = 'FormTextEdit'
  ClientHeight = 363
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  inline TextEdit: TFrameTextEdit
    Left = 0
    Top = 0
    Width = 428
    Height = 322
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 428
    ExplicitHeight = 322
    inherited MemoText: TMemo
      Width = 428
      Height = 322
      ExplicitWidth = 428
      ExplicitHeight = 322
    end
  end
  object GridPanel1: TGridPanel
    Left = 0
    Top = 322
    Width = 428
    Height = 41
    Align = alBottom
    Caption = 'GridPanel1'
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 1
        Control = ButtonOK
        Row = 0
      end
      item
        Column = 0
        Control = ButtonCancel
        Row = 0
      end>
    RowCollection = <
      item
        Value = 100.000000000000000000
      end>
    TabOrder = 1
    object ButtonOK: TButton
      Left = 214
      Top = 1
      Width = 213
      Height = 39
      Align = alClient
      Caption = #30830#23450
      TabOrder = 0
      OnClick = ButtonOKClick
    end
    object ButtonCancel: TButton
      Left = 1
      Top = 1
      Width = 213
      Height = 39
      Align = alClient
      Caption = #21462#28040
      TabOrder = 1
      OnClick = ButtonCancelClick
    end
  end
end
