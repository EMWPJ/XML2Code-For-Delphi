object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 299
  ClientWidth = 635
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
  inline FrameXML1: TFrameXML
    Left = 0
    Top = 0
    Width = 635
    Height = 299
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 635
    ExplicitHeight = 299
    inherited SplitterXML2Code: TSplitter
      Top = 294
      Width = 635
      ExplicitTop = 294
      ExplicitWidth = 635
    end
    inherited TreeXML: TTree
      Width = 635
      Height = 294
      ExplicitWidth = 635
      ExplicitHeight = 294
    end
  end
end
