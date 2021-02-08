object FrameTextEdit: TFrameTextEdit
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Align = alClient
  TabOrder = 0
  object MemoText: TMemo
    Left = 0
    Top = 0
    Width = 320
    Height = 240
    Align = alClient
    PopupMenu = PopupMenuText
    TabOrder = 0
  end
  object PopupMenuText: TPopupMenu
    Left = 128
    Top = 48
    object LoadMenu: TMenuItem
      Caption = 'Load'
      ShortCut = 16463
      OnClick = LoadMenuClick
    end
    object SaveMenu: TMenuItem
      Caption = 'Save'
      ShortCut = 16467
      OnClick = SaveMenuClick
    end
  end
  object OpenDialogText: TOpenDialog
    Left = 56
    Top = 112
  end
  object SaveDialogText: TSaveDialog
    Left = 176
    Top = 112
  end
end
