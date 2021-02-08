object FrameXML: TFrameXML
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object SplitterXML2Code: TSplitter
    Left = 0
    Top = 299
    Width = 451
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 360
    ExplicitWidth = 320
  end
  object TreeXML: TTree
    Left = 0
    Top = 0
    Width = 451
    Height = 299
    AnimatedZoomSteps = 1
    Grid.Visible = True
    ScrollMouseButton = mbLeft
    Selected.ShiftState = [ssCtrl]
    SingleSelection = False
    Zoom.AnimatedSteps = 1
    Zoom.KeyShift = [ssCtrl]
    Zoom.MouseButton = mbMiddle
    Zoom.UpLeftZooms = True
    OnClickShape = TreeXMLClickShape
    Align = alClient
    PopupMenu = PopupMenuXMLFile
    TabOrder = 0
  end
  object PopupMenuXMLFile: TPopupMenu
    Left = 88
    Top = 16
    object OpenXMLMenu: TMenuItem
      Caption = #25171#24320'XML'
      ShortCut = 16463
      OnClick = OpenXMLMenuClick
    end
    object SaveXMLMenu: TMenuItem
      Caption = #20445#23384'XML'
      ShortCut = 16467
      OnClick = SaveXMLMenuClick
    end
    object NewXMLMenu: TMenuItem
      Caption = #26032#24314'XML'
      ShortCut = 16462
      OnClick = NewXMLMenuClick
    end
  end
  object OpenDialogXML: TOpenDialog
    Left = 24
    Top = 16
  end
  object SaveDialogXML: TSaveDialog
    Left = 24
    Top = 64
  end
  object PopupMenuXMLNode: TPopupMenu
    Left = 88
    Top = 72
  end
end
