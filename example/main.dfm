object frmDrawObjects: TfrmDrawObjects
  Left = 259
  Top = 164
  Caption = 'DrawObjects'
  ClientHeight = 541
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  PopupMenu = PopupMenuDiagram
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    AlignWithMargins = True
    Left = 3
    Top = 519
    Width = 778
    Height = 19
    Panels = <>
    ParentFont = True
    SimplePanel = True
    SimpleText = 'Copyright '#169' Angus Johnson'
    UseSystemFont = False
  end
  object ScrollBox1: TScrollBox
    AlignWithMargins = True
    Left = 3
    Top = 38
    Width = 778
    Height = 475
    Align = alClient
    TabOrder = 1
    OnClick = ScrollBox1Click
    OnMouseDown = ScrollBox1MouseDown
    OnMouseMove = ScrollBox1MouseMove
    OnMouseUp = ScrollBox1MouseUp
    object PaintBox1: TPaintBox
      Left = 0
      Top = 0
      Width = 774
      Height = 471
      Align = alClient
      Visible = False
      OnMouseDown = ScrollBox1MouseDown
      OnMouseMove = ScrollBox1MouseMove
      OnMouseUp = ScrollBox1MouseUp
      OnPaint = PaintBox1Paint
      ExplicitLeft = -2
      ExplicitTop = -2
    end
  end
  object ToolBar1: TToolBar
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 778
    Height = 29
    ButtonHeight = 29
    ButtonWidth = 76
    Caption = 'ToolBar1'
    ShowCaptions = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Action = actOpen
    end
    object ToolButton2: TToolButton
      Left = 76
      Top = 0
      Action = actSave
      DropdownMenu = PopupMenuSave
      Style = tbsDropDown
    end
    object ToolButton3: TToolButton
      Left = 167
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object toolButtonNew: TToolButton
      Left = 175
      Top = 0
      Caption = '&New'
      DropdownMenu = PopupMenuNew
      ImageIndex = 0
      Style = tbsDropDown
      OnClick = toolButtonNewClick
    end
    object ToolButton5: TToolButton
      Left = 266
      Top = 0
      Width = 8
      Caption = 'ToolButton5'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 274
      Top = 0
      Action = actCopy
      Caption = '&Copy'
    end
    object ToolButton8: TToolButton
      Left = 350
      Top = 0
      Action = actCut
      Caption = 'Cut'
    end
    object ToolButton7: TToolButton
      Left = 426
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton6: TToolButton
      Left = 434
      Top = 0
      Action = actPaste
      Caption = '&Paste'
    end
    object ToolButton9: TToolButton
      Left = 510
      Top = 0
      Width = 8
      Caption = 'ToolButton9'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton10: TToolButton
      Left = 518
      Top = 0
      Action = actProperties
    end
    object ToolButton12: TToolButton
      Left = 594
      Top = 0
      Width = 8
      Caption = 'ToolButton12'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object ToolButton11: TToolButton
      Left = 602
      Top = 0
      Action = actPrint
    end
  end
  object PopupMenuDiagram: TPopupMenu
    Left = 708
    Top = 465
    object Properties1: TMenuItem
      Action = actProperties
      Default = True
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object NewRectangle1: TMenuItem
      Action = actRectangle
    end
    object NewDiamond1: TMenuItem
      Action = actDiamond
    end
    object NewEllipse1: TMenuItem
      Action = actEllipse
    end
    object NewPlainText1: TMenuItem
      Action = actPlainText
    end
    object NewTextBezier1: TMenuItem
      Action = actTextBezier
    end
    object NewArc1: TMenuItem
      Action = actArc
    end
    object NewPolygon1: TMenuItem
      Action = actPolygon
    end
    object NewStar1: TMenuItem
      Action = actStar
    end
    object NewSolidArrow1: TMenuItem
      Action = actArrow
    end
    object NewSolidBezier1: TMenuItem
      Action = actSolidBezier
    end
    object NewBitmapPic1: TMenuItem
      Action = actPic
    end
    object NewSolidPoint1: TMenuItem
      Action = actSolidPoint
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object NewLine1: TMenuItem
      Action = actLine
    end
    object NewLLine1: TMenuItem
      Action = actLLine
    end
    object NewZLine1: TMenuItem
      Action = actZLine
    end
    object NewBezier1: TMenuItem
      Action = actBezier
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Action = actDelete
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object BringtoFront1: TMenuItem
      Action = actBringToFront
    end
    object SendtoBack1: TMenuItem
      Action = actSendToBack
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object GrowLinefromTop1: TMenuItem
      Action = actGrowTop
    end
    object GrowLine1: TMenuItem
      Action = actGrow
    end
    object ShrinkLinefromTop1: TMenuItem
      Action = actShrinkTop
    end
    object ShrinkLinefromBottom1: TMenuItem
      Action = actShrink
    end
    object Rotate1: TMenuItem
      Action = actRotate
    end
    object FlipHorizontally1: TMenuItem
      Action = actFlip
    end
    object SplitPolyButton1: TMenuItem
      Caption = 'Split Polygon Button'
      OnClick = SplitPolyButton1Click
    end
    object DeletePolyButton1: TMenuItem
      Caption = 'Delete Polygon Button'
      OnClick = DeletePolyButton1Click
    end
    object SavePictoBMPfile1: TMenuItem
      Action = actSavePicToBMP
    end
  end
  object MainMenu: TMainMenu
    Left = 708
    Top = 433
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Action = actOpen
      end
      object Save2: TMenuItem
        Action = actSave
      end
      object Save1: TMenuItem
        Action = actSaveAs
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object DisableDesigning1: TMenuItem
        Caption = '&Disable Designing'
        OnClick = DisableDesigning1Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Action = actPrint
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = Exit1Click
      end
    end
    object Object1: TMenuItem
      Caption = '&Objects'
      object Properties2: TMenuItem
        Action = actProperties
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object DeleteObjects1: TMenuItem
        Action = actDelete
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object CutFocusedObjects2: TMenuItem
        Action = actCut
      end
      object CopyFocusedObjects1: TMenuItem
        Action = actCopy
      end
      object CopyAllObjectsasImage1: TMenuItem
        Action = actCopyAsBitmap
      end
      object CopyAllObjectsasMetafile1: TMenuItem
        Action = actCopyAsMetafile
      end
      object PasteObjects1: TMenuItem
        Action = actPaste
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object Align1: TMenuItem
        Caption = '&Align'
        object AlignLeft1: TMenuItem
          Action = actAlignLeft
        end
        object AlignTop1: TMenuItem
          Action = actAlignTop
        end
        object AlignRight1: TMenuItem
          Action = actAlignRight
        end
        object AlignBottom1: TMenuItem
          Action = actAlignBottom
        end
        object AlignCenterHorzontally1: TMenuItem
          Action = actAlignCenterHorz
        end
        object AlignCenterHorzontally2: TMenuItem
          Action = actAlignCenterVert
        end
        object N15: TMenuItem
          Caption = '-'
        end
        object SnaptoGrid1: TMenuItem
          Action = actSnapToGrid
        end
      end
      object Spacing1: TMenuItem
        Caption = '&Spacing'
        object SpaceEquallyHorizontally1: TMenuItem
          Action = actSpaceHorz
        end
        object SpaceEquallyVertically1: TMenuItem
          Action = actSpaceVert
        end
      end
      object Sizing1: TMenuItem
        Caption = 'Si&zing'
        object EqualWidths1: TMenuItem
          Action = actEqualWidths
        end
        object EqualHeights1: TMenuItem
          Action = actEqualHeights
        end
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object GrowLinefromTop2: TMenuItem
        Action = actGrowTop
      end
      object GrowLine2: TMenuItem
        Action = actGrow
      end
      object ShrinkLinefromTop2: TMenuItem
        Action = actShrinkTop
      end
      object ShrinkLinefromBottom2: TMenuItem
        Action = actShrink
      end
      object Rotate2: TMenuItem
        Action = actRotate
      end
    end
    object Configure1: TMenuItem
      Caption = '&Configure'
      object DefaultLineProperties1: TMenuItem
        Caption = 'Default &Line Properties'
        OnClick = DefaultLineProperties1Click
      end
      object DefaultSolidProperties1: TMenuItem
        Caption = 'Default &Solid Properties'
        OnClick = DefaultSolidProperties1Click
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object SetBackgroundColor1: TMenuItem
        Action = actBackground
      end
      object ShowGrid1: TMenuItem
        Action = actGrid
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object UseTextDialogPrompt1: TMenuItem
        Caption = 'Prompt for Text when creating new Solids'
        OnClick = UseTextDialogPrompt1Click
      end
      object UseHitTest1: TMenuItem
        Caption = 'Use HitTest for Object Selection'
        Checked = True
        OnClick = UseHitTest1Click
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 644
    Top = 433
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'dob'
    Filter = 'DrawObject Files|*.dob'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 612
    Top = 497
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dob'
    Filter = 'DrawObject Files|*.dob'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 740
    Top = 433
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 676
    Top = 433
  end
  object ActionManager1: TActionManager
    OnUpdate = ActionManager1Update
    Left = 612
    Top = 433
    StyleName = 'XP Style'
    object actRectangle: TAction
      Caption = 'New Rectangle'
      OnExecute = NewSolidBezier1Click
    end
    object actDiamond: TAction
      Caption = 'New Diamond'
      OnExecute = NewSolidBezier1Click
    end
    object actEllipse: TAction
      Caption = 'New Ellipse'
      OnExecute = NewSolidBezier1Click
    end
    object actPlainText: TAction
      Caption = 'New Plain Text'
      OnExecute = NewSolidBezier1Click
    end
    object actArc: TAction
      Caption = 'New Arc'
      OnExecute = NewSolidBezier1Click
    end
    object actPolygon: TAction
      Caption = 'New Polygon'
      OnExecute = NewSolidBezier1Click
    end
    object actStar: TAction
      Caption = 'New Star'
      OnExecute = NewSolidBezier1Click
    end
    object actArrow: TAction
      Caption = 'New Solid Arrow'
      OnExecute = NewSolidBezier1Click
    end
    object actTextBezier: TAction
      Caption = 'New Bezier Text'
      OnExecute = NewSolidBezier1Click
    end
    object actSolidBezier: TAction
      Caption = 'New Solid Bezier'
      OnExecute = NewSolidBezier1Click
    end
    object actPic: TAction
      Caption = 'New Bitmap Pic '
      OnExecute = NewSolidBezier1Click
    end
    object actSolidPoint: TAction
      Caption = 'New Solid Point'
      OnExecute = NewSolidBezier1Click
    end
    object actLine: TAction
      Caption = 'New Line'
      OnExecute = NewSolidBezier1Click
    end
    object actLLine: TAction
      Caption = 'New LLine'
      OnExecute = NewSolidBezier1Click
    end
    object actZLine: TAction
      Caption = 'New ZLine'
      OnExecute = NewSolidBezier1Click
    end
    object actBezier: TAction
      Caption = 'New Bezier'
      OnExecute = NewSolidBezier1Click
    end
    object actSendToBack: TAction
      Caption = 'Send to Back'
      ShortCut = 16450
      OnExecute = SendtoBack1Click
    end
    object actBringToFront: TAction
      Caption = 'Bring to Front'
      ShortCut = 16454
      OnExecute = BringtoFront1Click
    end
    object actProperties: TAction
      Caption = '&Properties'
      OnExecute = Properties1Click
    end
    object actDelete: TAction
      Caption = 'Delete Selected Objects'
      ShortCut = 46
      OnExecute = Delete1Click
    end
    object actCut: TAction
      Caption = 'Cut Focused Objects'
      ShortCut = 16472
      OnExecute = actCutExecute
    end
    object actCopy: TAction
      Caption = 'Copy Focused Objects'
      ShortCut = 16451
      OnExecute = CopyObject1Click
    end
    object actCopyAsBitmap: TAction
      Caption = 'Copy Objects as Bitmap'
      OnExecute = CopyAsBitmapClick
    end
    object actCopyAsMetafile: TAction
      Caption = 'Copy Objects as Metafile'
      OnExecute = actCopyAsMetafileClick
    end
    object actPaste: TAction
      Caption = '&Paste Objects'
      ShortCut = 16470
      OnExecute = PasteObject1Click
    end
    object actGrowTop: TAction
      Caption = 'Grow Line from Top'
      OnExecute = GrowLine1Click
    end
    object actGrow: TAction
      Caption = 'Grow Line from Bottom'
      OnExecute = GrowLine1Click
    end
    object actShrinkTop: TAction
      Caption = 'Shrink Line from Top'
      OnExecute = ShrinkLine1Click
    end
    object actShrink: TAction
      Caption = 'Shrink Line from Bottom'
      OnExecute = ShrinkLine1Click
    end
    object actRotate: TAction
      Caption = '&Rotate ...'
      ShortCut = 16466
      OnExecute = Rotate1Click
    end
    object actPrint: TAction
      Caption = '&Print Preview'
      ShortCut = 16464
      OnExecute = actPrintClick
    end
    object actBackground: TAction
      Caption = 'Bac&kground Color'
      ShortCut = 16459
      OnExecute = actBackgroundClick
    end
    object actSaveAs: TAction
      Caption = 'Save &As'
      OnExecute = actSaveAsClick
    end
    object actOpen: TAction
      Caption = '&Open'
      ShortCut = 16463
      OnExecute = actOpenClick
    end
    object actFlip: TAction
      Caption = 'Flip &Horizontally'
      OnExecute = actFlipExecute
    end
    object actSave: TAction
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = actSaveClick
    end
    object actSavePicToBMP: TAction
      Caption = 'Save Pic to &BMP file ...'
      OnExecute = actSavePicToBMPClick
    end
    object actAlignLeft: TAction
      Caption = 'Align &Left'
      OnExecute = actAlignLeftClick
    end
    object actAlignTop: TAction
      Caption = 'Align &Top'
      OnExecute = actAlignLeftClick
    end
    object actAlignRight: TAction
      Caption = 'Align &Right'
      OnExecute = actAlignLeftClick
    end
    object actAlignBottom: TAction
      Caption = 'Align &Bottom'
      OnExecute = actAlignLeftClick
    end
    object actAlignCenterHorz: TAction
      Caption = 'Align Center &Horizontally'
      OnExecute = actAlignLeftClick
    end
    object actAlignCenterVert: TAction
      Caption = 'Align Center &Vertically'
      OnExecute = actAlignLeftClick
    end
    object actSpaceHorz: TAction
      Caption = 'Space Equally &Horizontally'
      OnExecute = actSpaceHorzClick
    end
    object actSpaceVert: TAction
      Caption = 'Space Equally &Vertically'
      OnExecute = actSpaceHorzClick
    end
    object actEqualWidths: TAction
      Caption = 'Equal &Widths'
      OnExecute = actEqualWidthsClick
    end
    object actEqualHeights: TAction
      Caption = 'Equal &Heights'
      OnExecute = actEqualWidthsClick
    end
    object actGrid: TAction
      Caption = 'Show &Grid'
      ShortCut = 16455
      OnExecute = actGridClick
    end
    object actSnapToGrid: TAction
      Caption = 'Snap to Grid'
      OnExecute = actSnapToGridClick
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmap Pictures (*.bmp)|*.bmp'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 612
    Top = 465
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmap Pictures (*.bmp)|*.bmp'
    Left = 644
    Top = 497
  end
  object PrintDialog1: TPrintDialog
    Left = 740
    Top = 465
  end
  object PopupMenuSave: TPopupMenu
    Left = 644
    Top = 465
    object SaveAs1: TMenuItem
      Action = actSaveAs
    end
  end
  object PopupMenuNew: TPopupMenu
    Left = 676
    Top = 465
    object MenuItem3: TMenuItem
      Action = actRectangle
    end
    object MenuItem4: TMenuItem
      Action = actDiamond
    end
    object MenuItem5: TMenuItem
      Action = actEllipse
    end
    object MenuItem6: TMenuItem
      Action = actPlainText
    end
    object MenuItem7: TMenuItem
      Action = actTextBezier
    end
    object MenuItem8: TMenuItem
      Action = actArc
    end
    object MenuItem9: TMenuItem
      Action = actPolygon
    end
    object MenuItem10: TMenuItem
      Action = actStar
    end
    object MenuItem11: TMenuItem
      Action = actArrow
    end
    object MenuItem12: TMenuItem
      Action = actSolidBezier
    end
    object MenuItem13: TMenuItem
      Action = actPic
    end
    object MenuItem14: TMenuItem
      Action = actSolidPoint
    end
    object MenuItem15: TMenuItem
      Caption = '-'
    end
    object MenuItem16: TMenuItem
      Action = actLine
    end
    object MenuItem17: TMenuItem
      Action = actLLine
    end
    object MenuItem18: TMenuItem
      Action = actZLine
    end
    object MenuItem19: TMenuItem
      Action = actBezier
    end
  end
end
