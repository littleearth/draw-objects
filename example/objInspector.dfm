object ObjInspectForm: TObjInspectForm
  Left = 555
  Top = 182
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Object Inspector'
  ClientHeight = 622
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  PixelsPerInch = 96
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 36
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object eComponentInfo: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 5
      Width = 248
      Height = 26
      Margins.Top = 5
      Margins.Bottom = 5
      TabStop = False
      Align = alClient
      ReadOnly = True
      TabOrder = 1
      ExplicitLeft = 8
      ExplicitTop = 6
      ExplicitWidth = 233
      ExplicitHeight = 23
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 257
      Top = 3
      Width = 60
      Height = 30
      Align = alRight
      Caption = '&Close'
      TabOrder = 0
      OnClick = btnCloseClick
      ExplicitLeft = 250
      ExplicitTop = 6
      ExplicitHeight = 23
    end
  end
  object vle: TValueListEditor
    AlignWithMargins = True
    Left = 3
    Top = 39
    Width = 314
    Height = 567
    Align = alClient
    Color = clBtnFace
    DefaultColWidth = 160
    DefaultRowHeight = 19
    DisplayOptions = [doAutoColResize, doKeyColFixed]
    FixedCols = 1
    TabOrder = 1
    OnEditButtonClick = vleEditButtonClick
    OnKeyPress = vleKeyPress
    OnSetEditText = vleSetEditText
    OnValidate = vleValidate
    ExplicitLeft = 0
    ExplicitTop = 36
    ExplicitWidth = 320
    ExplicitHeight = 579
    ColWidths = (
      160
      148)
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 612
    Width = 314
    Height = 7
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 0
    ExplicitTop = 615
    ExplicitWidth = 320
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 24
    Top = 80
  end
  object ColorDialog1: TColorDialog
    Left = 24
    Top = 112
  end
end
