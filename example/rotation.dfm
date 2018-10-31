object RotationForm: TRotationForm
  Left = 771
  Top = 530
  BorderStyle = bsToolWindow
  Caption = 'Rotate Object'
  ClientHeight = 122
  ClientWidth = 218
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblAngle: TLabel
    Left = 30
    Top = 22
    Width = 82
    Height = 13
    Caption = '&Rotation Angle: 0'
    FocusControl = TrackBar1
  end
  object btnOK: TButton
    Left = 70
    Top = 79
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object TrackBar1: TTrackBar
    Left = 21
    Top = 45
    Width = 173
    Height = 23
    Max = 180
    Min = -180
    Frequency = 5
    TabOrder = 0
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
end
