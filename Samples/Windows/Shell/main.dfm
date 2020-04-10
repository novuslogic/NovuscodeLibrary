object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ShellDemo'
  ClientHeight = 671
  ClientWidth = 518
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnRunCommand: TButton
    Left = 8
    Top = 24
    Width = 121
    Height = 49
    Caption = 'RunCommand'
    TabOrder = 0
    OnClick = btnRunCommandClick
  end
  object btnRunCommandSilent: TButton
    Left = 8
    Top = 79
    Width = 121
    Height = 49
    Caption = 'RunCommandSilent'
    TabOrder = 1
    OnClick = btnRunCommandSilentClick
  end
  object Memo: TMemo
    Left = 8
    Top = 224
    Width = 497
    Height = 386
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object btnRunCommandCapture: TButton
    Left = 8
    Top = 134
    Width = 121
    Height = 49
    Caption = 'RunCommandCapture'
    TabOrder = 3
    OnClick = btnRunCommandCaptureClick
  end
end
