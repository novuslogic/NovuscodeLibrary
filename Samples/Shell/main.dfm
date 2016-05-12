object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ShellDemo'
  ClientHeight = 401
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
  object Button1: TButton
    Left = 8
    Top = 24
    Width = 121
    Height = 49
    Caption = 'Run Notepad'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 144
    Top = 24
    Width = 121
    Height = 49
    Caption = 'Run cmd.exe'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 279
    Top = 24
    Width = 121
    Height = 49
    Caption = 'Run Powershell'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Memo: TMemo
    Left = 8
    Top = 86
    Width = 393
    Height = 243
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object btnCmdDir: TButton
    Left = 8
    Top = 344
    Width = 153
    Height = 25
    Caption = 'Start Cmd.exe with Dir'
    TabOrder = 4
    OnClick = btnCmdDirClick
  end
end
