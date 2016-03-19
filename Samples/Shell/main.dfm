object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ShellDemo'
  ClientHeight = 291
  ClientWidth = 503
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
end
