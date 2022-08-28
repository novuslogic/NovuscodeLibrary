object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'InterpreterTest1'
  ClientHeight = 548
  ClientWidth = 1226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnShow = FormShow
  DesignSize = (
    1226
    548)
  TextHeight = 13
  object Memo: TMemo
    Left = 32
    Top = 30
    Width = 281
    Height = 479
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '//A comment'
      '/*A com'
      'ment ....*/'
      'x = 3.1415'
      'y = 0.23456'
      'z = 3.12E6'
      'w = 56.45E-6'
      'x = 4 + 6'
      'y = x * 2'
      'print (x)'
      'println()'
      'print (y)'
      's = "Hello World"'
      'print (s)'
      'x = 3.1415'
      'y = x/y + 2.0^x;'
      'if x > 5 then'
      '   begin'
      '   x = x + 1'
      '   end;'
      'a = "abcdefg 1234'
      '  56678"'
      'b = "abcdefg 1234'
      'xyz"')
    TabOrder = 0
    ExplicitHeight = 401
  end
  object ParseButton: TButton
    Left = 328
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Parse'
    TabOrder = 1
    OnClick = ParseButtonClick
  end
  object LexStringGrid: TStringGrid
    Left = 417
    Top = 30
    Width = 774
    Height = 479
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 4
    DefaultColWidth = 142
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 20
    TabOrder = 2
    ExplicitWidth = 592
    ExplicitHeight = 401
  end
end
