object Form1: TForm1
  Left = 540
  Top = 256
  BorderStyle = bsDialog
  Caption = 'CmpCompare'
  ClientHeight = 622
  ClientWidth = 881
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnFile: TButton
    Left = 8
    Top = 8
    Width = 177
    Height = 27
    Caption = 'Select File'
    TabOrder = 0
    OnClick = btnFileClick
  end
  object memoOut: TMemo
    Left = 8
    Top = 41
    Width = 865
    Height = 573
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object dlgOpen: TOpenDialog
    Left = 719
    Top = 563
  end
end
