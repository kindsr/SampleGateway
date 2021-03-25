object Main: TMain
  Left = 0
  Top = 0
  Caption = 'Main'
  ClientHeight = 539
  ClientWidth = 447
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 447
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 0
    object btnTCPServer: TButton
      Left = 0
      Top = 0
      Width = 75
      Height = 65
      Align = alLeft
      Caption = 'TCP Server Start'
      TabOrder = 0
      WordWrap = True
    end
    object btnUDPServer: TButton
      Left = 75
      Top = 0
      Width = 75
      Height = 65
      Align = alLeft
      Caption = 'UDP Server Start'
      TabOrder = 1
      WordWrap = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 65
    Width = 447
    Height = 474
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 1
    object messagesLog: TMemo
      Left = 0
      Top = 0
      Width = 447
      Height = 474
      Align = alClient
      BorderStyle = bsNone
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -17
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        '')
      ParentFont = False
      TabOrder = 0
      ExplicitTop = 205
      ExplicitWidth = 725
      ExplicitHeight = 345
    end
  end
  object Timer1: TTimer
    Left = 392
    Top = 24
  end
end
