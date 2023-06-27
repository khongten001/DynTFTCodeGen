object frmDynTFTCGRemoteSystemServerMain: TfrmDynTFTCGRemoteSystemServerMain
  Left = 0
  Top = 0
  Caption = 'DynTFT CG Remote System Server'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object lblAllocatedMemory: TLabel
    Left = 8
    Top = 253
    Width = 89
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Allocated Memory:'
    ParentShowHint = False
    ShowHint = True
  end
  object memLog: TMemo
    Left = 8
    Top = 24
    Width = 584
    Height = 202
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object prbAllocatedMemory: TProgressBar
    Left = 8
    Top = 272
    Width = 257
    Height = 17
    Anchors = [akLeft, akBottom]
    Smooth = True
    TabOrder = 1
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 3580
    MaxConnections = 1
    ReuseSocket = rsTrue
    OnExecute = IdTCPServer1Execute
    Left = 61
    Top = 53
  end
  object tmrStartup: TTimer
    Enabled = False
    Interval = 10
    OnTimer = tmrStartupTimer
    Left = 142
    Top = 53
  end
  object tmrPrepareClient: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrPrepareClientTimer
    Left = 240
    Top = 53
  end
  object tmrCloseClient: TTimer
    Enabled = False
    Interval = 1
    OnTimer = tmrCloseClientTimer
    Left = 344
    Top = 53
  end
  object tmrStats: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrStatsTimer
    Left = 448
    Top = 56
  end
end
