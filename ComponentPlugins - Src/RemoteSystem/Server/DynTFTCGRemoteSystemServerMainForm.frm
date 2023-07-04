object frmDynTFTCGRemoteSystemServerMain: TfrmDynTFTCGRemoteSystemServerMain
  Left = 387
  Height = 347
  Top = 43
  Width = 600
  Caption = 'DynTFT CG Remote System Server'
  ClientHeight = 347
  ClientWidth = 600
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  LCLVersion = '7.5'
  object memLog: TMemo
    Left = 8
    Height = 265
    Top = 24
    Width = 584
    Anchors = [akTop, akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object prbAllocatedMemory: TProgressBar
    Left = 8
    Height = 17
    Top = 320
    Width = 257
    Anchors = [akLeft]
    Smooth = True
    TabOrder = 1
  end
  object lblAllocatedMemory: TLabel
    Left = 8
    Height = 15
    Top = 296
    Width = 101
    Anchors = [akLeft]
    Caption = 'Allocated Memory:'
    ParentShowHint = False
    ShowHint = True
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
    Left = 476
    Top = 84
  end
  object tmrLogging: TTimer
    Interval = 200
    OnTimer = tmrLoggingTimer
    Left = 480
    Top = 152
  end
end
