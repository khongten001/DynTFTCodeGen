object frmDynTFTCGRemoteSystemServerMain: TfrmDynTFTCGRemoteSystemServerMain
  Left = 387
  Height = 240
  Top = 43
  Width = 600
  Caption = 'DynTFT CG Remote System Server'
  ClientHeight = 240
  ClientWidth = 600
  OnClose = FormClose
  OnCreate = FormCreate
  LCLVersion = '7.5'
  object memLog: TMemo
    Left = 8
    Height = 202
    Top = 24
    Width = 584
    Anchors = [akTop, akLeft, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object IdTCPServer1: TIdTCPServer
    Bindings = <>
    DefaultPort = 3580
    MaxConnections = 1
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
end
