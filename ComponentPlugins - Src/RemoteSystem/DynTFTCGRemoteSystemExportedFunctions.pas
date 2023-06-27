{   DynTFT  - graphic components for microcontrollers
    Copyright (C) 2017, 2022 VCC
    initial release date: 29 Dec 2017
    author: VCC

    This file is part of DynTFT project.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this file,
    You can obtain one at https://mozilla.org/MPL/2.0/.

    Copyright (c) 2022, VCC  https://github.com/VCC02

    Alternatively, the contents of this file may be used under the terms
    of the GNU Lesser General Public License Version 3, as described below:

    DynTFT is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, version 3 of the License.

    DynTFT is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with DynTFT, in COPYING.LESSER file.
    If not, see <http://www.gnu.org/licenses/>.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
    OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
}

unit DynTFTCGRemoteSystemExportedFunctions;

interface

uses
  Windows, DynTFTCodeGenSharedDataTypes, IdTCPClient;

function GetPluginName(APluginName: Pointer): Integer; stdcall;

procedure InitPlugin; stdcall;
procedure DonePlugin; stdcall;
function GetListOfComponents(AListOfComponents: Pointer): Integer; stdcall;
function GetListOfDoNotUseComponents(AListOfComponents: Pointer): Integer; stdcall;
function GetListOfLiveColorComponents(AListOfComponents: Pointer): Integer; stdcall;
procedure GetComponentIcon(ComponentIndex: Integer; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64); stdcall;
function GetSchemaDirName(ADirName: Pointer): Integer; stdcall;
procedure UpdateLiveColorConstants(AComponentIndex, AColorIndex, ANewColor: Integer); stdcall;
procedure BackupLiveColorConstants; stdcall;
procedure RestoreColorConstantsFromBackup; stdcall;
function CanUpdateLiveColorConstant(ComponentIndex, ColorIndex: Integer): Boolean; stdcall;
function GetLiveColorsConstCountByComponent(ComponentIndex: Integer): Integer; stdcall;
procedure DisplayDebugConsole(AMainFormHandle: THandle); stdcall;


//functions and vars used in RSPanelDrawing unit
//function ReadLnStringFromServer: string;
procedure SendPlainStringToServer(s: string);
procedure SendCommandToServer(ACmd, ACmdParam: string);
procedure SendPluginStartupCommandToServer;
procedure SendPluginPortCommandToServer;
procedure SendDisconnectFromPluginCommandToServer;
procedure SendPingCommandToServer;


const
  CPluginClientConnectTimeout =  1000;
  CPluginClientReConnectTimeout = 100;   //This is how long the UI is freezed when disconnected, on every FtmrCheckConnectionTimer call. Higher values may be needed when connecting over a network.


var
  FIdTCPClient: TIdTCPClient;
  FRemoteSystemServerAddress: string;
  FRemoteSystemServerPort: Word;
  FPluginServerPort: Word;

  
implementation


uses
  SysUtils, Classes, Forms, DynTFTSharedUtils, Graphics, DynTFTCodeGenImgFormRS,
  IdBaseComponent, IdComponent, IdTCPConnection, IdIOHandler, IniFiles,
  DynTFTUtils, RemoteSystemCommands, RSPanelDrawing
  {, SysInit}
  ;


const
  CPluginName = 'System Components';  //the same string is used in color themes which target a specific plugin
  CComponentCount = 20;


procedure SendPlainStringToServer(s: string);
begin
  FIdTCPClient.Socket.WriteLn(s);
end;


procedure SendCommandToServer(ACmd, ACmdParam: string);
begin
  FIdTCPClient.Socket.WriteLn(ACmd + '=' + ACmdParam);
end;


procedure SendPluginStartupCommandToServer;
begin
  SendCommandToServer(CCGRM_PluginStartup, CPluginName);
end;


procedure SendPluginPortCommandToServer;
begin
  SendCommandToServer(CCGRM_PluginPort, IntToStr(FPluginServerPort));
end;


procedure SendDisconnectFromPluginCommandToServer;
begin
  SendCommandToServer(CCGRM_DisconnectFromPlugin, CPluginName);
end;


procedure SendPluginDoneCommandToServer;
begin
  SendCommandToServer(CCGRM_PluginDone, CPluginName);
end;


procedure SendPingCommandToServer;
begin
  SendCommandToServer(CCGRM_Ping, CPluginName);
end;


procedure GetComponentIconFromServer(ComponentIndex: Integer; ATempStream: TMemoryStream);
begin
  SendCommandToServer(CCGRM_GetComponentIconFromServer, IntToStr(ComponentIndex));
  ATempStream.SetSize(FIdTCPClient.Socket.ReadInt64);
  FIdTCPClient.Socket.ReadStream(ATempStream, ATempStream.Size);
end;


function GetPluginPath: string;
var
  PathLen: Integer;
  Buffer: array[0..2048] of Char;
begin
  PathLen := GetModuleFileName(HInstance, @Buffer, SizeOf(Buffer));
  Result := string(Buffer);
  SetLength(Result, PathLen);
end;


procedure LoadSettings;
const
  CDefaultRSServerAddress: string = '127.0.0.1';
  CDefaultRSServerPort: Word = 3580;
  CDefaultPluginServerPort: Word = 3581;
var
  Ini: TMemIniFile;
begin
  try
    Ini := TMemIniFile.Create(ExtractFilePath(GetPluginPath) + CPluginName + '.ini');
    try
      FRemoteSystemServerAddress := Ini.ReadString('Settings', 'RSServerAddress', CDefaultRSServerAddress);
      FRemoteSystemServerPort := Ini.ReadInteger('Settings', 'RSServerPort', CDefaultRSServerPort);
      FPluginServerPort := Ini.ReadInteger('Settings', 'PluginServerPort', CDefaultPluginServerPort);
    finally
      Ini.Free;
    end;
  except
    on E: Exception do
    begin
      FRemoteSystemServerAddress := CDefaultRSServerAddress;
      FRemoteSystemServerPort := CDefaultRSServerPort;
      FPluginServerPort := CDefaultPluginServerPort;
      DynTFT_DebugConsole('Exception when loading plugin settings: ' + E.Message);
    end;
  end;
end;


procedure SaveSettings;
var
  Ini: TMemIniFile;
begin
  try
    Ini := TMemIniFile.Create(ExtractFilePath(GetPluginPath) + CPluginName + '.ini');
    try
      Ini.WriteString('Settings', 'RSServerAddress', FRemoteSystemServerAddress);
      Ini.WriteInteger('Settings', 'RSServerPort', FRemoteSystemServerPort);
      Ini.WriteInteger('Settings', 'PluginServerPort', FPluginServerPort);

      Ini.UpdateFile;
    finally
      Ini.Free;
    end;
  except

  end;
end;



function GetPluginName(APluginName: Pointer): Integer; stdcall;
begin
  Result := SetPointedContentFromString(CPluginName, APluginName);
end;


procedure InitPlugin; stdcall;
begin
  Application.Initialize;

  Application.CreateForm(TfrmImg, frmImg);   //Having a form with console or extra component images, is not a requirement for all plugins.
  DynTFT_AssignDebugConsole(frmImg.memLog);

  LoadSettings;

  try
    CreateCallbackTCPServer;
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Callback server can''t start. ' + E.Message);
      DynTFT_DebugConsole('If no other instance of this plugin is running, please wait about 1min, then start again.');
      frmImg.Show;
    end;
  end;

  FIdTCPClient := TIdTCPClient.Create(nil);
  try
    FIdTCPClient.ConnectTimeout := 1000;
    FIdTCPClient.ReadTimeout := 1000;
    FIdTCPClient.UseNagle := False;
    FIdTCPClient.Connect(FRemoteSystemServerAddress, FRemoteSystemServerPort);
    DynTFT_DebugConsole('Connected to server for DynTFTCGSystem components.');
    SendPluginStartupCommandToServer;
    SendPluginPortCommandToServer;
    DisplayConnectionStatus(True);
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Can''t connect to server. ' + E.Message + ' Make sure there is no other running instance of this plugin.');
      DisplayConnectionStatus(False);
    end;
  end;

  CreateConnectionTimer;
end;


procedure DonePlugin; stdcall;
begin
  DestroyConnectionTimer;
  
  try
    SendPluginDoneCommandToServer;
    FIdTCPClient.Disconnect;
  except
    on E: Exception do
      DynTFT_DebugConsole('Can''t disconnect from server. ' + E.Message);
  end;

  try
    DestroyCallbackTCPServer;
  finally
    frmImg.Close;
    frmImg := nil;

    FreeAndNil(FIdTCPClient);
  end;

  SaveSettings;
end;


function GetListOfComponents(AListOfComponents: Pointer): Integer; stdcall;  //This list has to match CComponentCount.
var
  AStringList: TStringList;
  ResultStr: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('DynTFTButton');
    AStringList.Add('DynTFTArrowButton');
    AStringList.Add('DynTFTPanel');
    AStringList.Add('DynTFTCheckBox');
    AStringList.Add('DynTFTScrollBar');
    AStringList.Add('DynTFTItems');
    AStringList.Add('DynTFTListBox');
    AStringList.Add('DynTFTLabel');
    AStringList.Add('DynTFTRadioButton');
    AStringList.Add('DynTFTRadioGroup');
    AStringList.Add('DynTFTTabButton');
    AStringList.Add('DynTFTPageControl');
    AStringList.Add('DynTFTEdit');
    AStringList.Add('DynTFTKeyButton');
    AStringList.Add('DynTFTVirtualKeyboard');
    AStringList.Add('DynTFTComboBox');
    AStringList.Add('DynTFTTrackBar');
    AStringList.Add('DynTFTProgressBar');
    AStringList.Add('DynTFTMessageBox');
    AStringList.Add('DynTFTVirtualTable');

    //the CComponentCount constant has to be updated when adding components

    ResultStr := AStringList.Text;      //CRLF separated component names
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


//'0' = use,  '1' = do not use
//this list has to match the number of components returned by GetListOfComponents
function GetListOfDoNotUseComponents(AListOfComponents: Pointer): Integer; stdcall;  //This list has to match CComponentCount.
var
  AStringList: TStringList;
  ResultStr: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('0'); //DynTFTButton
    AStringList.Add('0'); //DynTFTArrowButton
    AStringList.Add('0'); //DynTFTPanel
    AStringList.Add('0'); //DynTFTCheckBox
    AStringList.Add('0'); //DynTFTScrollBar
    AStringList.Add('1'); //DynTFTItems
    AStringList.Add('0'); //DynTFTListBox
    AStringList.Add('0'); //DynTFTLabel
    AStringList.Add('1'); //DynTFTRadioButton
    AStringList.Add('0'); //DynTFTRadioGroup
    AStringList.Add('1'); //DynTFTTabButton
    AStringList.Add('0'); //DynTFTPageControl
    AStringList.Add('0'); //DynTFTEdit
    AStringList.Add('1'); //DynTFTKeyButton
    AStringList.Add('0'); //DynTFTVirtualKeyboard
    AStringList.Add('0'); //DynTFTComboBox
    AStringList.Add('0'); //DynTFTTrackBar
    AStringList.Add('0'); //DynTFTProgressBar
    AStringList.Add('1'); //DynTFTMessageBox
    AStringList.Add('0'); //DynTFTVirtualTable

    //the CComponentCount constant has to be updated when adding components

    ResultStr := AStringList.Text;        //CRLF separated component usage
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


//The following description mentions two different DynTFTColorTheme.inc files. One is part of a color theme (loaded by DynTFTCodeGen), and the other is part of this plugin, linked through DynTFTConsts.pas and used in DynTFTColorThemeGenLiveColors.pas.
//This list has to match the length of LiveColors array and the order of its elements.  (see DynTFTColorThemeGenLiveColors.pas)
//It may contain a subset and/or superset of the list returned by GetListOfComponents.
//The component order does not have to match the order of component .inc files in DynTFTColorTheme.inc from a color theme anymore. This was a requirement of old DynTFTCodeGen, before moving components to plugins, where these lists were sorted (both LiveColors and <component>.inc files in DynTFTColorTheme.inc).
//For example, there is no ComboBox or PageControl, but there is Screen.
//This list is used by DynTFTCodeGen, to properly identify every component index when calling UpdateLiveColorConstants.
//As an old requirement, and also to avoid adding/maintaining more functions like this on the API, the number and order of color constants from a component .inc file (color theme), will still have to match the CL_All_<ComponentName>_Colors arrays from plugin's DynTFTColorTheme.inc file.
function GetListOfLiveColorComponents(AListOfComponents: Pointer): Integer; stdcall;
var
  AStringList: TStringList;
  ResultStr: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('DynTFTArrowButton');
    AStringList.Add('DynTFTButton');
    AStringList.Add('DynTFTCheckBox');
    AStringList.Add('DynTFTEdit');
    AStringList.Add('DynTFTItems');
    AStringList.Add('DynTFTKeyButton');
    AStringList.Add('DynTFTLabel');
    AStringList.Add('DynTFTListBox');
    AStringList.Add('DynTFTMessageBox');
    AStringList.Add('DynTFTPanel');
    AStringList.Add('DynTFTProgressBar');
    AStringList.Add('DynTFTRadioButton');
    AStringList.Add('DynTFTRadioGroup');
    AStringList.Add('DynTFTScreen');        //There is not equivalent component for DynTFTScreen. It represents an .inc file in color theme as for any other component.
    AStringList.Add('DynTFTScrollBar');
    AStringList.Add('DynTFTTabButton');
    AStringList.Add('DynTFTTrackBar');
    AStringList.Add('DynTFTVirtualKeyboard');

    ResultStr := AStringList.Text;      //CRLF separated component names
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


procedure DrawNoServerOnBitmap(ATempBitmap: TBitmap);
begin
  ATempBitmap.Width := 24;
  ATempBitmap.Height := 24;
  ATempBitmap.PixelFormat := pf24bit;
  ATempBitmap.Canvas.Font.Color := clBlue;
  ATempBitmap.Canvas.Font.Name := 'Tahoma';
  ATempBitmap.Canvas.Font.Size := 8;
  ATempBitmap.Canvas.TextOut(4, 0, 'No');
  ATempBitmap.Canvas.TextOut(1, 11, 'Srv');
end;


procedure GetComponentIcon(ComponentIndex: Integer; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64); stdcall;
var
  TempStream: TMemoryStream;
  TempBitmap: TBitmap;
  AContentPointer: Pointer;
begin
  if (ComponentIndex < 0) or (ComponentIndex > CComponentCount - 1) then
    raise Exception.Create('Index out of bounds (' + IntToStr(ComponentIndex) + ') when getting icon.');

  if not Assigned(ASetSizeCallback) then
    raise Exception.Create('ASetSizeCallback is not assigned. It is required to set the stream size before getting the icon contents.');

  TempStream := TMemoryStream.Create;
  try
    TempBitmap := TBitmap.Create;
    try
      try
        GetComponentIconFromServer(ComponentIndex, TempStream);
      except
        DrawNoServerOnBitmap(TempBitmap);
        TempBitmap.SaveToStream(TempStream);
      end;

      AContentPointer := ASetSizeCallback(TempStream.Size, AStreamID);   //DynTFTCodeGen should implement a callback, which sets the stream size based on this value

      Move(TempStream.Memory^, AContentPointer^, TempStream.Size);
    finally
      TempBitmap.Free;
    end;
  finally
    TempStream.Free;
  end;
end;


function GetSchemaDirName(ADirName: Pointer): Integer; stdcall;
var
  ResultStr: string;
begin
  ResultStr := 'SystemSchemas';                //Schema file names are expected to be of "<ComponentName>.dynscm" format.
  Result := SetPointedContentFromString(ResultStr, ADirName);
end;


procedure UpdateLiveColorConstants(AComponentIndex, AColorIndex, ANewColor: Integer); stdcall;
begin
  try
    SendCommandToServer(CCGRM_UpdateLiveColorConstants, 'Update');
    FIdTCPClient.Socket.Write(LongInt(AComponentIndex));
    FIdTCPClient.Socket.Write(LongInt(AColorIndex));
    FIdTCPClient.Socket.Write(LongInt(ANewColor));
  except
    on E: Exception do
      DynTFT_DebugConsole('Ex in UpdateLiveColorConstants: ' + E.Message);
  end;
end;


procedure BackupLiveColorConstants; stdcall;
begin
  try
    SendCommandToServer(CCGRM_BackupLiveColorConstants, 'Backup');
  except
    on E: Exception do
      DynTFT_DebugConsole('Ex in BackupLiveColorConstants: ' + E.Message);
  end;
end;


procedure RestoreColorConstantsFromBackup; stdcall;
begin
  try
    SendCommandToServer(CCGRM_RestoreColorConstantsFromBackup, 'Restore');
  except
    on E: Exception do
      DynTFT_DebugConsole('Ex in RestoreColorConstantsFromBackup: ' + E.Message);
  end;
end;


function CanUpdateLiveColorConstant(ComponentIndex, ColorIndex: Integer): Boolean; stdcall;
begin
  try
    SendCommandToServer(CCGRM_CanUpdateLiveColorConstant, 'CanUpdate');
    FIdTCPClient.Socket.Write(LongInt(ComponentIndex));
    FIdTCPClient.Socket.Write(LongInt(ColorIndex));

    Result := FIdTCPClient.Socket.ReadLn(#13#10, 2000, 131072) = 'True';
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Ex in CanUpdateLiveColorConstant: ' + E.Message);
      Result := False;
    end;
  end;
end;


function GetLiveColorsConstCountByComponent(ComponentIndex: Integer): Integer; stdcall;
begin
  try
    SendCommandToServer(CCGRM_GetLiveColorsConstCountByComponent, 'GetLive');
    FIdTCPClient.Socket.Write(LongInt(ComponentIndex));
    Result := FIdTCPClient.Socket.ReadInt32;
  except
    on E: Exception do
    begin
      DynTFT_DebugConsole('Ex in GetLiveColorsConstCountByComponent: ' + E.Message);
      Result := -1;
    end;
  end;
end;


procedure DisplayDebugConsole(AMainFormHandle: THandle); stdcall;
begin
  frmImg.Show;
end;

end.
