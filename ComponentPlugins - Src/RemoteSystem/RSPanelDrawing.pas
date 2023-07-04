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

unit RSPanelDrawing;

{ToDo

[in work] - Fix various synchronizations issues (client-server race conditions)
  - includes drawing a component over another (because there is no synchronization between drawing components and drawing primitives).  Currently, drawing the primitives (in exe) is done from a separate thread (see ReadDrawingCommands below).
  - maybe the primitives functions should have a way to identify the components they draw onto
[done] - Implement modifying properties by plugin  (requires the server to implement sending back the list of properties)
  This is needed to properly generate code for components like Items, ListBox, RadioGroup, PageControl.
[in work] - Implement a timer for verifying (pinging) server connection and automatically reconnect if disconnected
- Optimize the client-server API to send as less redundant data as possible. For example, color constants might be sent once.
  This optimization makes sense when refreshing multiple components in one action.
  [partially done] - Properties are converted to string using IntToStr. These should be replaced with IntToHex.
- Tweak timeouts, to improve speed.
}

interface

uses
  Windows, SysUtils, Classes, DynTFTCodeGenSharedDataTypes, ExtCtrls, IdTCPServer;

procedure RegisterAllComponentsEvents; stdcall;
procedure DrawPDynTFTComponentOnPanel(var APanelBase: TUIPanelBase; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef; ASetPropertiesCallback: TSetPropertiesCallback); stdcall;

procedure RegisterDynTFTDrawingProcedures(
  ADynTFT_Set_Pen_Callback: TDynTFT_Set_Pen_Callback;
  ADynTFT_Set_Brush_Callback: TDynTFT_Set_Brush_Callback;
  ADynTFT_Set_Font_Callback: TDynTFT_Set_Font_Callback;
  ADynTFT_Write_Text_Callback: TDynTFT_Write_Text_Callback;
  ADynTFT_Line_Callback: TDynTFT_Line_Callback;
  ADynTFT_H_Line_Callback: TDynTFT_H_Line_Callback;
  ADynTFT_V_Line_Callback: TDynTFT_V_Line_Callback;
  ADynTFT_Dot_Callback: TDynTFT_Dot_Callback;
  ADynTFT_Fill_Screen_Callback: TDynTFT_Fill_Screen_Callback;
  ADynTFT_Rectangle_Callback: TDynTFT_Rectangle_Callback;
  ADynTFT_Circle_Callback: TDynTFT_Circle_Callback;
  AGetTextWidthAndHeight_Callback: TDynTFT_GetTextWidthAndHeight_Callback;
  ADynTFT_DrawBitmap_Callback: TDynTFT_DrawBitmap_Callback ); stdcall;


procedure CreateCallbackTCPServer;
procedure DestroyCallbackTCPServer;

procedure CreateConnectionTimer;
procedure DestroyConnectionTimer;
procedure DisplayConnectionStatus(AIsConnected: Boolean);


var
  FIdTCPServer: TIdTCPServer;


implementation


uses
  DynTFTCGRemoteSystemExportedFunctions, RemoteSystemCommands, DynTFTPluginUtils,
  TFTCallbacks, DynTFTUtils, Graphics, DynTFTSharedUtils, DynTFTCodeGenImgFormRS,
  IdCustomTCPServer, IdContext, IdIOHandlerSocket, IdException,
  //IdSync,
  IdGlobal
  ;

//type
//
//  TSyncUIObj = class(TIdSync)    //only to have a dummy UI sync
//  protected
//    procedure DoSynchronize; override;
//  end;
//
//  TSyncReadDrawingCommandsObj = class(TIdSync)
//  private
//    FSocket: TIdIOHandlerSocket;
//    FCmdParam: string;
//  protected
//    procedure DoSynchronize; override;
//  end;

var
  FDrawingProcedures: TDrawDynTFTComponentProcArr;
  FtmrCheckConnection: TTimer;


//procedure TSyncUIObj.DoSynchronize;
//begin
//  SendMessage(frmImg.Handle, 1024 {WM_USER}, 0, 0);   //not fully effective
//end;
//
//
//procedure SyncThreadToUI;
//var
//  SyncObj: TSyncUIObj;
//begin
//  SyncObj := TSyncUIObj.Create;
//  try
//    SyncObj.Synchronize;
//  finally
//    SyncObj.Free;
//  end;
//end;


function EncodePanelBasePropertiesToString(APanel: TUIPanelBase): string;
begin
  Result := IntToStr(APanel.Width) + CRecFieldSeparator +
            IntToStr(APanel.Height) + CRecFieldSeparator +
            IntToStr(APanel.DynTFTComponentType) + CRecFieldSeparator +
            APanel.Caption + CRecFieldSeparator +
            IntToStr(APanel.Tag);
end;


function PropertiesOrEventsToString(var APropertiesOrEvents: TDynTFTDesignPropertyArr): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(APropertiesOrEvents) - 1 do
    Result := Result + APropertiesOrEvents[i].PropertyName + CRecFieldSeparator +
                       APropertiesOrEvents[i].PropertyValue + CRecFieldArrayItemSeparator;
end;


function SchemaConstantsToString(ASchemaConstants: TComponentConstantArr): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(ASchemaConstants) - 1 do
    Result := Result + ASchemaConstants[i].ConstantName + CRecFieldSeparator +
                       ASchemaConstants[i].ConstantDataType + CRecFieldSeparator +
                       IntToStr(ASchemaConstants[i].ConstantValueInt) + CRecFieldArrayItemSeparator;
end;


function ColorConstantsToString(var AColorConstants: TColorConstArr): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(AColorConstants) - 1 do
    Result := Result + AColorConstants[i].Name + CRecFieldSeparator +
                       IntToStr(AColorConstants[i].Value) + CRecFieldSeparator +
                       AColorConstants[i].SchemaValue + CRecFieldSeparator +
                       IntToStr(AColorConstants[i].InitialValue) + CRecFieldArrayItemSeparator;
end;


function FontSettingsToString(var AFontSettings: TFontSettingsArr): string;
var
  i: Integer;
begin
  Result := '';                            
  for i := 0 to Length(AFontSettings) - 1 do
    Result := Result + AFontSettings[i].FontPropertyValue + CRecFieldSeparator +
                       IntToHex(Int64(AFontSettings[i].SFont), 8) + CRecFieldSeparator +  // font address
                       AFontSettings[i].SFont.FontName + CRecFieldSeparator +
                       AFontSettings[i].SFont.IdentifierName + CRecFieldSeparator +
                       IntToHex(AFontSettings[i].SFont.FontSize, 2) + CRecFieldSeparator +
                       IntToHex(Ord(AFontSettings[i].SFont.Bold), 1) + CRecFieldSeparator +
                       IntToHex(Ord(AFontSettings[i].SFont.Italic), 1) + CRecFieldSeparator +
                       IntToHex(Ord(AFontSettings[i].SFont.Underline), 1) + CRecFieldSeparator +
                       IntToHex(Ord(AFontSettings[i].SFont.StrikeOut), 1) + CRecFieldSeparator +
                       IntToHex(AFontSettings[i].SFont.Charset, 2) + CRecFieldSeparator +
                       IntToHex(Ord(AFontSettings[i].SFont.Pitch), 1) + CRecFieldArrayItemSeparator;

end;


procedure SendComponentDataToServer(var APropertiesOrEvents: TDynTFTDesignPropertyArr; var ASchemaConstants: TComponentConstantArr; var AColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  PropertiesOrEventsStr: string;
  SchemaConstantsStr: string;
  ColorConstantsStr: string;
  FontSettings: string;
begin
  try
    PropertiesOrEventsStr := Replace1310To78(PropertiesOrEventsToString(APropertiesOrEvents));
    SchemaConstantsStr := Replace1310To78(SchemaConstantsToString(ASchemaConstants));
    ColorConstantsStr := Replace1310To78(ColorConstantsToString(AColorConstants));
    FontSettings := Replace1310To78(FontSettingsToString(AFontSettings));

    SendPlainStringToServer(PropertiesOrEventsStr);
    SendPlainStringToServer(SchemaConstantsStr);
    SendPlainStringToServer(ColorConstantsStr);
    SendPlainStringToServer(FontSettings);
  except
    on E: Exception do
      frmImg.AddToLog('Ex in SendComponentDataToServer: ' + E.Message);
  end;
end;


procedure Do_Set_Pen_Callback(CmdParam: string);
var
  AStringList: TStringList;
  pen_color: TColor;
  pen_width: Byte;
begin
  AStringList := TStringList.Create;
  try                      
    AStringList.Text := CmdParam;
    pen_color := HexToInt(AStringList.Values['pen_color']);
    pen_width := HexToInt(AStringList.Values['pen_width']);
    
    FDynTFT_Set_Pen_Callback(pen_color, pen_width);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Set_Brush_Callback(CmdParam: string);
var
  AStringList: TStringList;
  brush_enabled: Byte;
  brush_color: TColor;
  gradient_enabled, gradient_orientation: Byte;
  gradient_color_from, gradient_color_to: TColor;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    brush_enabled := HexToInt(AStringList.Values['brush_enabled']);
    brush_color := HexToInt(AStringList.Values['brush_color']);
    gradient_enabled := HexToInt(AStringList.Values['gradient_enabled']);
    gradient_orientation := HexToInt(AStringList.Values['gradient_orientation']);
    gradient_color_from := HexToInt(AStringList.Values['gradient_color_from']);
    gradient_color_to := HexToInt(AStringList.Values['gradient_color_to']);

    FDynTFT_Set_Brush_Callback(brush_enabled, brush_color, gradient_enabled, gradient_orientation, gradient_color_from, gradient_color_to);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Set_Font_Callback(CmdParam: string);
var
  AStringList: TStringList;
  activeFont: PByte;
  font_color: TColor;
  font_orientation: Word;
  FontAddress: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;

    FontAddress := AStringList.Values['activeFont'];
    if FontAddress = 'TFT_defaultFont' then
      activeFont := @TFT_defaultFont
    else
      activeFont := PByte(HexToInt(FontAddress));

    font_color := HexToInt(AStringList.Values['font_color']);
    font_orientation := HexToInt(AStringList.Values['font_orientation']);

    FDynTFT_Set_Font_Callback(activeFont, font_color, font_orientation);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Write_Text_Callback(CmdParam: string);
var
  AStringList: TStringList;
  AText: string;
  x, y: Word;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    AText := AStringList.Values['AText'];
    x := HexToInt(AStringList.Values['x']);
    y := HexToInt(AStringList.Values['y']);

    FDynTFT_Write_Text_Callback(AText, x, y);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Line_Callback(CmdParam: string);
var
  AStringList: TStringList;
  x1, y1, x2, y2: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    x1 := HexToInt(AStringList.Values['x1']);
    y1 := HexToInt(AStringList.Values['y1']);
    x2 := HexToInt(AStringList.Values['x2']);
    y2 := HexToInt(AStringList.Values['y2']);

    FDynTFT_Line_Callback(x1, y1, x2, y2);
  finally
    AStringList.Free;
  end;
end;


procedure Do_H_Line_Callback(CmdParam: string);
var
  AStringList: TStringList;
  x_start, x_end, y_pos: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    x_start := HexToInt(AStringList.Values['x_start']);
    x_end := HexToInt(AStringList.Values['x_end']);
    y_pos := HexToInt(AStringList.Values['y_pos']);

    FDynTFT_H_Line_Callback(x_start, x_end, y_pos);
  finally
    AStringList.Free;
  end;
end;


procedure Do_V_Line_Callback(CmdParam: string);
var
  AStringList: TStringList;
  y_start, y_end, x_pos: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    y_start := HexToInt(AStringList.Values['y_start']);
    y_end := HexToInt(AStringList.Values['y_end']);
    x_pos := HexToInt(AStringList.Values['x_pos']);

    FDynTFT_V_Line_Callback(y_start, y_end, x_pos);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Dot_Callback(CmdParam: string);
var
  AStringList: TStringList;
  x, y: Integer;
  Color: TColor;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    x := HexToInt(AStringList.Values['x']);
    y := HexToInt(AStringList.Values['y']);
    Color := HexToInt(AStringList.Values['Color']);

    FDynTFT_Dot_Callback(x, y, Color);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Fill_Screen_Callback(CmdParam: string);   //a bit useless
var
  AStringList: TStringList;
  Color: TColor;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    Color := HexToInt(AStringList.Values['Color']);

    FDynTFT_Fill_Screen_Callback(Color);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Rectangle_Callback(CmdParam: string);
var
  AStringList: TStringList;
  x_upper_left, y_upper_left, x_bottom_right, y_bottom_right: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    x_upper_left := HexToInt(AStringList.Values['x_upper_left']);
    y_upper_left := HexToInt(AStringList.Values['y_upper_left']);
    x_bottom_right := HexToInt(AStringList.Values['x_bottom_right']);
    y_bottom_right := HexToInt(AStringList.Values['y_bottom_right']);

    FDynTFT_Rectangle_Callback(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right);
  finally
    AStringList.Free;
  end;
end;


procedure Do_Circle_Callback(CmdParam: string);
var
  AStringList: TStringList;
  x_center, y_center, radius: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    x_center := HexToInt(AStringList.Values['x_center']);
    y_center := HexToInt(AStringList.Values['y_center']);
    radius := HexToInt(AStringList.Values['radius']);

    FDynTFT_Circle_Callback(x_center, y_center, radius);
  finally
    AStringList.Free;
  end;
end;


procedure Do_GetTextWidthAndHeight_Callback(ASocket: TIdIOHandlerSocket; CmdParam: string);
var
  AStringList: TStringList;
  AText: string;
  Width, Height: Word;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Text := CmdParam;
    AText := AStringList.Values['AText'];              //frmImg.AddToLog('Do_GetTextWidthAndHeight_Callback: ' + AText);

    try
      FGetTextWidthAndHeight_Callback(AText, Width, Height);
    except
      on E: Exception do
      begin
        frmImg.AddToLog('Do_GetTextWidthAndHeight_Callback: ' + E.Message);
        frmImg.Show;
      end;
    end;

    ASocket.WriteLn('Width=' + IntToHex(Width, 4) + #4#5 + 'Height=' + IntToHex(Height, 4));
  finally
    AStringList.Free;
  end;
end;


procedure Do_DrawBitmap_Callback(ASocket: TIdIOHandlerSocket; CmdParam: string);
var
  AStringList: TStringList;
  APointerToBmpStreamMem: Pointer;
  AContentSize: Int64;
  x, y: Integer;
  MemStream: TMemoryStream;
begin
  AStringList := TStringList.Create;
  MemStream := TMemoryStream.Create;
  try
    AStringList.Text := CmdParam;
    AContentSize := HexToInt(AStringList.Values['AContentSize']);
    x := HexToInt(AStringList.Values['x']);
    y := HexToInt(AStringList.Values['y']);

    MemStream.SetSize(AContentSize);
    APointerToBmpStreamMem := MemStream.Memory;
    ASocket.ReadStream(MemStream, AContentSize);
    MemStream.Position := 0;
    FDynTFT_DrawBitmap_Callback(APointerToBmpStreamMem, AContentSize, x, y);
  finally
    AStringList.Free;
    MemStream.Free;
  end;
end;


procedure ReadDrawingCommands(ASocket: TIdIOHandlerSocket; ACmdParam: string);  //This is not thread safe, but it's the only way (currently) to call drawing primitives in time. If using UI synchronization, the drawing canvas at exe side, is not available anymore.
var
  DrawingCmd: string;
  ListOfDrawingCommands: TStringList;
  i: Integer;
begin
  ACmdParam := Replace45To1310(ACmdParam);    //#4#5 are the "exterior" separators

  ListOfDrawingCommands := TStringList.Create;
  try
    ListOfDrawingCommands.Text := ACmdParam;
    for i := 0 to ListOfDrawingCommands.Count - 1 do
    begin
      DrawingCmd := Replace56To1310(ListOfDrawingCommands.Strings[i]);          //#5#6 are the "interior" separators

      if DrawingCmd > '' then
        case DrawingCmd[1] of
          CDPDynTFT_Set_Pen: Do_Set_Pen_Callback(DrawingCmd);
          CDPDynTFT_Set_Brush: Do_Set_Brush_Callback(DrawingCmd);
          CDPDynTFT_Set_Font: Do_Set_Font_Callback(DrawingCmd);
          CDPDynTFT_Write_Text: Do_Write_Text_Callback(DrawingCmd);
          CDPDynTFT_Line: Do_Line_Callback(DrawingCmd);
          CDPDynTFT_H_Line: Do_H_Line_Callback(DrawingCmd);
          CDPDynTFT_V_Line: Do_V_Line_Callback(DrawingCmd);
          CDPDynTFT_Dot: Do_Dot_Callback(DrawingCmd);
          CDPDynTFT_Fill_Screen: Do_Fill_Screen_Callback(DrawingCmd);
          CDPDynTFT_Rectangle: Do_Rectangle_Callback(DrawingCmd);
          CDPDynTFT_Circle: Do_Circle_Callback(DrawingCmd);
          CDPDynTFT_GetTextWidthAndHeight: Do_GetTextWidthAndHeight_Callback(ASocket, DrawingCmd);
          CDPDynTFT_DrawBitmap: Do_DrawBitmap_Callback(ASocket, DrawingCmd);
        end;
    end;
  finally
    ListOfDrawingCommands.Free;
  end;
end;


procedure ReadNewPropertyValues(ASocket: TIdIOHandlerSocket; ACmdParam: string; var PropertiesOrEvents: TDynTFTDesignPropertyArr);
var
  PropNameValues: TStringList;
  i: Integer;
  //PropIdx: Integer;
  PropName, PropValue: string;
begin
  ACmdParam := Replace45To1310(ACmdParam);    //#4#5 are the "exterior" separators
  PropNameValues := TStringList.Create;
  try
    PropNameValues.Text := ACmdParam;

    for i := 0 to PropNameValues.Count - 1 do
    begin
      PropName := PropNameValues.Names[i];
      PropValue := PropNameValues.ValueFromIndex[i];
      
      UpdateComponentPropertyByName(PropertiesOrEvents, PropName, PropValue);

//      PropIdx := GetPropertyIndexInPropertiesOrEventsByName(PropertiesOrEvents, PropName);   //code from UpdateComponentPropertyByName
//      if (PropIdx > -1) and (PropIdx < Length(PropertiesOrEvents)) then
//      begin
//        PropertiesOrEvents[PropIdx].PropertyValue := PropValue;
//        frmImg.AddToLog('Updating property "' + PropName + '" to ' + PropValue);
//      end;
    end;  
  finally
    PropNameValues.Free;
  end;
end;


//procedure TSyncReadDrawingCommandsObj.DoSynchronize;
//begin
//  ReadDrawingCommands(FSocket, FCmdParam);
//end;
//
//
//procedure ReadDrawingCommandsFromThread(ASocket: TIdIOHandlerSocket; ACmdParam: string);
//var
//  SyncObj: TSyncReadDrawingCommandsObj;
//begin
//  SyncObj := TSyncReadDrawingCommandsObj.Create;
//  try
//    SyncObj.FSocket := ASocket;
//    SyncObj.FCmdParam := ACmdParam;
//    SyncObj.Synchronize;
//  finally
//    SyncObj.Free;
//  end;
//end;


type
  TServerHandlers = class
  private
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure FtmrCheckConnectionTimer(Sender: TObject);
  end;

var
  FServerHandlers: TServerHandlers;

  
procedure CreateCallbackTCPServer;
begin
  FServerHandlers := TServerHandlers.Create;
  FIdTCPServer := TIdTCPServer.Create(nil);
  FIdTCPServer.OnExecute := FServerHandlers.IdTCPServerExecute;
  FIdTCPServer.DefaultPort := FPluginServerPort;
  FIdTCPServer.ReuseSocket := rsTrue;
  FIdTCPServer.MaxConnections := 10;
  FIdTCPServer.Active := True;
  frmImg.AddToLog('Listening on port ' + IntToStr(FIdTCPServer.DefaultPort) + '.');
end;


procedure DestroyCallbackTCPServer;
begin
  FIdTCPServer.StopListening;
  FIdTCPServer.Active := False;
  FreeAndNil(FIdTCPServer);
  FreeAndNil(FServerHandlers);
end;

                                            
procedure CreateConnectionTimer;
begin
  FtmrCheckConnection := TTimer.Create(nil);
  FtmrCheckConnection.Interval := 2000;
  FtmrCheckConnection.OnTimer := FServerHandlers.FtmrCheckConnectionTimer;
  FtmrCheckConnection.Enabled := True;
end;


procedure DestroyConnectionTimer;
begin
  FtmrCheckConnection.Free;
end;


procedure DisplayConnectionStatus(AIsConnected: Boolean);
const
  CDisplayStatus: array[Boolean] of string = (' Status: Not connected to RS server', ' Status: Connected to RS server');
begin
  frmImg.pnlStatus.Caption := CDisplayStatus[AIsConnected];

  if AIsConnected then
    frmImg.pnlStatus.Font.Color := clGreen
  else
    frmImg.pnlStatus.Font.Color := clMaroon;
end;


const
  CDrawingCmdTimeout = 2000;
  CMaxLineLen = 131072;         //Please increase this value, if a project with multiple components displays "Max line length exceeded."



procedure TServerHandlers.IdTCPServerExecute(AContext: TIdContext);
var
  Cmd, CmdParam: string;
begin
  try
    Cmd := AContext.Connection.Socket.ReadLn(#13#10, CDrawingCmdTimeout, CMaxLineLen);
    CmdParam := Copy(Cmd, Pos('=', Cmd) + 1, MaxInt);
    Cmd := Copy(Cmd, 1, Pos('=', Cmd) - 1);

    if Cmd = '' then
    begin
      //frmImg.AddToLog('Received empty drawing command.  Cmd = ' + Cmd);
      Exit;
    end;

    if Cmd = CCGRM_CallbackDraw then  //used for drawing bitmaps
    begin
      //SyncThreadToUI; //not as effective as DynTFT_DebugConsole, which writes to a TMemo
      //DynTFT_DebugConsole('CCGRM_CallbackDraw...'); //required, to avoid "Out of system resources." error message.   - replaced by below call to frmImg.AddToLog
      frmImg.AddToLog('CCGRM_CallbackDraw...');
      //frmImg.AddToLog('CCGRM_CallbackDraw... CmdParam = ' + CmdParam);
      ReadDrawingCommands{FromThread}(AContext.Connection.Socket, CmdParam);    ///////////// if synchronizing using "FromThread", it seems the race condition is less likely to reproduce, but the CDPDynTFT_GetTextWidthAndHeight callback is not called when needed, because the UI is not available then. The UI becomes available after the call to the component's drawing procedure, which is too late.

      Exit;
    end;

    if Cmd = CCGRM_PluginStartup then
    begin
      AContext.Connection.Socket.WriteLn('Plugin is ready.');
      frmImg.AddToLog('RS server (back connection) available.');   //It's safe to call DynTFT_DebugConsole from thread, in Delphi only, and only if the log is a system component, which uses SendMessage to get/set content.
      Exit;
    end;

    Sleep(2);
  except
    on E: EIdException do
      raise;

    on E: Exception do
      frmImg.AddToLog('Ex in callback server: ' + E.Message);
  end;
end;


procedure TServerHandlers.FtmrCheckConnectionTimer(Sender: TObject);
begin
  try
    SendPingCommandToServer;
    DisplayConnectionStatus(True);
  except
    on E: Exception do
    begin
      try
        FIdTCPClient.ConnectTimeout := CPluginClientReConnectTimeout;
        try
          FIdTCPClient.Connect(FRemoteSystemServerAddress, FRemoteSystemServerPort);
        finally
          FIdTCPClient.ConnectTimeout := CPluginClientConnectTimeout;
        end;
        
        frmImg.AddToLog('Connected to server for DynTFTCGSystem components.');
        SendPluginStartupCommandToServer;
        SendPluginPortCommandToServer;
        SendPingCommandToServer;
        DisplayConnectionStatus(True);
      except
        DisplayConnectionStatus(False);    //False means not connected
      end;
    end;
  end;
end;

//////////////////////////


procedure DrawDynTFTComponentProc(APanel: TUIPanelBase; CompType: string; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  try
    SendCommandToServer(CompType, EncodePanelBasePropertiesToString(APanel));
    SendComponentDataToServer(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
  except
    on E: Exception do
    begin
      frmImg.AddToLog('Ex on sending cmd to server.  ' + CompType + ' callback ex: ' + E.Message);    // Please increase the value of CMaxLineLen, if a project with multiple components displays "Max line length exceeded."
      raise
    end;
  end;

  if FIdTCPClient.Socket = nil then
    raise Exception.Create('Not connected to server.');

  try
    ReadDrawingCommands(FIdTCPClient.Socket, FIdTCPClient.Socket.ReadLn(#13#10, CDrawingCmdTimeout, CMaxLineLen));

    ReadNewPropertyValues(FIdTCPClient.Socket, FIdTCPClient.Socket.ReadLn(#13#10, CDrawingCmdTimeout, CMaxLineLen), PropertiesOrEvents);
  except
    on E: Exception do
      frmImg.AddToLog(CompType + ' callback ex: ' + E.Message);    // Please increase the value of CMaxLineLen, if a project with multiple components displays "Max line length exceeded."
  end;
end;


procedure TDrawDynTFTComponentProc_Button(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_Button, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;



procedure TDrawDynTFTComponentProc_ArrowButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_ArrowButton, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_Panel(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_Panel, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_CheckBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_CheckBox, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_ScrollBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_ScrollBar, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_Items(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_Items, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_ListBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_ListBox, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_Label(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_Label, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_RadioButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_RadioButton, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_RadioGroup(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_RadioGroup, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_TabButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_TabButton, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_PageControl(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_PageControl, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_Edit(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_Edit, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_KeyButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_KeyButton, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_VirtualKeyboard(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_VirtualKeyboard, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_ComboBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_ComboBox, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_TrackBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_TrackBar, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_ProgressBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_ProgressBar, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_MessageBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_MessageBox, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure TDrawDynTFTComponentProc_VirtualTable(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
begin
  DrawDynTFTComponentProc(APanel, CCGRM_DrawPDynTFTComponentOnPanel_VirtualTable, PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings);
end;


procedure RegisterAllComponentsEvents; stdcall;
begin
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Button);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_ArrowButton);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Panel);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_CheckBox);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_ScrollBar);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Items);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_ListBox);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Label);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_RadioButton);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_RadioGroup);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_TabButton);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_PageControl);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Edit);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_KeyButton);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_VirtualKeyboard);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_ComboBox);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_TrackBar);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_ProgressBar);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_MessageBox);
  RegisterCompDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_VirtualTable);
end;


procedure DrawPDynTFTComponentOnPanel(var APanelBase: TUIPanelBase; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef; ASetPropertiesCallback: TSetPropertiesCallback); stdcall;
begin
  {$IFNDEF UserTFTCommands}
    raise Exception.Create('UserTFTCommands compiler directive is not defined. Please use DynTFTCodeGen callbacks.');   //UserTFTCommands should be defined at project level. Please rebuild the project after that.
  {$ENDIF}

  try
    DrawPDynTFTComponentOnPanelBase(APanelBase, FDrawingProcedures, APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings, ASetPropertiesCallback);
  except
    on E: Exception do
    begin
      FDynTFT_Set_Pen_Callback(clMaroon, 1);
      FDynTFT_Set_Brush_Callback(1, clCream, 0, 0, 0, 0);
      FDynTFT_Rectangle_Callback(0, 0, APanelBase.Width - 1, APanelBase.Height - 1);
      FDynTFT_Write_Text_Callback('Ex on drawing comp type: ' + IntToStr(APanelBase.DynTFTComponentType), 5, 2);
      FDynTFT_Write_Text_Callback(APanelBase.Caption, 5, 20);
      FDynTFT_Write_Text_Callback(E.Message, 5, 42);
    end;
  end;
end;


procedure RegisterDynTFTDrawingProcedures(
  ADynTFT_Set_Pen_Callback: TDynTFT_Set_Pen_Callback;
  ADynTFT_Set_Brush_Callback: TDynTFT_Set_Brush_Callback;
  ADynTFT_Set_Font_Callback: TDynTFT_Set_Font_Callback;
  ADynTFT_Write_Text_Callback: TDynTFT_Write_Text_Callback;
  ADynTFT_Line_Callback: TDynTFT_Line_Callback;
  ADynTFT_H_Line_Callback: TDynTFT_H_Line_Callback;
  ADynTFT_V_Line_Callback: TDynTFT_V_Line_Callback;
  ADynTFT_Dot_Callback: TDynTFT_Dot_Callback;
  ADynTFT_Fill_Screen_Callback: TDynTFT_Fill_Screen_Callback;
  ADynTFT_Rectangle_Callback: TDynTFT_Rectangle_Callback;
  ADynTFT_Circle_Callback: TDynTFT_Circle_Callback;
  AGetTextWidthAndHeight_Callback: TDynTFT_GetTextWidthAndHeight_Callback;
  ADynTFT_DrawBitmap_Callback: TDynTFT_DrawBitmap_Callback ); stdcall;
begin
  FDynTFT_Set_Pen_Callback := ADynTFT_Set_Pen_Callback;
  FDynTFT_Set_Brush_Callback := ADynTFT_Set_Brush_Callback;
  FDynTFT_Set_Font_Callback := ADynTFT_Set_Font_Callback;
  FDynTFT_Write_Text_Callback := ADynTFT_Write_Text_Callback;
  FDynTFT_Line_Callback := ADynTFT_Line_Callback;
  FDynTFT_H_Line_Callback := ADynTFT_H_Line_Callback;
  FDynTFT_V_Line_Callback := ADynTFT_V_Line_Callback;
  FDynTFT_Dot_Callback := ADynTFT_Dot_Callback;
  FDynTFT_Fill_Screen_Callback := ADynTFT_Fill_Screen_Callback;
  FDynTFT_Rectangle_Callback := ADynTFT_Rectangle_Callback;
  FDynTFT_Circle_Callback := ADynTFT_Circle_Callback;
  FGetTextWidthAndHeight_Callback := AGetTextWidthAndHeight_Callback;
  FDynTFT_DrawBitmap_Callback := ADynTFT_DrawBitmap_Callback;

  {$IFNDEF UserTFTCommands}
    raise Exception.Create('UserTFTCommands compiler directive is not defined. This will cause access violations when drawing.' + #13#10 +
                           ' It is required to set the drawing library to DynTFTCodeGen callbacks.');
  {$ENDIF}
end;


initialization
  SetLength(FDrawingProcedures, 0);
  //GCanvas := nil;
  
end.
