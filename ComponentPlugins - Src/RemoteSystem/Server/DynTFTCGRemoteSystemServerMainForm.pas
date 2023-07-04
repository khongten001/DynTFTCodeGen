{   DynTFT  - graphic components for microcontrollers
    Copyright (C) 2017, 2023 VCC
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

unit DynTFTCGRemoteSystemServerMainForm;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Types,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IdTCPServer, IdSync, IdCustomTCPServer, IdContext, IdTCPClient,
  IdBaseComponent, IdComponent, ComCtrls, PollingFIFO;

type

  { TfrmDynTFTCGRemoteSystemServerMain }

  TfrmDynTFTCGRemoteSystemServerMain = class(TForm)
    IdTCPServer1: TIdTCPServer;
    memLog: TMemo;
    tmrLogging: TTimer;
    tmrPrepareClient: TTimer;
    tmrCloseClient: TTimer;
    tmrStartup: TTimer;
    lblAllocatedMemory: TLabel;
    prbAllocatedMemory: TProgressBar;
    tmrStats: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure tmrCloseClientTimer(Sender: TObject);
    procedure tmrLoggingTimer(Sender: TObject);
    procedure tmrPrepareClientTimer(Sender: TObject);
    procedure tmrStartupTimer(Sender: TObject);
    procedure tmrStatsTimer(Sender: TObject);
  private
    FLoggingFIFO: TPollingFIFO;

    procedure AddToLog(s: string);
    procedure AddToLogFromThread(s: string);
    procedure PrepareClient;
    procedure CloseClient;
  public

  end;

  TPrepareClientSyncObj = class(TIdSync)
  protected
    procedure DoSynchronize; override;
  end;

  TCloseClientSyncObj = class(TIdSync)
  protected
    procedure DoSynchronize; override;
  end;


var
  frmDynTFTCGRemoteSystemServerMain: TfrmDynTFTCGRemoteSystemServerMain;

implementation

{$IFDEF FPC}
  {$R *.frm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}


uses
  RemoteSystemCommands, IdIOHandlerSocket, ComponentIcons, DynTFTPluginUtils, PanelDrawing,
  DynTFTCodeGenSharedDataTypes, TFT, DynTFTSharedUtils, DynTFTColorThemeGenLiveColors,
  MemManager, DynTFTBaseDrawing;


const
  CMaxLineLen = 131072;


var
  DrawingCommands: array of string;
  FDrawingProcedures: TDrawDynTFTComponentProcArr;
  FIdTCPClient: TIdTCPClient;
  FWHLabel: TLabel;
  FPluginAddress: string;
  FPluginPort: Integer;


procedure TPrepareClientSyncObj.DoSynchronize;
begin
  frmDynTFTCGRemoteSystemServerMain.tmrPrepareClient.Enabled := True;
end;


procedure TCloseClientSyncObj.DoSynchronize;
begin
  frmDynTFTCGRemoteSystemServerMain.tmrCloseClient.Enabled := True;
end;


{ TfrmDynTFTCGRemoteSystemServerMain }

procedure TfrmDynTFTCGRemoteSystemServerMain.FormCreate(Sender: TObject);
begin
  FPluginPort := 0;
  FLoggingFIFO := TPollingFIFO.Create;

  tmrStartup.Enabled := True;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.FormDestroy(Sender: TObject);
begin
  FLoggingFIFO.Free;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  try
    try
      FIdTCPClient.Disconnect;
    finally
      FreeAndNil(FIdTCPClient);
    end;
  except
  end;
end;



procedure DrawServerBugOnBitmap(ATempBitmap: TBitmap);
begin
  ATempBitmap.Width := 24;
  ATempBitmap.Height := 24;
  ATempBitmap.PixelFormat := pf24bit;
  ATempBitmap.Canvas.Font.Color := clBlue;
  ATempBitmap.Canvas.Font.Name := 'Tahoma';
  ATempBitmap.Canvas.Font.Size := 8;
  ATempBitmap.Canvas.TextOut(4, 0, 'Srv');
  ATempBitmap.Canvas.TextOut(4, 11, 'Bug');
end;


function SetStreamSizeCallback(ANewSize: Int64; AStreamID: Int64): Pointer;
var
  TempMemStream: TMemoryStream;
begin
  TempMemStream := TMemoryStream(Pointer(AStreamID));
  TempMemStream.SetSize(ANewSize);
  Result := TempMemStream.Memory;
end;


procedure Handle_GetComponentIconFromServer(ASocket: TIdIOHandlerSocket; ACmdParam: string);
var
  ComponentIndex: Integer;
  Stream: TMemoryStream;
  StreamID: Int64;
  Bmp: TBitmap;
begin
  ComponentIndex := StrToInt64Def(ACmdParam, -1);
  Stream := TMemoryStream.Create;
  try
    StreamID := Int64(Pointer(Stream));
    try
      GetComponentIconFromImageList(ComponentIndex, dmIcons.imglstComponents, @SetStreamSizeCallback, StreamID);
      Stream.Position := 0;
      ASocket.Write(Stream.Size);
      ASocket.Write(Stream);
    except
      on E: Exception do
      begin
        frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Handle_GetComponentIconFromServer: ' + E.Message);
        Bmp := TBitmap.Create;
        try
          DrawServerBugOnBitmap(Bmp);
          Bmp.SaveToStream(Stream);
          Stream.Position := 0;
          ASocket.Write(Stream.Size);
          ASocket.Write(Stream);
        finally
          Bmp.Free;
        end;
      end;
    end;
  finally
    Stream.Free;
  end;
end;


procedure PropertiesOrEventsStrToArr(PropertiesOrEventsStr: string; var ATempPropertiesOrEvents: TDynTFTDesignPropertyArr);
var
  ListOfValues: TStringList;
  SepLen: Integer;
  Str: string;
  i: Integer;
  PropertyName: string;
  PropertyValue: string;
begin
  SepLen := Length(CRecFieldSeparator);
  ListOfValues := TStringList.Create;
  try
    ListOfValues.Text := Replace45To1310(PropertiesOrEventsStr);
    SetLength(ATempPropertiesOrEvents, ListOfValues.Count);
    SepLen := Length(CRecFieldSeparator);

    for i := 0 to ListOfValues.Count - 1 do
    begin
      Str := ListOfValues.Strings[i];
      PropertyName := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      PropertyValue := Str;

      ATempPropertiesOrEvents[i].PropertyName := PropertyName;
      ATempPropertiesOrEvents[i].PropertyValue := Replace78To1310(PropertyValue); //for items, radio buttons and tab buttons
    end;
  finally
    ListOfValues.Free;
  end;
end;


procedure SchemaConstantsStrToArr(SchemaConstantsStr: string; var ATempSchemaConstants: TComponentConstantArr);
var
  ListOfValues: TStringList;
  SepLen: Integer;
  i: Integer;
  Str: string;
  ConstantName: string;
  ConstantDataType: string;
  ConstantValueInt: Int64;
begin
  SepLen := Length(CRecFieldSeparator);
  ListOfValues := TStringList.Create;
  try
    ListOfValues.Text := Replace45To1310(SchemaConstantsStr);
    SetLength(ATempSchemaConstants, ListOfValues.Count);

    for i := 0 to ListOfValues.Count - 1 do
    begin
      Str := ListOfValues.Strings[i];
      ConstantName := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      ConstantDataType := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      ConstantValueInt := StrToInt64Def(Str, 0);

      ATempSchemaConstants[i].ConstantName := ConstantName;
      ATempSchemaConstants[i].ConstantDataType := ConstantDataType;
      ATempSchemaConstants[i].ConstantValueInt := ConstantValueInt;
    end;
  finally
    ListOfValues.Free;
  end;
end;


procedure ColorConstantsStrToArr(ColorConstantsStr: string; var ATempColorConstants: TColorConstArr);
var
  ListOfValues: TStringList;
  SepLen: Integer;
  i: Integer;
  Str: string;
  ColorName: string;
  ColorValue: TColor;
  SchemaValue: string;
  InitialValue: TColor;
begin
  SepLen := Length(CRecFieldSeparator);
  ListOfValues := TStringList.Create;
  try
    ListOfValues.Text := Replace45To1310(ColorConstantsStr);
    SetLength(ATempColorConstants, ListOfValues.Count);

    for i := 0 to ListOfValues.Count - 1 do
    begin
      Str := ListOfValues.Strings[i];
      ColorName := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      ColorValue := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), clLime);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      SchemaValue := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      InitialValue := StrToIntDef(Str, 0);

      ATempColorConstants[i].Name := ColorName;
      ATempColorConstants[i].Value := ColorValue;
      ATempColorConstants[i].SchemaValue := SchemaValue;
      ATempColorConstants[i].InitialValue := InitialValue;
    end;
  finally
    ListOfValues.Free;
  end;
end;


procedure FontSettingsStrToArr(FontSettingsStr: string; var ATempFontSettings: TFontSettingsArr);
var
  ListOfValues: TStringList;
  SepLen: Integer;
  i: Integer;
  Str: string;
  FontPropertyValue: string;
  FontAddress: string; //a pointer, valid in plugin's address space, not here on server
  FontName: string;
  //IdentifierName: string;
  FontSize: Integer;
  Bold: Boolean;
  Italic: Boolean;
  Underline: Boolean;
  StrikeOut: Boolean;
  Charset: Integer;
  Pitch: Byte;
begin
  SepLen := Length(CRecFieldSeparator);
  ListOfValues := TStringList.Create;
  try
    ListOfValues.Text := Replace45To1310(FontSettingsStr);
    SetLength(ATempFontSettings, ListOfValues.Count);

    for i := 0 to ListOfValues.Count - 1 do
    begin
      Str := ListOfValues.Strings[i];
      FontPropertyValue := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      FontAddress := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('FontAddress = ' + FontAddress);

      FontName := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);
      if FontName = '' then
        FontName := 'Tahoma';  //better have some useful default value than none

      //IdentifierName := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      FontSize := HexToInt(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1));
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Bold := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Italic := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Underline := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      StrikeOut := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Charset := HexToInt(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1));  //1 = DEFAULT_CHARSET
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Pitch := HexToInt(Str); //can be 0, 1, 2

      New(ATempFontSettings[i].SFont);
      ATempFontSettings[i].FontPropertyValue := FontPropertyValue;
      ATempFontSettings[i].SFont^.FontName := FontName;
      ATempFontSettings[i].SFont^.IdentifierName := FontAddress; //IdentifierName; //IdentifierName is useless here, so reuse the field for FontAddress, as it is needed later
      ATempFontSettings[i].SFont^.FontSize := FontSize;
      ATempFontSettings[i].SFont^.Bold := Bold;
      ATempFontSettings[i].SFont^.Italic := Italic;
      ATempFontSettings[i].SFont^.Underline := Underline;
      ATempFontSettings[i].SFont^.StrikeOut := StrikeOut;
      ATempFontSettings[i].SFont^.Charset := Charset;
      ATempFontSettings[i].SFont^.Pitch := TFontPitch(Pitch);
    end;
  finally
    ListOfValues.Free;
  end;
end;


procedure PanelBaseSettingsToPanelBase(PanelBaseStr: string; var ATempPanel: TUIPanelBase);
var
  Str: string;
  SepLen: Integer;
begin
  Str := PanelBaseStr;
  SepLen := Length(CRecFieldSeparator);

  ATempPanel.Width := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 100);
  Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

  ATempPanel.Height := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 100);
  Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

  ATempPanel.DynTFTComponentType := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 0);
  Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

  ATempPanel.Caption := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1);
  Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

  ATempPanel.Tag := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 0);
end;


procedure Handle_DrawPDynTFTComponentOnPanel(ASocket: TIdIOHandlerSocket; ACompName, ACmdParam: string);
var
  PropertiesOrEvents, SchemaConstants, ColorConstants, FontSettings: string;
  TempPropertiesOrEvents, BkpPropertiesOrEvents: TDynTFTDesignPropertyArr;
  InitialPropertiesOrEvents: TDynTFTDesignPropertyArr;
  TempSchemaConstants: TComponentConstantArr;
  TempColorConstants: TColorConstArr;
  TempFontSettings: TFontSettingsArr;
  i: Integer;
  TempPanelBase: TUIPanelBase;
  DrawingResponse: string;
  s: string;
  IndexInTemp: Integer;
begin
  //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('DrawPDynTFTComponentOnPanel: ' + ACompName);

  try
    try
      PropertiesOrEvents := ASocket.ReadLn(#13#10, 400, CMaxLineLen);
      SchemaConstants := ASocket.ReadLn(#13#10, 400, CMaxLineLen);
      ColorConstants := ASocket.ReadLn(#13#10, 400, CMaxLineLen);
      FontSettings := ASocket.ReadLn(#13#10, 400, CMaxLineLen);
    except
      on E: Exception do
        frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Reading component info from plugin ex: ' + E.Message);
    end;

    try
      PropertiesOrEventsStrToArr(PropertiesOrEvents, TempPropertiesOrEvents);
      SchemaConstantsStrToArr(SchemaConstants, TempSchemaConstants);
      ColorConstantsStrToArr(ColorConstants, TempColorConstants);
      FontSettingsStrToArr(FontSettings, TempFontSettings);
      PanelBaseSettingsToPanelBase(ACmdParam, TempPanelBase);

      SetLength(InitialPropertiesOrEvents, Length(TempPropertiesOrEvents));
      try
        for i := 0 to Length(TempPropertiesOrEvents) - 1 do
          InitialPropertiesOrEvents[i] := TempPropertiesOrEvents[i];

        SetLength(DrawingCommands, 0);
        try
          FDrawingProcedures[TempPanelBase.DynTFTComponentType](TempPanelBase, TempPropertiesOrEvents, TempSchemaConstants, TempColorConstants, TempFontSettings);

          DrawingResponse := '';
          for i := 0 to Length(DrawingCommands) - 1 do
            DrawingResponse := DrawingResponse + DrawingCommands[i] + CRecFieldArrayItemSeparator;

          ASocket.WriteLn(DrawingResponse);
        finally
          SetLength(DrawingCommands, 0);
        end;


        s := '';
        for i := 0 to Length(InitialPropertiesOrEvents) - 1 do
          if InitialPropertiesOrEvents[i].PropertyName = TempPropertiesOrEvents[i].PropertyName then
          begin
            if InitialPropertiesOrEvents[i].PropertyValue <> TempPropertiesOrEvents[i].PropertyValue then
            begin
              s := s + InitialPropertiesOrEvents[i].PropertyName + '=' + TempPropertiesOrEvents[i].PropertyValue + #4#5;
              //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('.. setting (=)   ' + InitialPropertiesOrEvents[i].PropertyName + '=' + TempPropertiesOrEvents[i].PropertyValue);
            end;
          end
          else
          begin  //handle the case of inserting / adding or removing properties
            IndexInTemp := GetPropertyIndexInPropertiesOrEventsByName(TempPropertiesOrEvents, InitialPropertiesOrEvents[i].PropertyName);
            if (IndexInTemp > -1) and (InitialPropertiesOrEvents[IndexInTemp].PropertyName = TempPropertiesOrEvents[i].PropertyValue) then ;
              if InitialPropertiesOrEvents[i].PropertyValue <> TempPropertiesOrEvents[i].PropertyValue then
              begin
                s := s + InitialPropertiesOrEvents[i].PropertyName + '=' + TempPropertiesOrEvents[i].PropertyValue + #4#5;
                //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('.. setting (<>)    ' + InitialPropertiesOrEvents[i].PropertyName + '=' + TempPropertiesOrEvents[i].PropertyValue);
              end;
          end;
      finally
        SetLength(InitialPropertiesOrEvents, 0);
      end;

      ASocket.WriteLn(s); //this is the list of modified properties
    finally
      SetLength(TempPropertiesOrEvents, 0);
      SetLength(TempSchemaConstants, 0);
      SetLength(TempColorConstants, 0);

      for i := 0 to Length(TempFontSettings) - 1 do
        Dispose(TempFontSettings[i].SFont);
      SetLength(TempFontSettings, 0);
    end;
  except
    on E: Exception do
      frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Ex in DrawPDynTFTComponentOnPanel: "' + E.Message + '".  Component type: ' + IntToStr(TempPanelBase.DynTFTComponentType));
  end;
end;


procedure AddDrawingCommand(ACmd: string);
begin
  SetLength(DrawingCommands, Length(DrawingCommands) + 1);
  DrawingCommands[Length(DrawingCommands) - 1] := ACmd;
end;


procedure Set_Pen_Callback(pen_color: TColor; pen_width: Byte);
var
  Res: string;
begin
  Res := CDPDynTFT_Set_Pen + CDrawingCmdFieldSeparator +
         'pen_color=' + IntToHex(pen_color, 6) + CDrawingCmdFieldSeparator +
         'pen_width=' + IntToHex(pen_width, 2);

  AddDrawingCommand(Res);
end;


procedure Set_Brush_Callback(brush_enabled: Byte; brush_color: TColor; gradient_enabled, gradient_orientation: Byte; gradient_color_from, gradient_color_to: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Set_Brush + CDrawingCmdFieldSeparator +
         'brush_enabled=' + IntToHex(brush_enabled, 1) + CDrawingCmdFieldSeparator +
         'brush_color=' + IntToHex(brush_color, 6) + CDrawingCmdFieldSeparator +
         'gradient_enabled=' + IntToHex(gradient_enabled, 1) + CDrawingCmdFieldSeparator +
         'gradient_orientation=' + IntToHex(gradient_orientation, 1) + CDrawingCmdFieldSeparator +
         'gradient_color_from=' + IntToHex(gradient_color_from, 6) + CDrawingCmdFieldSeparator +
         'gradient_color_to=' + IntToHex(gradient_color_to, 6);

  AddDrawingCommand(Res);
end;


procedure Set_Font_Callback(activeFont: PByte; font_color: TColor; font_orientation: Word);
var
  Res: string;
  FontAddress: string;
  TempWorkCanvas: TCanvas;
begin
  TempWorkCanvas := FWHLabel.Canvas;

  try
    //IdentifierName is used to store the pointer, since the actual identifier name is not needed here on server.
    FontAddress := PDynTFTFontSettings(activeFont)^.IdentifierName;

    Res := CDPDynTFT_Set_Font + CDrawingCmdFieldSeparator +
           'activeFont=' + FontAddress + CDrawingCmdFieldSeparator +
           'font_color=' + IntToHex(font_color, 6) + CDrawingCmdFieldSeparator +
           'font_orientation=' + IntToHex(font_orientation, 1);

    try
      TempWorkCanvas.Lock;
      try
        TempWorkCanvas.Font.Name := PDynTFTFontSettings(activeFont)^.FontName;
        TempWorkCanvas.Font.Size := PDynTFTFontSettings(activeFont)^.FontSize;
        TempWorkCanvas.Font.Style := [];

        if PDynTFTFontSettings(activeFont)^.Bold then
          TempWorkCanvas.Font.Style := TempWorkCanvas.Font.Style + [fsBold];

        if PDynTFTFontSettings(activeFont)^.Italic then
          TempWorkCanvas.Font.Style := TempWorkCanvas.Font.Style + [fsItalic];

        if PDynTFTFontSettings(activeFont)^.Underline then
          TempWorkCanvas.Font.Style := TempWorkCanvas.Font.Style + [fsUnderline];

        if PDynTFTFontSettings(activeFont)^.StrikeOut then
          TempWorkCanvas.Font.Style := TempWorkCanvas.Font.Style + [fsStrikeOut];

        TempWorkCanvas.Font.CharSet := PDynTFTFontSettings(activeFont)^.Charset;
        TempWorkCanvas.Font.Pitch := PDynTFTFontSettings(activeFont)^.Pitch;
      finally
        TempWorkCanvas.UnLock;
      end;
    except
      on E: Exception do
        frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Ex on setting local font: ' + E.Message);
    end;

  except
    on E: Exception do
    begin
      frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Can''t properly process fonts: ' + E.Message);
      Res := CDPDynTFT_Set_Font + CDrawingCmdFieldSeparator;
    end;
  end;

  AddDrawingCommand(Res);
end;


procedure Write_Text_Callback(AText: string; x, y: Word);
var
  Res: string;
begin
  Res := CDPDynTFT_Write_Text + CDrawingCmdFieldSeparator +
         'AText=' + AText + CDrawingCmdFieldSeparator +
         'x=' + IntToHex(x, 1) + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y, 1);

  AddDrawingCommand(Res);
end;


procedure Line_Callback(x1, y1, x2, y2: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Line + CDrawingCmdFieldSeparator +
         'x1=' + IntToHex(x1, 1) + CDrawingCmdFieldSeparator +
         'y1=' + IntToHex(y1, 1) + CDrawingCmdFieldSeparator +
         'x2=' + IntToHex(x2, 1) + CDrawingCmdFieldSeparator +
         'y2=' + IntToHex(y2, 1);

  AddDrawingCommand(Res);
end;


procedure H_Line_Callback(x_start, x_end, y_pos: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_H_Line + CDrawingCmdFieldSeparator +
         'x_start=' + IntToHex(x_start, 1) + CDrawingCmdFieldSeparator +
         'x_end=' + IntToHex(x_end, 1) + CDrawingCmdFieldSeparator +
         'y_pos=' + IntToHex(y_pos, 1);

  AddDrawingCommand(Res);
end;


procedure V_Line_Callback(y_start, y_end, x_pos: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_V_Line + CDrawingCmdFieldSeparator +
         'y_start=' + IntToHex(y_start, 1) + CDrawingCmdFieldSeparator +
         'y_end=' + IntToHex(y_end, 1) + CDrawingCmdFieldSeparator +
         'x_pos=' + IntToHex(x_pos, 1);

  AddDrawingCommand(Res);
end;


procedure Dot_Callback(x, y: Integer; Color: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Dot + CDrawingCmdFieldSeparator +
         'x=' + IntToHex(x, 1) + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y, 1) + CDrawingCmdFieldSeparator +
         'Color=' + IntToHex(Color, 6);

  AddDrawingCommand(Res);
end;


procedure Fill_Screen_Callback(color: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Fill_Screen + CDrawingCmdFieldSeparator +
         'Color=' + IntToHex(color, 6);

  AddDrawingCommand(Res);
end;


procedure Rectangle_Callback(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Rectangle + CDrawingCmdFieldSeparator +
         'x_upper_left=' + IntToHex(x_upper_left, 1) + CDrawingCmdFieldSeparator +
         'y_upper_left=' + IntToHex(y_upper_left, 1) + CDrawingCmdFieldSeparator +
         'x_bottom_right=' + IntToHex(x_bottom_right, 1) + CDrawingCmdFieldSeparator +
         'y_bottom_right=' + IntToHex(y_bottom_right, 1);

  AddDrawingCommand(Res);
end;


procedure Circle_Callback(x_center, y_center, radius: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Circle + CDrawingCmdFieldSeparator +
         'x_center=' + IntToHex(x_center, 1) + CDrawingCmdFieldSeparator +
         'y_center=' + IntToHex(y_center, 1) + CDrawingCmdFieldSeparator +
         'radius=' + IntToHex(radius, 1);

  AddDrawingCommand(Res);
end;


procedure GetTextWidthAndHeight_Callback(AText: string; var Width, Height: Word);
var
  TempTextSize: TSize;
//  AStringList: TStringList;
//  WidthHeight: string;
  TempWorkCanvas: TCanvas;
begin
  TempWorkCanvas := FWHLabel.Canvas;
  TempWorkCanvas.Lock;
  try
    TempTextSize := TempWorkCanvas.TextExtent(AText);
    Width := TempTextSize.cx;
    Height := TempTextSize.cy;
  finally
    TempWorkCanvas.UnLock;
  end;

  if AText > '' then
    if Width = 0 then
      frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Text "' + AText + '" has width = 0.  FontSize = ' + IntToStr(TempWorkCanvas.Font.Size) + '  FontName = "' + TempWorkCanvas.Font.Name + '".');

  //The following approach uses DynTFTCodeGen's canvas, which should be more accurate than the local canvas (TempWorkCanvas).
  //If this server runs on the same OS as DynTFTCodeGen, then no visible difference should exist between the two renderings.
  //However, because of so many calls (and calling callbacks) through TCP, there are race conditions which cause deadlocks.
  //Noticeable differences will appear, for example, when this server is run on Linux and DynTFTCodeGen uses fonts which are not available in Linux (e.g. Tahoma).
  //Anyway, the above approach, using a local canvas is way more faster and reliable. Still, not perfect.

  {
  //pair readln with SendPlainStringToServer('Width=' + IntToHex(Width, 4) + #4#5 + 'Height=' + IntToHex(Height, 4));
  FIdTCPClient.Socket.WriteLn(CCGRM_CallbackDraw + '=' + CDPDynTFT_GetTextWidthAndHeight + CDrawingCmdFieldSeparator + 'AText=' + AText);
  WidthHeight := FIdTCPClient.Socket.ReadLn(#13#10, 200, CMaxLineLen);

  //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Read WidthHeight: ' + WidthHeight);

  AStringList := TStringList.Create;
  try
    AStringList.Text := Replace45To1310(WidthHeight);
    Width := HexToInt(AStringList.Values['Width']);
    Height := HexToInt(AStringList.Values['Height']);
  finally
    AStringList.Free;
  end;
  }
  //AddDrawingCommand(Res);   //commented because this procedure already calls Socket.WriteLn / Socket.ReadLn
end;


procedure DrawBitmap_Callback(APointerToBmpStreamMem: Pointer; AContentSize: Int64; x, y: Integer);
var
  Res: string;
  MemStream: TMemoryStream;
begin
  Res := CDPDynTFT_DrawBitmap + CDrawingCmdFieldSeparator +
         'AContentSize=' + IntToHex(AContentSize, 1) + CDrawingCmdFieldSeparator +
         'x=' + IntToHex(x, 1) + CDrawingCmdFieldSeparator + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y, 1);

  MemStream := TMemoryStream.Create;
  try
    MemStream.SetSize(AContentSize);
    Move(APointerToBmpStreamMem^, MemStream.Memory^, AContentSize);
    MemStream.Position := 0;
    FIdTCPClient.Socket.WriteLn(CCGRM_CallbackDraw + '=' + Res);
    FIdTCPClient.Socket.Write(MemStream, AContentSize);
  finally
    MemStream.Free;
  end;

  //AddDrawingCommand(Res);  //commented because this procedure already calls Socket.WriteLn
end;


procedure Handle_RegisterDynTFTDrawingProcedures;
begin
  RegisterDynTFTDrawingProcedures(@Set_Pen_Callback,
                                  @Set_Brush_Callback,
                                  @Set_Font_Callback,
                                  @Write_Text_Callback,
                                  @Line_Callback,
                                  @H_Line_Callback,
                                  @V_Line_Callback,
                                  @Dot_Callback,
                                  @Fill_Screen_Callback,
                                  @Rectangle_Callback,
                                  @Circle_Callback,
                                  @GetTextWidthAndHeight_Callback,
                                  @DrawBitmap_Callback);
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.PrepareClient;
var
  Response: string;
begin
  FWHLabel := TLabel.Create(Self);       //This has to be recreated for every connection, otherwise it's useless (some resource gets owned and never released).
  FWHLabel.Parent := Self;
  FWHLabel.AutoSize := False;
  FWHLabel.Width := 1000;
  FWHLabel.Height := 100;

  FIdTCPClient := TIdTCPClient.Create(nil);

  try
    FIdTCPClient.ConnectTimeout := 1000;
    FIdTCPClient.ReadTimeout := 1000;
    FIdTCPClient.UseNagle := False;
    FIdTCPClient.Connect(FPluginAddress, FPluginPort);
    AddToLog('Connected to callback server from DynTFTCGRemoteSystem plugin.');

    Application.ProcessMessages;
    FIdTCPClient.Socket.WriteLn(CCGRM_PluginStartup + '=RS');
    Response := FIdTCPClient.Socket.ReadLn(#13#10, 500, CMaxLineLen);

    AddToLog('Plugin startup response: ' + Response);
  except
    on E: Exception do
      AddToLog('Can''t connect to callback server. ' + E.Message);
  end;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.CloseClient;
begin
  try
    try
      FIdTCPClient.Socket.CloseGracefully;
      FIdTCPClient.Disconnect;
    except
      on E: Exception do
        AddToLogFromThread('Can''t disconnect from callback server. Maybe it''s disconnected already. ' + E.Message);
    end;

    FreeAndNil(FIdTCPClient);
  except
    on E: Exception do
      AddToLogFromThread('Can''t properly destroy client module. ' + E.Message);
  end;

  FreeAndNil(FWHLabel);
end;


procedure PrepareClientFromThread;
var
  SynObj: TPrepareClientSyncObj;
begin
  SynObj := TPrepareClientSyncObj.Create;
  try
    SynObj.Synchronize;
  finally
    SynObj.Free;
  end;
end;


procedure CloseClientFromThread;
var
  SynObj: TCloseClientSyncObj;
begin
  SynObj := TCloseClientSyncObj.Create;
  try
    SynObj.Synchronize;
  finally
    SynObj.Free;
  end;
end;


procedure Handle_UpdateLiveColorConstants(ASocket: TIdIOHandlerSocket);
var
  AComponentIndex, AColorIndex, ANewColor: Integer;
begin
  AComponentIndex := ASocket.ReadInt32;
  AColorIndex := ASocket.ReadInt32;
  ANewColor := ASocket.ReadInt32;

  LiveColors[AComponentIndex]^[AColorIndex]^ := ANewColor;
end;


procedure Handle_BackupLiveColorConstants;
var
  i, j: Integer;
begin
  for i := 0 to Length(BackupLiveColors) - 1 do
    for j := 0 to LiveColorsConstCount[i] - 1 do
    begin
      try
        BackupLiveColors[i]^[j] := LiveColors[i]^[j]^;
      except
        on E: Exception do
          frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Exception on BackupLiveColorConstants at i = ' + IntToStr(i) + '  j = ' + IntToStr(j) + '    ' + E.Message);
      end;
    end;
end;


procedure Handle_RestoreColorConstantsFromBackup;
var
  i, j: Integer;
begin
  for i := 0 to Length(BackupLiveColors) - 1 do
    for j := 0 to LiveColorsConstCount[i] - 1 do
      LiveColors[i]^[j]^ := BackupLiveColors[i]^[j];
end;


procedure Handle_CanUpdateLiveColorConstant(ASocket: TIdIOHandlerSocket);
var
  ComponentIndex, ColorIndex: Integer;
begin
  ComponentIndex := ASocket.ReadInt32;
  ColorIndex := ASocket.ReadInt32;

  if ComponentIndex > Length(LiveColors) - 1 then
  begin
    //DynTFT_DebugConsole('Can''t update LiveColors array. ComponentIndex is greater than array length - 1. ComponentIndex = ' + IntToStr(ComponentIndex) + '  Length = ' + IntToStr(Length(LiveColors)));
    ASocket.WriteLn('False');
    Exit;
  end;

  if ColorIndex > LiveColorsConstCount[ComponentIndex] - 1 then
  begin
    //DynTFT_DebugConsole('Can''t update LiveColors array. ColorIndex is greater than color constants count - 1. ColorIndex = ' + IntToStr(ColorIndex) + '  Count = ' + IntToStr(LiveColorsConstCount[ComponentIndex]));
    ASocket.WriteLn('False');
    Exit;
  end;

  ASocket.WriteLn('True');
end;


procedure Handle_GetLiveColorsConstCountByComponent(ASocket: TIdIOHandlerSocket);
var
  ComponentIndex, Res: Integer;
begin
  ComponentIndex := ASocket.ReadInt32;

  if (ComponentIndex < 0) or (ComponentIndex > Length(LiveColors) - 1) then
    Res := -1
  else
    Res := LiveColorsConstCount[ComponentIndex];

  ASocket.Write(LongInt(Res));
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.IdTCPServer1Execute(AContext: TIdContext);
var
  Cmd, CmdParam: string;
begin
  Cmd := AContext.Connection.Socket.ReadLn(#13#10, 50, CMaxLineLen);

  if Cmd = '' then
  begin
    Sleep(2);
    Exit;
  end;

  CmdParam := Copy(Cmd, Pos('=', Cmd) + 1, MaxInt);
  Cmd := Copy(Cmd, 1, Pos('=', Cmd) - 1);

  if Pos(CCGRM_DrawPDynTFTComponentOnPanelPrefix, Cmd) = 1 then
  begin
    Handle_DrawPDynTFTComponentOnPanel(AContext.Connection.Socket, Copy(Cmd, Length(CCGRM_DrawPDynTFTComponentOnPanelPrefix) + 1, MaxInt), CmdParam);
    Exit;
  end;

  if Cmd = CCGRM_GetComponentIconFromServer then
  begin
    Handle_GetComponentIconFromServer(AContext.Connection.Socket, CmdParam);
    Exit;
  end;

  if Cmd = CCGRM_PluginStartup then
  begin
    AddToLogFromThread(Cmd);
    Exit;
  end;

  if Cmd = CCGRM_PluginDone then
  begin
    AddToLogFromThread(Cmd);
    AddToLogFromThread('');
    CloseClientFromThread;
    Exit;
  end;

  if Cmd = CCGRM_PluginPort then
  begin
    FPluginAddress := AContext.Connection.Socket.Binding.PeerIP;
    FPluginPort := StrToIntDef(CmdParam, 100);
    AddToLogFromThread('Plugin port set to ' + IntToStr(FPluginPort));
    PrepareClientFromThread;
    Exit;
  end;

  if Cmd = CCGRM_DisconnectFromPlugin then
  begin
    AddToLogFromThread(Cmd);
    CloseClientFromThread;
    Exit;
  end;

  {if Cmd = CCGRM_Ping then
  begin
    //blink something on the main window
    Exit;
  end; }

  if Cmd = CCGRM_UpdateLiveColorConstants then
  begin
    Handle_UpdateLiveColorConstants(AContext.Connection.Socket);
    Exit;
  end;

  if Cmd = CCGRM_BackupLiveColorConstants then
  begin
    Handle_BackupLiveColorConstants;
    Exit;
  end;

  if Cmd = CCGRM_RestoreColorConstantsFromBackup then
  begin
    Handle_RestoreColorConstantsFromBackup;
    Exit;
  end;

  if Cmd = CCGRM_CanUpdateLiveColorConstant then
  begin
    Handle_CanUpdateLiveColorConstant(AContext.Connection.Socket);
    Exit;
  end;

  if Cmd = CCGRM_GetLiveColorsConstCountByComponent then
  begin
    Handle_GetLiveColorsConstCountByComponent(AContext.Connection.Socket);
    Exit;
  end;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.tmrCloseClientTimer(Sender: TObject);
begin
  tmrCloseClient.Enabled := False;
  CloseClient;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.tmrPrepareClientTimer(
  Sender: TObject);
begin
  tmrPrepareClient.Enabled := False;
  PrepareClient;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.AddToLog(s: string);
begin
  FLoggingFIFO.Put(s);
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.AddToLogFromThread(s: string);
begin
  FLoggingFIFO.Put(s);
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.tmrLoggingTimer(Sender: TObject);
var
  TempStrings: TStringList;
  i: Integer;
begin
  //FLoggingFIFO.PopAll(memLogErr.Lines); //this resets the memo view to the first line

  try
    TempStrings := TStringList.Create;
    try
      FLoggingFIFO.PopAll(TempStrings);

      for i:= 0 to TempStrings.Count - 1 do
        memLog.Lines.Add(DateTimeToStr(Now) + '  ' + TempStrings[i]);  //adding lines, one by one, instead of calling AddStrings, to leave the focus to the last line
    finally
      TempStrings.Free;
    end;
  except
    on E: Exception do
      memLog.Lines.Add('Exception on adding to log: ' + E.Message);
  end;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.tmrStartupTimer(Sender: TObject);
begin
  tmrStartup.Enabled := False;

  try
    IdTCPServer1.Active := True;
    AddToLog('Listening on port ' + IntToStr(IdTCPServer1.DefaultPort));
  except
    on E: Exception do
      AddToLog('Can''t listen on port ' + IntToStr(IdTCPServer1.DefaultPort) + '. ' + E.Message);
  end;

  RegisterAllComponentsEvents;
  GetDrawingProcedures(FDrawingProcedures);
  AddToLogFromThread('Registered ' + IntToStr(Length(FDrawingProcedures)) + ' components.');

  Handle_RegisterDynTFTDrawingProcedures;

  prbAllocatedMemory.Max := {$IFDEF UseSmallMM} MaxMM {$ELSE} HEAP_SIZE {$ENDIF};
  lblAllocatedMemory.Hint := 'Available: ' + IntToStr(prbAllocatedMemory.Max) + ' Bytes';

  tmrStats.Enabled := True;  
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.tmrStatsTimer(Sender: TObject);
var
  AllocatedMemSize: Integer;
begin
  AllocatedMemSize := {$IFDEF UseSmallMM} MaxMM {$ELSE} HEAP_SIZE {$ENDIF} - MM_TotalFreeMemSize;
  if prbAllocatedMemory.Position <> AllocatedMemSize then
  begin
    prbAllocatedMemory.Position := AllocatedMemSize;
    lblAllocatedMemory.Caption := 'Allocated Memory: ' + IntToStr(AllocatedMemSize) + ' B    (' + IntToStr(AllocatedMemSize shr 10) + ' KB)';
  end;

  if MM_error then
  begin
    lblAllocatedMemory.Font.Color := clRed;
    lblAllocatedMemory.Font.Style := [fsBold];
  end;
end;

end.

