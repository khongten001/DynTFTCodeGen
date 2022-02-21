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

unit DynTFTCGRemoteSystemServerMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IdTCPServer, IdSync, IdCustomTCPServer, IdContext, IdTCPClient;

type

  { TfrmDynTFTCGRemoteSystemServerMain }

  TfrmDynTFTCGRemoteSystemServerMain = class(TForm)
    IdTCPServer1: TIdTCPServer;
    memLog: TMemo;
    tmrStartup: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure tmrStartupTimer(Sender: TObject);
  private
    procedure AddToLog(s: string);
    procedure AddToLogFromThread(s: string);
  public

  end;

  TSyncObj = class(TIdSync)
    FMsg: string;
  protected
    procedure DoSynchronize; override;
  end;

var
  frmDynTFTCGRemoteSystemServerMain: TfrmDynTFTCGRemoteSystemServerMain;

implementation

{$R *.frm}


uses
  RemoteSystemCommands, IdIOHandlerSocket, ComponentIcons, DynTFTPluginUtils, PanelDrawing,
  DynTFTCodeGenSharedDataTypes, TFT, DynTFTSharedUtils;


var
  DrawingCommands: array of string;
  FDrawingProcedures: TDrawDynTFTComponentProcArr;
  FIdTCPClient: TIdTCPClient;



procedure TSyncObj.DoSynchronize;
begin
  frmDynTFTCGRemoteSystemServerMain.AddToLog(FMsg);
end;

{ TfrmDynTFTCGRemoteSystemServerMain }

procedure TfrmDynTFTCGRemoteSystemServerMain.FormCreate(Sender: TObject);
begin
  tmrStartup.Enabled := True;
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

      FontSize := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 8);
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Bold := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Italic := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Underline := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      StrikeOut := Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1) = '1';
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Charset := StrToIntDef(Copy(Str, 1, Pos(CRecFieldSeparator, Str) - 1), 1);  //1 = DEFAULT_CHARSET
      Delete(Str, 1, Pos(CRecFieldSeparator, Str) + SepLen - 1);

      Pitch := StrToIntDef(Str, 0); //can be 0, 1, 2

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
  TempSchemaConstants: TComponentConstantArr;
  TempColorConstants: TColorConstArr;
  TempFontSettings: TFontSettingsArr;
  i: Integer;
  TempPanelBase: TUIPanelBase;
  DrawingResponse: string;
begin
  //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('DrawPDynTFTComponentOnPanel: ' + ACompName);

  try
    try
      PropertiesOrEvents := ASocket.ReadLn(#13#10, 400);
      SchemaConstants := ASocket.ReadLn(#13#10, 400);
      ColorConstants := ASocket.ReadLn(#13#10, 400);
      FontSettings := ASocket.ReadLn(#13#10, 400);
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
      frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Ex in DrawPDynTFTComponentOnPanel: ' + E.Message);
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
         'pen_color=' + IntToHex(pen_color) + CDrawingCmdFieldSeparator +
         'pen_width=' + IntToHex(pen_width);

  AddDrawingCommand(Res);
end;


procedure Set_Brush_Callback(brush_enabled: Byte; brush_color: TColor; gradient_enabled, gradient_orientation: Byte; gradient_color_from, gradient_color_to: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Set_Brush + CDrawingCmdFieldSeparator +
         'brush_enabled=' + IntToHex(brush_enabled) + CDrawingCmdFieldSeparator +
         'brush_color=' + IntToHex(brush_color) + CDrawingCmdFieldSeparator +
         'gradient_enabled=' + IntToHex(gradient_enabled) + CDrawingCmdFieldSeparator +
         'gradient_orientation=' + IntToHex(gradient_orientation) + CDrawingCmdFieldSeparator +
         'gradient_color_from=' + IntToHex(gradient_color_from) + CDrawingCmdFieldSeparator +
         'gradient_color_to=' + IntToHex(gradient_color_to);

  AddDrawingCommand(Res);
end;


procedure Set_Font_Callback(activeFont: PByte; font_color: TColor; font_orientation: Word);
var
  Res: string;
  FontAddress: string;
begin
  try
    //IdentifierName is used to store the pointer, since the actual identifier name is not needed here on server.
    FontAddress := PDynTFTFontSettings(activeFont)^.IdentifierName;

    Res := CDPDynTFT_Set_Font + CDrawingCmdFieldSeparator +
           'activeFont=' + FontAddress + CDrawingCmdFieldSeparator +
           'font_color=' + IntToHex(font_color) + CDrawingCmdFieldSeparator +
           'font_orientation=' + IntToHex(font_orientation);
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
         'x=' + IntToHex(x) + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y);

  AddDrawingCommand(Res);
end;


procedure Line_Callback(x1, y1, x2, y2: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Line + CDrawingCmdFieldSeparator +
         'x1=' + IntToHex(x1) + CDrawingCmdFieldSeparator +
         'y1=' + IntToHex(y1) + CDrawingCmdFieldSeparator +
         'x2=' + IntToHex(x2) + CDrawingCmdFieldSeparator +
         'y2=' + IntToHex(y2);

  AddDrawingCommand(Res);
end;


procedure H_Line_Callback(x_start, x_end, y_pos: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_H_Line + CDrawingCmdFieldSeparator +
         'x_start=' + IntToHex(x_start) + CDrawingCmdFieldSeparator +
         'x_end=' + IntToHex(x_end) + CDrawingCmdFieldSeparator +
         'y_pos=' + IntToHex(y_pos);

  AddDrawingCommand(Res);
end;


procedure V_Line_Callback(y_start, y_end, x_pos: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_V_Line + CDrawingCmdFieldSeparator +
         'y_start=' + IntToHex(y_start) + CDrawingCmdFieldSeparator +
         'y_end=' + IntToHex(y_end) + CDrawingCmdFieldSeparator +
         'x_pos=' + IntToHex(x_pos);

  AddDrawingCommand(Res);
end;


procedure Dot_Callback(x, y: Integer; Color: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Dot + CDrawingCmdFieldSeparator +
         'x=' + IntToHex(x) + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y) + CDrawingCmdFieldSeparator +
         'Color=' + IntToHex(Color);

  AddDrawingCommand(Res);
end;


procedure Fill_Screen_Callback(color: TColor);
var
  Res: string;
begin
  Res := CDPDynTFT_Fill_Screen + CDrawingCmdFieldSeparator +
         'Color=' + IntToHex(color);

  AddDrawingCommand(Res);
end;


procedure Rectangle_Callback(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Rectangle + CDrawingCmdFieldSeparator +
         'x_upper_left=' + IntToHex(x_upper_left) + CDrawingCmdFieldSeparator +
         'y_upper_left=' + IntToHex(y_upper_left) + CDrawingCmdFieldSeparator +
         'x_bottom_right=' + IntToHex(x_bottom_right) + CDrawingCmdFieldSeparator +
         'y_bottom_right=' + IntToHex(y_bottom_right);

  AddDrawingCommand(Res);
end;


procedure Circle_Callback(x_center, y_center, radius: Integer);
var
  Res: string;
begin
  Res := CDPDynTFT_Circle + CDrawingCmdFieldSeparator +
         'x_center=' + IntToHex(x_center) + CDrawingCmdFieldSeparator +
         'y_center=' + IntToHex(y_center) + CDrawingCmdFieldSeparator +
         'radius=' + IntToHex(radius);

  AddDrawingCommand(Res);
end;


procedure GetTextWidthAndHeight_Callback(AText: string; var Width, Height: Word);
var
  AStringList: TStringList;
  WidthHeight: string;
begin
  //pair readln with SendPlainStringToServer('Width=' + IntToHex(Width, 4) + #4#5 + 'Height=' + IntToHex(Height, 4));
  FIdTCPClient.Socket.WriteLn(CCGRM_CallbackDraw + '=' + CDPDynTFT_GetTextWidthAndHeight + CDrawingCmdFieldSeparator + 'AText=' + AText);
  WidthHeight := FIdTCPClient.Socket.ReadLn(#13#10, 200);

  //frmDynTFTCGRemoteSystemServerMain.AddToLogFromThread('Read WidthHeight: ' + WidthHeight);

  AStringList := TStringList.Create;
  try
    AStringList.Text := Replace45To1310(WidthHeight);
    Width := HexToInt(AStringList.Values['Width']);
    Height := HexToInt(AStringList.Values['Height']);
  finally
    AStringList.Free;
  end;

  //AddDrawingCommand(Res);   //commented because this procedure already calls Socket.WriteLn / Socket.ReadLn
end;


procedure DrawBitmap_Callback(APointerToBmpStreamMem: Pointer; AContentSize: Int64; x, y: Integer);
var
  Res: string;
  MemStream: TMemoryStream;
begin
  Res := CDPDynTFT_DrawBitmap + CDrawingCmdFieldSeparator +
         'AContentSize=' + IntToHex(AContentSize) + CDrawingCmdFieldSeparator +
         'x=' + IntToHex(x) + CDrawingCmdFieldSeparator + CDrawingCmdFieldSeparator +
         'y=' + IntToHex(y);

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


procedure TfrmDynTFTCGRemoteSystemServerMain.IdTCPServer1Execute(AContext: TIdContext);
var
  Cmd, CmdParam: string;
begin
  Cmd := AContext.Connection.Socket.ReadLn(#13#10, 50);   

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

    FIdTCPClient := TIdTCPClient.Create(nil);
    try
      FIdTCPClient.ConnectTimeout := 1000;
      FIdTCPClient.ReadTimeout := 1000;
      FIdTCPClient.UseNagle := False;
      FIdTCPClient.Connect('127.0.0.1', 3581);
      AddToLog('Connected to callback server from DynTFTCGRemoteSystem plugin.');
      FIdTCPClient.Socket.WriteLn('CCGRM_PluginStartup=RS');
    except
      on E: Exception do
        AddToLogFromThread('Can''t connect to callback server. ' + E.Message);
    end;
    Exit;
  end;

  if Cmd = CCGRM_PluginDone then
  begin
    AddToLogFromThread(Cmd);
    AddToLogFromThread('');

    try
      FIdTCPClient.Disconnect;
    except
      on E: Exception do
        AddToLogFromThread('Can''t disconnect from callback server. Maybe it''s disconnected already. ' + E.Message);
    end;
    Exit;
  end;
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.AddToLog(s: string);
begin
  memLog.Lines.Add(DateTimeToStr(Now) + '  ' + s);
end;


procedure TfrmDynTFTCGRemoteSystemServerMain.AddToLogFromThread(s: string);
var
  SynObj: TSyncObj;
begin
  SynObj := TSyncObj.Create;
  try
    SynObj.FMsg := s;
    SynObj.Synchronize;
  finally
    SynObj.Free;
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
end;

end.

