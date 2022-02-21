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

unit DynTFTPluginUtils;

interface

uses
  SysUtils, Classes, DynTFTCodeGenSharedDataTypes, Graphics, DynTFTSharedUtils,
  TFT, DynTFTTypes, ImgList, Controls;


type
  TDrawDynTFTComponentProc = procedure(APanelBase: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
  TDrawDynTFTComponentProcArr = array of TDrawDynTFTComponentProc;

var
  Items_Content: TStringList;
  Items_Visibility: TStringList;
  Items_Enabling: TStringList;


procedure UpdateBaseProperties(APanelBase: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var BaseProperties: TDynTFTBaseProperties);
function GetConstantIntValueFromSchema(var SchemaConstants: TComponentConstantArr; ConstantName: string; DefaultValue: Integer): Integer;
function FontPropertyValueToSFont(var AFontSettings: TFontSettingsArr; PropertyValue: string): PDynTFTFontSettings;
procedure GetComponentIconFromImageList(ComponentIndex: Integer; AImgLst: TImageList; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64);
procedure RegisterCompDrawingProcedure(var ACompDrawingProcedures: TDrawDynTFTComponentProcArr; ADrawingProc: TDrawDynTFTComponentProc);
procedure DrawPDynTFTComponentOnPanelBase(var APanelBase: TUIPanelBase; var ACompDrawingProcedures: TDrawDynTFTComponentProcArr; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef; ASetPropertiesCallback: TSetPropertiesCallback);
function Replace45To1310(s: string): string;
function Replace1310To45(s: string): string;
function Replace56To1310(s: string): string;
function Replace1310To56(s: string): string;
function Replace78To1310(s: string): string;
function Replace1310To78(s: string): string;


implementation


uses
  DynTFTBaseDrawing, DynTFTUtils;


procedure UpdateBaseProperties(APanelBase: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var BaseProperties: TDynTFTBaseProperties);
begin
  BaseProperties.ComponentType := APanelBase.DynTFTComponentType;
  BaseProperties.Left := 0;
  BaseProperties.Top := 0;
  BaseProperties.Width := APanelBase.Width - 1;
  BaseProperties.Height := APanelBase.Height - 1;
  BaseProperties.Visible := CVISIBLE;
  BaseProperties.Focused := CUNFOCUSED;
  BaseProperties.Parent := nil;
  BaseProperties.CompState := CRELEASED;
  BaseProperties.CanHandleMessages := False;
  BaseProperties.OnMouseDownUser := nil;
  BaseProperties.OnMouseMoveUser := nil;
  BaseProperties.OnMouseUpUser := nil;

  BaseProperties.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  BaseProperties.ScreenIndex := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ScreenIndex'), 0);
end;


function GetConstantIntValueFromSchema(var SchemaConstants: TComponentConstantArr; ConstantName: string; DefaultValue: Integer): Integer;
var
  i: Integer;
begin
  Result := DefaultValue;
  if Length(ConstantName) > 0 then
    if ConstantName[1] in ['0'..'9'] then
    begin
      Result := StrToIntDef(ConstantName, DefaultValue);
      Exit;
    end;

  for i := Length(SchemaConstants) - 1 downto 0 do   //start with component colors, then "fallback" to base colors
    if SchemaConstants[i].ConstantName = ConstantName then
    begin
      Result := SchemaConstants[i].ConstantValueInt;
      Exit;
    end;
end;


//=========================

function FontPropertyValueToSFont(var AFontSettings: TFontSettingsArr; PropertyValue: string): PDynTFTFontSettings;
var
  Index: Integer;
begin
  if PropertyValue = '@TFT_defaultFont' then
    Result := @TFT_defaultFont
  else
    Result := @TFT_fallbackFont;

  Index := GetFontPropertyValueIndexInAllFonts(AFontSettings, PropertyValue);
  if Index > -1 then
    Result := AFontSettings[Index].SFont;
end;


procedure GetComponentIconFromImageList(ComponentIndex: Integer; AImgLst: TImageList; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64);
var
  ATempStream: TMemoryStream;
  ABitmap: TBitmap;
  AContentPointer: Pointer;
begin
  if (ComponentIndex < 0) or (ComponentIndex > AImgLst.Count - 1) then
    raise Exception.Create('Index out of bounds (' + IntToStr(ComponentIndex) + ') when getting icon.');

  if not Assigned(ASetSizeCallback) then
    raise Exception.Create('ASetSizeCallback is not assigned. It is required to set the stream size before getting the icon contents.');

  ATempStream := TMemoryStream.Create;
  try
    ABitmap := TBitmap.Create;
    try
      AImgLst.GetBitmap(ComponentIndex, ABitmap);
      ABitmap.SaveToStream(ATempStream);

      AContentPointer := ASetSizeCallback(ATempStream.Size, AStreamID);   //DynTFTCodeGen should implement a callback, which sets the stream size based on this value

      Move(ATempStream.Memory^, AContentPointer^, ATempStream.Size);
    finally
      ABitmap.Free;
    end;
  finally
    ATempStream.Free;
  end;
end;


procedure RegisterCompDrawingProcedure(var ACompDrawingProcedures: TDrawDynTFTComponentProcArr; ADrawingProc: TDrawDynTFTComponentProc);
begin
  SetLength(ACompDrawingProcedures, Length(ACompDrawingProcedures) + 1);
  ACompDrawingProcedures[Length(ACompDrawingProcedures) - 1] := ADrawingProc;
end;


procedure DrawPDynTFTComponentOnPanelBase(var APanelBase: TUIPanelBase; var ACompDrawingProcedures: TDrawDynTFTComponentProcArr; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef; ASetPropertiesCallback: TSetPropertiesCallback);
var
  TempPropertiesOrEvents, BkpPropertiesOrEvents: TDynTFTDesignPropertyArr;
  TempSchemaConstants: TComponentConstantArr;
  TempColorConstants: TColorConstArr;
  TempFontSettings: TFontSettingsArr;
  i: Integer;
  TempStringList: TStringList;
  TempPropertiesOrEventsRef: TDynArrayRef;
begin
  if (APanelBase.DynTFTComponentType > -1) and (APanelBase.DynTFTComponentType < Length(ACompDrawingProcedures)) then
  begin
    try
      //the internal structure of dynamic arrays differs between Delphi and FreePascal  (at least the array length), so copy the contents:

      SetLength(TempPropertiesOrEvents, APropertiesOrEvents.Len);
      SetLength(BkpPropertiesOrEvents, APropertiesOrEvents.Len);
      for i := 0 to APropertiesOrEvents.Len - 1 do
      begin
        TempPropertiesOrEvents[i] := TDynTFTDesignProperty(Pointer(QWord(APropertiesOrEvents.AddrOfFirst) + i * SizeOf(TDynTFTDesignProperty))^);
        BkpPropertiesOrEvents[i] := TempPropertiesOrEvents[i];
      end;

      SetLength(TempSchemaConstants, ASchemaConstants.Len);
      for i := 0 to ASchemaConstants.Len - 1 do
        TempSchemaConstants[i] := TComponentConstant(Pointer(QWord(ASchemaConstants.AddrOfFirst) + i * SizeOf(TComponentConstant))^);

      SetLength(TempColorConstants, AColorConstants.Len);
      for i := 0 to AColorConstants.Len - 1 do
        TempColorConstants[i] := TColorConst(Pointer(QWord(AColorConstants.AddrOfFirst) + i * SizeOf(TColorConst))^);

      SetLength(TempFontSettings, AFontSettings.Len);
      for i := 0 to AFontSettings.Len - 1 do
        TempFontSettings[i] := TFontSettings(Pointer(QWord(AFontSettings.AddrOfFirst) + i * SizeOf(TFontSettings))^);

      ACompDrawingProcedures[APanelBase.DynTFTComponentType](APanelBase, TempPropertiesOrEvents, TempSchemaConstants, TempColorConstants, TempFontSettings);

      //there may be components with modified properties, as a result of calls to UpdateComponentPropertyByName
      TempPropertiesOrEventsRef.Len := Length(TempPropertiesOrEvents);
      if TempPropertiesOrEventsRef.Len > 0 then
        TempPropertiesOrEventsRef.AddrOfFirst := @TempPropertiesOrEvents[0]
      else
        TempPropertiesOrEventsRef.AddrOfFirst := nil;

      ASetPropertiesCallback(TempPropertiesOrEventsRef, APropertiesOrEvents);   // a callback is required, to do the copy operation on DynTFTCodeGen's side, because it's not safe for the plugin to overwrite strings into DynTFTCodeGen and also the temp array is cleared from memory when exiting DrawPDynTFTComponentOnPanelBase
    except
      on E: Exception do
      begin
        TempStringList := TStringList.Create;
        try
          TempStringList.Text := E.Message;
          if TempStringList.Count > 0 then
            TempStringList.Strings[0] := 'Ex: ' + TempStringList.Strings[0];

          for i := 0 to TempStringList.Count - 1 do
            DynTFT_Write_Text(TempStringList.Strings[i], 0, i * 15);
        finally
          TempStringList.Free;
        end;
      end;
    end;
  end
  else
  begin
    DynTFT_Set_Pen(clRed, 1);
    DynTFT_Set_Brush(1, clWhite, 0, 0, 0, 0);
    DynTFT_Rectangle(0, 0, APanelBase.Width - 1, APanelBase.Height - 1);
    DynTFT_Write_Text('Unimplemented at index: ' + IntToStr(APanelBase.DynTFTComponentType), 5, 2);
    DynTFT_Write_Text(APanelBase.Caption, 5, 20);
  end;
end;

                                               //Do not refactor the replacement functions into a single one, because they are faster this way.
function Replace45To1310(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #4) and (Result[i + 1] = #5) then
    begin
      Result[i] := #13;
      Result[i + 1] := #10;
    end;
end;


function Replace1310To45(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #13) and (Result[i + 1] = #10) then
    begin
      Result[i] := #4;
      Result[i + 1] := #5;
    end;
end;


function Replace56To1310(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #5) and (Result[i + 1] = #6) then
    begin
      Result[i] := #13;
      Result[i + 1] := #10;
    end;
end;


function Replace1310To56(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #13) and (Result[i + 1] = #10) then
    begin
      Result[i] := #5;
      Result[i + 1] := #6;
    end;
end;


function Replace78To1310(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #7) and (Result[i + 1] = #8) then
    begin
      Result[i] := #13;
      Result[i + 1] := #10;
    end;
end;


function Replace1310To78(s: string): string;
var
  i: Integer;
begin
  Result := s;
  if Length(Result) < 2 then
    Exit;

  for i := 1 to Length(Result) - 1 do
    if (Result[i] = #13) and (Result[i + 1] = #10) then
    begin
      Result[i] := #7;
      Result[i + 1] := #8;
    end;
end;

end.
