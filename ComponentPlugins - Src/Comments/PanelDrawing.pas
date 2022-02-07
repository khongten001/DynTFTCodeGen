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

unit PanelDrawing;

interface


uses
  Windows, DynTFTCodeGenSharedDataTypes;

procedure RegisterAllComponentsEvents; stdcall;
procedure DrawPDynTFTComponentOnPanel(var APanelBase: TUIPanelBase; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef); stdcall;

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
  

implementation


uses
  SysUtils, Graphics, DynTFTSharedUtils, DynTFTPluginUtils, TFTCallbacks,
  DynTFTConsts, DynTFTUtils, MemManager, DynTFTBaseDrawing, TFT;

  
type
  TDynTFTComment = record
    Caption: string;
    Color: TColor;
    Font_Color: TColor;
    ActiveFont: PByte;
  end;
  PDynTFTComment = ^TDynTFTComment;


var
  FDrawingProcedures: TDrawDynTFTComponentProcArr;
    

procedure PrepareComment(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTComment: PDynTFTComment);
begin
  ADynTFTComment^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTComment^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTComment^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTComment^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure TDrawDynTFTComponentProc_Comment(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTComment: PDynTFTComment;
begin
  New(ADynTFTComment);
  try
    ADynTFTComment^.Caption := 'no comment';
    ADynTFTComment^.Color := CL_WHITE;
    ADynTFTComment^.Font_Color := CL_GRAY;
    ADynTFTComment^.ActiveFont := nil;

    PrepareComment(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTComment);

    DynTFT_Set_Pen(ADynTFTComment.Color, 1);
    DynTFT_Set_Brush(1, ADynTFTComment.Color, 0, 0, 0, 0);
    DynTFT_Rectangle(0, 0, APanel.Width - 1, APanel.Height - 1);

    DynTFT_Set_Font(ADynTFTComment.ActiveFont, ADynTFTComment.Font_Color, FO_HORIZONTAL);
    DynTFT_Write_Text(ADynTFTComment^.Caption, 0, 0);
  finally
    Dispose(ADynTFTComment);
  end;
end;


procedure RegisterAllComponentsEvents; stdcall;
begin
  MM_Init;
  DynTFTInitComponentTypeRegistration;
  DynTFTInitComponentContainers;
  UseTFTTrueColor := True;
  
  RegisterDrawingProcedure(FDrawingProcedures, TDrawDynTFTComponentProc_Comment);
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
  ADynTFT_DrawBitmap_Callback: TDynTFT_DrawBitmap_Callback );  stdcall;
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
end;


procedure DrawPDynTFTComponentOnPanel(var APanelBase: TUIPanelBase; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef); stdcall;
begin
  DrawPDynTFTComponentOnPanelBase(APanelBase, FDrawingProcedures, APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings);
end;


initialization
  SetLength(FDrawingProcedures, 0);

end.
