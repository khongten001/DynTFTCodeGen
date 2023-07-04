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

{$DEFINE IsDesktop}

uses
  DynTFTCodeGenSharedDataTypes, DynTFTPluginUtils;

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

procedure GetDrawingProcedures(var ADrawingProcedures: TDrawDynTFTComponentProcArr);

implementation


uses
  SysUtils, Graphics, Classes, Math,
  {$IFDEF FPC} DynTFTCodeGenImgFormFP, {$ELSE} DynTFTCodeGenImgForm, {$ENDIF}
  DynTFTSharedUtils, TFTCallbacks,

  MemManager, TFT, DynTFTUtils, DynTFTTypes, DynTFTBaseDrawing, DynTFTConsts,
  DynTFTButton, DynTFTArrowButton, DynTFTPanel, DynTFTCheckBox, DynTFTScrollBar,
  DynTFTItems, DynTFTListBox, DynTFTLabel, DynTFTRadioButton, DynTFTRadioGroup,
  DynTFTTabButton, DynTFTPageControl, DynTFTEdit, DynTFTKeyButton,
  DynTFTVirtualKeyboard, DynTFTComboBox, DynTFTTrackBar, DynTFTProgressBar,
  DynTFTMessageBox, DynTFTVirtualTable;


var
  FCompDrawingProcedures: TDrawDynTFTComponentProcArr;

  
function ItemIsVisible(Item: string): Boolean;
begin
  Result := (Item = '') or (Item = '1') or (UpperCase(Trim(Item)) = 'TRUE');
end;


function ItemIsEnabled(Item: string): Boolean;
begin
  Result := (Item = '') or (Item = '1') or (UpperCase(Trim(Item)) = 'TRUE');
end;


procedure ItemsOnGetItem(AComp: PPtrRec; Index: LongInt; var ItemText: string);
begin
  if Index < 0 then
  begin
    ItemText := 'out: ' + IntToStr(Index);  //debugging info
    Exit;
  end;

  if Items_Content.Count > 0 then
    ItemText := Items_Content.Strings[Index];
end;


procedure VTItemsOnGetItem(AComp: PPtrRec; Index, Column: LongInt; var ItemText: string);
begin
  if Index < 0 then
  begin
    ItemText := 'out: ' + IntToStr(Index);  //debugging info
    Exit;
  end;

  if Items_Content.Count > 0 then
    ItemText := Items_Content.Strings[Index] + ' ' + IntToStr(Column);
end;


procedure ItemsOnGetItemVisibility(AComp: PPtrRec; Index: LongInt; var ItemText: string {$IFDEF ItemsVisibility}; IsVisible: PBoolean {$ENDIF} {$IFDEF ItemsEnabling}; IsEnabled: PBoolean {$ENDIF});
begin
  if Index < 0 then
  begin
    DynTFT_DebugConsole('index out of bounds in GetItemVisibility: ' + IntToStr(Index));  //debugging info
    Exit;
  end;

  IsVisible^ := ItemIsVisible(Items_Visibility.Strings[Index]);
  IsEnabled^ := ItemIsEnabled(Items_Enabling.Strings[Index]);
end;


procedure ItemsOnDrawIcon(AItems: PPtrRec; Index, ItemY: LongInt; var ItemText: string {$IFDEF ItemsEnabling}; IsEnabled: Boolean {$ENDIF});
var
  IconLeft: TSInt;
  TempItemHeight: Word;
begin
  IconLeft := PDynTFTItems(TPtrRec(AItems))^.BaseProps.Left + CIconIndent;   //If the compiler reports that CIconIndent is not found, then please enable ListIcons at project level.

  TempItemHeight := PDynTFTItems(TPtrRec(AItems))^.ItemHeight;

  DynTFT_Set_Pen($00A0A0, 1);
  DynTFT_Set_Brush(1, CL_YELLOW, 0, 0, 0, 0);
  DynTFT_Rectangle(IconLeft, ItemY, IconLeft + TempItemHeight, ItemY + TempItemHeight - 1);

  DynTFT_Set_Pen(CL_OLIVE, 1);
  DynTFT_Line(IconLeft, ItemY, IconLeft + TempItemHeight - 5, ItemY + 3);
  DynTFT_V_Line(ItemY + 3, ItemY + TempItemHeight, IconLeft + TempItemHeight - 5 - 1);
end;


procedure VTItemsOnDrawIcon(AItems: PPtrRec; Index, Column, ItemY: LongInt; var ItemText: string {$IFDEF ItemsEnabling}; IsEnabled: Boolean {$ENDIF});
begin
  ItemsOnDrawIcon(AItems, Index, ItemY {- (Column and 1) shl 1}, ItemText, {$IFDEF ItemsEnabling} IsEnabled {$ENDIF});
end;


function GetTotalComponentVisibleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Items_Visibility.Count - 1 do
    if ItemIsVisible(Items_Visibility.Strings[i]) then
      Inc(Result);
end;  


//=========================


procedure UpdateBasePropertiesToAnArrowButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var BaseProperties: TDynTFTBaseProperties);
begin
  BaseProperties.ComponentType := DynTFTGetArrowButtonComponentType;
  BaseProperties.Left := 0;
  BaseProperties.Top := 0;
  BaseProperties.Width := 10;
  BaseProperties.Height := 10;
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



procedure PrepareButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTButton: PDynTFTButton);
begin
  ADynTFTButton^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTButton^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTButton^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTButton^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));  //ActiveFont is defined when DynTFTFontSupport exists at project level
end;


procedure PrepareArrowButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTArrowButton: PDynTFTArrowButton; PropertyNamePrefix: string = '');
begin                                                                                                                                                            
  ADynTFTArrowButton^.ArrowDir := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ArrowDir'), CUndefinedArrow);
  ADynTFTArrowButton^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Color'), clRed);
  ADynTFTArrowButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PreparePanel(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTPanel: PDynTFTPanel);
begin
  ADynTFTPanel^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTPanel^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTPanel^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTPanel^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTPanel^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareCheckBox(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTCheckBox: PDynTFTCheckBox);
begin
  ADynTFTCheckBox^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTCheckBox^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTCheckBox^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTCheckBox^.Checked := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Checked') = 'True';
  ADynTFTCheckBox^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTCheckBox^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareScrollBar(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTScrollBar: PDynTFTScrollBar; PropertyNamePrefix: string = '');
begin
  UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTScrollBar^.BtnInc^.BaseProps);
  UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTScrollBar^.BtnDec^.BaseProps);
  UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTScrollBar^.BtnScroll^.BaseProps);

  if ADynTFTScrollBar^.BaseProps.Enabled = CENABLED then
    ADynTFTScrollBar^.BtnScroll^.Color := CL_DynTFTScrollBar_PanelBackground
  else
    ADynTFTScrollBar^.BtnScroll^.Color := CL_DynTFTScrollBar_DisabledBackground;

  ADynTFTScrollBar^.BtnScroll^.ArrowDir := CNoArrow;
  ADynTFTScrollBar^.BtnScroll^.BaseProps.CompState := CDISABLEPRESSING;

  ADynTFTScrollBar^.BtnInc^.Color := CL_DynTFTArrowButton_Background;
  ADynTFTScrollBar^.BtnDec^.Color := CL_DynTFTArrowButton_Background;

  ADynTFTScrollBar^.Min := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Min'), 0);
  ADynTFTScrollBar^.Max := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Max'), 10);
  ADynTFTScrollBar^.Position := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Position'), 0);
  ADynTFTScrollBar^.Direction := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Direction'), CScrollBarHorizDir);

  ADynTFTScrollBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTScrollBar^.BtnScroll^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTScrollBar^.BtnInc^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTScrollBar^.BtnDec^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareItems(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTItems: PDynTFTItems; PropertyNamePrefix: string = '');
var
  i: Integer;
begin
  ADynTFTItems^.BaseProps.Top := 1;

  Items_Content.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Strings');
  Items_Visibility.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ItemsVisible');
  Items_Enabling.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ItemsEnabled');

  for i := Items_Visibility.Count to Items_Content.Count - 1 do
    Items_Visibility.Add('1');

  for i := Items_Enabling.Count to Items_Content.Count - 1 do
    Items_Enabling.Add('1');

  ADynTFTItems^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Font_Color'), clAqua);
  ADynTFTItems^.BackgroundColor := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'BackgroundColor'), clRed);

  ADynTFTItems^.ItemHeight := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ItemHeight'), 16);
  ADynTFTItems^.ItemIndex := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ItemIndex'), -1);
  ADynTFTItems^.FirstDisplayablePosition := 0;
  ADynTFTItems^.Count := Items_Content.Count;
  ADynTFTItems^.TotalVisibleCount := GetTotalComponentVisibleCount;
  ADynTFTItems^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTItems^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ActiveFont')));
  ADynTFTItems^.VisibleIcons := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'VisibleIcons') = 'True';
end;                                                                                            


procedure PrepareListBox(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTListBox: PDynTFTListBox);
begin
  ADynTFTListBox^.Items^.BaseProps.Left := 1;
  ADynTFTListBox^.Items^.BaseProps.Width := ADynTFTListBox^.BaseProps.Width - CScrollBarArrBtnWidthHeight - 1;
  ADynTFTListBox^.Items^.BaseProps.Height := ADynTFTListBox^.BaseProps.Height - 2;

  ADynTFTListBox^.VertScrollBar^.BaseProps.Left := ADynTFTListBox^.BaseProps.Width - CScrollBarArrBtnWidthHeight + 1;
  ADynTFTListBox^.VertScrollBar^.BaseProps.Top := 1;
  ADynTFTListBox^.VertScrollBar^.BaseProps.Width := CScrollBarArrBtnWidthHeight - 2;
  ADynTFTListBox^.VertScrollBar^.BaseProps.Height := ADynTFTListBox^.BaseProps.Height - 2;

  ADynTFTListBox^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTListBox^.Items^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTListBox^.VertScrollBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareLabel(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTLabel: PDynTFTLabel);
begin
  ADynTFTLabel^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTLabel^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTLabel^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTLabel^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTLabel^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareRadioButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTRadioButton: PDynTFTRadioButton; SkipSettingCheckedAndCaption: Boolean = False);
begin
  if not SkipSettingCheckedAndCaption then
  begin
    ADynTFTRadioButton^.Checked := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Checked') = 'True';
    ADynTFTRadioButton^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  end;
  
  ADynTFTRadioButton^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTRadioButton^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTRadioButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTRadioButton^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareRadioGroup(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTRadioGroup: PDynTFTRadioGroup);
begin
  ADynTFTRadioGroup^.ButtonCount := 0;
  ADynTFTRadioGroup^.ItemIndex := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ItemIndex'), -1);
  ADynTFTRadioGroup^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');
  ADynTFTRadioGroup^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTRadioGroup^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTRadioGroup^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTRadioGroup^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareTabButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTTabButton: PDynTFTTabButton; SkipSettingCaption: Boolean = False);
begin
  if not SkipSettingCaption then
    ADynTFTTabButton^.Caption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Caption');

  ADynTFTTabButton^.SelectedColor := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'SelectedColor'), clRed);
  ADynTFTTabButton^.UnselectedColor := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'UnselectedColor'), clFuchsia);
  ADynTFTTabButton^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTTabButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTTabButton^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PreparePageControl(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTPageControl: PDynTFTPageControl);
begin
  ADynTFTPageControl^.PageCount := 0;
  ADynTFTPageControl^.ActiveIndex := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveIndex'), -1);
  ADynTFTPageControl^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareEdit(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTEdit: PDynTFTEdit; PropertyNamePrefix: string = ''; SkipSettingReadonly: Boolean = False);
begin
  ADynTFTEdit^.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Text');
  ADynTFTEdit^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Color'), clRed);
  ADynTFTEdit^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Font_Color'), clAqua);
  ADynTFTEdit^.PasswordText := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'PasswordText') = 'True';

  ADynTFTEdit^.FirstDispChIndex := 0; 

  if not SkipSettingReadonly then
    ADynTFTEdit^.Readonly := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Readonly') = 'True';

  ADynTFTEdit^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTEdit^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'ActiveFont'))); 
end;


procedure PrepareKeyButton(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTKeyButton: PDynTFTKeyButton);
begin
  ADynTFTKeyButton^.UpCaption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'UpCaption');
  ADynTFTKeyButton^.DownCaption := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'DownCaption');
  ADynTFTKeyButton^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTKeyButton^.Font_Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Font_Color'), clAqua);
  ADynTFTKeyButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTKeyButton^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareVirtualKeyboard(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTVirtualKeyboard: PDynTFTVirtualKeyboard);
begin
  ADynTFTVirtualKeyboard^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clRed);
  ADynTFTVirtualKeyboard^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareComboBox(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; AComboBox: PDynTFTComboBox);
begin
  AComboBox^.Editable := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Editable') = 'True';
  AComboBox^.DroppedDown := False;

  PrepareEdit(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, AComboBox^.Edit, 'Edit^.', True);
  AComboBox^.Edit^.BaseProps.ComponentType := DynTFTGetEditComponentType;
  AComboBox^.Edit^.BaseProps.Left := 0;
  AComboBox^.Edit^.BaseProps.Top := 0;
  AComboBox^.Edit^.BaseProps.Height := AComboBox^.BaseProps.Height;
  AComboBox^.Edit^.BaseProps.Width := AComboBox^.BaseProps.Width - AComboBox^.BaseProps.Height;

  PrepareArrowButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AComboBox^.ArrowButton, 'ArrowButton^.');
  AComboBox^.ArrowButton^.BaseProps.ComponentType := DynTFTGetArrowButtonComponentType;
  AComboBox^.ArrowButton^.BaseProps.Left := AComboBox^.BaseProps.Left + AComboBox^.BaseProps.Width - AComboBox^.BaseProps.Height + 1;
  AComboBox^.ArrowButton^.BaseProps.Top := 0;
  AComboBox^.ArrowButton^.BaseProps.Height := AComboBox^.BaseProps.Height {- 1};
  AComboBox^.ArrowButton^.BaseProps.Width := AComboBox^.BaseProps.Height - 1;

  PrepareItems(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, AComboBox^.ListBox^.Items, 'ListBox^.Items^.');

  AComboBox^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  AComboBox^.Edit^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  AComboBox^.ArrowButton^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  AComboBox^.ListBox^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  AComboBox^.ListBox^.Items^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareTrackBar(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTTrackBar: PDynTFTTrackBar; PropertyNamePrefix: string = '');
begin
  UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTTrackBar^.BtnTrack^.BaseProps);

  if ADynTFTTrackBar^.BaseProps.Enabled = CENABLED then
    ADynTFTTrackBar^.BtnTrack^.Color := CL_DynTFTScrollBar_PanelBackground
  else
    ADynTFTTrackBar^.BtnTrack^.Color := CL_DynTFTScrollBar_DisabledBackground;

  ADynTFTTrackBar^.BtnTrack^.ArrowDir := CNoArrow;
  ADynTFTTrackBar^.BtnTrack^.BaseProps.CompState := CDISABLEPRESSING;

  ADynTFTTrackBar^.Min := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Min'), 0);
  ADynTFTTrackBar^.Max := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Max'), 10);
  ADynTFTTrackBar^.Position := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Position'), 0);
  ADynTFTTrackBar^.Orientation := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'Orientation'), CTrackBarHorizDir);
  ADynTFTTrackBar^.TickMode := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, PropertyNamePrefix + 'TickMode'), CTrackBarComputeEveryTick);
  ADynTFTTrackBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareProgressBar(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; ADynTFTProgressBar: PDynTFTProgressBar);
begin
  ADynTFTProgressBar^.Color := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Color'), clAqua);
  ADynTFTProgressBar^.BackgroundColor := GetColorConstByNameFromAllColorConsts(ColorConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'BackgroundColor'), clRed);

  ADynTFTProgressBar^.Min := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Min'), 0);
  ADynTFTProgressBar^.Max := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Max'), 10);
  ADynTFTProgressBar^.Position := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Position'), 0);

  ADynTFTProgressBar^.Orientation := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Orientation'), CProgressBarHorizDir);

  ADynTFTProgressBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
end;


procedure PrepareMessageBox(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; AMessageBox: PDynTFTMessageBox);
begin
  //AMessageBox^.ActiveFont := PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont')));
end;


procedure PrepareVirtualTable(var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr; ADynTFTVirtualTable: PDynTFTVirtualTable);
var
  i: Integer;
  LocalHeaderItems: TStringList;
  ColumnWidths: TStringList;
  TempListBox: PDynTFTListBox;
  TempPanel: PDynTFTPanel;
begin
  ADynTFTVirtualTable^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTVirtualTable^.VertScrollBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);
  ADynTFTVirtualTable^.HorizScrollBar^.BaseProps.Enabled := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Enabled'), 1);

  TempListBox := PDynTFTListBox(ADynTFTVirtualTable^.Columns^.Content[0]);
  PrepareItems(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, TempListBox^.Items, 'Items^.');

  LocalHeaderItems := TStringList.Create;
  try
    LocalHeaderItems.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'HeaderCaptions');

    ////////////////////////////////  a similar for loop must be defined in schema file at component initialization / implementation, to actually create the columns
    for i := 1 to LocalHeaderItems.Count - 1 do //starts at 1, because there is already a first column
    begin
      TempListBox := DynTFTAddColumnToVirtualTable(ADynTFTVirtualTable);
      PrepareItems(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, TempListBox^.Items, 'Items^.');
    end;

    ColumnWidths := TStringList.Create;
    try
      ColumnWidths.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ColumnWidths');

      for i := 0 to LocalHeaderItems.Count - 1 do
      begin
        TempPanel := PDynTFTPanel(ADynTFTVirtualTable^.HeaderItems.Content^[i]);
        TempPanel.Caption := LocalHeaderItems.Strings[i];

        try
          TempPanel.BaseProps.Width := StrToIntDef(ColumnWidths.Strings[i], 7);
        except //there may be AV, if the user did not define all column widths
          TempPanel.BaseProps.Width := 120;
          TempPanel.Caption := 'Undefined width';
        end;
      end;
    finally
      ColumnWidths.Free;
    end;
  finally
    LocalHeaderItems.Free;
  end;
end;


///========================


procedure TDrawDynTFTComponentProc_Button(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTButton: PDynTFTButton;
begin
  New(ADynTFTButton);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTButton^.BaseProps);
    PrepareButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTButton);
    DynTFTDrawButton(ADynTFTButton, True);
  finally
    Dispose(ADynTFTButton);
  end;
end;


procedure TDrawDynTFTComponentProc_ArrowButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTArrowButton: PDynTFTArrowButton;
begin
  New(ADynTFTArrowButton);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTArrowButton^.BaseProps);
    PrepareArrowButton(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTArrowButton);
    DynTFTDrawArrowButton(ADynTFTArrowButton, True);
  finally
    Dispose(ADynTFTArrowButton);
  end;
end;


procedure TDrawDynTFTComponentProc_Panel(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTPanel: PDynTFTPanel;
begin
  New(ADynTFTPanel);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTPanel^.BaseProps);
    PreparePanel(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTPanel);
    DynTFTDrawPanel(ADynTFTPanel, True);
  finally
    Dispose(ADynTFTPanel);
  end;
end;


procedure TDrawDynTFTComponentProc_CheckBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTCheckBox: PDynTFTCheckBox;
begin
  New(ADynTFTCheckBox);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTCheckBox^.BaseProps);
    PrepareCheckBox(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTCheckBox);
    DynTFTDrawCheckBox(ADynTFTCheckBox, True);
  finally
    Dispose(ADynTFTCheckBox);
  end;
end;


procedure TDrawDynTFTComponentProc_ScrollBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTScrollBar: PDynTFTScrollBar;
begin
  New(ADynTFTScrollBar);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTScrollBar^.BaseProps);

    New(ADynTFTScrollBar^.BtnInc);
    New(ADynTFTScrollBar^.BtnDec);
    New(ADynTFTScrollBar^.BtnScroll);
    try
      PrepareScrollBar(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTScrollBar);
      DynTFTDrawScrollBar(ADynTFTScrollBar, True);
    finally
      Dispose(ADynTFTScrollBar^.BtnInc);
      Dispose(ADynTFTScrollBar^.BtnDec);
      Dispose(ADynTFTScrollBar^.BtnScroll);
    end;
  finally
    Dispose(ADynTFTScrollBar);
  end;
end;


procedure TDrawDynTFTComponentProc_Items(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTItems: PDynTFTItems;
begin
  New(ADynTFTItems);
  Items_Content := TStringList.Create;
  Items_Visibility := TStringList.Create;
  Items_Enabling := TStringList.Create;
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTItems^.BaseProps);
    PrepareItems(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTItems);

    UpdateComponentPropertyByName(PropertiesOrEvents, 'Count', IntToStr(ADynTFTItems^.Count));
    UpdateComponentPropertyByName(PropertiesOrEvents, 'TotalVisibleCount', IntToStr(ADynTFTItems^.TotalVisibleCount));

    New(ADynTFTItems^.OnGetItem);
    New(ADynTFTItems^.OnGetItemVisibility);
    New(ADynTFTItems^.OnDrawIcon);
    try
      ADynTFTItems^.OnGetItem^ := ItemsOnGetItem;
      ADynTFTItems^.OnGetItemVisibility^ := ItemsOnGetItemVisibility;
      ADynTFTItems^.OnDrawIcon^ := ItemsOnDrawIcon;
      DynTFTDrawitems(ADynTFTItems, True);
    finally
      Dispose(ADynTFTItems^.OnGetItem);
      Dispose(ADynTFTItems^.OnGetItemVisibility);
      Dispose(ADynTFTItems^.OnDrawIcon);
    end;
  finally
    Dispose(ADynTFTItems);
    Items_Content.Free;
    Items_Visibility.Free;
    Items_Enabling.Free;
  end;
end;


procedure TDrawDynTFTComponentProc_ListBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTListBox: PDynTFTListBox;
begin
  New(ADynTFTListBox);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTListBox^.BaseProps);

    New(ADynTFTListBox^.Items);
    New(ADynTFTListBox^.VertScrollBar);
    try
      New(ADynTFTListBox^.VertScrollBar^.BtnInc);
      New(ADynTFTListBox^.VertScrollBar^.BtnDec);
      New(ADynTFTListBox^.VertScrollBar^.BtnScroll);
    
      Items_Content := TStringList.Create;
      Items_Visibility := TStringList.Create;
      Items_Enabling := TStringList.Create;
      try
        UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTListBox^.Items^.BaseProps);
        UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTListBox^.VertScrollBar^.BaseProps);
        ADynTFTListBox^.Items^.BaseProps.ComponentType := DynTFTGetItemsComponentType;
        ADynTFTListBox^.VertScrollBar^.BaseProps.ComponentType := DynTFTGetScrollBarComponentType;
        
        PrepareItems(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTListBox^.Items, 'Items^.');

        UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTListBox^.VertScrollBar^.BtnInc^.BaseProps);
        UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTListBox^.VertScrollBar^.BtnDec^.BaseProps);
        UpdateBasePropertiesToAnArrowButton(PropertiesOrEvents, ADynTFTListBox^.VertScrollBar^.BtnScroll^.BaseProps);

        if ADynTFTListBox^.VertScrollBar^.BaseProps.Enabled = CENABLED then
          ADynTFTListBox^.VertScrollBar^.BtnScroll^.Color := CL_DynTFTScrollBar_PanelBackground
        else
          ADynTFTListBox^.VertScrollBar^.BtnScroll^.Color := CL_DynTFTScrollBar_DisabledBackground;

        ADynTFTListBox^.VertScrollBar^.BtnScroll^.ArrowDir := CNoArrow;
        ADynTFTListBox^.VertScrollBar^.BtnScroll^.BaseProps.CompState := CDISABLEPRESSING;

        ADynTFTListBox^.VertScrollBar^.BtnInc^.Color := CL_DynTFTArrowButton_Background;
        ADynTFTListBox^.VertScrollBar^.BtnDec^.Color := CL_DynTFTArrowButton_Background;

        ADynTFTListBox^.VertScrollBar^.Min := 0;
        ADynTFTListBox^.VertScrollBar^.Max := 10;
        ADynTFTListBox^.VertScrollBar^.Position := 0;
        ADynTFTListBox^.VertScrollBar^.Direction := CScrollBarVertDir;


        PrepareListBox(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTListBox);

        UpdateComponentPropertyByName(PropertiesOrEvents, 'Items^.Count', IntToStr(ADynTFTListBox^.Items^.Count));
        UpdateComponentPropertyByName(PropertiesOrEvents, 'Items^.TotalVisibleCount', IntToStr(ADynTFTListBox^.Items^.TotalVisibleCount));

        New(ADynTFTListBox^.Items^.OnGetItem);
        New(ADynTFTListBox^.Items^.OnGetItemVisibility);
        New(ADynTFTListBox^.Items^.OnDrawIcon);
        try
          ADynTFTListBox^.Items^.OnGetItem^ := ItemsOnGetItem;
          ADynTFTListBox^.Items^.OnGetItemVisibility^ := ItemsOnGetItemVisibility;
          ADynTFTListBox^.Items^.OnDrawIcon^ := ItemsOnDrawIcon;
          DynTFTDrawitems(ADynTFTListBox^.Items, True);
        finally
          Dispose(ADynTFTListBox^.Items^.OnGetItem);
          Dispose(ADynTFTListBox^.Items^.OnGetItemVisibility);
          Dispose(ADynTFTListBox^.Items^.OnDrawIcon);
        end;

        DynTFTDrawScrollBar(ADynTFTListBox^.VertScrollBar, True);
        DynTFTDrawListBox(ADynTFTListBox, True);
      finally
        Dispose(ADynTFTListBox^.VertScrollBar^.BtnInc);
        Dispose(ADynTFTListBox^.VertScrollBar^.BtnDec);
        Dispose(ADynTFTListBox^.VertScrollBar^.BtnScroll);
      end;
    finally
      Dispose(ADynTFTListBox^.Items);
      Dispose(ADynTFTListBox^.VertScrollBar);
      Items_Content.Free;
      Items_Visibility.Free;
      Items_Enabling.Free;
    end;
  finally
    Dispose(ADynTFTListBox);
  end;
end;


procedure TDrawDynTFTComponentProc_Label(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTLabel: PDynTFTLabel;
begin
  New(ADynTFTLabel);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTLabel^.BaseProps);
    PrepareLabel(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTLabel);
    DynTFTDrawLabel(ADynTFTLabel, True);
  finally
    Dispose(ADynTFTLabel);
  end;
end;


procedure TDrawDynTFTComponentProc_RadioButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTRadioButton: PDynTFTRadioButton;
begin
  New(ADynTFTRadioButton);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTRadioButton^.BaseProps);
    PrepareRadioButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTRadioButton);
    DynTFTDrawRadioButton(ADynTFTRadioButton, True);
  finally
    Dispose(ADynTFTRadioButton);
  end;
end;


procedure TDrawDynTFTComponentProc_RadioGroup(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTRadioGroup: PDynTFTRadioGroup;
  i: Integer;
  AButtonCaptionsList: TStringList;
  AButton: PDynTFTRadioButton;
  GrpCaptionHeight, ButtonIndent, MinButtonHeight, ButtonYOffset: Integer;
  TempWidth, TempHeight: Word;
begin
  New(ADynTFTRadioGroup);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTRadioGroup^.BaseProps);
    PrepareRadioGroup(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTRadioGroup);

    AButtonCaptionsList := TStringList.Create;
    try
      DynTFT_Set_Font(ADynTFTRadioGroup^.ActiveFont, CL_LIME, FO_HORIZONTAL);
      AButtonCaptionsList.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Items');

      GetTextWidthAndHeight(ADynTFTRadioGroup^.Caption, TempWidth, TempHeight);
      GrpCaptionHeight := TempHeight;
      GetTextWidthAndHeight('fpW', TempWidth, TempHeight);

      ButtonIndent := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ButtonIndent'), 5);
      MinButtonHeight := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'MinButtonHeight'), TempHeight);
      ButtonYOffset := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ButtonYOffset'), 0);

      //do not manually set  ADynTFTRadioGroup^.ButtonCount, because it is incremented by DynTFTAddRadioButtonToRadioGroup !
      for i := 0 to AButtonCaptionsList.Count - 1 do
      begin
        New(AButton);
        New(AButton^.OnOwnerInternalMouseDown);
        New(AButton^.OnOwnerInternalMouseMove);
        New(AButton^.OnOwnerInternalMouseUp);
        New(AButton^.OnOwnerInternalBeforeDestroy);

        UpdateBaseProperties(APanel, PropertiesOrEvents, AButton^.BaseProps);
        AButton^.BaseProps.ComponentType := DynTFTGetRadioButtonComponentType;

        PrepareRadioButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, AButton, True);
        DynTFTAddRadioButtonToRadioGroup(ADynTFTRadioGroup, AButton);
        AButton^.Caption := AButtonCaptionsList.Strings[i];

        AButton^.BaseProps.Height := MinButtonHeight;

        GetTextWidthAndHeight(AButton^.Caption, TempWidth, TempHeight);
        AButton^.BaseProps.Width := TempWidth;

        AButton^.BaseProps.Left := ButtonIndent;
        AButton^.BaseProps.Top := GrpCaptionHeight + i * MinButtonHeight + ButtonYOffset;
      end;

      UpdateComponentPropertyByName(PropertiesOrEvents, 'ItemHeight', IntToStr(MinButtonHeight));
      UpdateComponentPropertyByName(PropertiesOrEvents, 'ButtonCount', IntToStr(AButtonCaptionsList.Count));

      try
        DynTFTDrawRadioGroup(ADynTFTRadioGroup, True);
      finally
        for i := 0 to ADynTFTRadioGroup^.ButtonCount - 1 do
        begin
          Dispose(ADynTFTRadioGroup^.Buttons[i]);
          New(ADynTFTRadioGroup^.Buttons[i]^.OnOwnerInternalMouseDown);
          New(ADynTFTRadioGroup^.Buttons[i]^.OnOwnerInternalMouseMove);
          New(ADynTFTRadioGroup^.Buttons[i]^.OnOwnerInternalMouseUp);
          New(ADynTFTRadioGroup^.Buttons[i]^.OnOwnerInternalBeforeDestroy);
        end;
      end;
    finally
      AButtonCaptionsList.Free;
    end;
  finally
    Dispose(ADynTFTRadioGroup);
  end;
end;


procedure TDrawDynTFTComponentProc_TabButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTTabButton: PDynTFTTabButton;
begin
  New(ADynTFTTabButton);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTTabButton^.BaseProps);
    PrepareTabButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTTabButton);
    DynTFTDrawTabButton(ADynTFTTabButton, True);
  finally
    Dispose(ADynTFTTabButton);
  end;
end;


procedure TDrawDynTFTComponentProc_PageControl(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTPageControl: PDynTFTPageControl;
  i: Integer;
  AButtonCaptionsList: TStringList;
  AButton: PDynTFTTabButton;
  ButtonHeight, MaxButtonWidth, TotalWidth, MinButtonWidth: Integer;
  ArrangementIsVertical: Boolean;
  AllButtonWidths, AllButtonLefts: string;
  HorizontalTextSpacing: Integer;
  TempWidth, TempHeight: Word;
begin
  New(ADynTFTPageControl);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTPageControl^.BaseProps);
    PreparePageControl(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTPageControl);

    DynTFT_Set_Font(PByte(FontPropertyValueToSFont(AFontSettings, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ActiveFont'))), CL_LIME, FO_HORIZONTAL);

    AButtonCaptionsList := TStringList.Create;
    try
      AButtonCaptionsList.Text := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Items');
      ArrangementIsVertical := UpperCase(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'TabArrangement')) = UpperCase('CVertical');

      GetTextWidthAndHeight('fpW', TempWidth, TempHeight);
      ButtonHeight := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ButtonHeight'), TempHeight) + 2;
      MinButtonWidth := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'MinButtonWidth'), 10);
      HorizontalTextSpacing := StrToIntDef(GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'HorizontalTextSpacing'), 0);

      MaxButtonWidth := 0;
      if ArrangementIsVertical then
      begin
        for i := 0 to AButtonCaptionsList.Count - 1 do
        begin
          GetTextWidthAndHeight(AButtonCaptionsList.Strings[i], TempWidth, TempHeight);

          if MaxButtonWidth < TempWidth then
            MaxButtonWidth := TempWidth;
        end;
      end;
      
      Inc(MaxButtonWidth, 8 + HorizontalTextSpacing);
      TotalWidth := 0;

      AllButtonWidths := '';
      AllButtonLefts := '';
      //do not manually set  ADynTFTPageControl^.PageCount !
      for i := 0 to AButtonCaptionsList.Count - 1 do
      begin
        New(AButton);
        New(AButton^.OnOwnerInternalMouseDown);
        New(AButton^.OnOwnerInternalMouseMove);
        New(AButton^.OnOwnerInternalMouseUp);
        New(AButton^.OnOwnerInternalBeforeDestroy);

        UpdateBaseProperties(APanel, PropertiesOrEvents, AButton^.BaseProps);
        AButton^.BaseProps.ComponentType := DynTFTGetTabButtonComponentType;

        DynTFTAddTabButtonToPageControl(ADynTFTPageControl, AButton);
        PrepareTabButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, AButton, True);
        AButton^.Caption := AButtonCaptionsList.Strings[i];

        AButton^.BaseProps.Height := ButtonHeight;

        if ArrangementIsVertical then
        begin
          AButton^.BaseProps.Left := 0;
          AButton^.BaseProps.Top := i * (AButton^.BaseProps.Height + 1);
          AButton^.BaseProps.Width := Max(MaxButtonWidth, MinButtonWidth);
        end
        else
        begin
          GetTextWidthAndHeight(AButton^.Caption, TempWidth, TempHeight);
          
          AButton^.BaseProps.Top := 0;
          AButton^.BaseProps.Width := Max(TempWidth + 4 + HorizontalTextSpacing, MinButtonWidth); //a usable minimum
          AButton^.BaseProps.Left := TotalWidth;
          Inc(TotalWidth, AButton^.BaseProps.Width + 1);
        end;

        AllButtonWidths := AllButtonWidths + IntToStr(AButton^.BaseProps.Width);
        AllButtonLefts := AllButtonLefts + IntToStr(AButton^.BaseProps.Left);
        if i < AButtonCaptionsList.Count - 1 then
        begin
          AllButtonWidths := AllButtonWidths + #13#10;
          AllButtonLefts :=AllButtonLefts + #13#10;
        end;
      end;

      UpdateComponentPropertyByName(PropertiesOrEvents, 'AllButtonWidths', AllButtonWidths);
      UpdateComponentPropertyByName(PropertiesOrEvents, 'AllButtonLefts', AllButtonLefts);
      UpdateComponentPropertyByName(PropertiesOrEvents, 'PageCount', IntToStr(AButtonCaptionsList.Count));

      try
        DynTFT_Set_Pen(CL_DynTFTTabButton_DarkEdge, 1);
        DynTFT_Set_Brush(1, CL_DynTFTTabButton_UnselectedBackground, 0, 0, 0, 0);
        DynTFT_Rectangle(0, 0, ADynTFTPageControl^.BaseProps.Width, ADynTFTPageControl^.BaseProps.Height);

        DynTFTDrawPageControl(ADynTFTPageControl, True);
      finally
        for i := 0 to ADynTFTPageControl^.PageCount - 1 do
        begin
          Dispose(ADynTFTPageControl^.TabButtons[i]);
          New(ADynTFTPageControl^.TabButtons[i]^.OnOwnerInternalMouseDown);
          New(ADynTFTPageControl^.TabButtons[i]^.OnOwnerInternalMouseMove);
          New(ADynTFTPageControl^.TabButtons[i]^.OnOwnerInternalMouseUp);
          New(ADynTFTPageControl^.TabButtons[i]^.OnOwnerInternalBeforeDestroy);
        end;
      end;
    finally
      AButtonCaptionsList.Free;
    end;
  finally
    Dispose(ADynTFTPageControl);
  end;
end;


procedure TDrawDynTFTComponentProc_Edit(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTEdit: PDynTFTEdit;
begin
  New(ADynTFTEdit);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTEdit^.BaseProps);
    PrepareEdit(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTEdit);
    ADynTFTEdit^.FirstDispChIndex := 1;
    DynTFTDrawEditWithoutCaret(ADynTFTEdit, True);
  finally
    Dispose(ADynTFTEdit);
  end;
end;


procedure TDrawDynTFTComponentProc_KeyButton(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTKeyButton: PDynTFTKeyButton;
begin
  New(ADynTFTKeyButton);
  try
    New(ADynTFTKeyButton^.OnGenerateDrawingUser);
    try
      ADynTFTKeyButton^.OnGenerateDrawingUser^ := nil;
      
      UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTKeyButton^.BaseProps);
      PrepareKeyButton(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTKeyButton);
      DynTFTDrawKeyButton(ADynTFTKeyButton, True);
    finally
      Dispose(ADynTFTKeyButton^.OnGenerateDrawingUser);
    end;
  finally
    Dispose(ADynTFTKeyButton);
  end;
end;


procedure TDrawDynTFTComponentProc_VirtualKeyboard(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTVirtualKeyboard: PDynTFTVirtualKeyboard;
  MemStream: TMemoryStream;
begin
  New(ADynTFTVirtualKeyboard);
  try
    New(ADynTFTVirtualKeyboard^.OnCharKey);
    New(ADynTFTVirtualKeyboard^.OnSpecialKey);
    try
      ADynTFTVirtualKeyboard^.OnCharKey^ := nil;
      ADynTFTVirtualKeyboard^.OnSpecialKey^ := nil;

      UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTVirtualKeyboard^.BaseProps);
      PrepareVirtualKeyboard(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTVirtualKeyboard);

      if (APanel.Tag = 0) and (ADynTFTVirtualKeyboard^.BaseProps.Width > 310) and (ADynTFTVirtualKeyboard^.BaseProps.Height > 180) then
      begin
        APanel.Tag := 1;

        MemStream := TMemoryStream.Create;
        try
          frmImg.imgVirtualKeyboard.Picture.Bitmap.SaveToStream(MemStream);
          FDynTFT_DrawBitmap_Callback(MemStream.Memory, MemStream.Size, 0, 0);
        finally
          MemStream.Free;
        end;
      end;                                                                             
    finally
      Dispose(ADynTFTVirtualKeyboard^.OnCharKey);
      Dispose(ADynTFTVirtualKeyboard^.OnSpecialKey);
    end;
  finally
    Dispose(ADynTFTVirtualKeyboard);
  end;
end;


procedure TDrawDynTFTComponentProc_ComboBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTComboBox: PDynTFTComboBox;
begin
  New(ADynTFTComboBox);
  try
    New(ADynTFTComboBox^.Edit);
    New(ADynTFTComboBox^.ArrowButton);
    New(ADynTFTComboBox^.ListBox);
    New(ADynTFTComboBox^.ListBox^.Items);
    try
      Items_Content := TStringList.Create;
      Items_Visibility := TStringList.Create;
      Items_Enabling := TStringList.Create;
      try
        UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTComboBox^.BaseProps);
        UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTComboBox^.ArrowButton^.BaseProps);
        UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTComboBox^.Edit^.BaseProps);
        PrepareComboBox(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTComboBox);

        UpdateComponentPropertyByName(PropertiesOrEvents, 'ListBox^.Items^.Count', IntToStr(ADynTFTComboBox^.ListBox^.Items^.Count));
        UpdateComponentPropertyByName(PropertiesOrEvents, 'ListBox^.Items^.TotalVisibleCount', IntToStr(ADynTFTComboBox^.ListBox^.Items^.TotalVisibleCount));

        DynTFTDrawComboBox(ADynTFTComboBox, True);
      finally
        Dispose(ADynTFTComboBox^.Edit);
        Dispose(ADynTFTComboBox^.ArrowButton);
        Dispose(ADynTFTComboBox^.ListBox^.Items);
        Dispose(ADynTFTComboBox^.ListBox);
      end;
    finally
      Items_Content.Free;
      Items_Visibility.Free;
      Items_Enabling.Free;
    end;
  finally
    Dispose(ADynTFTComboBox);
  end;
end;


procedure TDrawDynTFTComponentProc_TrackBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTTrackBar: PDynTFTTrackBar;
begin
  New(ADynTFTTrackBar);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTTrackBar^.BaseProps);

    New(ADynTFTTrackBar^.BtnTrack);
    try
      PrepareTrackBar(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTTrackBar);
      DynTFTDrawTrackBar(ADynTFTTrackBar, True);
    finally
      Dispose(ADynTFTTrackBar^.BtnTrack);
    end;
  finally
    Dispose(ADynTFTTrackBar);
  end;
end;


procedure TDrawDynTFTComponentProc_ProgressBar(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTProgressBar: PDynTFTProgressBar;
begin
  New(ADynTFTProgressBar);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTProgressBar^.BaseProps);

    PrepareProgressBar(PropertiesOrEvents, SchemaConstants, ColorConstants, ADynTFTProgressBar);
    DynTFTDrawProgressBar(ADynTFTProgressBar, True);
  finally
    Dispose(ADynTFTProgressBar);
  end;
end;


procedure TDrawDynTFTComponentProc_MessageBox(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  AMessageBox: PDynTFTMessageBox;
  MBMsg, MBTitle: string;
  TextWidth: Word;
  ButtonsType: Integer;
  MessageBox_Left: Integer;
  MessageBox_Top: Integer;
  TempWidth, TempHeight: Word;
begin
  New(AMessageBox);
  try
    UpdateBaseProperties(APanel, PropertiesOrEvents, AMessageBox^.BaseProps);
    PrepareMessageBox(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, AMessageBox);

    MBMsg := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Text');
    MBTitle := GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'Title');
    ButtonsType := GetConstantIntValueFromSchema(SchemaConstants, GetPropertyValueInPropertiesOrEventsByName(PropertiesOrEvents, 'ButtonsType'), CDynTFT_MB_OKCANCEL);

    DynTFT_Set_Font(@TFT_defaultFont, CL_BLACK, FO_HORIZONTAL);  //use @TFT_defaultFont for MessageBox, because in the real app, it is called with ShowMessageBox, which has no font
    GetTextWidthAndHeight(MBMsg, TempWidth, TempHeight);
    TextWidth := TempWidth;

    TextWidth := Max(170, TextWidth + 20);

    if TextWidth > APanel.Width then
      MessageBox_Left := 0
    else
      MessageBox_Left := (Integer(APanel.Width) - Integer(TextWidth)) shr 1;

    MessageBox_Top := (APanel.Height - 100) shr 1;  // 100 is the initial height

    AMessageBox^.BaseProps.Left := MessageBox_Left;
    AMessageBox^.BaseProps.Top := MessageBox_Top;
    AMessageBox^.BaseProps.Width := TextWidth;
    AMessageBox^.BaseProps.Height := 100; //see DynTFTShowMessageBox

    New(AMessageBox^.BtnOK);
    New(AMessageBox^.BtnCancel);
    try
      UpdateBaseProperties(APanel, PropertiesOrEvents, AMessageBox^.BtnOK^.BaseProps);
      UpdateBaseProperties(APanel, PropertiesOrEvents, AMessageBox^.BtnCancel^.BaseProps);

      AMessageBox^.BtnOK^.ActiveFont := @TFT_defaultFont;
      AMessageBox^.BtnCancel^.ActiveFont := @TFT_defaultFont;

      AMessageBox^.BtnOK^.Caption := 'OK';
      AMessageBox^.BtnOK^.Color := CL_DynTFTButton_Background;
      AMessageBox^.BtnOK^.Font_Color := CL_DynTFTButton_EnabledFont;

      AMessageBox^.BtnCancel^.Caption := 'Cancel';
      AMessageBox^.BtnCancel^.Color := CL_DynTFTButton_Background;
      AMessageBox^.BtnCancel^.Font_Color := CL_DynTFTButton_EnabledFont;

      AMessageBox^.BtnOK^.BaseProps.ComponentType := DynTFTGetButtonComponentType;
      AMessageBox^.BtnCancel^.BaseProps.ComponentType := DynTFTGetButtonComponentType;

      AMessageBox^.BtnOK.BaseProps.Left := AMessageBox^.BaseProps.Left + AMessageBox^.BaseProps.Width - 120;
      AMessageBox^.BtnOK.BaseProps.Top := AMessageBox^.BaseProps.Top + 70;
      AMessageBox^.BtnOK^.BaseProps.Width := 50;
      AMessageBox^.BtnOK^.BaseProps.Height := 20;

      AMessageBox^.BtnCancel.BaseProps.Left := AMessageBox^.BaseProps.Left + AMessageBox^.BaseProps.Width - 60;
      AMessageBox^.BtnCancel.BaseProps.Top := AMessageBox^.BaseProps.Top + 70;
      AMessageBox^.BtnCancel^.BaseProps.Width := 50;
      AMessageBox^.BtnCancel^.BaseProps.Height := 20;

      DynTFTPrepareMessageBoxContent(AMessageBox, MBMsg, MBTitle, ButtonsType);

      DynTFT_Set_Pen(CL_LIGHTGRAY, 1);
      DynTFT_Set_Brush(1, clCream, 0, 0, 0, 0);
      DynTFT_Rectangle(0, 0, APanel.Width - 1, APanel.Height - 1);

      DynTFTDrawMessageBox(AMessageBox, True);
    finally
      Dispose(AMessageBox^.BtnOK);
      Dispose(AMessageBox^.BtnCancel);
    end;
  finally
    Dispose(AMessageBox);
  end;
end;


procedure TDrawDynTFTComponentProc_VirtualTable(APanel: TUIPanelBase; var PropertiesOrEvents: TDynTFTDesignPropertyArr; var SchemaConstants: TComponentConstantArr; var ColorConstants: TColorConstArr; var AFontSettings: TFontSettingsArr);
var
  ADynTFTVirtualTable: PDynTFTVirtualTable;
  TempListBox: PDynTFTListBox;
  i: Integer;
begin
  try
    ADynTFTVirtualTable := DynTFTVirtualTable_Create(0, 0, 0, 100, 170);
  except                                                                ////////////// eventually, remove this exception handling
    on E: Exception do
      raise Exception.Create(E.Message + '  on DynTFTVirtualTable_Create');
  end;

  try
    try
      UpdateBaseProperties(APanel, PropertiesOrEvents, ADynTFTVirtualTable^.BaseProps);

      Items_Content := TStringList.Create;
      Items_Visibility := TStringList.Create;
      Items_Enabling := TStringList.Create;
      try
        PrepareVirtualTable(PropertiesOrEvents, SchemaConstants, ColorConstants, AFontSettings, ADynTFTVirtualTable);
        UpdateComponentPropertyByName(PropertiesOrEvents, 'ColumnCount', IntToStr(ADynTFTVirtualTable^.Columns.Len));

        for i := 0 to ADynTFTVirtualTable^.Columns.Len - 1 do
        begin
          TempListBox := PDynTFTListBox(ADynTFTVirtualTable^.Columns.Content^[i]);

          UpdateComponentPropertyByName(PropertiesOrEvents, 'Items^.Count', IntToStr(TempListBox^.Items^.Count));
          UpdateComponentPropertyByName(PropertiesOrEvents, 'Items^.TotalVisibleCount', IntToStr(TempListBox^.Items^.TotalVisibleCount));
        end;

        for i := 0 to ADynTFTVirtualTable^.Columns.Len - 1 do
        begin
          TempListBox := PDynTFTListBox(ADynTFTVirtualTable^.Columns.Content^[i]);

          //TempListBox^.Items^.OnGetItem^ := ItemsOnGetItem;           //do not set Items^.OnGetItem^, because it is used internally by the VirtualTable
          TempListBox^.Items^.OnGetItemVisibility^ := ItemsOnGetItemVisibility;
          //TempListBox^.Items^.OnDrawIcon^ := ItemsOnDrawIcon;         //do not set Items^.OnDrawIcon^, because it is used internally by the VirtualTable

          ADynTFTVirtualTable^.OnGetItem^ := VTItemsOnGetItem;
          ADynTFTVirtualTable^.OnDrawIcon^ := VTItemsOnDrawIcon;
        end;

        DynTFTDrawVirtualTable(ADynTFTVirtualTable, True);
      finally
        Items_Content.Free;
        Items_Visibility.Free;
        Items_Enabling.Free;
      end;
    except
      on EE: Exception do
      begin
        try
          DynTFT_Set_Pen(CL_LIGHTGRAY, 1);
          DynTFT_Set_Brush(1, clCream, 0, 0, 0, 0);
          DynTFT_Rectangle(0, 0, APanel.Width - 1, APanel.Height - 1);

          DynTFT_Set_Font(PDynTFTListBox(ADynTFTVirtualTable^.Columns.Content^[0])^.Items^.ActiveFont, CL_GREEN, FO_HORIZONTAL);
          DynTFT_Write_Text('Ex on drawing VirtualTable', 2, 3);
        except
          on E: Exception do
            raise Exception.Create('VirtualTable is still in work (' + E.Message + ')  EE="' + EE.Message + '"');
        end;
      end;
    end;
  finally
    try
      DynTFTVirtualTable_Destroy(ADynTFTVirtualTable);
    except                                                            ////////////// eventually, remove this exception handling
      on E: Exception do
        raise Exception.Create(E.Message + '  on DynTFTVirtualTable_Destroy');
    end;
  end;
end;


procedure RegisterAllComponentsEvents; stdcall;
begin
  TFT_Init(1000, 1000);
  MM_Init;
  DynTFT_AssignDebugConsole(frmImg.memLog);

  DynTFTInitComponentTypeRegistration;
  DynTFTInitComponentContainers;
  UseTFTTrueColor := True;
  
  frmImg.Caption := 'DynTFT System Plugin';
  {$IFDEF FPC}
    frmImg.Caption := frmImg.Caption + ' (FP)';
  {$ENDIF}

  DynTFTRegisterButtonEvents;              // {$IFDEF IsDesktop}DynTFT_DebugConsole('Button type: ' + IntToStr(DynTFTGetButtonComponentType));{$ENDIF}
  DynTFTRegisterArrowButtonEvents;         // {$IFDEF IsDesktop}DynTFT_DebugConsole('ArrowButton type: ' + IntToStr(DynTFTGetArrowButtonComponentType));{$ENDIF}
  DynTFTRegisterPanelEvents;               // {$IFDEF IsDesktop}DynTFT_DebugConsole('Panel type: ' + IntToStr(DynTFTGetPanelComponentType));{$ENDIF}
  DynTFTRegisterCheckBoxEvents;            // {$IFDEF IsDesktop}DynTFT_DebugConsole('CheckBox type: ' + IntToStr(DynTFTGetCheckBoxComponentType));{$ENDIF}
  DynTFTRegisterScrollBarEvents;           // {$IFDEF IsDesktop}DynTFT_DebugConsole('ScrollBar type: ' + IntToStr(DynTFTGetScrollBarComponentType));{$ENDIF}
  DynTFTRegisterItemsEvents;               // {$IFDEF IsDesktop}DynTFT_DebugConsole('Items type: ' + IntToStr(DynTFTGetItemsComponentType));{$ENDIF}
  DynTFTRegisterListBoxEvents;             // {$IFDEF IsDesktop}DynTFT_DebugConsole('ListBox type: ' + IntToStr(DynTFTGetListBoxComponentType));{$ENDIF}
  DynTFTRegisterLabelEvents;               // {$IFDEF IsDesktop}DynTFT_DebugConsole('Label type: ' + IntToStr(DynTFTGetLabelComponentType));{$ENDIF}
  DynTFTRegisterRadioButtonEvents;         // {$IFDEF IsDesktop}DynTFT_DebugConsole('RadioButton type: ' + IntToStr(DynTFTGetRadioButtonComponentType));{$ENDIF}
  DynTFTRegisterRadioGroupEvents;          // {$IFDEF IsDesktop}DynTFT_DebugConsole('RadioGroup type: ' + IntToStr(DynTFTGetRadioGroupComponentType));{$ENDIF}
  DynTFTRegisterTabButtonEvents;           // {$IFDEF IsDesktop}DynTFT_DebugConsole('TabButton type: ' + IntToStr(DynTFTGetTabButtonComponentType));{$ENDIF}
  DynTFTRegisterPageControlEvents;         // {$IFDEF IsDesktop}DynTFT_DebugConsole('PageControl type: ' + IntToStr(DynTFTGetPageControlComponentType));{$ENDIF}
  DynTFTRegisterEditEvents;                // {$IFDEF IsDesktop}DynTFT_DebugConsole('Edit type: ' + IntToStr(DynTFTGetEditComponentType));{$ENDIF}
  DynTFTRegisterKeyButtonEvents;           // {$IFDEF IsDesktop}DynTFT_DebugConsole('KeyButton type: ' + IntToStr(DynTFTGetKeyButtonComponentType));{$ENDIF}
  DynTFTRegisterVirtualKeyboardEvents;     // {$IFDEF IsDesktop}DynTFT_DebugConsole('VirtualKeyboard type: ' + IntToStr(DynTFTGetVirtualKeyboardComponentType));{$ENDIF}
  DynTFTRegisterComboBoxEvents;            // {$IFDEF IsDesktop}DynTFT_DebugConsole('ComboBox type: ' + IntToStr(DynTFTGetComboBoxComponentType));{$ENDIF}
  DynTFTRegisterTrackBarEvents;            // {$IFDEF IsDesktop}DynTFT_DebugConsole('TrackBar type: ' + IntToStr(DynTFTGetTrackBarComponentType));{$ENDIF}
  DynTFTRegisterProgressBarEvents;         // {$IFDEF IsDesktop}DynTFT_DebugConsole('ProgressBar type: ' + IntToStr(DynTFTGetProgressBarComponentType));{$ENDIF}
  DynTFTRegisterMessageBoxEvents;          // {$IFDEF IsDesktop}DynTFT_DebugConsole('MessageBox type: ' + IntToStr(DynTFTGetMessageBoxComponentType));{$ENDIF}
  DynTFTRegisterVirtualTableEvents;        // {$IFDEF IsDesktop}DynTFT_DebugConsole('VirtualTable type: ' + IntToStr(DynTFTGetVirtualTableComponentType));{$ENDIF}

  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_Button);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_ArrowButton);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_Panel);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_CheckBox);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_ScrollBar);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_Items);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_ListBox);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_Label);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_RadioButton);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_RadioGroup);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_TabButton);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_PageControl);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_Edit);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_KeyButton);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_VirtualKeyboard);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_ComboBox);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_TrackBar);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_ProgressBar);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_MessageBox);
  RegisterCompDrawingProcedure(FCompDrawingProcedures, TDrawDynTFTComponentProc_VirtualTable);
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

  {$IFNDEF UserTFTCommands}
    raise Exception.Create('UserTFTCommands compiler directive is not defined. This will cause access violations when drawing.' + #13#10 +
                           ' It is required to set the drawing library to DynTFTCodeGen callbacks.');
  {$ENDIF}
end;


procedure DrawPDynTFTComponentOnPanel(var APanelBase: TUIPanelBase; APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings: TDynArrayRef; ASetPropertiesCallback: TSetPropertiesCallback); stdcall;
begin
  {$IFNDEF UserTFTCommands}
    raise Exception.Create('UserTFTCommands compiler directive is not defined. Please use DynTFTCodeGen callbacks.');   //UserTFTCommands should be defined at project level. Please rebuild the project after that.
  {$ENDIF}

  DrawPDynTFTComponentOnPanelBase(APanelBase, FCompDrawingProcedures, APropertiesOrEvents, ASchemaConstants, AColorConstants, AFontSettings, ASetPropertiesCallback);
end;


procedure GetDrawingProcedures(var ADrawingProcedures: TDrawDynTFTComponentProcArr);
var
  i: Integer;
begin
  SetLength(ADrawingProcedures, Length(FCompDrawingProcedures));
  for i := 0 to Length(FCompDrawingProcedures) - 1 do
    ADrawingProcedures[i] := FCompDrawingProcedures[i];
end;


initialization
  SetLength(FCompDrawingProcedures, 0);
  GCanvas := nil;
  
end.
