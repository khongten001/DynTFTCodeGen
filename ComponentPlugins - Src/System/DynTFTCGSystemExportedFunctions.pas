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

unit DynTFTCGSystemExportedFunctions;

interface

uses
  DynTFTCodeGenSharedDataTypes;

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


implementation


uses
  SysUtils, Classes, Forms, ComponentIcons, DynTFTCodeGenImgForm,
  DynTFTPluginUtils, DynTFTColorThemeGenLiveColors, DynTFTSharedUtils,
  DynTFTUtils;

const
  CPluginName = 'System Components';  //the same string is used in color themes which target a specific plugin 


function GetPluginName(APluginName: Pointer): Integer; stdcall;
begin
  Result := SetPointedContentFromString(CPluginName, APluginName);
end;



procedure InitPlugin; stdcall;
begin
  Application.Initialize;
  Application.CreateForm(TfrmImg, frmImg);   //Having a form with console or extra component images, is not a requirement for all plugins.
  Application.CreateForm(TdmIcons, dmIcons); //Having a TImageList (or something similar) with component icons (24px x 24px, 24-bit color bitmaps) is a requirement for all plugins. It does not have to be on a DataModule.
                                             //Icons can be loaded from disk, received by other means or even created at runtime, but they have to be ready when DynTFTCodeGen calls GetComponentIcon.

end;


procedure DonePlugin; stdcall;
begin
  FreeAndNil(dmIcons);

  frmImg.Close;
  frmImg := nil;
end;



function GetListOfComponents(AListOfComponents: Pointer): Integer; stdcall;  //This list has to match the number of icons from dmIcons.imglstComponents in data module.
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

    ResultStr := AStringList.Text;      //CRLF separated component names
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


//'0' = use,  '1' = do not use
//this list has to match the number of components returned by GetListOfComponents
function GetListOfDoNotUseComponents(AListOfComponents: Pointer): Integer; stdcall;  //This list has to match the number of icons from dmIcons.imglstComponents in data module.
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


procedure GetComponentIcon(ComponentIndex: Integer; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64); stdcall;
begin
  GetComponentIconFromImageList(ComponentIndex, dmIcons.imglstComponents, ASetSizeCallback, AStreamID);  //24px x 24px, 24-bit color bitmaps
end;


function GetSchemaDirName(ADirName: Pointer): Integer; stdcall;  //expected to be relative to the dll, in the same folder
var
  ResultStr: string;
begin
  ResultStr := 'SystemSchemas';                //Schema file names are expected to be of "<ComponentName>.dynscm" format.
  Result := SetPointedContentFromString(ResultStr, ADirName);
end;


procedure UpdateLiveColorConstants(AComponentIndex, AColorIndex, ANewColor: Integer); stdcall;
begin
  LiveColors[AComponentIndex]^[AColorIndex]^ := ANewColor;
end;


procedure BackupLiveColorConstants; stdcall;
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
          raise Exception.Create('Exception on BackupLiveColorConstants at i = ' + IntToStr(i) + '  j = ' + IntToStr(j) + '   ' + E.Message);
      end;
    end;
end;


procedure RestoreColorConstantsFromBackup; stdcall;
var
  i, j: Integer;
begin
  for i := 0 to Length(BackupLiveColors) - 1 do
    for j := 0 to LiveColorsConstCount[i] - 1 do
      LiveColors[i]^[j]^ := BackupLiveColors[i]^[j];
end;


function CanUpdateLiveColorConstant(ComponentIndex, ColorIndex: Integer): Boolean; stdcall;
begin
  Result := True;
  if ComponentIndex > Length(LiveColors) - 1 then
  begin
    //DynTFT_DebugConsole('Can''t update LiveColors array. ComponentIndex is greater than array length - 1. ComponentIndex = ' + IntToStr(ComponentIndex) + '  Length = ' + IntToStr(Length(LiveColors)));
    Result := False;
    Exit;
  end;

  if ColorIndex > LiveColorsConstCount[ComponentIndex] - 1 then
  begin
    //DynTFT_DebugConsole('Can''t update LiveColors array. ColorIndex is greater than color constants count - 1. ColorIndex = ' + IntToStr(ColorIndex) + '  Count = ' + IntToStr(LiveColorsConstCount[ComponentIndex]));
    Result := False;
    Exit;
  end;
end;


function GetLiveColorsConstCountByComponent(ComponentIndex: Integer): Integer; stdcall;  //used by DynTFTCodeGen to display error messages if the number of colors does not match
begin                                                                                    
  if (ComponentIndex < 0) or (ComponentIndex > Length(LiveColors) - 1) then
    Result := -1
  else
    Result := LiveColorsConstCount[ComponentIndex];
end;


procedure DisplayDebugConsole(AMainFormHandle: THandle); stdcall;    //Right-click on DynTFTCodeGen's list of components and select "Display debug console..." to call this.
begin
  frmImg.Show;
end;

end.
