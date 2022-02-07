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

  //DynTFTCodeGen reads schema files to get the color count, defined for each component.
  //Those numbers (from schema files) have to be less than or equal to the following ColorCount constants.
  //It is also possible for a component to define its color constants, and the same time, that component to be missing from color themes.
  //For example DynTFTPageControl defines 3 color constants, but it won't be part of any color theme.
  //This means that the following const count does not have to match the value of LiveColorComponentCount constant from DynTFTColorThemeGenLiveColors.pas
  //However, the following color counts have to match the number of defined colors in <ComponentName>_<ThemeName>Colors.inc files from color theme folders.
  //The order, in which colors are defined in the following CL_All_<ComponentName>_Colors arrays, has to match the order in <ComponentName>_<ThemeName>Colors.inc files.
  
  CL_All_DynTFTArrowButton_ColorsCount = 6;
  CL_All_DynTFTButton_ColorsCount = 6;
  CL_All_DynTFTCheckBox_ColorsCount = 11;
  CL_All_DynTFTEdit_ColorsCount = 4;
  CL_All_DynTFTItems_ColorsCount = 8;
  CL_All_DynTFTKeyButton_ColorsCount = 7;
  CL_All_DynTFTLabel_ColorsCount = 3;
  CL_All_DynTFTListBox_ColorsCount = 1;
  CL_All_DynTFTPanel_ColorsCount = 6;
  CL_All_DynTFTProgressBar_ColorsCount = 4;
  CL_All_DynTFTRadioButton_ColorsCount = 5;
  CL_All_DynTFTRadioGroup_ColorsCount = 4;
  CL_All_DynTFTScreen_ColorsCount = 1;
  CL_All_DynTFTScrollBar_ColorsCount = 3;
  CL_All_DynTFTTabButton_ColorsCount = 6;
  CL_All_DynTFTPageControl_ColorsCount = 3;
  CL_All_DynTFTTrackBar_ColorsCount = 8;
  CL_All_DynTFTVirtualKeyboard_ColorsCount = 4;
  CL_All_DynTFTMessageBox_ColorsCount = 6;

var        
  {$I DynTFT_VarColors\DynTFTButton_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTArrowButton_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTPanel_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTCheckBox_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTScrollBar_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTItems_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTListBox_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTLabel_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTRadioButton_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTRadioGroup_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTTabButton_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTPageControl_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTEdit_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTKeyButton_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTVirtualKeyboard_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTTrackBar_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTProgressBar_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTScreen_VarColors.inc}
  {$I DynTFT_VarColors\DynTFTMessageBox_VarColors.inc}

  CL_All_DynTFTButton_Colors: array[0..CL_All_DynTFTButton_ColorsCount - 1] of ^TColor = (@CL_DynTFTButton_LightEdge, @CL_DynTFTButton_DarkEdge, @CL_DynTFTButton_FocusRectangle, @CL_DynTFTButton_Background, @CL_DynTFTButton_EnabledFont, @CL_DynTFTButton_DisabledFont);
  CL_All_DynTFTArrowButton_Colors: array[0..CL_All_DynTFTArrowButton_ColorsCount - 1] of ^TColor = (@CL_DynTFTArrowButton_LightEdge, @CL_DynTFTArrowButton_DarkEdge, @CL_DynTFTArrowButton_FocusRectangle, @CL_DynTFTArrowButton_Background, @CL_DynTFTArrowButton_EnabledArrow, @CL_DynTFTArrowButton_DisabledArrow);
  CL_All_DynTFTPanel_Colors: array[0..CL_All_DynTFTPanel_ColorsCount - 1] of ^TColor = (@CL_DynTFTPanel_LightEdge, @CL_DynTFTPanel_DarkEdge, @CL_DynTFTPanel_FocusRectangle, @CL_DynTFTPanel_Background, @CL_DynTFTPanel_EnabledFont, @CL_DynTFTPanel_DisabledFont);
  CL_All_DynTFTCheckBox_Colors: array[0..CL_All_DynTFTCheckBox_ColorsCount - 1] of ^TColor = (@CL_DynTFTCheckBox_LightEdge, @CL_DynTFTCheckBox_DarkEdge, @CL_DynTFTCheckBox_LightDarkEdge, @CL_DynTFTCheckBox_DarkDarkEdge, @CL_DynTFTCheckBox_EnabledBox, @CL_DynTFTCheckBox_DisabledBox, @CL_DynTFTCheckBox_FocusRectangle, @CL_DynTFTCheckBox_Background, @CL_DynTFTCheckBox_Tick, @CL_DynTFTCheckBox_EnabledFont, @CL_DynTFTCheckBox_DisabledFont);
  CL_All_DynTFTScrollBar_Colors: array[0..CL_All_DynTFTScrollBar_ColorsCount - 1] of ^TColor = (@CL_DynTFTScrollBar_EnabledBackground, @CL_DynTFTScrollBar_DisabledBackground, @CL_DynTFTScrollBar_PanelBackground);
  CL_All_DynTFTItems_Colors: array[0..CL_All_DynTFTItems_ColorsCount - 1] of ^TColor = (@CL_DynTFTItems_DarkEdge, @CL_DynTFTItems_FocusRectangle, @CL_DynTFTItems_UnFocusRectangle, @CL_DynTFTItems_EnabledFont, @CL_DynTFTItems_DisabledFont, @CL_DynTFTItems_Border, @CL_DynTFTItems_Background, @CL_DynTFTItems_SelectedItem);
  CL_All_DynTFTListBox_Colors: array[0..CL_All_DynTFTListBox_ColorsCount - 1] of ^TColor = (@CL_DynTFTListBox_Border);
  CL_All_DynTFTLabel_Colors: array[0..CL_All_DynTFTLabel_ColorsCount - 1] of ^TColor = (@CL_DynTFTLabel_Background, @CL_DynTFTLabel_EnabledFont, @CL_DynTFTLabel_DisabledFont);
  CL_All_DynTFTRadioButton_Colors: array[0..CL_All_DynTFTRadioButton_ColorsCount - 1] of ^TColor = (@CL_DynTFTRadioButton_Background, @CL_DynTFTRadioButton_EnabledFont, @CL_DynTFTRadioButton_DisabledFont, @CL_DynTFTRadioButton_ExteriorCircle, @CL_DynTFTRadioButton_Dot);
  CL_All_DynTFTRadioGroup_Colors: array[0..CL_All_DynTFTRadioGroup_ColorsCount - 1] of ^TColor = (@CL_DynTFTRadioGroup_Background, @CL_DynTFTRadioGroup_EnabledFont, @CL_DynTFTRadioGroup_DisabledFont, @CL_DynTFTRadioGroup_Border);
  CL_All_DynTFTTabButton_Colors: array[0..CL_All_DynTFTTabButton_ColorsCount - 1] of ^TColor = (@CL_DynTFTTabButton_LightEdge, @CL_DynTFTTabButton_DarkEdge, @CL_DynTFTTabButton_SelectedBackground, @CL_DynTFTTabButton_UnselectedBackground, @CL_DynTFTTabButton_EnabledFont, @CL_DynTFTTabButton_DisabledFont);
  CL_All_DynTFTPageControl_Colors: array[0..CL_All_DynTFTPageControl_ColorsCount - 1] of ^TColor = (@CL_DynTFTTabButton_SelectedBackground, @CL_DynTFTTabButton_UnselectedBackground, @CL_DynTFTTabButton_EnabledFont);
  CL_All_DynTFTEdit_Colors: array[0..CL_All_DynTFTEdit_ColorsCount - 1] of ^TColor = (@CL_DynTFTEdit_Border, @CL_DynTFTEdit_Background, @CL_DynTFTEdit_EnabledFont, @CL_DynTFTEdit_DisabledFont);
  CL_All_DynTFTKeyButton_Colors: array[0..CL_All_DynTFTKeyButton_ColorsCount - 1] of ^TColor = (@CL_DynTFTKeyButton_LightEdge, @CL_DynTFTKeyButton_DarkEdge, @CL_DynTFTKeyButton_FocusRectangle, @CL_DynTFTKeyButton_Background, @CL_DynTFTKeyButton_SpecialKeyActiveBackground, @CL_DynTFTKeyButton_EnabledFont, @CL_DynTFTKeyButton_DisabledFont);
  CL_All_DynTFTVirtualKeyboard_Colors: array[0..CL_All_DynTFTVirtualKeyboard_ColorsCount - 1] of ^TColor = (@CL_DynTFTVirtualKeyboard_Border, @CL_DynTFTVirtualKeyboard_Background, @CL_DynTFTVirtualKeyboard_EnabledKeyDrawing, @CL_DynTFTVirtualKeyboard_DisabledKeyDrawing);
  CL_All_DynTFTTrackBar_Colors: array[0..CL_All_DynTFTTrackBar_ColorsCount - 1] of ^TColor = (@CL_DynTFTTrackBar_EnabledBackground, @CL_DynTFTTrackBar_DisabledBackground, @CL_DynTFTTrackBar_PanelBackground, @CL_DynTFTTrackBar_EnabledNotch, @CL_DynTFTTrackBar_DisabledNotch, @CL_DynTFTTrackBar_EnabledTick, @CL_DynTFTTrackBar_DisabledTick, @CL_DynTFTTrackBar_LightEdge);
  CL_All_DynTFTProgressBar_Colors: array[0..CL_All_DynTFTProgressBar_ColorsCount - 1] of ^TColor = (@CL_DynTFTProgressBar_Progress, @CL_DynTFTProgressBar_Background, @CL_DynTFTProgressBar_LightEdge, @CL_DynTFTProgressBar_DarkEdge);
  CL_All_DynTFTScreen_Colors: array[0..CL_All_DynTFTScreen_ColorsCount - 1] of ^TColor = (@CL_DynTFTScreen_Background);
  CL_All_DynTFTMessageBox_Colors: array[0..CL_All_DynTFTMessageBox_ColorsCount - 1] of ^TColor = (@CL_DynTFTMessageBox_LightEdge, @CL_DynTFTMessageBox_DarkEdge, @CL_DynTFTMessageBox_Background, @CL_DynTFTMessageBox_TitleBar, @CL_DynTFTMessageBox_MessageFont, @CL_DynTFTMessageBox_TitleFont);

  CL_All_DynTFTButton_InitColors: array[0..CL_All_DynTFTButton_ColorsCount - 1] of TColor;
  CL_All_DynTFTArrowButton_InitColors: array[0..CL_All_DynTFTArrowButton_ColorsCount - 1] of TColor;
  CL_All_DynTFTPanel_InitColors: array[0..CL_All_DynTFTPanel_ColorsCount - 1] of TColor;
  CL_All_DynTFTCheckBox_InitColors: array[0..CL_All_DynTFTCheckBox_ColorsCount - 1] of TColor;
  CL_All_DynTFTScrollBar_InitColors: array[0..CL_All_DynTFTScrollBar_ColorsCount - 1] of TColor;
  CL_All_DynTFTItems_InitColors: array[0..CL_All_DynTFTItems_ColorsCount - 1] of TColor;
  CL_All_DynTFTListBox_InitColors: array[0..CL_All_DynTFTListBox_ColorsCount - 1] of TColor;
  CL_All_DynTFTLabel_InitColors: array[0..CL_All_DynTFTLabel_ColorsCount - 1] of TColor;
  CL_All_DynTFTRadioButton_InitColors: array[0..CL_All_DynTFTRadioButton_ColorsCount - 1] of TColor;
  CL_All_DynTFTRadioGroup_InitColors: array[0..CL_All_DynTFTRadioGroup_ColorsCount - 1] of TColor;
  CL_All_DynTFTTabButton_InitColors: array[0..CL_All_DynTFTTabButton_ColorsCount - 1] of TColor;
  CL_All_DynTFTPageControl_InitColors: array[0..CL_All_DynTFTPageControl_ColorsCount - 1] of TColor;
  CL_All_DynTFTEdit_InitColors: array[0..CL_All_DynTFTEdit_ColorsCount - 1] of TColor;
  CL_All_DynTFTKeyButton_InitColors: array[0..CL_All_DynTFTKeyButton_ColorsCount - 1] of TColor;
  CL_All_DynTFTVirtualKeyboard_InitColors: array[0..CL_All_DynTFTVirtualKeyboard_ColorsCount - 1] of TColor;
  CL_All_DynTFTTrackBar_InitColors: array[0..CL_All_DynTFTTrackBar_ColorsCount - 1] of TColor;
  CL_All_DynTFTProgressBar_InitColors: array[0..CL_All_DynTFTProgressBar_ColorsCount - 1] of TColor;
  CL_All_DynTFTScreen_InitColors: array[0..CL_All_DynTFTScreen_ColorsCount - 1] of TColor;
  CL_All_DynTFTMessageBox_InitColors: array[0..CL_All_DynTFTMessageBox_ColorsCount - 1] of TColor;
