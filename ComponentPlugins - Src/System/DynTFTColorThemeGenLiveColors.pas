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

unit DynTFTColorThemeGenLiveColors;

interface

uses
  DynTFTConsts, Graphics;

type
  TColorArray = array[0..0] of ^TColor;
  PColorArray = ^TColorArray;

  TBackupColorArray = array[0..0] of TColor;
  PBackupColorArray = ^TBackupColorArray;

const
  LiveColorComponentCount = 18; //this has to match the component count (number of .inc files) in DynTFTColorTheme.inc from a color theme
                                //not all components, exported by a plugin, will have "themed" color constants 
var
  //the order of the following array elements does not have to match the order of inc files in DynTFTColorTheme.inc from a color theme anymore, because the GetListOfLiveColorComponents function exports the list to DynTFTCodeGen.
  LiveColors: array[0..LiveColorComponentCount - 1] of PColorArray = (
    @CL_All_DynTFTArrowButton_Colors,
    @CL_All_DynTFTButton_Colors,
    @CL_All_DynTFTCheckBox_Colors,
    @CL_All_DynTFTEdit_Colors,
    @CL_All_DynTFTItems_Colors,
    @CL_All_DynTFTKeyButton_Colors,
    @CL_All_DynTFTLabel_Colors,
    @CL_All_DynTFTListBox_Colors,
    @CL_All_DynTFTMessageBox_Colors,
    @CL_All_DynTFTPanel_Colors,
    //@CL_All_DynTFTPageControl_Colors,
    @CL_All_DynTFTProgressBar_Colors,
    @CL_All_DynTFTRadioButton_Colors,
    @CL_All_DynTFTRadioGroup_Colors,
    @CL_All_DynTFTScreen_Colors,
    @CL_All_DynTFTScrollBar_Colors,
    @CL_All_DynTFTTabButton_Colors,
    @CL_All_DynTFTTrackBar_Colors,
    @CL_All_DynTFTVirtualKeyboard_Colors
  );

  BackupLiveColors: array[0..LiveColorComponentCount - 1] of PBackupColorArray = (
    @CL_All_DynTFTArrowButton_InitColors,
    @CL_All_DynTFTButton_InitColors,
    @CL_All_DynTFTCheckBox_InitColors,
    @CL_All_DynTFTEdit_InitColors,
    @CL_All_DynTFTItems_InitColors,
    @CL_All_DynTFTKeyButton_InitColors,
    @CL_All_DynTFTLabel_InitColors,
    @CL_All_DynTFTListBox_InitColors,
    @CL_All_DynTFTMessageBox_InitColors,
    @CL_All_DynTFTPanel_InitColors,
    //@CL_All_DynTFTPageControl_InitColors,
    @CL_All_DynTFTProgressBar_InitColors,
    @CL_All_DynTFTRadioButton_InitColors,
    @CL_All_DynTFTRadioGroup_InitColors,
    @CL_All_DynTFTScreen_InitColors,
    @CL_All_DynTFTScrollBar_InitColors,
    @CL_All_DynTFTTabButton_InitColors,
    @CL_All_DynTFTTrackBar_InitColors,
    @CL_All_DynTFTVirtualKeyboard_InitColors
  );

  LiveColorsConstCount: array[0..LiveColorComponentCount - 1] of Integer = (
    CL_All_DynTFTArrowButton_ColorsCount,
    CL_All_DynTFTButton_ColorsCount,
    CL_All_DynTFTCheckBox_ColorsCount,
    CL_All_DynTFTEdit_ColorsCount,
    CL_All_DynTFTItems_ColorsCount,
    CL_All_DynTFTKeyButton_ColorsCount,
    CL_All_DynTFTLabel_ColorsCount,
    CL_All_DynTFTListBox_ColorsCount,
    CL_All_DynTFTMessageBox_ColorsCount,
    //CL_All_DynTFTPageControl_ColorsCount,
    CL_All_DynTFTPanel_ColorsCount,
    CL_All_DynTFTProgressBar_ColorsCount,
    CL_All_DynTFTRadioButton_ColorsCount,
    CL_All_DynTFTRadioGroup_ColorsCount,
    CL_All_DynTFTScreen_ColorsCount,
    CL_All_DynTFTScrollBar_ColorsCount,
    CL_All_DynTFTTabButton_ColorsCount,
    CL_All_DynTFTTrackBar_ColorsCount,
    CL_All_DynTFTVirtualKeyboard_ColorsCount
  );
  
implementation

end.
