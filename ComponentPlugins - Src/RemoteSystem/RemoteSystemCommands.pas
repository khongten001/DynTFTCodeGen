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

unit RemoteSystemCommands;

interface

const
  CRecFieldSeparator = #1#3#4#1#4;
  CDrawingCmdFieldSeparator = #5#6;
  CRecFieldArrayItemSeparator = #4#5;

  CCGRM_PluginStartup = 'PluginStartup';
  CCGRM_PluginDone = 'PluginDone';
  CCGRM_GetComponentIconFromServer = 'GetComponentIconFromServer';


  //DynTFT system components:
  CCGRM_DrawPDynTFTComponentOnPanelPrefix = 'DrawPDynTFTComponentOnPanel_';

  CCGRM_DrawPDynTFTComponentOnPanel_Button = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'Button';
  CCGRM_DrawPDynTFTComponentOnPanel_ArrowButton = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'ArrowButton';
  CCGRM_DrawPDynTFTComponentOnPanel_Panel = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'Panel';
  CCGRM_DrawPDynTFTComponentOnPanel_CheckBox = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'CheckBox';
  CCGRM_DrawPDynTFTComponentOnPanel_ScrollBar = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'ScrollBar';
  CCGRM_DrawPDynTFTComponentOnPanel_Items = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'Items';
  CCGRM_DrawPDynTFTComponentOnPanel_ListBox = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'ListBox';
  CCGRM_DrawPDynTFTComponentOnPanel_Label = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'Label';
  CCGRM_DrawPDynTFTComponentOnPanel_RadioButton = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'RadioButton';
  CCGRM_DrawPDynTFTComponentOnPanel_RadioGroup = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'RadioGroup';
  CCGRM_DrawPDynTFTComponentOnPanel_TabButton = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'TabButton';
  CCGRM_DrawPDynTFTComponentOnPanel_PageControl = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'PageControl';
  CCGRM_DrawPDynTFTComponentOnPanel_Edit = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'Edit';
  CCGRM_DrawPDynTFTComponentOnPanel_KeyButton = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'KeyButton';
  CCGRM_DrawPDynTFTComponentOnPanel_VirtualKeyboard = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'VirtualKeyboard';
  CCGRM_DrawPDynTFTComponentOnPanel_ComboBox = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'ComboBox';
  CCGRM_DrawPDynTFTComponentOnPanel_TrackBar = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'TrackBar';
  CCGRM_DrawPDynTFTComponentOnPanel_ProgressBar = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'ProgressBar';
  CCGRM_DrawPDynTFTComponentOnPanel_MessageBox = CCGRM_DrawPDynTFTComponentOnPanelPrefix + 'MessageBox';

  //DynTFT drawing primitives
  CDPDynTFT_Set_Pen = '0';
  CDPDynTFT_Set_Brush = '1';
  CDPDynTFT_Set_Font = '2';
  CDPDynTFT_Write_Text = '3';
  CDPDynTFT_Line = '4';
  CDPDynTFT_H_Line = '5';
  CDPDynTFT_V_Line = '6';
  CDPDynTFT_Dot = '7';
  CDPDynTFT_Fill_Screen = '8';
  CDPDynTFT_Rectangle = '9';
  CDPDynTFT_Circle = 'A';
  CDPDynTFT_GetTextWidthAndHeight = 'B';
  CDPDynTFT_DrawBitmap = 'C';


  //Callback commands
  CCGRM_CallbackDraw = 'Draw';

implementation

end.
