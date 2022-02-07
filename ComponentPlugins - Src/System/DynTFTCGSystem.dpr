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

library DynTFTCGSystem;


uses
  SysUtils,
  Classes,
  Graphics,
  Forms,
  ComponentIcons in 'ComponentIcons.pas' {dmIcons: TDataModule},
  DynTFTCodeGenImgForm in 'DynTFTCodeGenImgForm.pas' {frmImg},
  PanelDrawing in 'PanelDrawing.pas',
  DynTFTCodeGenSharedDataTypes in '..\DynTFTCodeGenSharedDataTypes.pas',
  DynTFTSharedUtils in '..\DynTFTSharedUtils.pas',
  DynTFTPluginUtils in '..\DynTFTPluginUtils.pas',
  DynTFTConsts in 'D:\DynTFT\DynTFTConsts.pas',
  DynTFTBaseDrawing in 'D:\DynTFT\DynTFTBaseDrawing.pas',
  DynTFTTypes in 'D:\DynTFT\DynTFTTypes.pas',
  DynTFTUtils in 'D:\DynTFT\DynTFTUtils.pas',
  DynTFTFonts in 'DynTFTFonts.pas',
  MemManager in 'D:\DynTFT\MemManager.pas',
  DynTFTButton in 'D:\DynTFT\DynTFTButton.pas',
  DynTFTArrowButton in 'D:\DynTFT\DynTFTArrowButton.pas',
  DynTFTPanel in 'D:\DynTFT\DynTFTPanel.pas',
  DynTFTCheckBox in 'D:\DynTFT\DynTFTCheckBox.pas',
  DynTFTScrollBar in 'D:\DynTFT\DynTFTScrollBar.pas',
  DynTFTItems in 'D:\DynTFT\DynTFTItems.pas',
  DynTFTListBox in 'D:\DynTFT\DynTFTListBox.pas',
  DynTFTLabel in 'D:\DynTFT\DynTFTLabel.pas',
  DynTFTRadioButton in 'D:\DynTFT\DynTFTRadioButton.pas',
  DynTFTRadioGroup in 'D:\DynTFT\DynTFTRadioGroup.pas',
  DynTFTTabButton in 'D:\DynTFT\DynTFTTabButton.pas',
  DynTFTPageControl in 'D:\DynTFT\DynTFTPageControl.pas',
  DynTFTEdit in 'D:\DynTFT\DynTFTEdit.pas',
  DynTFTKeyButton in 'D:\DynTFT\DynTFTKeyButton.pas',
  DynTFTVirtualKeyboard in 'D:\DynTFT\DynTFTVirtualKeyboard.pas',
  DynTFTComboBox in 'D:\DynTFT\DynTFTComboBox.pas',
  DynTFTTrackBar in 'D:\DynTFT\DynTFTTrackBar.pas',
  DynTFTProgressBar in 'D:\DynTFT\DynTFTProgressBar.pas',
  DynTFTMessageBox in 'D:\DynTFT\DynTFTMessageBox.pas',
  TFT in 'D:\DynTFT\TFT.pas',
  DynTFTColorThemeGenLiveColors in 'DynTFTColorThemeGenLiveColors.pas',
  DynTFTCGSystemExportedFunctions in 'DynTFTCGSystemExportedFunctions.pas',
  TFTCallbacks in '..\TFTCallbacks.pas';

{$R *.res}

              //All of the following functions/procedures are expected to be implemented in all plugins, regardless of the number of components they implement, or if they have a console or not.
exports
  GetPluginName,
  InitPlugin,
  DonePlugin,

  DrawPDynTFTComponentOnPanel,
  RegisterAllComponentsEvents,
  RegisterDynTFTDrawingProcedures,
  GetListOfComponents,
  GetListOfDoNotUseComponents,
  GetListOfLiveColorComponents,
  GetComponentIcon,
  GetSchemaDirName,
  
  UpdateLiveColorConstants,
  BackupLiveColorConstants,
  RestoreColorConstantsFromBackup,
  CanUpdateLiveColorConstant,
  GetLiveColorsConstCountByComponent,

  DisplayDebugConsole;

begin

end.
