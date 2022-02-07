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

unit TFTCallbacks;

interface

uses
  DynTFTCodeGenSharedDataTypes;

var
  FDynTFT_Set_Pen_Callback: TDynTFT_Set_Pen_Callback;
  FDynTFT_Set_Brush_Callback: TDynTFT_Set_Brush_Callback;
  FDynTFT_Set_Font_Callback: TDynTFT_Set_Font_Callback;
  FDynTFT_Write_Text_Callback: TDynTFT_Write_Text_Callback;
  FDynTFT_Line_Callback: TDynTFT_Line_Callback;
  FDynTFT_H_Line_Callback: TDynTFT_H_Line_Callback;
  FDynTFT_V_Line_Callback: TDynTFT_V_Line_Callback;
  FDynTFT_Dot_Callback: TDynTFT_Dot_Callback;
  FDynTFT_Fill_Screen_Callback: TDynTFT_Fill_Screen_Callback;
  FDynTFT_Rectangle_Callback: TDynTFT_Rectangle_Callback;
  FDynTFT_Circle_Callback: TDynTFT_Circle_Callback;
  FGetTextWidthAndHeight_Callback: TDynTFT_GetTextWidthAndHeight_Callback;
  FDynTFT_DrawBitmap_Callback: TDynTFT_DrawBitmap_Callback;

implementation


end.
