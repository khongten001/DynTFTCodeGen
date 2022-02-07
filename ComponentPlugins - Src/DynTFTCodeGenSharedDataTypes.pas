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

unit DynTFTCodeGenSharedDataTypes;

interface

uses
  Windows, Graphics, TFT;

type
  TColorConst = record
    Name: string;
    Value: TColor;
    SchemaValue: string; //used when the color is not directly resolved to an integer in schema, e.g.:  CL_DynTFTProgressBar_Progress = CL_HIGHLIGHT instead of $00FF9933
    //ValueStr: string;
    InitialValue: TColor; //value set from schema. Used when creating a new project.
  end;

  TColorConstArr = array of TColorConst;


  TFontSettings = record
    FontPropertyValue: string;  //e.g.  '@default_value
    SFont: PDynTFTFontSettings;
  end;

  TFontSettingsArr = array of TFontSettings;


  //used on Designer, for non-base properties like Caption, Progress, Color and events like OnChange
  TDynTFTDesignProperty = record
    PropertyName: string;
    PropertyValue: string;
  end;
                                      
  TDynTFTDesignPropertyArr = array of TDynTFTDesignProperty;


  TComponentConstant = record
    ConstantName: string;
    ConstantDataType: string;
    ConstantValue: string;
    ConstantValueInt: Int64; //Integer; //can also be Byte or Word
  end;

  TComponentConstantArr = array of TComponentConstant;

  TUIPanelBase = record
    Width, Height, DynTFTComponentType: Integer;
    CanvasHandle: HDC;
    Caption: string;
    Tag: Integer;
  end;

  //Pass this structure instead of  "var DynArrVar", because Delphi stores the length of array at Addr(array) - 4, while FPC stores its max index.
  //This means that if a Delphi app passes a 3-element array to a FPC, then the FPC app thinks that it is a 4-element array.
  TDynArrayRef = record
    AddrOfFirst: Pointer;
    Len: Integer;
  end;

  //called by plugin, to set the destination stream size and get stream memory
  //this callback is implemented by DynTFTCodeGen
  TSetSizeCallback = function(NewSize: Int64; StreamID: Int64): Pointer;

  TSetPropertiesCallback = procedure(ASrcPropertiesOrEventsRef, ADestPropertiesOrEventsRef: TDynArrayRef);     //Src is the plugin's modified content, while Dest is the initial CodeGen's array

  //Drawing callbacks  - called by plugins into DynTFTCodeGen, to draw DynTFT components

  //TDynTFT_Init_Callback = procedure(display_width, display_height: Word);
  TDynTFT_Set_Pen_Callback = procedure(pen_color: TColor; pen_width: Byte);
  TDynTFT_Set_Brush_Callback = procedure(brush_enabled: Byte; brush_color: TColor; gradient_enabled, gradient_orientation: Byte; gradient_color_from, gradient_color_to: TColor);
  TDynTFT_Set_Font_Callback = procedure(activeFont: PByte; font_color: TColor; font_orientation: Word);
  TDynTFT_Write_Text_Callback = procedure(AText: string; x, y: Word);
  TDynTFT_Line_Callback = procedure(x1, y1, x2, y2: Integer);
  TDynTFT_H_Line_Callback = procedure(x_start, x_end, y_pos: Integer);
  TDynTFT_V_Line_Callback = procedure(y_start, y_end, x_pos: Integer);
  TDynTFT_Dot_Callback = procedure(x, y: Integer; Color: TColor);
  TDynTFT_Fill_Screen_Callback = procedure(color: TColor);
  TDynTFT_Rectangle_Callback = procedure(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right: Integer);
  TDynTFT_Circle_Callback = procedure(x_center, y_center, radius: Integer);
  TDynTFT_GetTextWidthAndHeight_Callback = procedure(AText: string; var Width, Height: Word);
  TDynTFT_DrawBitmap_Callback = procedure(APointerToBmpStreamMem: Pointer; AContentSize: Int64; x, y: Integer);


  

  {$IFNDEF FPC}
    QWord = Int64; /////////////////////////////////////// this should also depend on Delphi version, because newer versions might know what QWord is
                   //anyway, QWord should be unsigned, but for D2006, this should be fine
  {$ENDIF}

implementation

end.
