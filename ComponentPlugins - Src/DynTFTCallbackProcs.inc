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

  procedure DynTFT_Init(display_width, display_height: Word);
  begin
    //
  end;
  

  procedure DynTFT_Set_Pen(pen_color: TColor; pen_width: Byte);
  begin
    FDynTFT_Set_Pen_Callback(pen_color and $00FFFFFF, pen_width);
  end;


  procedure DynTFT_Set_Brush(brush_enabled: Byte; brush_color: TColor; gradient_enabled, gradient_orientation: Byte; gradient_color_from, gradient_color_to: TColor);
  begin
    FDynTFT_Set_Brush_Callback(brush_enabled, brush_color and $00FFFFFF, gradient_enabled, gradient_orientation, gradient_color_from and $00FFFFFF, gradient_color_to and $00FFFFFF);
  end;


  procedure DynTFT_Set_Font(activeFont: PByte; font_color: TColor; font_orientation: Word);
  begin                                 
    FDynTFT_Set_Font_Callback(activeFont, font_color and $00FFFFFF, font_orientation);
  end;

  
  procedure DynTFT_Write_Text({$IFDEF IsMCU} var {$ENDIF} AText: string; x, y: Word);
  begin
    FDynTFT_Write_Text_Callback(AText, x, y);
  end;


  procedure DynTFT_Line(x1, y1, x2, y2: Integer);
  begin
    FDynTFT_Line_Callback(x1, y1, x2, y2);
  end;


  procedure DynTFT_H_Line(x_start, x_end, y_pos: Integer);
  begin
    FDynTFT_H_Line_Callback(x_start, x_end, y_pos);
  end;


  procedure DynTFT_V_Line(y_start, y_end, x_pos: Integer);
  begin
    FDynTFT_V_Line_Callback(y_start, y_end, x_pos);
  end;


  procedure DynTFT_Dot(x, y: Integer; Color: TColor);
  begin
    FDynTFT_Dot_Callback(x, y, Color);
  end;


  procedure DynTFT_Fill_Screen(color: TColor);
  begin
    FDynTFT_Fill_Screen_Callback(color);
  end;


  procedure DynTFT_Rectangle(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right: Integer);
  begin
    FDynTFT_Rectangle_Callback(x_upper_left, y_upper_left, x_bottom_right, y_bottom_right);
  end;


  procedure DynTFT_Circle(x_center, y_center, radius: Integer);
  begin
    FDynTFT_Circle_Callback(x_center, y_center, radius);
  end;


  procedure GetTextWidthAndHeight(AText: string; var Width, Height: Word);
  begin
    FGetTextWidthAndHeight_Callback(AText, Width, Height);
  end;
