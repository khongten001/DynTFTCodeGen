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

unit DynTFTSharedUtils;

interface

uses
  DynTFTCodeGenSharedDataTypes, Graphics;

const
  CMaxSharedStringLength = 65535;
    

function BelongsTo(x, ALeftLimit, ARightLimit: Integer): Boolean;
function HexToInt(s: string): Cardinal;
function HexToInt64(s: string): Int64;
function Int64ToHexClone(Value: Int64; Digits: Integer): string;  
  

function GetPropertyIndexInPropertiesOrEventsByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string): Integer;
function GetPropertyValueInPropertiesOrEventsByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string): string;

function GetFontPropertyValueIndexInAllFonts(var AFontSettings: TFontSettingsArr; PropertyValue: string): Integer;
function GetColorConstByNameFromAllColorConsts(var AllColorConsts: TColorConstArr; ColorName: string; DefaultColor: TColor): TColor;


procedure UpdateComponentPropertyByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string; NewValue: string);

function SetPointedContentFromString(ASrc: string; ADest: Pointer; AMaxLen: Integer = CMaxSharedStringLength): Integer;
procedure SetPointedContentToString(ASrc: Pointer; var ADest: string);


implementation


uses
  Windows, SysUtils{, Classes};


function BelongsTo(x, ALeftLimit, ARightLimit: Integer): Boolean;
begin
  Result := (x <= ARightLimit) and (x >= ALeftLimit);
end;


function Pow16(x: Byte): Cardinal;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result shl 4;
end;


function Pow16_Int64(x: Byte): Int64;
var
  i: Byte;
begin
  Result := 1;
  for i := 1 to x do
    Result := Result shl 4;
end;


function HexaDigitToByte(ch: Char): Byte;
begin
  Result := 0;
  Ch := UpCase(Ch);
  if Ch in ['0'..'9'] then
  begin
    Result := Ord(Ch) - 48;
    Exit;
  end;

  if Ch in ['A'..'F'] then
    Result := Ord(Ch) - 65 + 10;
end;


function HexToInt(s: string): Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16(Length(s) - i);
end;


function HexToInt64(s: string): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
    if s[i] in ['0'..'9', 'a'..'f', 'A'..'F'] then
      Result := Result + HexaDigitToByte(s[i]) * Pow16_Int64(Length(s) - i);
end;


function Int64ToHexClone(Value: Int64; Digits: Integer): string;  //use this, because IntToHex from SysUtils is not always called properly with Int64 (maybe because of overloading or type mismatch)
var
  s: array[0..20] of Byte;
  LastDigit: Int64;
  n: DWord; //number of digits
  i: DWord;
  DValue: Int64;
  Zeros: string;
begin
  DValue := Value;

  n := 0;
  repeat
    LastDigit := DValue and $F;
    DValue := DValue shr 4;
    if LastDigit < 10 then
      s[n] := LastDigit + 48
    else
      s[n] := LastDigit + 55;   //65-10
      
    Inc(n);
  until DValue = 0;

  Setlength(Result, n);

  if n > 1 then
  begin
    for i := 0 to n - 1 do
      Result[i + 1] := Chr(s[n - i - 1]);

    Result[n + 1] := #0;
  end
  else
  begin
    Result[1] := Chr(s[0]);
    Result[2] := #0;
  end;

  if Digits > Length(Result) then
  begin
    Setlength(Zeros, Digits - Integer(n));
    for i := 1 to Length(Zeros) do
      Zeros[i] := '0';

    Result := Zeros + Result;
  end;
end;
  

function GetPropertyIndexInPropertiesOrEventsByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string): Integer; //Returns -1 if component does not have that property
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(PropertiesOrEvents) - 1 do
    if PropertiesOrEvents[i].PropertyName = PropertyName then
    begin
      Result := i;
      Break;
    end;
end;


function GetPropertyValueInPropertiesOrEventsByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string): string;
var
  PropertyIndex: Integer;
  s: string;
begin
  PropertyIndex := GetPropertyIndexInPropertiesOrEventsByName(PropertiesOrEvents, PropertyName);
  if PropertyIndex < 0 then
  begin
    s := 'Property not found: ' + PropertyName + #13#10 +
         ' Make sure it is implemented for the used component.' + #13#10 +
         ' Also, verify the schema file if it exists / can be found.' + #13#10 +
         ' The schema file name should match the result of exported GetSchemaDirName function.';
    PropertyIndex := GetPropertyIndexInPropertiesOrEventsByName(PropertiesOrEvents, 'ObjectName');
    if PropertyIndex > -1 then
      s := s + #13#10 + 'ObjectName: ' + PropertiesOrEvents[PropertyIndex].PropertyValue;

    raise Exception.Create(s);
  end;
  Result := PropertiesOrEvents[PropertyIndex].PropertyValue;
end;


function GetFontPropertyValueIndexInAllFonts(var AFontSettings: TFontSettingsArr; PropertyValue: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(AFontSettings) - 1 do
    if AFontSettings[i].FontPropertyValue = PropertyValue then
    begin
      Result := i;
      Exit;
    end;
end;


function GetColorConstByNameFromAllColorConsts(var AllColorConsts: TColorConstArr; ColorName: string; DefaultColor: TColor): TColor;
var
  i: Integer;
begin
  if ColorName = '' then
  begin
    Result := DefaultColor;
    Exit;
  end;

  if ColorName[1] = '$' then
    Result := HexToInt(ColorName)
  else
  begin
    if ColorName[1] in ['0'..'9'] then
      Result := StrToIntDef(ColorName, clBlack)
    else
    begin
      Result := DefaultColor;
      for i := 0 to Length(AllColorConsts) - 1 do
        if AllColorConsts[i].Name = ColorName then
        begin
          Result := AllColorConsts[i].Value;
          Break;
        end;
    end;
  end;
end;


procedure UpdateComponentPropertyByName(var PropertiesOrEvents: TDynTFTDesignPropertyArr; PropertyName: string; NewValue: string);
var
  PropIdx: Integer;
begin
  PropIdx := GetPropertyIndexInPropertiesOrEventsByName(PropertiesOrEvents, PropertyName);
  if (PropIdx > -1) and (PropIdx < Length(PropertiesOrEvents)) then
    PropertiesOrEvents[PropIdx].PropertyValue := NewValue;
end;


function SetPointedContentFromString(ASrc: string; ADest: Pointer; AMaxLen: Integer = CMaxSharedStringLength): Integer;
begin
  if Length(ASrc) > AMaxLen then
  begin
    ASrc := 'Can''t set the string over its allocated size. Please increase CMaxSharedStringLength to at least ' + IntToStr(Length(ASrc)) + ', or simply pass a bigger value.';
    //PluginLastError := ASrc;
  end;

  Result := Length(ASrc);
  if Result > AMaxLen then
    Result := AMaxLen;

  if ASrc = '' then
    ASrc := '0';   //prevent copying from nil

  Move(ASrc[1], ADest^, Result + 1); //use + 1, to copy the null terminating character
end;


procedure SetPointedContentToString(ASrc: Pointer; var ADest: string);
begin
  if ASrc = nil then
  begin
    ADest := '';
    Exit;
  end;

  SetLength(ADest, strlen(ASrc));
  Move(ASrc^, ADest[1], Length(ADest));
end;

end.
