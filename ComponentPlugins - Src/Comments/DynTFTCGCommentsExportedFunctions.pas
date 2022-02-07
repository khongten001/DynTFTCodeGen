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

unit DynTFTCGCommentsExportedFunctions;

interface


uses
  Windows, DynTFTCodeGenSharedDataTypes;

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
  SysUtils, Classes, Forms, ComponentIcons,
  DynTFTPluginUtils, DynTFTSharedUtils;

const
  CPluginName = 'Comments';


function GetPluginName(APluginName: Pointer): Integer; stdcall;
begin
  Result := SetPointedContentFromString(CPluginName, APluginName);
end;



procedure InitPlugin; stdcall;
begin
  dmIcons := TdmIcons.Create(nil);
end;


procedure DonePlugin; stdcall;
begin
  FreeAndNil(dmIcons);
end;


function GetListOfComponents(AListOfComponents: Pointer): Integer; stdcall;  //This list has to match the number of icons from dmIcons.imglstComponents in data module.
var
  AStringList: TStringList;
  ResultStr: string;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('DynTFTComment');

    ResultStr := AStringList.Text;
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
    AStringList.Add('0'); //DynTFTComment

    ResultStr := AStringList.Text;
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


//For a detailed description (not needed for Comments plugin), see DynTFTCGSystem's GetListOfLiveColorComponents function.
function GetListOfLiveColorComponents(AListOfComponents: Pointer): Integer; stdcall;
var
  AStringList: TStringList;
  ResultStr: string;                                                            
begin
  AStringList := TStringList.Create;
  try
    AStringList.Add('DynTFTComment'); 

    ResultStr := AStringList.Text;
    Result := SetPointedContentFromString(ResultStr, AListOfComponents);
  finally
    AStringList.Free;
  end;
end;


procedure GetComponentIcon(ComponentIndex: Integer; ASetSizeCallback: TSetSizeCallback; AStreamID: Int64); stdcall;
begin
  GetComponentIconFromImageList(ComponentIndex, dmIcons.imglstComponents, ASetSizeCallback, AStreamID);
end;


function GetSchemaDirName(ADirName: Pointer): Integer; stdcall;  //expected to be relative to the dll, in the same folder
var
  ResultStr: string;
begin
  ResultStr := 'CommentSchemas';                //Schema file names are expected to be of "<ComponentName>.dynscm" format.
  Result := SetPointedContentFromString(ResultStr, ADirName);
end;


procedure UpdateLiveColorConstants(AComponentIndex, AColorIndex, ANewColor: Integer); stdcall;
begin
  //dummy for comments
end;


procedure BackupLiveColorConstants; stdcall;
begin
  //dummy for comments
end;


procedure RestoreColorConstantsFromBackup; stdcall;
begin
  //dummy for comments
end;


function CanUpdateLiveColorConstant(ComponentIndex, ColorIndex: Integer): Boolean; stdcall;
begin
  Result := False;     //False for comments
end;


function GetLiveColorsConstCountByComponent(ComponentIndex: Integer): Integer; stdcall;  //used by DynTFTCodeGen to display error messages if the number of colors does not match
begin
  Result := 0; //always return 0 for this component
end;


procedure DisplayDebugConsole(AMainFormHandle: THandle); stdcall;
begin
  MessageBox(AMainFormHandle, 'No debug console for this plugin.', PChar(CPluginName), MB_ICONINFORMATION);
end;

end.
