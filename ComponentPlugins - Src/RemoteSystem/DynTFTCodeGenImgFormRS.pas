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

unit DynTFTCodeGenImgFormRS;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls;

type
  TfrmImg = class(TForm)
    memLog: TMemo;
    pnlStatus: TPanel;
    btnSettings: TButton;
    procedure FormClose(Sender: TObject; var {$IFDEF FPC}CloseAction {$ELSE} Action {$ENDIF}: TCloseAction);
    procedure btnSettingsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmImg: TfrmImg;

implementation

{$R *.dfm}

uses
  RSSettingsForm, DynTFTCGRemoteSystemExportedFunctions, RSPanelDrawing,
  DynTFTUtils, IdGlobal, IdTCPServer;


procedure TfrmImg.btnSettingsClick(Sender: TObject);
var
  tk: Int64;
  OldRemoteSystemServerAddress: string;
  OldRemoteSystemServerPort: Word;
begin
  OldRemoteSystemServerAddress := FRemoteSystemServerAddress;
  OldRemoteSystemServerPort := FRemoteSystemServerPort;

  if EditServerSettings(FRemoteSystemServerAddress, FRemoteSystemServerPort, FPluginServerPort) then
    if (FIdTCPServer.DefaultPort <> FPluginServerPort) or
       (OldRemoteSystemServerAddress <> FRemoteSystemServerAddress) or
       (OldRemoteSystemServerPort <> FRemoteSystemServerPort) then
    begin
      try
        FIdTCPServer.ReuseSocket := rsFalse;
        DynTFT_DebugConsole('Notifying RS about changing port (RS should disconnect from plugin)...');

        try
          SendDisconnectFromPluginCommandToServer;
        except
          on E: Exception do
            DynTFT_DebugConsole('Exception notifying the RS server about disconnection. Maybe the server is not available: ' + E.Message);
        end;

        tk := {$IFDEF FPC} GetTickCount64; {$ELSE} GetTickCount; {$ENDIF}
        repeat
          Application.ProcessMessages;
          Sleep(10);
        until {$IFDEF FPC} GetTickCount64 {$ELSE} GetTickCount {$ENDIF} - tk > 3000;

        DynTFT_DebugConsole('Waiting for the server module to go off...');
        FIdTCPServer.Active := False;

        tk := {$IFDEF FPC} GetTickCount64; {$ELSE} GetTickCount; {$ENDIF}
        repeat
          Application.ProcessMessages;
          Sleep(10);
        until {$IFDEF FPC} GetTickCount64 {$ELSE} GetTickCount {$ENDIF} - tk > 3000;
      except
        on E: Exception do                                                                 //proably no exception here
          DynTFT_DebugConsole('Exception closing the server module at plugin side: ' + E.Message);
      end;

      try
        FIdTCPServer.Free;
        Sleep(10);
        Application.ProcessMessages;
        CreateCallbackTCPServer;

        DynTFT_DebugConsole('Updating the server module at plugin side to port: ' + IntToStr(FPluginServerPort));
        FIdTCPServer.DefaultPort := FPluginServerPort;
      except
        on E: Exception do
          DynTFT_DebugConsole('Exception recreating the server module at plugin side: ' + E.Message);
      end;

      try
        FIdTCPServer.Active := True;
        FIdTCPServer.ReuseSocket := rsTrue; //set this after setting Active to true, to be used on reconnecting only
      except
        on E: Exception do
          DynTFT_DebugConsole('Exception updating the server module at plugin side: ' + E.Message);
      end;

      try
        SendPluginPortCommandToServer;
      except
        on E: Exception do
          DynTFT_DebugConsole('Exception notifying the RS server about port changing. Maybe the server is not available: ' + E.Message);
      end;
    end;
end;


procedure TfrmImg.FormClose(Sender: TObject; var {$IFDEF FPC}CloseAction {$ELSE} Action {$ENDIF}: TCloseAction);
begin
  {$IFDEF FPC}CloseAction {$ELSE} Action {$ENDIF} := caNone;     //All plugins should implement this, because otherwise, closing the form will close DynTFTCodeGen.
  Hide;
end;

end.
