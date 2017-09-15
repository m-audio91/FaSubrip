program FaSubrip;
{ FaSubrip: autodetects and converts encoding of farsi (persian) subrip files.
  with additional functionalities.

  Copyright (C) 2017 Mohammadreza Bahrami m.audio91@gmail.com

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, uUrlLabel
  { you can add units after this };

var
  i: Word;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFaSubripMain, FaSubripMain);
  if Application.ParamCount < 1 then
    FaSubripMain.FAutoRun := False
  else
  begin
    SetLength(FaSubripMain.FInputFiles, Application.ParamCount);
    for i := 1 to Application.ParamCount do
      FaSubripMain.FInputFiles[i-1] := Application.Params[i];
    FaSubripMain.FAutoRun := True;
  end;
  Application.Run;
end.

