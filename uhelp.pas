unit uhelp;
{ FaSubrip: autodetects and converts encoding of farsi (persian) subrip files.
  with additional functionalities.

  Copyright (C) 2019 Mohammadreza Bahrami m.audio91@gmail.com

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

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFaSubripHelp }

  TFaSubripHelp = class(TForm)
    Header: TPanel;
    HeaderCap: TLabel;
    Contents: TPanel;
    ContentsContainer: TScrollBox;
    procedure FormShow(Sender: TObject);
  private
    FHelpsAreSet: Boolean;
  public
    property HelpsAreSet: Boolean read FHelpsAreSet default False;
    procedure AddTitle(const S: String);
    procedure AddDescription(const S: String);
  end;

var
  FaSubripHelp: TFaSubripHelp;

implementation

{$R *.lfm}

{ TFaSubripHelp }

procedure TFaSubripHelp.FormShow(Sender: TObject);
var
  i: Integer;
begin
  ContentsContainer.VertScrollBar.Position := 0;
  if HelpsAreSet then Exit;
  for i := 0 to Contents.ControlCount-1 do
  begin
    Contents.Controls[i].Align := alTop;
    Contents.Controls[i].Top := Contents.Height;
  end;
  FHelpsAreSet := True;
end;

procedure TFaSubripHelp.AddTitle(const S: String);
var
  L: TLabel;
begin
  L := TLabel.Create(Self);
  with L do
  begin
    Parent := Contents;
    BiDiMode := bdRightToLeft;
    BorderSpacing.Around := 4;
    BorderSpacing.Top := Font.GetTextHeight(S)*3;
    Caption := S+':';
    Font.Style := Font.Style+[fsBold];
    WordWrap := True;
  end;
end;

procedure TFaSubripHelp.AddDescription(const S: String);
var
  L: TLabel;
begin
  L := TLabel.Create(Self);
  with L do
  begin
    Parent := Contents;
    BiDiMode := bdRightToLeft;
    BorderSpacing.Around := 4;
    Caption := S;
    WordWrap := True;
  end;
end;

end.

