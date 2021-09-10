unit uabout;
{ FaSubrip: autodetects and converts encoding of farsi (persian) subrip files.
  with additional functionalities.

  Copyright (C) 2016-2021 Mohammadreza Bahrami m.audio91@gmail.com

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uUrlLabel, CommonGUIUtils;

type

  { TFaSubripAbout }

  TFaSubripAbout = class(TForm)
    Header: TPanel;
    Logo: TImage;
    LinksBox: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    MailUrlL: TUrlLabelEx;
    ContactUrlL: TUrlLabelEx;
    IssuesUrlL: TUrlLabelEx;
    UpdatesUrlL: TUrlLabelEx;
  end;

var
  FaSubripAbout: TFaSubripAbout;

implementation

{$R *.lfm}

const
  MailUrl = 'mailto:maudio91@gmail.com';
  ContactUrl = 'http://mohammadrezab.blogsky.com';
  IssuesUrl = 'https://github.com/m-audio91/FaSubrip/issues';
  UpdatesUrl = 'https://github.com/m-audio91/FaSubrip/releases/latest';

resourcestring
  rsMail = 'ایمیل';
  rsBlog = 'بلاگ';
  rsIssueReporting = 'گزارش خطا در گیت هاب';
  rsUpdatesCaption = 'دریافت نسخه جدید';

{ TFaSubripAbout }

procedure TFaSubripAbout.FormCreate(Sender: TObject);
begin
  UpdatesUrlL := TUrlLabelEx.Create(Self);
  with UpdatesUrlL do
  begin
    Parent := LinksBox;
    Alignment := taCenter;
    Caption := rsUpdatesCaption;
    URL := UpdatesUrl;
    ShowHint := True;
    HighlightColor := clHighlight;
  end;
  IssuesUrlL := TUrlLabelEx.Create(Self);
  with IssuesUrlL do
  begin
    Parent := LinksBox;
    Alignment := taCenter;
    Caption := rsIssueReporting;
    URL := IssuesUrl;
    HighlightColor := clHighlight;
  end;
  MailUrlL := TUrlLabelEx.Create(Self);
  with MailUrlL do
  begin
    Parent := LinksBox;
    Alignment := taCenter;
    Caption := rsMail;
    URL := MailUrl;
    HighlightColor := clHighlight;
  end;
  ContactUrlL := TUrlLabelEx.Create(Self);
  with ContactUrlL do
  begin
    Parent := LinksBox;
    Alignment := taCenter;
    Caption := rsBlog;
    URL := ContactUrl;
    HighlightColor := clHighlight;
  end;
end;

procedure TFaSubripAbout.FormShow(Sender: TObject);
begin
  CheckDisplayInScreen(Self);
end;


end.

