unit umain;
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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  LCLType, StdCtrls, IniPropStorage, ExtCtrls, LazUTF8, LConvEncoding,
  uUrlLabel, LazFileUtils, CommonStrUtils, CommonFileUtils;

type

  { TFaSubripMain }

  TFaSubripMain = class(TForm)
    AppendEncodingToFileName: TCheckBox;
    OutFileEncodingL: TLabel;
    OutFileEncoding: TComboBox;
    OpenSubs: TButton;
    CensorshipPhrases: TCheckBox;
    Description: TLabel;
    OpenDlg: TOpenDialog;
    OpenSubsContainer: TPanel;
    SaveDirDlg: TSelectDirectoryDialog;
    ArabicCharsToFarsi: TCheckBox;
    Header: TPanel;
    StripHTMLStyleTags: TCheckBox;
    StripHTMLFontTags: TCheckBox;
    Extras: TGroupBox;
    IniProps: TIniPropStorage;
    CensExtraPhrases: TFileNameEdit;
    SaveDlg: TSaveDialog;
    Footer: TPanel;
    CensorshipExtraPhrases: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure OpenSubsClick(Sender: TObject);
  private
    FSrt: String;
    FBatchMode: Boolean;
    FBatchOutDir: String;
    FInputFile: String;
    procedure DoRun;
    procedure ProcessSubtitle;
    function TryReadSubtitle: Boolean;
    procedure ClearBOMs(var S: String);
    procedure SwapArabicChars(var S: String);
    procedure StripHTMLTags(var S: String);
    procedure CensorshipImpolitePhrases(var S: String);
    procedure CensorshipExtraPhrasesFile(var S: String);
    procedure PromptExport;
    procedure ExportSubtitle(const Subtitle: String);
    procedure ShowGuide(Sender: TObject);
  public
    FInputFiles: array of String;
    FAutoRun: Boolean;
  end;

var
  FaSubripMain: TFaSubripMain;

implementation

{$R *.lfm}
{$R badphrases.res}

const
  ContactUrl = 'http://mohammadrezab.blogsky.com';
  IssuesUrl = 'https://github.com/m-audio91/FaSubrip/issues';
  LicenseUrl = 'https://www.gnu.org/licenses/gpl-3.0.en.html';
  UpdatesUrl = 'https://github.com/m-audio91/FaSubrip/releases/latest';
  SourceUrl = 'https://github.com/m-audio91/FaSubrip';
  LicenseCaption = 'GNU GPL 3.0';
  HTMLTags: array[0..11] of String = ('<i>', '</i>', '{i}', '{/i}', '<b>',
  '</b>', '{b}', '{/b}', '<u>', '</u>', '{u}', '{/u}');
  HTMLFontTagOpen = '<font';
  HTMLFontTagClose = '</font>';
  HTMLTagEnd = '>';
  CensorMask = '*****';
  wFaSubed = '_FaSubrip';
  extSrt = '.srt';
  BadPhrasesResName = 'BADPHRASES';
  ArabicChars: array[0..1] of String = ('ك', 'ي');
  FarsiChars: array[0..1] of String = ('ک', 'ی');
  EncodingNames: array[0..2] of String = ('UTF-8', 'WINDOWS-1256', 'UTF-16');
  UTFBOMs: array[0..4] of String = (UTF8BOM, UTF16BEBOM, UTF16LEBOM, UTF32BEBOM,
    UTF32LEBOM);

resourcestring
  rsAllDone = 'عملیات انجام شد';
  rsSelectOutDir = 'پوشه ای برای قرار دادن فایل های خروجی انتخاب کنید';
  rsContact = 'ارتباط';
  rsGuide = 'راهنمای تنظیمات';
  rsIssueReporting = 'پشتیبانی';
  rsLicenseHint = 'فاسابریپ و متن آن تحت این مجوز برای عموم منتشر گردیده است'
    +LineEnding + 'متن برنامه و طریقه کامپایل آن در آدرس زیر موجود است'
    +LineEnding + SourceUrl;
  rsUpdatesCaption = 'بروزرسانی ها';
  rsUpdateHint = 'برای مطلع شدن از آخرین بروزرسانی ها کلیک کنید';

{ TFaSubripMain }

procedure TFaSubripMain.FormCreate(Sender: TObject);
var
  Url0: TCustomUrlLabel;
  Url1,Url2,Url3,Url4: TUrlLabelEx;
begin
  Url0 := TCustomUrlLabel.Create(Self);
  with Url0 do
  begin
    Parent := Header;
    Align := alLeft;
    Caption := rsGuide;
    OnClick := @ShowGuide;
    Font.Color := $0086C6E4;
    BorderSpacing.Left := 8;
    BorderSpacing.Top := 4;
    BorderSpacing.Bottom := 4;
  end;
  Url1 := TUrlLabelEx.Create(Self);
  with Url1 do
  begin
    Parent := Header;
    Align := alRight;
    Caption := rsContact;
    URL := ContactUrl;
    Font.Color := $0086C6E4;
    BorderSpacing.Right := 8;
    BorderSpacing.Top := 4;
    BorderSpacing.Bottom := 4;
  end;
  Url2 := TUrlLabelEx.Create(Self);
  with Url2 do
  begin
    Parent := Header;
    Align := alRight;
    Caption := rsIssueReporting;
    URL := IssuesUrl;
    Font.Color := $0086C6E4;
    BorderSpacing.Right := 8;
    BorderSpacing.Top := 4;
    BorderSpacing.Bottom := 4;
  end;
  Url3 := TUrlLabelEx.Create(Self);
  with Url3 do
  begin
    Parent := Footer;
    Align := alRight;
    Caption := LicenseCaption;
    URL := LicenseUrl;
    ShowHint := True;
    Hint := rsLicenseHint;
    HighlightColor := clHighlight;
  end;
  Url4 := TUrlLabelEx.Create(Self);
  with Url4 do
  begin
    Parent := Footer;
    Align := alLeft;
    Caption := rsUpdatesCaption;
    URL := UpdatesUrl;
    ShowHint := True;
    Hint := rsUpdateHint;
    HighlightColor := clHighlight;
  end;
end;

procedure TFaSubripMain.FormShow(Sender: TObject);
begin
  if FAutoRun then
  begin
    DoRun;
    Close;
  end;
end;

procedure TFaSubripMain.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
var
  i: Integer;
begin
  FInputFiles := nil;
  SetLength(FInputFiles, Length(FileNames));
  for i := 0 to High(FileNames) do
    FInputFiles[i] := FileNames[i];
  DoRun;
end;

procedure TFaSubripMain.OpenSubsClick(Sender: TObject);
var
  i: Integer;
begin
  if OpenDlg.Execute then
  begin
    FInputFiles := nil;
    SetLength(FInputFiles, OpenDlg.Files.Count);
    for i := 0 to OpenDlg.Files.Count-1 do
      FInputFiles[i] := OpenDlg.Files[i];
    DoRun;
  end;
end;

procedure TFaSubripMain.DoRun;
var
  AFile: String;
begin
  OpenDlg.InitialDir := ExtractFilePath(FInputFiles[0]);
  if High(FInputFiles) > 0 then
  begin
    FBatchMode := True;
    ShowMessage(rsSelectOutDir);
    SaveDirDlg.InitialDir := OpenDlg.InitialDir;
    if not SaveDirDlg.Execute then
    begin
      FBatchMode := False;
      Exit;
    end;
    FBatchOutDir := SaveDirDlg.FileName;
    for AFile in FInputFiles do
    begin
      FInputFile := AFile;
      ProcessSubtitle;
    end;
    ShowMessage(rsAllDone);
    FBatchMode := False;
  end
  else
  begin
    FBatchMode := False;
    FInputFile := FInputFiles[0];
    ProcessSubtitle;
  end;
end;

procedure TFaSubripMain.ProcessSubtitle;
begin
  if not LowerCase(ExtractFileExt(FInputFile)).Equals(extSrt) then Exit;
  if not TryReadSubtitle then Exit;
  ClearBOMs(FSrt);
  SwapArabicChars(FSrt);
  StripHTMLTags(FSrt);
  CensorshipExtraPhrasesFile(FSrt);
  CensorshipImpolitePhrases(FSrt);
  if FBatchMode then
    ExportSubtitle(GenFileName(FInputFile, wFaSubed, extSrt, True, FBatchOutDir))
  else
    PromptExport;
end;

function TFaSubripMain.TryReadSubtitle: Boolean;
var
  bs: TBytesStream;
  Enc: TEncoding;
  sl: TStringList;
  SrtEnc: String;
begin
  Result := False;
  Enc := Default(TEncoding);
  bs := TBytesStream.Create;
  sl := TStringList.Create;
  try
    bs.LoadFromFile(FInputFile);
    bs.Position := 0;
    SrtEnc := GuessEncoding(PChar(bs.Bytes));
    if SrtEnc = 'ucs2le' then
      FSrt := UTF16ToUTF8(PWideChar(bs.Bytes), bs.Size div SizeOf(WideChar))
    else if (SrtEnc = 'cp1252')
    or (SrtEnc = 'cp1256')
    or (SrtEnc = 'ISO-8859-1') then
      FSrt := ConvertEncoding(PChar(bs.Bytes), EncodingCP1256, EncodingUTF8)
    else
    begin
      sl.DefaultEncoding := Enc.UTF8;
      sl.LoadFromStream(bs, Enc.UTF8);
      FSrt := sl.Text;
    end;
  finally
    bs.Free;
    sl.Free;
  end;
  Result := not IsEmptyStr(FSrt);
end;

procedure TFaSubripMain.ClearBOMs(var S: String);
var
  ABOM: String;
begin
  for ABOM in UTFBOMs do
    DeleteAllOccurrences(ABOM, S);
end;

procedure TFaSubripMain.SwapArabicChars(var S: String);
begin
  if OutFileEncoding.ItemIndex = 1 then
    ArabicCharsToFarsi.State := cbUnchecked;
  if ArabicCharsToFarsi.State <> cbChecked then
    S := ReplaceStrings(S, FarsiChars, ArabicChars)
  else
    S := ReplaceStrings(S, ArabicChars, FarsiChars);
end;

procedure TFaSubripMain.StripHTMLTags(var S: String);
var
  ATag: String;
begin
  if StripHTMLStyleTags.State = cbChecked then
  begin
    for ATag in HTMLTags do
      DeleteAllOccurrences(ATag, S);
  end;
  if StripHTMLFontTags.State = cbChecked then
  begin
    DeleteAllOccurrencesVL(HTMLFontTagOpen, HTMLTagEnd, S);
    DeleteAllOccurrences(HTMLFontTagClose, S);
  end;
end;

procedure TFaSubripMain.CensorshipImpolitePhrases(var S: String);
var
  rs: TResourceStream;
  sl: TStringList;
  i,j: Cardinal;
  Enc: TEncoding;
begin
  if CensorshipPhrases.State <> cbChecked then Exit;
  Enc := Default(TEncoding);
  sl := TStringList.Create;
  try
    rs := TResourceStream.Create(HInstance, BadPhrasesResName, RT_RCDATA);
    sl.LoadFromStream(rs, Enc.UTF8);
    for i := 0 to sl.Count-1 do
        sl[i] := ReplaceStrings(sl[i], ArabicChars, FarsiChars);
    j := sl.Count;
    for i := 0 to j-1 do
      sl.Add(ReplaceStrings(sl[i], FarsiChars, ArabicChars));
    for i := 0 to sl.Count-1 do
      DeleteAllOccurrences(sl[i], S, CensorMask);
  finally
    sl.Free;
    rs.Free;
  end;
end;

procedure TFaSubripMain.CensorshipExtraPhrasesFile(var S: String);
var
  sl: TStringList;
  i,j: Cardinal;
  Enc: TEncoding;
begin
  if CensorshipExtraPhrases.State <> cbChecked then Exit;
  if IsEmptyStr(CensExtraPhrases.Text) then Exit;
  if not FileExists(CensExtraPhrases.Text) then Exit;
  if not FileIsText(CensExtraPhrases.Text) then Exit;
  Enc := Default(TEncoding);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(CensExtraPhrases.Text, Enc.UTF8);
    for i := 0 to sl.Count-1 do
      sl[i] := ReplaceStrings(sl[i], ArabicChars, FarsiChars);
    j := sl.Count;
    for i := 0 to j-1 do
      sl.Add(ReplaceStrings(sl[i], FarsiChars, ArabicChars));
    for i := 0 to sl.Count-1 do
    begin
      if (sl.Values[sl.Names[i]] <> EmptyStr) then
        S := S.Replace(sl.Names[i], sl.Values[sl.Names[i]])
      else if sl[i].EndsWith('=') then
        S := S.Replace(sl[i].Remove(sl[i].Length-1), EmptyStr)
      else
        DeleteAllOccurrences(sl[i], S, CensorMask);
    end;
  finally
    sl.Free;
  end;
end;

procedure TFaSubripMain.PromptExport;
begin
  with SaveDlg do
  begin
    InitialDir := OpenDlg.InitialDir;
    FileName := GenFileName(FInputFile, wFaSubed, extSrt, False);
    if Execute then
    begin
      if not LowerCase(ExtractFileExt(FileName)).Equals(extSrt) then
        FileName := FileName + extSrt;
      ExportSubtitle(FileName);
    end;
  end;
end;

procedure TFaSubripMain.ExportSubtitle(const Subtitle: String);
var
  sl: TStringList;
  Enc: TEncoding;
  Sub: String;
begin
  if AppendEncodingToFileName.State = cbChecked then
    Sub := GenFileName(Subtitle, EncodingNames[OutFileEncoding.ItemIndex])
  else
    Sub := Subtitle;
  Enc := Default(TEncoding);
  sl := TStringList.Create;
  try
    case OutFileEncoding.ItemIndex of
    0:begin
      sl.DefaultEncoding := Enc.UTF8;
      sl.Text := FSrt;
      sl.TextLineBreakStyle := DefaultTextLineBreakStyle;
      sl.SaveToFile(Sub, Enc.UTF8);
      end;
    1:begin
      sl.DefaultEncoding := Enc.ANSI;
      sl.Text := ConvertEncoding(FSrt, EncodingUTF8, EncodingCP1256);
      sl.TextLineBreakStyle := DefaultTextLineBreakStyle;
      sl.SaveToFile(Sub);
      end;
    2:begin
      sl.DefaultEncoding := Enc.Unicode;
      sl.Text := FSrt;
      sl.TextLineBreakStyle := DefaultTextLineBreakStyle;
      sl.SaveToFile(Sub, Enc.Unicode);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TFaSubripMain.ShowGuide(Sender: TObject);
var
  HelpForm: TForm;
  Labels: array[0..15] of TLabel;
  i: Integer;
begin
  HelpForm := TForm.CreateNew(Self);
  try
    with HelpForm do
    begin
      DefaultMonitor := dmActiveForm;
      Position := poOwnerFormCenter;
      Caption := rsGuide;
      AutoScroll := True;
      HorzScrollBar.Visible := False;
      VertScrollBar.Tracking := True;
      VertScrollBar.Smooth := True;
      ChildSizing.ControlsPerLine := 1;
      ChildSizing.EnlargeHorizontal := crsHomogenousChildResize;
      ChildSizing.ShrinkHorizontal := crsHomogenousChildResize;
      ChildSizing.Layout := cclLeftToRightThenTopToBottom;
      ChildSizing.VerticalSpacing := 2;
      ChildSizing.LeftRightSpacing := 8;
      ChildSizing.TopBottomSpacing := 8;
    end;

    for i := 0 to High(Labels) do
    begin
      Labels[i] := TLabel.Create(HelpForm);
      Labels[i].Parent := HelpForm;
      Labels[i].BiDiMode := bdRightToLeft;
      Labels[i].WordWrap := True;
      if (i = 0) or (i mod 2 = 0) then
      begin
        Labels[i].BorderSpacing.Top := 8;
        Labels[i].Font.Height := Canvas.GetTextHeight('AText')*2;
      end;
    end;

    Labels[0].Caption := OpenSubs.Caption;
    Labels[1].Caption := OpenSubs.Hint;
    Labels[2].Caption := StripHTMLFontTags.Caption;
    Labels[3].Caption := StripHTMLFontTags.Hint;
    Labels[4].Caption := StripHTMLStyleTags.Caption;
    Labels[5].Caption := StripHTMLStyleTags.Hint;
    Labels[6].Caption := ArabicCharsToFarsi.Caption;
    Labels[7].Caption := ArabicCharsToFarsi.Hint;
    Labels[8].Caption := CensorshipPhrases.Caption;
    Labels[9].Caption := CensorshipPhrases.Hint;
    Labels[10].Caption := CensorshipExtraPhrases.Caption;
    Labels[11].Caption := CensorshipExtraPhrases.Hint;
    Labels[12].Caption := OutFileEncodingL.Caption;
    Labels[13].Caption := OutFileEncodingL.Hint;
    Labels[14].Caption := AppendEncodingToFileName.Caption;
    Labels[15].Caption := AppendEncodingToFileName.Hint;

    HelpForm.ShowModal;
  finally
    HelpForm.Free;
  end;
end;

end.

