unit umain;
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

{$mode objfpc}{$H+}{$modeswitch arrayoperators+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  LCLType, StdCtrls, IniPropStorage, ExtCtrls, Buttons, ComCtrls,
  LazUTF8, LConvEncoding, uUrlLabel, LazFileUtils, DividerBevel, uabout,
  CommonGUIUtils, CommonStrUtils, CommonFileUtils, uSimpleHelp, uNumeditFloat,
  uTimeSlice, uSubripFile;

type

  { TFaSubripMain }

  TFaSubripMain = class(TForm)
    AppendEncodingToFileName: TCheckBox;
    EndingPunctuationsL: TLabel;
    EndingPunctuations: TComboBox;
    CensorshipLevel: TComboBox;
    ArabicCharsToFarsiL: TLabel;
    AppendEncodingToFileNameL: TLabel;
    Container1: TPanel;
    EnglishNumbers: TCheckBox;
    EnglishNumbersL: TLabel;
    ImgsLight: TImageList;
    SubContainer5: TPanel;
    Container6: TPanel;
    SubContainer6: TPanel;
    SubContainer1: TPanel;
    Container2: TPanel;
    SubContainer2: TPanel;
    Container3: TPanel;
    SubContainer3: TPanel;
    Container4: TPanel;
    SubContainer4: TPanel;
    Container5: TPanel;
    SettingContainersGrid: TPanel;
    ReplaceSourceFileL: TLabel;
    PhrasesCensorshipL: TLabel;
    StripHTMLFontTagsL: TLabel;
    StripHTMLStyleTagsL: TLabel;
    Imgs: TImageList;
    SettingsNotifierL: TLabel;
    SettingsNotifier: TPanel;
    ReplaceSourceFile: TCheckBox;
    OutFileEncodingL: TLabel;
    OutFileEncoding: TComboBox;
    OpenSubs: TButton;
    PhrasesCensorship: TCheckBox;
    Description: TLabel;
    OpenDlg: TOpenDialog;
    OpenSubsContainer: TPanel;
    SaveDirDlg: TSelectDirectoryDialog;
    ArabicCharsToFarsi: TCheckBox;
    Header: TPanel;
    SettingsShow: TSpeedButton;
    SettingsHelps: TSpeedButton;
    StripHTMLStyleTags: TCheckBox;
    StripHTMLFontTags: TCheckBox;
    IniProps: TIniPropStorage;
    PhrasesCensorshipFile: TFileNameEdit;
    SaveDlg: TSaveDialog;
    Footer: TPanel;
    DragNotifierL: TLabel;
    HeaderLinks: TPanel;
    OptionalSettings: TDividerBevel;
    SubtitleDelay: TBitBtn;
    SubtitleDelayL: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure IniPropsRestoringProperties(Sender: TObject);
    procedure PhrasesCensorshipFileKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure OpenSubsClick(Sender: TObject);
    procedure SettingsHelpsClick(Sender: TObject);
    procedure SettingsShowClick(Sender: TObject);
    procedure SubtitleDelayClick(Sender: TObject);
  private
    FHelpWindow: TSimpleHelp;
    FSrt: String;
    FDelay: Double;
    FBatchMode: Boolean;
    FBatchOutDir: String;
    FInputFile: String;
    FMaxHeight: Integer;
    FMinHeight: Integer;
    FCollapsed: Boolean;
    function OutputEncoding: String;
    function OutputSuffix: String;
    function OutputDirValid(const Dir: String): Boolean;
    procedure DoRun;
    procedure ProcessSubtitle;
    function TryReadSubtitle: Boolean;
    procedure ClearUnicodeSpecificChars(var S: String);
    procedure SwapArabicChars(var S: String);
    procedure ReplaceFarsiNums(var S: String);
    procedure ApplyDelay(var S: String);
    procedure StripHTMLTags(var S: String);
    procedure CensorPhrases(var S: String);
    procedure CorrectEndingPunctuations(var S: String);
    procedure PromptExport;
    procedure ExportSubtitle(const Subtitle: String);
    procedure SupportUrlClick(Sender: TObject);
  public
    SupportUrlL: TCustomUrlLabel;
    LicenseUrlL: TUrlLabelEx;
    FInputFiles: array of String;
    FAutoRun: Boolean;
  published
    property Collapsed: Boolean read FCollapsed write FCollapsed;
  end;

  TIntegerDict = record
    s,
    e: Integer;
  end;

  TIntegerDictArray = array of TIntegerDict;

var
  FaSubripMain: TFaSubripMain;

implementation

{$R *.lfm}
{$R badphrases.res}

const
  LicenseUrl = 'https://www.gnu.org/licenses/gpl-3.0.en.html';
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
  BadPhrasesStartMarker = '[startofbadphrases]';
  BadPhrasesEndMarker = '[endofbadphrases]';
  MoreBadPhrasesStartMarker = '[startofmorebadphrases]';
  MoreBadPhrasesEndMarker = '[endofmorebadphrases]';
  ArabicChars: array[0..1] of String = ('ك', 'ي');
  FarsiChars: array[0..1] of String = ('ک', 'ی');
  FarsiNums: array[0..9] of String = ('۰','۱','۲','۳','۴','۵','۶','۷','۸','۹');
  EnglishNums: array[0..9] of String = ('0','1','2','3','4','5','6','7','8','9');
  UnicodeBOMs: array[0..4] of String = (UTF8BOM, UTF16BEBOM, UTF16LEBOM, UTF32BEBOM,
    UTF32LEBOM);
  UnicodeDirectionControllers: array[0..11] of String = (#$E2#$80#$8F,
    #$D8#$9C, #$E2#$80#$8E, #$E2#$80#$AA, #$E2#$80#$AB, #$E2#$80#$AD,
    #$E2#$80#$AC, #$E2#$80#$AE, #$E2#$81#$A6, #$E2#$81#$A7, #$E2#$81#$A8,
    #$E2#$81#$A9);
  CommonPunctuations: array[0..9] of String = ('!',',','.',':',';',
    '?','‚','؟','؛','،');

resourcestring
  rsAllDone = 'عملیات انجام شد';
  rsSelectOutDir = 'پوشه ای برای قرار دادن فایل های خروجی انتخاب کنید';
  rsLicenseHint = 'فاسابریپ و متن آن تحت این مجوز برای عموم منتشر گردیده است'
    +LineEnding + 'متن برنامه و طریقه کامپایل آن در آدرس زیر موجود است'
    +LineEnding + SourceUrl;
  rsDirIsNotWritable = 'محل انتخابی برای خروجی قابلیت نوشتن ندارد';
  rsError = 'خطا';
  rsSupport = 'پشتیبانی';
  rsProcessSettings = 'تنظیمات پردازش زیرنویس';
  rsOutputFileSettings = 'تنظیمات ذخیره خروجی';

{ TFaSubripMain }

procedure TFaSubripMain.FormCreate(Sender: TObject);
begin
  DefaultMacOSMenu(Self);
  SupportUrlL := TCustomUrlLabel.Create(Self);
  with SupportUrlL do
  begin
    Parent := HeaderLinks;
    Caption := rsSupport;
    Align := alRight;
    Font.Color := $0086C6E4;
    OnClick := @SupportUrlClick;
  end;
  LicenseUrlL := TUrlLabelEx.Create(Self);
  with LicenseUrlL do
  begin
    Parent := Footer;
    Align := alLeft;
    Caption := LicenseCaption;
    URL := LicenseUrl;
    ShowHint := True;
    Hint := rsLicenseHint;
    HighlightColor := clHighlight;
  end;
  Collapsed := True;
  FDelay := 0;
end;

procedure TFaSubripMain.PhrasesCensorshipFileKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then (Sender as TFileNameEdit).RunDialog;
end;

procedure TFaSubripMain.IniPropsRestoringProperties(Sender: TObject);
begin
  SessionProperties := SessionProperties+';Collapsed';
end;

procedure TFaSubripMain.FormShow(Sender: TObject);
begin 
  {$ifdef darwin}
  SubContainer2.BorderSpacing.Right := 6;
  SubContainer4.BorderSpacing.Right := 6;
  SubContainer5.BorderSpacing.Right := 6;
  OpenSubsContainer.Color := clDefault;
  SettingsNotifier.Color := clDefault;
  SettingContainersGrid.Color := clDefault;
  Footer.Color := clDefault;
  {$endif}
  AutoSize := True;
  FMaxHeight := Height;
  with SettingContainersGrid do
  begin
    Self.Constraints.MinWidth := Width+BorderSpacing.Left+BorderSpacing.Right;
    Visible := False;
  end;
  FMinHeight := Height;
  if not Collapsed then
    SettingsShowClick(SettingsShow);
  {$ifndef darwin}
  if FAutoRun then
  begin
    DoRun;
    Close;
  end;
  {$endif}
  CheckDisplayInScreen(Self);
end; 

procedure TFaSubripMain.FormActivate(Sender: TObject);
begin
  if HasDarkBackgroundColor(Self) then
  begin
    SettingsShow.Images := ImgsLight;
    SettingsHelps.Images := ImgsLight;
    SubtitleDelay.Images := ImgsLight;
    PhrasesCensorshipFile.Images := ImgsLight;
    DragNotifierL.Font.Color := clDefault;
    SettingsNotifierL.Font.Color := clDefault;
  end;
  OnActivate := nil;
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

procedure TFaSubripMain.SettingsShowClick(Sender: TObject);
var
  h: Integer;
begin
  SettingContainersGrid.Visible := SettingContainersGrid.Visible = False;
  case SettingContainersGrid.Visible of
  True: begin
    h := FMaxHeight;
    SettingsShow.ImageIndex:= 1;
    end;
  False: begin
    h := FMinHeight;
     SettingsShow.ImageIndex:= 0;
    end;
  end;
  Collapsed := not SettingContainersGrid.Visible;
  {$ifdef linux}
  Constraints.MinHeight := h;
  Constraints.MaxHeight := h;
  AdjustSize;
  {$endif}
end;

procedure TFaSubripMain.SubtitleDelayClick(Sender: TObject);
var
  ne: TNumEditFloat;
begin
  ne := TNumEditFloat.Create(nil);
  try
    ne.HeaderText := SubtitleDelayL.Caption;
    ne.DecimalPlaces := 3;
    ne.Value := FDelay;
    ne.Increment := 0.1;
    if ne.ShowModal = mrOK then
      FDelay := ne.Value;
  finally
    SubtitleDelay.Caption := ': '+FDelay.ToString;
    ne.Free;
  end;
end;

function TFaSubripMain.OutputEncoding: String;
begin
  Result := 'UTF-8';
  if String(OutFileEncoding.Text).Contains('ANSI') then
    Result := 'ANSI'
  else if String(OutFileEncoding.Text).Contains('UTF-16') then
    Result := 'UTF-16';
end;

function TFaSubripMain.OutputSuffix: String;
begin
  Result := EmptyStr;
  if String(OutFileEncoding.Text).Contains('mark') then
    Result := 'noRTL';
  if String(OutFileEncoding.Text).Contains('ANSI') then
    Result := '(1256)';
  Result := OutputEncoding+Result;
end;

function TFaSubripMain.OutputDirValid(const Dir: String): Boolean;
begin
  Result := TryDirectoryIsWritable(Dir);
  if not Result then
    ShowError(rsDirIsNotWritable, rsError);
end;

procedure TFaSubripMain.DoRun;
var
  AFile: String;
begin
  OpenDlg.InitialDir := ExtractFilePath(FInputFiles[0]);
  if High(FInputFiles) > 0 then
  begin
    FBatchMode := True;
    if ReplaceSourceFile.State = cbChecked then
      FBatchOutDir := IncludeTrailingPathDelimiter(OpenDlg.InitialDir)
    else
    begin
      ShowMessage(rsSelectOutDir);
      SaveDirDlg.InitialDir := OpenDlg.InitialDir;
      if not SaveDirDlg.Execute then
      begin
        FBatchMode := False;
        Exit;
      end;
      FBatchOutDir := SaveDirDlg.FileName;
    end;
    if not OutputDirValid(FBatchOutDir) then Exit;
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
    if OutputDirValid(OpenDlg.InitialDir) then
      ProcessSubtitle;
  end;
  FDelay := 0;
  SubtitleDelay.Caption := ': '+FDelay.ToString;
end;

procedure TFaSubripMain.ProcessSubtitle;
begin
  if not LowerCase(ExtractFileExt(FInputFile)).Equals(extSrt) then Exit;
  if not TryReadSubtitle then Exit;
  ClearUnicodeSpecificChars(FSrt);
  SwapArabicChars(FSrt);
  ReplaceFarsiNums(FSrt);
  ApplyDelay(FSrt);
  StripHTMLTags(FSrt);
  CensorPhrases(FSrt);
  CorrectEndingPunctuations(FSrt);
  if FBatchMode then
    ExportSubtitle(GenFileName(FInputFile, EmptyStr, extSrt, True, FBatchOutDir, true))
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
    if SrtEnc = 'ucs2be' then
    begin
      sl.DefaultEncoding := Enc.BigEndianUnicode;
      sl.LoadFromStream(bs, Enc.BigEndianUnicode);
      FSrt := sl.Text;
    end
    else if SrtEnc = 'ucs2le' then
      FSrt := UTF16ToUTF8(PWideChar(bs.Bytes), bs.Size div SizeOf(WideChar))
    else if (' cp1252 cp1256 ISO-8859-1 ').Contains(SrtEnc) then
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

procedure TFaSubripMain.ClearUnicodeSpecificChars(var S: String);
var
  c: String;
begin
  for c in UnicodeBOMs do
    DeleteAllOccurrences(c, S);
  if OutputSuffix.Contains('noRTL')
  or OutputSuffix.Contains('ANSI') then
    for c in UnicodeDirectionControllers do
      DeleteAllOccurrences(c, S);
end;

procedure TFaSubripMain.SwapArabicChars(var S: String);
begin
  if OutputEncoding.Equals('ANSI') then
    ArabicCharsToFarsi.State := cbUnchecked;
  if ArabicCharsToFarsi.State <> cbChecked then
    S := ReplaceStrings(S, FarsiChars, ArabicChars)
  else
    S := ReplaceStrings(S, ArabicChars, FarsiChars);
end;

procedure TFaSubripMain.ReplaceFarsiNums(var S: String);
begin
  if OutputEncoding.Equals('ANSI') then
    EnglishNumbers.State := cbChecked;
  if EnglishNumbers.State = cbChecked then
    S := ReplaceStrings(s, FarsiNums, EnglishNums);
end;

procedure TFaSubripMain.ApplyDelay(var S: String);
var
  sa: TStringArray;
  ts: TTimeSlice;
  i,j: Integer;
begin
  if FDelay=0 then Exit;
  ts.Initialize(DefaultSubripTimeSliceFormat);
  sa := S.Split(LineEndings);
  j := 0;
  repeat
    i := FindInArray(sa, ts.TimeSliceFormat.SliceSep, j);
    if i>=0 then
    begin
      ts.ValueAsString := sa[i];
      if ts.Valid then
      begin
        ts.Delay := FDelay;
        ts.Value := ts.ValueWithDelay;
        sa[i] := ts.ValueAsString;
      end;
    end;
    j := i+1;
  until i<0;
  S := S.Join(LineEnding,sa);
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

procedure TFaSubripMain.CensorPhrases(var S: String);
var
  rs: TResourceStream;
  sl: TStringList;
  sa: TStringArray;
  i,j: Cardinal;
  Enc: TEncoding;
begin
  if PhrasesCensorship.State <> cbChecked then Exit;
  i := CensorshipLevel.ItemIndex;
  if i = 2 then
  begin
    if IsEmptyStr(PhrasesCensorshipFile.Text) then Exit;
    if not FileExists(PhrasesCensorshipFile.Text) then Exit;
    if not FileIsText(PhrasesCensorshipFile.Text) then Exit;
  end;
  Enc := Default(TEncoding);
  sl := TStringList.Create;
  sa := nil;
  try
    case i of
    0..1: begin
      rs := TResourceStream.Create(HInstance, BadPhrasesResName, RT_RCDATA);
      sl.LoadFromStream(rs, Enc.UTF8);
      rs.Free;
      for j := sl.IndexOf(BadPhrasesStartMarker)+1
      to sl.IndexOf(BadPhrasesEndMarker)-1 do
        sa := sa+[sl.Strings[j]];
      if i = 1 then
      begin
        for j := sl.IndexOf(MoreBadPhrasesStartMarker)+1
        to sl.IndexOf(MoreBadPhrasesEndMarker)-1 do
          sa := sa+[sl.Strings[j]];
      end;
      end;
    2: begin
      sl.LoadFromFile(PhrasesCensorshipFile.Text, Enc.UTF8);
      sa := StringListToArray(sl);
      end;
    end;
    sl.AddStrings(sa,True);
    sl.Duplicates := TDuplicates.dupIgnore;
    for i := 0 to High(sa) do
    begin
      sl.Add(ReplaceStrings(sa[i], ArabicChars, FarsiChars));
      sl.Add(ReplaceStrings(sa[i], FarsiChars, ArabicChars));
    end;
    s := NoChainedSpaces(s);
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

procedure TFaSubripMain.CorrectEndingPunctuations(var S: String);
var
  sa: TStringArray;
  i,j,sCount,eCount: Integer;
  r: String;
  d: TIntegerDictArray;
begin
  if EndingPunctuations.ItemIndex < 1 then Exit;
  sa := S.Split(LineEndings);
  SetLength(d, Length(sa));
  for j := Low(sa) to High(sa) do
  begin
    sCount := 0;
    repeat
      i := UTF8Copy(sa[j],sCount+1,1).IndexOfAny(CommonPunctuations);
      if i>-1 then
        Inc(sCount);
    until i<0;
    d[j].s := sCount;
    r := UTF8ReverseString(sa[j]);
    eCount := 0;
    repeat
      i := UTF8Copy(r,eCount+1,1).IndexOfAny(CommonPunctuations);
      if i>-1 then
        Inc(eCount);
    until i<0;
    d[j].e := eCount;
  end;
  for i := Low(sa) to High(sa) do
  begin
    j := UTF8Length(sa[i]);
    case EndingPunctuations.ItemIndex of
    1: sa[i] := UTF8RightStr(sa[i],d[i].e)
        +UTF8LeftStr(sa[i],j-d[i].e);
    2: sa[i] := UTF8RightStr(sa[i],j-d[i].s)
        +UTF8LeftStr(sa[i],d[i].s);
    3: sa[i] := UTF8RightStr(sa[i],j-d[i].s);
    4: sa[i] := UTF8LeftStr(sa[i],j-d[i].e);
    5: sa[i] := UTF8Copy(sa[i],d[i].s+1,j-(d[i].s+d[i].e));
    end;
  end;
  S := S.Join(LineEnding,sa);
end;

procedure TFaSubripMain.PromptExport;
begin
  if ReplaceSourceFile.State <> cbChecked then
  begin
    with SaveDlg do
    begin
      InitialDir := OpenDlg.InitialDir;
      if AppendEncodingToFileName.State = cbChecked then
        FileName :=
        GenFileName(FInputFile, '_'+OutputSuffix, extSrt, False)
      else
        FileName := GenFileName(FInputFile, wFaSubed, extSrt, False);
      if Execute then
      begin
        if not LowerCase(ExtractFileExt(FileName)).Equals(extSrt) then
          FileName := FileName + extSrt;
        if OutputDirValid(ExtractFilePath(FileName)) then
          ExportSubtitle(FileName);
      end;
    end;
  end
  else
  begin
    if OutputDirValid(OpenDlg.InitialDir) then
      ExportSubtitle(FInputFile);
  end;
end;

procedure TFaSubripMain.ExportSubtitle(const Subtitle: String);
var
  sl: TStringList;
  Enc: TEncoding;
  Sub: String;
begin
  Sub := Subtitle;
  if FInputFile.Equals(Sub) and FileExists(Sub) then
    DeleteFile(Sub);
  if AppendEncodingToFileName.State = cbChecked then
  begin
    if not Sub.EndsWith(OutputSuffix+extSrt) then
      Sub := GenFileName(Sub, '_'+OutputSuffix);
  end;
  Enc := Default(TEncoding);
  sl := TStringList.Create;
  try
    case OutputEncoding of
    'UTF-8':begin
      sl.DefaultEncoding := Enc.UTF8;
      sl.Text := FSrt;
      sl.TextLineBreakStyle := DefaultTextLineBreakStyle;
      sl.SaveToFile(Sub, Enc.UTF8);
      end;
    'ANSI':begin
      sl.DefaultEncoding := Enc.ANSI;
      sl.Text := ConvertEncoding(FSrt, EncodingUTF8, EncodingCP1256);
      sl.TextLineBreakStyle := tlbsCRLF;
      sl.SaveToFile(Sub,True);
      end;
    'UTF-16':begin
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

procedure TFaSubripMain.SupportUrlClick(Sender: TObject);
begin
  FaSubripAbout.ShowModal;
end;

procedure TFaSubripMain.SettingsHelpsClick(Sender: TObject);
begin
  if not Assigned(FHelpWindow) then
  begin
    FHelpWindow := TSimpleHelp.Create(Self);
    with FHelpWindow do
    begin
      BiDiModeContents := bdRightToLeft;
      Title := SettingsHelps.Hint;
      MaxLineLength := 120;
      AddSection('');
      AddCollapsible(OpenSubs.Caption,OpenSubs.Hint);
      AddSection(rsProcessSettings);
      AddCollapsible(StripHTMLFontTagsL.Caption,StripHTMLFontTagsL.Hint);
      AddCollapsible(StripHTMLStyleTagsL.Caption,StripHTMLStyleTagsL.Hint);
      AddCollapsible(ArabicCharsToFarsiL.Caption,ArabicCharsToFarsiL.Hint); 
      AddCollapsible(EnglishNumbersL.Caption,EnglishNumbersL.Hint);
      AddCollapsible(SubtitleDelayL.Caption,SubtitleDelayL.Hint);
      AddCollapsible(EndingPunctuationsL.Caption,EndingPunctuationsL.Hint);
      AddCollapsible(PhrasesCensorshipL.Caption,PhrasesCensorshipL.Hint);
      AddSection(rsOutputFileSettings);
      AddCollapsible(OutFileEncodingL.Caption,OutFileEncodingL.Hint);
      AddCollapsible(AppendEncodingToFileNameL.Caption,AppendEncodingToFileNameL.Hint);
      AddCollapsible(ReplaceSourceFileL.Caption,ReplaceSourceFileL.Hint);
    end;
  end;
  FHelpWindow.Show;
end;

end.

