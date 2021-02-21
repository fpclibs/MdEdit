{
 *******************************************************************************
 *                                                                             *
 *  LGPL with linking exception (like Lazarus). See the file license.md,       *
 *  included in this distribution, for details about the copyright.            *
 *                                                                             *
 *  This program is distributed in the hope that it will be useful,            *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       *
 *                                                                             *
 *  Sources: ndreas Peter Luft, January 10 2021                                *
 *                                                                             *
 *******************************************************************************
}
unit CnfSyneditFrameThemes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, ComCtrls, Dialogs, SynEdit, LazFileUtils,
  LazUTF8, FrameSettingsBase, CnfSyneditPreview, CnfSyneditOptions,
  // Cnf
  CnfSyneditFrameColours, Storage, CnfDialogComboBoxItems, Language,
  SysUtils;
type

  { TCnfSyneditFrameThemes }

  TCnfSyneditFrameThemes = class(TFrameSettingsBase)
    ButtonThemesExport: TButton;
    ButtonThemesLoad: TButton;
    ComboBoxThemes: TComboBox;
    ComboBoxLanguage: TComboBox;
    ComboBoxFileExtensions: TComboBox;
    LabelSelectMode: TLabel;
    LabelSelectScheme: TLabel;
    LabelSelectExtension: TLabel;
    LabelSelectThemes: TLabel;
    OpenDialogScheme: TOpenDialog;
    RadioButtonLightMode: TRadioButton;
    RadioButtonDarkMode: TRadioButton;
    SaveDialogScheme: TSaveDialog;
    SynEditTheme: TSynEdit;
    EditFileExtensions: TEdit;

    procedure ButtonThemesExportClick(Sender: TObject);
    procedure ButtonThemesLoadClick(Sender: TObject);
    procedure ComboBoxLanguageChange(Sender: TObject);
    procedure ComboBoxThemesChange(Sender: TObject);
    procedure RadioButtonDarkModeChange(Sender: TObject);
    procedure RadioButtonLightModeChange(Sender: TObject);

  private
    mHighligterPreview: THighligterPreview;
    mContentPreview:    TContentPreview;
    mEditorOptions:     TEditorOptions;

    mThemePreview: Boolean;
    mFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    mCBox: TComboBoxItems;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  Initialize();

    procedure SetSettings(); override;
    procedure GetSettings(); override;
    Function  GetTreePath: TStrArray override;

  private
    function GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);

    function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;

    procedure SchemeSave(const actfile: string);
    procedure SchemeLoad(const actfile: string);
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}


{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TCnfSyneditFrameThemes.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);
  mThemePreview := false;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TCnfSyneditFrameThemes.Destroy;
begin
  mFileExtensions.Free;
  mCBox.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.Initialize();
begin
  if mCBox = Nil then mCBox := TComboBoxItems.Create
  else Exit;
  mCBox.Settings := Settings;
  mCBox.Path     := 'EditorOptions/Themes/RecentThemeFiles';
  mCBox.ComboBox := ComboBoxThemes;
  mCBox.LoadDialog := OpenDialogScheme;
  mCBox.SaveDialog := SaveDialogScheme;
  mCBox.OnLoadFile := @SchemeLoad;
  mCBox.OnSaveFile := @SchemeSave;
  mCBox.Extension  := '.xml';
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.SetSettings();
var
  i, curLanguageID: integer;
  scheme: String;
  theme:  String;
begin
  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mContentPreview    := TContentPreview   (Settings.Services.FindFirstByClass(TContentPreview));
  mEditorOptions     := TEditorOptions    (Settings.Services.FindFirstByClass(TEditorOptions));

  if mThemePreview = false then
  begin
    Initialize();
    mCBox.FileSet('');

    mContentPreview.AddPreviewEditor(SynEditTheme);

    ComboBoxLanguage.Items.Clear;
    curLanguageID := -1;

    for i := 0 to mEditorOptions.HighlighterList.Count - 1 do
    begin
      scheme := mEditorOptions.HighlighterList[i].SynClass.GetLanguageName;
      ComboBoxLanguage.Items.Add(scheme);
      if scheme = 'Markdown' then curLanguageID := i;
    end;
    ComboBoxLanguage.ItemIndex := curLanguageID;

    if curLanguageID >= 0 then
    begin
      EditFileExtensions.Text := mEditorOptions.HighlighterList[curLanguageID].FileExtensions;
    end;

    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).Settings := Settings;
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).ReadSettings(mEditorOptions);
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).UpdateScheme (curLanguageID);

    mThemePreview := true;
  end;
  mCBox.FileSet('');

  theme := Settings.Read('EditorOptions/Themes/Value', 'LightMode');
  if theme = 'DarkMode' then
  begin
    RadioButtonDarkMode.Checked  := True;
    RadioButtonLightMode.Checked := False;
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).SetCurrentSchemeAll('DarkMode');
  end
  else
  begin
    RadioButtonDarkMode.Checked  := False;
    RadioButtonLightMode.Checked := True;
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).SetCurrentSchemeAll('LightMode');
  end;
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.GetSettings();
var
  flag : Boolean;
begin
  flag := RadioButtonLightMode.Checked;
  if flag then
  begin
    Settings.Write('EditorOptions/Themes/Value', 'LightMode');
  end;
  if RadioButtonDarkMode.Checked then
  begin
    Settings.Write('EditorOptions/Themes/Value', 'DarkMode');
  end;
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TCnfSyneditFrameThemes.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Editor', 'Themes');
begin
  ary[0] := LStr('T_CnfSyneditFrame_TreeEditor',       ary[0]);
  ary[1] := LStr('T_CnfSyneditFrameThemes_TreeThemes', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  RadioButtonDarkModeChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.RadioButtonDarkModeChange(Sender: TObject);
begin
  if RadioButtonDarkMode.Checked then
  begin
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).SetCurrentSchemeAll('DarkMode');
    Settings.Write('EditorOptions/Themes/Value', 'DarkMode');
  end;
end;


{-------------------------------------------------------------------------------
  RadioButtonLightModeChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.RadioButtonLightModeChange(Sender: TObject);
begin
  if RadioButtonLightMode.Checked then
  begin
    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).SetCurrentSchemeAll('LightMode');
    Settings.Write('EditorOptions/Themes/Value', 'LightMode');
  end;
end;


{-------------------------------------------------------------------------------
  ComboBoxLanguageChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.ComboBoxLanguageChange(Sender: TObject);
var
  i: integer;
  language: String;
begin
  if ComboBoxLanguage.ItemIndex >= 0 then
  begin
    i := ComboBoxLanguage.ItemIndex;
    language := ComboBoxLanguage.Items[i];
    if language = '' then begin end;
    EditFileExtensions.Text := mEditorOptions.HighlighterList[i].FileExtensions;

    TCnfSyneditFrameColours(mEditorOptions.ColoursPage).UpdateScheme(i);
  end;
end;


{-------------------------------------------------------------------------------
  ButtonThemesLoadClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.ButtonThemesLoadClick(Sender: TObject);
begin
  mCBox.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonThemesExportClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.ButtonThemesExportClick(Sender: TObject);
begin
  mCBox.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxThemesChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.ComboBoxThemesChange(Sender: TObject);
begin
  mCBox.FileChange();
end;


{-------------------------------------------------------------------------------
  SchemeSave
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.SchemeSave(const actfile: string);
var
  storage     : TAppStorage;
  NewScheme   : TColorScheme;
  schameName  : String;
  scheme      : String;
  schemeClass : TColorScheme;
  colorSchemeSettings: TColorSchemeFactory;

begin
  schameName := 'LightMode';
  if RadioButtonDarkMode.Checked then schameName := 'DarkMode';

  scheme := 'LightMode';
  if RadioButtonDarkMode.Checked then scheme := 'DarkMode';

  storage := TAppStorage.CreateClean(actfile);
  storage.SetValue('MdEdit/ColorSchemes/Names/Count', 1);
  storage.SetValue('MdEdit/ColorSchemes/Names/Item1/Value', schameName);

  colorSchemeSettings := TColorSchemeFactory.Create;

  colorSchemeSettings.Assign(mEditorOptions.UserColorSchemeGroup);

  NewScheme := TColorScheme.Create(scheme);

//  schemeClass := colorSchemeSettings.ColorSchemeGroup['Pascal Classic'];
  schemeClass := colorSchemeSettings.ColorSchemeGroup[scheme];

  NewScheme.Assign(schemeClass);
  NewScheme.SaveToXml(storage, 'MdEdit/ColorSchemes/', nil);
  NewScheme.Free;

  FreeAndNil(colorSchemeSettings);

  InvalidateFileStateCache;
  storage.Flush;
  storage.Free;
end;


{-------------------------------------------------------------------------------
  SchemeLoad
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.SchemeLoad(const actfile: string);
var
  storage : TAppStorage;
  factory : TColorSchemeFactory;
  scheme  : String;
  default : String;
  path    : String;
  res     : TModalResult;
begin
  default := 'LightMode';
  if RadioButtonDarkMode.Checked then default := 'DarkMode';

  storage := TAppStorage.Create(actfile);

  path := 'MdEdit/ColorSchemes/';
  scheme := storage.Read(path + 'Names/Item1/Value', '');

  if default <> scheme then
  begin
    res := QuestionDlg(

      LStr('T_CnfSyneditFrameThemes_SchemeLoadErrHead',  'Load File'),
      LStr('T_CnfSyneditFrameThemes_SchemeLoadErrBody',  'The Scheme of the file is: ') + scheme + LineEnding + LineEnding +
      LStr('T_CnfSyneditFrameThemes_SchemeLoadQuestion', 'Do you whant to load in anyway?'),
      mtWarning, [
        mrYes, LStr('T_FormMain_MsgYes', '&Ja'),
        mrCancel, LStr('T_FormMain_MsgCancel',  '&Abbrechen')
      ], 0
    );
    if res = mrCancel then exit;
  end;

  if storage.HasPath(path + 'Globals/Scheme' + scheme, false) = false then
  begin
    scheme := '';
  end;

  factory := TCnfSyneditFrameColours(mEditorOptions.ColoursPage).UnsavedColorSchemeSettings;
  factory.LoadSchemeFromXml(storage, path, default, scheme);

  TCnfSyneditFrameColours(mEditorOptions.ColoursPage).UpdateScheme(ComboBoxLanguage.ItemIndex);
end;


{-------------------------------------------------------------------------------
  GetCurFileExtensions
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameThemes.GetCurFileExtensions(const LanguageName: String): String;
var
  i: Integer;
begin
  if mFileExtensions = nil then
    Result := ''
  else
    Result := mFileExtensions.Values[LanguageName];
  if Result = '' then
  begin
    i := mEditorOptions.HighlighterList.FindByName(LanguageName);
    if i >= 0 then
      Result := mEditorOptions.HighlighterList[i].FileExtensions;
  end;
end;


{-------------------------------------------------------------------------------
  SetCurFileExtensions
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.SetCurFileExtensions(const LanguageName, FileExtensions: String);
begin
  if mFileExtensions = nil then
    mFileExtensions := TStringList.Create;
  mFileExtensions.Values[LanguageName] := FileExtensions;
end;


{-------------------------------------------------------------------------------
  IndexInStringList
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameThemes.IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
var
  i: Integer;
begin
  for i := 0 to List.Count - 1 do begin
    case Cmp of
    cstCaseSensitive:   if List[i] = s then exit(i);
    cstCaseInsensitive: if UTF8CompareText (List[i], s) = 0 then exit(i);
    cstFilename:        if CompareFilenames(List[i], s) = 0 then exit(i);
    end;
  end;
  Result := -1;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameThemes.OnReplaceLng();
begin
  LCap('T_CnfSyneditFrameThemes_LabelSelectMode',      LabelSelectMode);
  LCap('T_CnfSyneditFrameThemes_RadioButtonLightMode', RadioButtonLightMode);
  LCap('T_CnfSyneditFrameThemes_RadioButtonDarkMode',  RadioButtonDarkMode);
  LCap('T_CnfSyneditFrameThemes_LabelSelectScheme',    LabelSelectScheme);
  LCap('T_CnfSyneditFrameThemes_LabelSelectExtension', LabelSelectExtension);
  LCap('T_CnfSyneditFrameThemes_LabelSelectThemes',    LabelSelectThemes);
  LCap('T_General_BtnLoad',                            ButtonThemesLoad);
  LCap('T_General_BtnExport',                          ButtonThemesExport);
end;



end.

