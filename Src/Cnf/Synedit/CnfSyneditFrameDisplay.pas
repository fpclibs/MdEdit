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
 *  Sources: Convert parts of Lazarus IDE unit to fpcLibs                      *
 *           Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
unit CnfSyneditFrameDisplay;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc,
  // LCL
  Graphics, Dialogs, StdCtrls, Spin, LCLType, Controls, LazUTF8, LazFileUtils,
  // SynEdit
  SynEdit, SynEditMouseCmds, SynGutterLineNumber, SynGutterLineOverview,
  SynGutter,
  // Cnf
  FrameSettingsBase,  CnfSyneditOptions, CnfSyneditFrameGeneral,
  CnfSyneditFrameColours, CnfSyneditPreview,

  Language, SysUtils;


type
  { TCnfSyneditFrameDisplay }

  TCnfSyneditFrameDisplay = class(TFrameSettingsBase)
    CheckBoxTopInfoView: TCheckBox;
    CheckBoxShowOverview: TCheckBox;
    CheckBoxDisableAntialiasing: TCheckBox;
    DisplayPreview: TSynEdit;
    EditorFontButton: TButton;
    EditorFontComboBox: TComboBox;
    GroupBoxEditorFont: TGroupBox;
    EditorFontSizeSpinEdit: TSpinEdit;
    LabelEditorFontSize: TLabel;
    ExtraCharSpacingComboBox: TComboBox;
    LabelExtraCharSpacing: TLabel;
    ExtraLineSpacingComboBox: TComboBox;
    LabelExtraLineSpacing: TLabel;
    LabelGutterSeparatorIndex: TLabel;
    GroupBoxMarginAndGutter: TGroupBox;
    LabelRightMarginColorLink: TLabel;
    RightMarginComboBox: TComboBox;
    LabelRightMargin: TLabel;
    CheckBoxShowLineNumbers: TCheckBox;
    LabelShowOnlyLineNumbersMultiplesOf: TLabel;
    ShowOnlyLineNumbersMultiplesOfSpinEdit: TSpinEdit;
    GutterSeparatorIndexSpinBox: TSpinEdit;
    CheckBoxVisibleGutter: TCheckBox;
    CheckBoxVisibleRightMargin: TCheckBox;
    procedure EditorFontButtonClick(Sender: TObject);
    procedure EditorFontComboBoxEditingDone(Sender: TObject);
    procedure EditorFontSizeSpinEditChange(Sender: TObject);
    procedure ComboboxOnExit(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboboxOnChange(Sender: TObject);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure RightMarginColorLinkClick(Sender: TObject);
    procedure RightMarginColorLinkMouseEnter(Sender: TObject);
    procedure RightMarginColorLinkMouseLeave(Sender: TObject);
    procedure ShowLineNumbersCheckBoxClick(Sender: TObject);
  private
    ODisplayPreview: TSynEdit;
    FUpdatingFontSizeRange: Boolean;

    mHighligterPreview: THighligterPreview;
    mContentPreview: TContentPreview;
    mEditorOptions: TEditorOptions;

    function FontSizeNegativeToPositive(NegativeSize: Integer): Integer;
    function GeneralPage: TCnfSyneditFrameGeneral; inline;
    procedure SetEditorFontSizeSpinEditValue(FontSize: Integer);

    procedure FontDialogApplyClicked(Sender: TObject);
    function DoSynEditMouse(var {%H-}AnInfo: TSynEditMouseActionInfo;
      {%H-}HandleActionProc: TSynEditMouseActionHandler): Boolean;
    procedure OnReplaceLng();

  public
    constructor Create(AOwner: TComponent); override;

    function GetTitle: String;
    procedure Setup({%H-}ADialog: TFrameSettingsBase);
    procedure ReadSettings({%H-}AOptions: TEditorOptions);
    procedure WriteSettings(AOptions: TEditorOptions);
//    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  public
    procedure SetSettings(); override;
    procedure GetSettings(); override;
    Function  GetTreePath: TStrArray override;

  end;


function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
procedure SetComboBoxText(AComboBox: TComboBox; const AText: String;
  Cmp: TCmpStrType; MaxCount: integer = 1000);


implementation

{$R *.lfm}


uses
  LCLIntf;


{-------------------------------------------------------------------------------
  IndexInStringList
------------------------------------------------------------------------------}
function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
var
  i: Integer;
begin
  for i:=0 to List.Count-1 do begin
    case Cmp of
    cstCaseSensitive:   if List[i]=s then exit(i);
    cstCaseInsensitive: if UTF8CompareText(List[i],s)=0 then exit(i);
    cstFilename:        if CompareFilenames(List[i],s)=0 then exit(i);
    end;
  end;
  Result:=-1;
end;


{-------------------------------------------------------------------------------
  SetComboBoxText
------------------------------------------------------------------------------}
procedure SetComboBoxText(AComboBox:TComboBox; const AText: String;
  Cmp: TCmpStrType; MaxCount: integer);
var
  a: integer;
begin
  if AText<>'' then begin
    a := IndexInStringList(AComboBox.Items,Cmp,AText);
    if a >= 0 then
      AComboBox.ItemIndex := a
    else
    begin
      AComboBox.Items.Insert(0,AText);
      AComboBox.ItemIndex:=IndexInStringList(AComboBox.Items,Cmp,AText);
      if MaxCount<2 then MaxCount:=2;
      while AComboBox.Items.Count>MaxCount do
        AComboBox.Items.Delete(AComboBox.Items.Count-1);
    end;
  end;
  AComboBox.Text := AText;
end;


{ TCnfSyneditFrameDisplay }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TCnfSyneditFrameDisplay.Create(AOwner: TComponent);
begin
  ODisplayPreview := Nil;
  inherited Create(AOwner);

  OLng.AddListener(@OnReplaceLng);
end;


{-------------------------------------------------------------------------------
  Setup
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.Setup(ADialog: TFrameSettingsBase);
begin
  // Prevent the caret from moving
  DisplayPreview.RegisterMouseActionSearchHandler(@DoSynEditMouse);
//  FDialog := ADialog;
  FUpdatingFontSizeRange := False;

  LCap('dlgMarginGutter', GroupBoxMarginAndGutter);
  LCap('dlgVisibleRightMargin', CheckBoxVisibleRightMargin);
  LCap('dlgVisibleGutter', CheckBoxVisibleGutter);
  LCap('dlgShowLineNumbers', CheckBoxShowLineNumbers);
  LCap('lisEveryNThLineNumber', LabelShowOnlyLineNumbersMultiplesOf);
  LCap('dlgGutterSeparatorIndex', LabelGutterSeparatorIndex);
  LCap('dlgRightMargin', LabelRightMargin);
  LCap('dlgDefaultEditorFont', GroupBoxEditorFont);
  LCap('dlgEditorFontSize', LabelEditorFontSize);
  LCap('dlgExtraCharSpacing', LabelExtraCharSpacing);
  LCap('dlgExtraLineSpacing', LabelExtraLineSpacing);
  LCap('dlgDisableAntialiasing', CheckBoxDisableAntialiasing);
  LCap('dlgColorLink', LabelRightMarginColorLink);
  LCap('lisShowOverviewGutter', CheckBoxShowOverview);
  LCap('lisTopInfoView', CheckBoxTopInfoView);

  if ODisplayPreview = Nil then
  begin
  with GeneralPage do
    mContentPreview.AddPreviewEditor(DisplayPreview);
    ODisplayPreview := DisplayPreview;

    with TSynGutterSeparator.Create(ODisplayPreview.RightGutter.Parts) do
      Name := 'DPSynGutterSeparatorR2';
    with TSynGutterLineOverview.Create(ODisplayPreview.RightGutter.Parts) do begin
      Name := 'DPSynGutterLineOverview1';
      with TSynGutterLOvProviderModifiedLines.Create(Providers) do
        Priority := 9;
      with TSynGutterLOvProviderCurrentPage.Create(Providers) do
        Priority := 1;
    end;
    with TSynGutterSeparator.Create(ODisplayPreview.RightGutter.Parts) do begin
      Name := 'DPSynGutterSeparatorR3';
      AutoSize := False;
      Width := 1;
      LineWidth := 0;
    end;
  end;

end;


{-------------------------------------------------------------------------------
  GeneralPage
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameDisplay.GeneralPage: TCnfSyneditFrameGeneral;
begin
  Result := TCnfSyneditFrameGeneral(mEditorOptions.GeneralPage);
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.SetSettings();
begin
  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mContentPreview    := TContentPreview   (Settings.Services.FindFirstByClass(TContentPreview));
  mEditorOptions     := TEditorOptions    (Settings.Services.FindFirstByClass(TEditorOptions));

  with mEditorOptions do
  begin
    // init the spin-edit first, since it does not trigger on change,
    // but is copied when checkboxes are initialized
    ShowOnlyLineNumbersMultiplesOfSpinEdit.Value := Settings.Read('SynEdit/Options/ShowOnlyLineNumbersMultiplesOf', ShowOnlyLineNumbersMultiplesOf);
    GutterSeparatorIndexSpinBox.Value := GutterSeparatorIndex;
    CheckBoxVisibleRightMargin.Checked := VisibleRightMargin;
    CheckBoxVisibleGutter.Checked := VisibleGutter;
    CheckBoxShowLineNumbers.Checked := ShowLineNumbers;
    CheckBoxVisibleRightMargin.Checked := VisibleRightMargin;
    SetComboBoxText(RightMarginComboBox, IntToStr(RightMargin), cstCaseInsensitive);
    SetComboBoxText(EditorFontComboBox, EditorFont,cstCaseInsensitive);
    SetEditorFontSizeSpinEditValue(EditorFontSize);
    SetComboBoxText(ExtraCharSpacingComboBox, IntToStr(ExtraCharSpacing), cstCaseInsensitive);
    SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(ExtraLineSpacing), cstCaseInsensitive);
    CheckBoxDisableAntialiasing.Checked := DisableAntialiasing;
    CheckBoxShowOverview.Checked := ShowOverviewGutter;
    CheckBoxTopInfoView.Checked  := TopInfoView;
  end;
  LabelShowOnlyLineNumbersMultiplesOf.Enabled    := CheckBoxShowLineNumbers.Checked;
  ShowOnlyLineNumbersMultiplesOfSpinEdit.Enabled := CheckBoxShowLineNumbers.Checked;
  Setup(self);
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.GetSettings();
begin
  WriteSettings(mEditorOptions);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TCnfSyneditFrameDisplay.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Editor', 'Anzeige');
begin
  ary[0] := LStr('T_CnfSyneditFrame_TreeEditor',         ary[0]);
  ary[1] := LStr('T_CnfSyneditFrameDisplay_TreeDisplay', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  GetTitle
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameDisplay.GetTitle: String;
begin
  Result := LStr('dlgEdDisplay', 'Display');
end;


{-------------------------------------------------------------------------------
  ReadSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.ReadSettings(AOptions: TEditorOptions);
begin
  with mEditorOptions do
  begin
    // init the spin-edit first, since it does not trigger on change,
    // but is copied when checkboxes are initialized
    ShowOnlyLineNumbersMultiplesOfSpinEdit.Value := ShowOnlyLineNumbersMultiplesOf;
    GutterSeparatorIndexSpinBox.Value := GutterSeparatorIndex;
    CheckBoxVisibleRightMargin.Checked := VisibleRightMargin;
    CheckBoxVisibleGutter.Checked := VisibleGutter;
    CheckBoxShowLineNumbers.Checked := ShowLineNumbers;
    CheckBoxVisibleRightMargin.Checked := VisibleRightMargin;
    SetComboBoxText(RightMarginComboBox, IntToStr(RightMargin), cstCaseInsensitive);
    SetComboBoxText(EditorFontComboBox, EditorFont, cstCaseInsensitive);
    SetEditorFontSizeSpinEditValue(EditorFontSize);
    SetComboBoxText(ExtraCharSpacingComboBox, IntToStr(ExtraCharSpacing), cstCaseInsensitive);
    SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(ExtraLineSpacing), cstCaseInsensitive);
    CheckBoxDisableAntialiasing.Checked := DisableAntialiasing;
    CheckBoxShowOverview.Checked := ShowOverviewGutter;
    CheckBoxTopInfoView.Checked := TopInfoView;
  end;
  LabelShowOnlyLineNumbersMultiplesOf.Enabled := CheckBoxShowLineNumbers.Checked;
  ShowOnlyLineNumbersMultiplesOfSpinEdit.Enabled := CheckBoxShowLineNumbers.Checked;
end;


{-------------------------------------------------------------------------------
  WriteSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.WriteSettings(AOptions: TEditorOptions);
begin
  with AOptions as TEditorOptions do
  begin
    VisibleRightMargin := CheckBoxVisibleRightMargin.Checked;
    VisibleGutter := CheckBoxVisibleGutter.Checked;
    ShowLineNumbers := CheckBoxShowLineNumbers.Checked;
    ShowOnlyLineNumbersMultiplesOf := ShowOnlyLineNumbersMultiplesOfSpinEdit.Value;
    GutterSeparatorIndex := GutterSeparatorIndexSpinBox.Value;
    VisibleRightMargin := CheckBoxVisibleRightMargin.Checked;
    RightMargin := StrToIntDef(RightMarginComboBox.Text, 80);
    EditorFont := EditorFontComboBox.Text;
    EditorFontSize := EditorFontSizeSpinEdit.Value;
    ExtraCharSpacing := StrToIntDef(ExtraCharSpacingComboBox.Text, ExtraCharSpacing);
    ExtraLineSpacing := StrToIntDef(ExtraLineSpacingComboBox.Text, ExtraLineSpacing);
    DisableAntialiasing := CheckBoxDisableAntialiasing.Checked;
    ShowOverviewGutter := CheckBoxShowOverview.Checked;
    TopInfoView := CheckBoxTopInfoView.Checked;
  end;
end;


{-------------------------------------------------------------------------------
  FontSizeNegativeToPositive
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameDisplay.FontSizeNegativeToPositive(NegativeSize: Integer): Integer;
var
  tm: TTextMetric;
begin
  DisplayPreview.Canvas.Font.Assign(DisplayPreview.Font);
  if LCLIntf.GetTextMetrics(DisplayPreview.Canvas.Handle, tm{%H-}) then
    Result := -(NegativeSize + MulDiv(tm.tmInternalLeading, 72, DisplayPreview.Font.PixelsPerInch))
  else
    Result := -NegativeSize;
end;


{-------------------------------------------------------------------------------
  FontDialogApplyClicked
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.FontDialogApplyClicked(Sender: TObject);
var
  ed: TPreviewEdit;
begin
  with GeneralPage do
  begin
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do ed.Font.Assign(TFontDialog(Sender).Font);
  end;

  SetComboBoxText(EditorFontComboBox, DisplayPreview.Font.Name,cstCaseInsensitive);
  SetEditorFontSizeSpinEditValue(DisplayPreview.Font.Size);
end;


{-------------------------------------------------------------------------------
  DoSynEditMouse
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameDisplay.DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := true;
end;


{-------------------------------------------------------------------------------
  EditorFontButtonClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.EditorFontButtonClick(Sender: TObject);
var
  FontDialog: TFontDialog;
  CurFontSize: Integer;
begin
  FontDialog := TFontDialog.Create(nil);
  try
    with FontDialog do
    begin
      Font.Name := EditorFontComboBox.Text;
      CurFontSize := EditorFontSizeSpinEdit.Value;
      if CurFontSize < 0 then
      begin
        CurFontSize := FontSizeNegativeToPositive(CurFontSize);
        RepairEditorFontSize(CurFontSize);
      end;
      Font.Size := CurFontSize;
      Options := Options + [fdApplyButton];
      OnApplyClicked := @FontDialogApplyClicked;
      if Execute then
        FontDialogApplyClicked(FontDialog);
    end;
  finally
    FontDialog.Free;
  end;
end;


{-------------------------------------------------------------------------------
  EditorFontComboBoxEditingDone
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.EditorFontComboBoxEditingDone(Sender: TObject);
var
ed: TPreviewEdit;
begin
  with GeneralPage do
  begin
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do ed.Font.Name := EditorFontComboBox.Text;
  end;
end;


{-------------------------------------------------------------------------------
  EditorFontSizeSpinEditChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.EditorFontSizeSpinEditChange(Sender: TObject);
var
  NewVal: Integer;
  ed: TPreviewEdit;
  s: TCaption;
begin
  s := EditorFontSizeSpinEdit.Text;
  if copy(trim(s),1,1) = '-' then begin
    if EditorFontSizeSpinEdit.MinValue > 0 then begin
      EditorFontSizeSpinEdit.MinValue := -100;
      EditorFontSizeSpinEdit.MaxValue := -EditorOptionsMinimumFontSize;
      EditorFontSizeSpinEdit.Text := s;
    end
    else
    if EditorFontSizeSpinEdit.Value > -EditorOptionsMinimumFontSize then
      EditorFontSizeSpinEdit.Value := -EditorOptionsMinimumFontSize;
  end
  else begin
    if EditorFontSizeSpinEdit.MinValue < 0 then begin
      EditorFontSizeSpinEdit.MaxValue := 100;
      EditorFontSizeSpinEdit.MinValue := EditorOptionsMinimumFontSize;
      EditorFontSizeSpinEdit.Text := s;
    end
    else
    if EditorFontSizeSpinEdit.Value < EditorOptionsMinimumFontSize then
      EditorFontSizeSpinEdit.Value := EditorOptionsMinimumFontSize;
  end;

  NewVal := EditorFontSizeSpinEdit.Value;
  with GeneralPage do
  begin
     mContentPreview.SetFirst(); ed := Nil;
     while mContentPreview.GetNext(ed) do ed.Font.Size := NewVal;
  end;
end;


{-------------------------------------------------------------------------------
  ComboboxOnExit
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.ComboboxOnExit(Sender: TObject);
var
  NewVal: Integer;
  ed: TPreviewEdit;
begin
  if Sender = ExtraCharSpacingComboBox then
  begin
    NewVal := StrToIntDef(ExtraCharSpacingComboBox.Text, DisplayPreview.ExtraCharSpacing);
    SetComboBoxText(ExtraCharSpacingComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
    begin
      mContentPreview.SetFirst(); ed := Nil;
      while mContentPreview.GetNext(ed) do ed.ExtraCharSpacing := NewVal;
    end;
  end
  else
  if Sender = ExtraLineSpacingComboBox then
  begin
    NewVal := StrToIntDef(ExtraLineSpacingComboBox.Text, DisplayPreview.ExtraLineSpacing);
    SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
    begin
      mContentPreview.SetFirst(); ed := Nil;
      while mContentPreview.GetNext(ed) do ed.ExtraLineSpacing := NewVal;
    end;
  end
  else
  if Sender = RightMarginComboBox then
  begin
    NewVal := StrToIntDef(RightMarginComboBox.Text, DisplayPreview.RightEdge);
    SetComboBoxText(RightMarginComboBox, IntToStr(NewVal),cstCaseInsensitive);
    with GeneralPage do
    begin
      mContentPreview.SetFirst(); ed := Nil;
      while mContentPreview.GetNext(ed) do
      begin
        ed.RightEdge := NewVal;
        if CheckBoxVisibleRightMargin.Checked then
          ed.Options := ed.Options - [eoHideRightMargin]
        else
          ed.Options := ed.Options + [eoHideRightMargin];
      end;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ComboBoxOnKeyDown
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;


{-------------------------------------------------------------------------------
  ComboboxOnChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.ComboboxOnChange(Sender: TObject);
var
  ComboBox: TComboBox absolute Sender;
begin
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;


{-------------------------------------------------------------------------------
  GeneralCheckBoxOnChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.GeneralCheckBoxOnChange(Sender: TObject);
var
  ed: TPreviewEdit;
  AGeneralPage: TCnfSyneditFrameGeneral;
  Separator: TSynGutterSeparator;
begin
  AGeneralPage := GeneralPage;

  if AGeneralPage = nil then
    Exit;

  with AGeneralPage do
  begin
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do
      if ed <> nil then
      begin
        ed.Gutter.Visible := CheckBoxVisibleGutter.Checked;
        ed.RightGutter.Visible := CheckBoxShowOverview.Checked;
        ed.Gutter.LineNumberPart.Visible
          := CheckBoxShowLineNumbers.Checked;
        if Assigned(ed.Gutter.Parts.ByClass[TSynGutterLineNumber, 0]) then
          TSynGutterLineNumber(ed.Gutter.Parts.ByClass[TSynGutterLineNumber, 0])
            .ShowOnlyLineNumbersMultiplesOf := ShowOnlyLineNumbersMultiplesOfSpinEdit.Value;

        Separator := TSynGutterSeparator(ed.Gutter.Parts.ByClass[TSynGutterSeparator, 0]);
        if Assigned(Separator) then
        begin
          Separator.Visible := GutterSeparatorIndexSpinBox.Value <> -1;
          if Separator.Visible then
            Separator.Index := GutterSeparatorIndexSpinBox.Value;
        end;
        ed.RightEdge := StrToIntDef(RightMarginComboBox.Text, 80);
        if CheckBoxVisibleRightMargin.Checked then
          ed.Options := ed.Options - [eoHideRightMargin]
        else
          ed.Options := ed.Options + [eoHideRightMargin];
        if CheckBoxDisableAntialiasing.Checked then
          ed.Font.Quality := fqNonAntialiased
        else
          ed.Font.Quality := fqDefault;
      end;
  end;
end;


{-------------------------------------------------------------------------------
  RightMarginColorLinkClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.RightMarginColorLinkClick(Sender: TObject);
begin

end;


{-------------------------------------------------------------------------------
  RightMarginColorLinkMouseEnter
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.RightMarginColorLinkMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;


{-------------------------------------------------------------------------------
  RightMarginColorLinkMouseLeave
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.RightMarginColorLinkMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;


{-------------------------------------------------------------------------------
  ShowLineNumbersCheckBoxClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.ShowLineNumbersCheckBoxClick(Sender: TObject);
begin
  ShowOnlyLineNumbersMultiplesOfSpinEdit.Enabled := CheckBoxShowLineNumbers.Checked;
  LabelShowOnlyLineNumbersMultiplesOf.Enabled := CheckBoxShowLineNumbers.Checked;
end;


{-------------------------------------------------------------------------------
  SetEditorFontSizeSpinEditValue
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.SetEditorFontSizeSpinEditValue(FontSize: Integer);
begin
  FUpdatingFontSizeRange := True;
  if FontSize < 0 then begin
    EditorFontSizeSpinEdit.MinValue := -100;
    EditorFontSizeSpinEdit.MaxValue := -EditorOptionsMinimumFontSize;
  end
  else begin
    EditorFontSizeSpinEdit.MaxValue := 100;
    EditorFontSizeSpinEdit.MinValue := EditorOptionsMinimumFontSize;
  end;
  FUpdatingFontSizeRange := False;
  EditorFontSizeSpinEdit.Value := FontSize;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameDisplay.OnReplaceLng();
begin
  LCap('T_CnfSyneditFrameDisplay_GroupBoxMarginAndGutter',     GroupBoxMarginAndGutter);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxVisibleRightMargin',  CheckBoxVisibleRightMargin);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxVisibleGutter',       CheckBoxVisibleGutter);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxShowLineNumbers',     CheckBoxShowLineNumbers);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxShowOverview',        CheckBoxShowOverview);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxTopInfoView',         CheckBoxTopInfoView);
  LCap('T_CnfSyneditFrameDisplay_LabelRightMargin',            LabelRightMargin);
  LCap('T_CnfSyneditFrameDisplay_LabelRightMarginColorLink',   LabelRightMarginColorLink);
  LCap('T_CnfSyneditFrameDisplay_LabelGutterSeparatorIndex',   LabelGutterSeparatorIndex);
  LCap('T_CnfSyneditFrameDisplay_LabelShowOnlyLineNumbersMultiplesOf', LabelShowOnlyLineNumbersMultiplesOf);
  LCap('T_CnfSyneditFrameDisplay_GroupBoxEditorFont',          GroupBoxEditorFont);
  LCap('T_CnfSyneditFrameDisplay_LabelEditorFontSize',         LabelEditorFontSize);
  LCap('T_CnfSyneditFrameDisplay_CheckBoxDisableAntialiasing', CheckBoxDisableAntialiasing);
  LCap('T_CnfSyneditFrameDisplay_LabelExtraLineSpacing',       LabelExtraLineSpacing);
  LCap('T_CnfSyneditFrameDisplay_LabelExtraCharSpacing',       LabelExtraCharSpacing);
end;



end.

