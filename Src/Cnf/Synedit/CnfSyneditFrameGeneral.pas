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
unit CnfSyneditFrameGeneral;

// {$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, LazUTF8, LazFileUtils,
  // LCL
  LCLProc, LCLType, StdCtrls, Controls, Graphics, ExtCtrls,
  // LazControls
  DividerBevel, Language,
  // SynEdit
  SynEdit, SynHighlighterPas, SynPluginMultiCaret,
  // Cnf
  CnfSyneditOptions, FrameSettingsBase, CnfSyneditPreview;

type
  TPreviewEditor = TSynEdit;

  { TCnfSyneditFrameGeneral }

  TCnfSyneditFrameGeneral = class(TFrameSettingsBase)
    CheckBoxAlwaysVisibleCursor: TCheckBox;
    DividerBevelBlock: TDividerBevel;
    DividerBevelCaret: TDividerBevel;
    CheckBoxCaretMoveClearsSelection: TCheckBox;
    CenterLabel: TLabel;
    CheckBoxMultiCaretColumnMode: TCheckBox;
    CheckBoxMultiCaretDelSkipCr: TCheckBox;
    CheckBoxMultiCaretMode: TCheckBox;
    CheckBoxScrollHint: TCheckBox;
    CheckBoxCursorSkipsSelection: TCheckBox;
    CheckBoxCursorSkipsTab: TCheckBox;
    CheckBoxEndKeyJumpsToNearestStart: TCheckBox;
    CheckBoxGroupUndo: TCheckBox;
    CheckBoxHalfPageScroll: TCheckBox;
    CheckBoxHomeKeyJumpsToNearestStart: TCheckBox;
    CheckBoxKeepCursorX: TCheckBox;
    DividerBevelMultiCaretGroup: TDividerBevel;
    CheckBoxMultiCaretOnColumnSelection: TCheckBox;
    CheckBoxOverwriteBlock: TCheckBox;
    PanelBG: TPanel;
    CheckBoxPersistentBlock: TCheckBox;
    CheckBoxPersistentCursor: TCheckBox;
    CheckBoxPersistentCursorNoBlink: TCheckBox;
    CheckBoxScrollByOneLess: TCheckBox;
    DividerBevelScroll: TDividerBevel;
    CheckBoxScrollPastEndFile: TCheckBox;
    CheckBoxScrollPastEndLine: TCheckBox;
    CheckBoxUndoAfterSave: TCheckBox;
    DividerBevelUndo: TDividerBevel;
    ComboBoxUndoLimit: TComboBox;
    LabelUndoLimit: TLabel;

    procedure CheckBoxAlwaysVisibleCursorChange(Sender: TObject);
    procedure CheckBoxCaretMoveClearsSelectionChange(Sender: TObject);
    procedure CheckBoxCursorSkipsSelectionChange(Sender: TObject);
    procedure CheckBoxCursorSkipsTabChange(Sender: TObject);
    procedure CheckBoxEndKeyJumpsToNearestStartChange(Sender: TObject);
    procedure CheckBoxGroupUndoChange(Sender: TObject);
    procedure CheckBoxHalfPageScrollChange(Sender: TObject);
    procedure CheckBoxHomeKeyJumpsToNearestStartChange(Sender: TObject);
    procedure CheckBoxKeepCursorXChange(Sender: TObject);
    procedure CheckBoxOverwriteBlockChange(Sender: TObject);
    procedure CheckBoxPersistentBlockChange(Sender: TObject);
    procedure CheckBoxPersistentCursorChange(Sender: TObject);
    procedure CheckBoxPersistentCursorNoBlinkChange(Sender: TObject);
    procedure CheckBoxScrollByOneLessChange(Sender: TObject);
    procedure CheckBoxScrollPastEndFileChange(Sender: TObject);
    procedure CheckBoxScrollPastEndLineChange(Sender: TObject);

  private
    mPasExtendedKeywordsMode : Boolean;
    mPasStringKeywordMode    : TSynPasStringMode;

    mHighligterPreview: THighligterPreview;
    mContentPreview:    TContentPreview;
    mEditorOptions:     TEditorOptions;

    function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
    procedure SetComboBoxText(AComboBox:TComboBox; const AText: String; Cmp: TCmpStrType; MaxCount: integer = 1000);
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption);  overload;
    procedure SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2); overload;
    procedure UpdatePrevieEdits;
    procedure SetExtendedKeywordsMode(const AValue: Boolean);
    procedure SetStringKeywordMode(const AValue: TSynPasStringMode);
    procedure OnReplaceLng();

  protected
    procedure CreateHandle; override;

  public

    constructor Create(AOwner: TComponent); override;
    procedure Setup({%H-}ADialog: TFrameSettingsBase);

    property PasExtendedKeywordsMode: Boolean
             read mPasExtendedKeywordsMode write SetExtendedKeywordsMode default False;
    property PasStringKeywordMode: TSynPasStringMode
             read mPasStringKeywordMode write SetStringKeywordMode default spsmDefault;

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;
    procedure ReadSettings();
    procedure WriteSettings();

  end;

var
  OFrameCnfSyneditGeneral: TCnfSyneditFrameGeneral = Nil;

implementation

{$R *.lfm}

{ TCnfSyneditFrameGeneral }

{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.SetSettings();
begin
  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mContentPreview    := TContentPreview   (Settings.Services.FindFirstByClass(TContentPreview));
  mEditorOptions     := TEditorOptions    (Settings.Services.FindFirstByClass(TEditorOptions));

  mEditorOptions.Load();
  ReadSettings();

  // multi caret
  DividerBevelMultiCaretGroup.Visible         := False;
  CheckBoxMultiCaretOnColumnSelection.Visible := False;
  CheckBoxMultiCaretColumnMode.Visible        := False;
  CheckBoxMultiCaretMode.Visible              := False;
  CheckBoxMultiCaretDelSkipCr.Visible         := False;
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.GetSettings();
begin
  WriteSettings();
end;


{-------------------------------------------------------------------------------
  ReadSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.ReadSettings();
var
  ed: TPreviewEdit;
begin

  with mEditorOptions do
  begin
    // undo
    CheckBoxUndoAfterSave.Checked := UndoAfterSave;
    CheckBoxGroupUndo.Checked     := eoGroupUndo in SynEditOptions;
    SetComboBoxText(ComboBoxUndoLimit, IntToStr(UndoLimit), cstCaseInsensitive);

    // scroll
    CheckBoxHalfPageScroll.Checked    := eoHalfPageScroll in SynEditOptions;
    CheckBoxScrollByOneLess.Checked   := eoScrollByOneLess in SynEditOptions;
    CheckBoxScrollPastEndFile.Checked := eoScrollPastEoF in SynEditOptions;
    CheckBoxScrollPastEndLine.Checked := eoScrollPastEoL in SynEditOptions;
    CheckBoxScrollHint.Checked        := eoShowScrollHint in SynEditOptions;

    // cursor
    CheckBoxKeepCursorX.Checked                 := eoKeepCaretX in SynEditOptions;
    CheckBoxPersistentCursor.Checked            := eoPersistentCaret in SynEditOptions;
    CheckBoxPersistentCursorNoBlink.Checked     := eoPersistentCaretStopBlink in SynEditOptions2;
    CheckBoxAlwaysVisibleCursor.Checked         := eoAlwaysVisibleCaret in SynEditOptions2;
    CheckBoxCursorSkipsSelection.Checked        := eoCaretSkipsSelection in SynEditOptions2;
    CheckBoxCaretMoveClearsSelection.Checked    := eoCaretMoveEndsSelection in SynEditOptions2;
    CheckBoxCursorSkipsTab.Checked              := eoCaretSkipTab in SynEditOptions2;
    CheckBoxHomeKeyJumpsToNearestStart.Checked  := eoEnhanceHomeKey in SynEditOptions;
    CheckBoxEndKeyJumpsToNearestStart.Checked   := eoEnhanceEndKey in SynEditOptions2;
    CheckBoxMultiCaretOnColumnSelection.Checked := MultiCaretOnColumnSelect;
    CheckBoxMultiCaretDelSkipCr.Checked         := MultiCaretDeleteSkipLineBreak;

    // block
    CheckBoxPersistentBlock.Checked := eoPersistentBlock in SynEditOptions2;
    CheckBoxOverwriteBlock.Checked  := eoOverwriteBlock in SynEditOptions2;

    mContentPreview.SetFirst();  ed := Nil;
    while mContentPreview.GetNext(ed) do GetSynEditPreviewSettings(ed);
  end;

  Setup(self);
end;


{-------------------------------------------------------------------------------
  WriteSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.WriteSettings();

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption); overload;
  begin
    if AValue then
      mEditorOptions.SynEditOptions := mEditorOptions.SynEditOptions + [AnOption]
    else
      mEditorOptions.SynEditOptions := mEditorOptions.SynEditOptions - [AnOption];
  end;

  procedure UpdateOptionFromBool(AValue: Boolean; AnOption: TSynEditorOption2); overload;
  begin
    if AValue then
      mEditorOptions.SynEditOptions2 := mEditorOptions.SynEditOptions2 + [AnOption]
    else
      mEditorOptions.SynEditOptions2 := mEditorOptions.SynEditOptions2 - [AnOption];
  end;

var
  i: integer;
begin
  with mEditorOptions do
  begin
    // undo
    UndoAfterSave := CheckBoxUndoAfterSave.Checked;
    UpdateOptionFromBool(CheckBoxGroupUndo.Checked, eoGroupUndo);
    i := StrToIntDef(ComboBoxUndoLimit.Text, 32767);
    if i < 1 then
      i := 1;
    if i > 32767 then
      i := 32767;
    UndoLimit := i;

    // scroll
    UpdateOptionFromBool(CheckBoxHalfPageScroll.Checked, eoHalfPageScroll);
    UpdateOptionFromBool(CheckBoxScrollByOneLess.Checked, eoScrollByOneLess);
    UpdateOptionFromBool(CheckBoxScrollPastEndFile.Checked, eoScrollPastEoF);
    UpdateOptionFromBool(CheckBoxScrollPastEndLine.Checked, eoScrollPastEoL);
    UpdateOptionFromBool(CheckBoxScrollHint.Checked, eoShowScrollHint);

    // cursor
    UpdateOptionFromBool(CheckBoxKeepCursorX.Checked, eoKeepCaretX);
    UpdateOptionFromBool(CheckBoxPersistentCursor.Checked, eoPersistentCaret);
    UpdateOptionFromBool(CheckBoxPersistentCursorNoBlink.Checked, eoPersistentCaretStopBlink);
    UpdateOptionFromBool(CheckBoxAlwaysVisibleCursor.Checked, eoAlwaysVisibleCaret);
    UpdateOptionFromBool(CheckBoxCursorSkipsSelection.Checked, eoCaretSkipsSelection);
    UpdateOptionFromBool(CheckBoxCaretMoveClearsSelection.Checked, eoCaretMoveEndsSelection);
    UpdateOptionFromBool(CheckBoxCursorSkipsTab.Checked, eoCaretSkipTab);
    UpdateOptionFromBool(CheckBoxHomeKeyJumpsToNearestStart.Checked, eoEnhanceHomeKey);
    UpdateOptionFromBool(CheckBoxEndKeyJumpsToNearestStart.Checked, eoEnhanceEndKey);
    MultiCaretOnColumnSelect := CheckBoxMultiCaretOnColumnSelection.Checked;
    MultiCaretDeleteSkipLineBreak := CheckBoxMultiCaretDelSkipCr.Checked;

    // block
    UpdateOptionFromBool(CheckBoxPersistentBlock.Checked, eoPersistentBlock);
    UpdateOptionFromBool(CheckBoxOverwriteBlock.Checked, eoOverwriteBlock);
  end;
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TCnfSyneditFrameGeneral.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Editor', 'Allgemein');
begin
  ary[0] := LStr('T_CnfSyneditFrame_TreeEditor',         ary[0]);
  ary[1] := LStr('T_CnfSyneditFrameGeneral_TreeGeneral', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  AlwaysVisibleCursorCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxAlwaysVisibleCursorChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxAlwaysVisibleCursor.Checked, eoAlwaysVisibleCaret);
end;


{-------------------------------------------------------------------------------
  CaretMoveClearsSelectionCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxCaretMoveClearsSelectionChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxCaretMoveClearsSelection.Checked, eoCaretMoveEndsSelection);
end;


{-------------------------------------------------------------------------------
  CursorSkipsSelectionCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxCursorSkipsSelectionChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxCursorSkipsSelection.Checked, eoCaretSkipsSelection);
end;


{-------------------------------------------------------------------------------
  CursorSkipsTabCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxCursorSkipsTabChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxCursorSkipsTab.Checked, eoCaretSkipTab);
end;


{-------------------------------------------------------------------------------
  EndKeyJumpsToNearestStartCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxEndKeyJumpsToNearestStartChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxEndKeyJumpsToNearestStart.Checked, eoEnhanceEndKey);
end;


{-------------------------------------------------------------------------------
  GroupUndoCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxGroupUndoChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxGroupUndo.Checked, eoGroupUndo);
end;


{-------------------------------------------------------------------------------
  HalfPageScrollCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxHalfPageScrollChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxHalfPageScroll.Checked, eoHalfPageScroll);
end;


{-------------------------------------------------------------------------------
  HomeKeyJumpsToNearestStartCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxHomeKeyJumpsToNearestStartChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxHomeKeyJumpsToNearestStart.Checked, eoEnhanceHomeKey);
end;


{-------------------------------------------------------------------------------
  KeepCursorXCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxKeepCursorXChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxKeepCursorX.Checked, eoKeepCaretX);
end;


{-------------------------------------------------------------------------------
  OverwriteBlockCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxOverwriteBlockChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxKeepCursorX.Checked, eoOverwriteBlock);
end;


{-------------------------------------------------------------------------------
  PersistentBlockCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxPersistentBlockChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxPersistentBlock.Checked, eoPersistentBlock);
end;


{-------------------------------------------------------------------------------
  PersistentCursorCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxPersistentCursorChange(Sender: TObject);
begin
  SetPreviewOption(CheckBoxPersistentCursor.Checked, eoPersistentCaret);
end;


{-------------------------------------------------------------------------------
  PersistentCursorNoBlinkCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxPersistentCursorNoBlinkChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxPersistentCursorNoBlink.Checked, eoPersistentCaretStopBlink);
end;


{-------------------------------------------------------------------------------
  ScrollByOneLessCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxScrollByOneLessChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxScrollByOneLess.Checked, eoScrollByOneLess);
end;


{-------------------------------------------------------------------------------
  ScrollPastEndFileCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxScrollPastEndFileChange (
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxScrollPastEndFile.Checked, eoScrollPastEoF);
end;


{-------------------------------------------------------------------------------
  ScrollPastEndLineCheckBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CheckBoxScrollPastEndLineChange(
  Sender: TObject);
begin
  SetPreviewOption(CheckBoxScrollPastEndLine.Checked, eoScrollPastEoL);
end;


{-------------------------------------------------------------------------------
  CreateHandle
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.CreateHandle;
var
  i, w: Integer;
  c: TControl;
begin
  inherited;
  w := 150;
  for i := 0 to ControlCount - 1 do begin
    c := Controls[i];
    if not (c is TCheckBox) then Continue;
    w := Max(w, Canvas.TextExtent(c.Caption).cx);
  end;
  Constraints.MinWidth := 2 * w + 60;
end;


{-------------------------------------------------------------------------------
  IndexInStringList
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameGeneral.IndexInStringList(List: TStrings;
  Cmp: TCmpStrType; s: string): integer;
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
procedure TCnfSyneditFrameGeneral.SetComboBoxText(AComboBox:TComboBox; const AText: String;
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


{-------------------------------------------------------------------------------
  SetPreviewOption
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption);
var
  ed: TPreviewEdit;
begin
  mContentPreview.SetFirst(); ed := Nil;
  while mContentPreview.GetNext(ed) do
  begin
    if ed <> Nil then
      if AValue then
        ed.Options := ed.Options + [AnOption]
      else
        ed.Options := ed.Options - [AnOption];
  end;
end;


{-------------------------------------------------------------------------------
  SetPreviewOption
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.SetPreviewOption(AValue: Boolean; AnOption: TSynEditorOption2);
var
  ed: TPreviewEdit;
begin
  mContentPreview.SetFirst(); ed := Nil;
  while mContentPreview.GetNext(ed) do
  begin
    if ed <> Nil then
      if AValue then
        ed.Options2 := ed.Options2 + [AnOption]
      else
        ed.Options2 := ed.Options2 - [AnOption];
  end;
end;


{-------------------------------------------------------------------------------
  UpdatePrevieEdits
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.UpdatePrevieEdits;
var
  ed: TPreviewEdit;
begin
  mContentPreview.SetFirst(); ed := Nil;
  while mContentPreview.GetNext(ed) do
  begin
    if ed.Highlighter is TSynPasSyn then begin
      TSynPasSyn(ed.Highlighter).ExtendedKeywordsMode := PasExtendedKeywordsMode;
      TSynPasSyn(ed.Highlighter).StringKeywordMode := PasStringKeywordMode;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  SetExtendedKeywordsMode
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.SetExtendedKeywordsMode(const AValue: Boolean);
begin
  if mPasExtendedKeywordsMode = AValue then exit;
  mPasExtendedKeywordsMode := AValue;
  UpdatePrevieEdits;
end;


{-------------------------------------------------------------------------------
  SetStringKeywordMode
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.SetStringKeywordMode(const AValue: TSynPasStringMode);
begin
  if mPasStringKeywordMode = AValue then exit;
  mPasStringKeywordMode := AValue;
  UpdatePrevieEdits;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TCnfSyneditFrameGeneral.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OLng.AddListener(@OnReplaceLng);

  if mEditorOptions <> nil then begin
    PasExtendedKeywordsMode := mEditorOptions.PasExtendedKeywordsMode;
  end;
  OFrameCnfSyneditGeneral := self;
end;


{-------------------------------------------------------------------------------
  Setup
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.Setup(ADialog: TFrameSettingsBase);
begin

end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameGeneral.OnReplaceLng();
begin
  // undo
  LCap('T_CnfSyneditFrameGeneral_DividerBevelUndo', DividerBevelUndo);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxUndoAfterSave', CheckBoxUndoAfterSave);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxGroupUndo', CheckBoxGroupUndo);
  LCap('T_CnfSyneditFrameGeneral_LabelUndoLimit', LabelUndoLimit);

  // scroll
  LCap('T_CnfSyneditFrameGeneral_DividerBevelScroll', DividerBevelScroll);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxHalfPageScroll', CheckBoxHalfPageScroll);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxScrollByOneLess', CheckBoxScrollByOneLess);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxScrollPastEndFile', CheckBoxScrollPastEndFile);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxScrollPastEndLine', CheckBoxScrollPastEndLine);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxScrollHint', CheckBoxScrollHint);

  // caret + key navigation
  LCap('T_CnfSyneditFrameGeneral_DividerBevelCaret', DividerBevelCaret);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxKeepCursorX', CheckBoxKeepCursorX);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxPersistentCursor', CheckBoxPersistentCursor);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxPersistentCursorNoBlink', CheckBoxPersistentCursorNoBlink);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxAlwaysVisibleCursor', CheckBoxAlwaysVisibleCursor);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxCursorSkipsSelection', CheckBoxCursorSkipsSelection);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxCaretMoveClearsSelection', CheckBoxCaretMoveClearsSelection);

  //dlgCursorMoveClearsSelection
  LCap('T_CnfSyneditFrameGeneral_CheckBoxCursorSkipsTab', CheckBoxCursorSkipsTab);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxHomeKeyJumpsToNearestStart', CheckBoxHomeKeyJumpsToNearestStart);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxEndKeyJumpsToNearestStart', CheckBoxEndKeyJumpsToNearestStart);

  // multi caret
  LCap('T_CnfSyneditFrameGeneral_DividerBevelMultiCaretGroup', DividerBevelMultiCaretGroup);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxMultiCaretOnColumnSelection', CheckBoxMultiCaretOnColumnSelection);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxMultiCaretColumnMode', CheckBoxMultiCaretColumnMode);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxMultiCaretMode', CheckBoxMultiCaretMode);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxMultiCaretDelSkipCr', CheckBoxMultiCaretDelSkipCr);

  // Block
  LCap('T_CnfSyneditFrameGeneral_DividerBevelBlock', DividerBevelBlock);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxPersistentBlock', CheckBoxPersistentBlock);
  LCap('T_CnfSyneditFrameGeneral_CheckBoxOverwriteBlock', CheckBoxOverwriteBlock);
end;




initialization



end.

