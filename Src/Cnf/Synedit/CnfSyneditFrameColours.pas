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
unit CnfSyneditFrameColours;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, types, typinfo, sysutils,
  // LazUtils
  Laz2_XMLCfg, LazFileUtils, LazUTF8, Forms,
  // LCL
  LCLProc, LCLIntf, StdCtrls, ExtCtrls, Graphics, ComCtrls, Dialogs, Menus,
  // SynEdit
  SynEdit, SynEditMiscClasses, SynGutterCodeFolding, SynGutterLineNumber,
  SynEditTypes, SynGutterChanges, SynEditMouseCmds, SynEditHighlighter,
  // Configuration
  CnfSyneditFrameColorAttrEditor, CnfSyneditPreview,
  CnfSyneditOptions, CnfSyneditHighlighterDef, CnfSyneditFrameGeneral,
  //General
  FrameSettingsBase, Language, LCLType;

type

  TCmpStrType = (
    cstCaseSensitive,
    cstCaseInsensitive,
    cstFilename
    );

  // for priority
  TMarkupField = (mfForeGround, mfBackGround, mfFrame, mfStyle, mfUnknown);

  { TCnfSyneditFrameColours }

  TCnfSyneditFrameColours = class(TFrameSettingsBase)
    ExportSaveDialog: TSaveDialog;
    PanelColorAttribEditor: TPanel;
    PanelTop: TPanel;
    LanguageMenu: TPopupMenu;
    ColorSchemeMenu: TPopupMenu;
    Splitter1: TSplitter;
    ColorElementTree: TTreeView;
    ColorPreview: TSynEdit;

    procedure btnExportClick(Sender: TObject);
    procedure ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var {%H-}PaintImages, DefaultDraw: Boolean);
    procedure ColorElementTreeChange(Sender: TObject; {%H-}Node: TTreeNode);
    procedure ColorElementTreeClick(Sender: TObject);
    procedure ColorElementTreeKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure ColorPreviewMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure DoColorChanged(Sender: TObject);

  private
    mFrameColorAttrEditor: TCnfSyneditFrameColorAttrEditor;
    FTempColorSchemeSettings: TColorSchemeFactory;
    mColorPreview: TSynEdit;

    FCurHighlightElement: TColorSchemeAttribute;

    FFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    FColorSchemes: TStringList;    // list of LanguageName=ColorScheme

    FCurrentHighlighter: TSrcIDEHighlighter;
    FCurrentColorScheme: TColorSchemeLanguage;
    FIsEditingDefaults: Boolean;
    CurLanguageID: Integer;

    mHighligterPreview: THighligterPreview;
    mSyneditPreview: TSyneditPreview;
    mContentPreview: TContentPreview;
    mEditorOptions: TEditorOptions;

  private

    function AttrForNode(ANode: TTreeNode): TColorSchemeAttribute;
    procedure SetAttrPriorVal(AnAttr: TColorSchemeAttribute; AField: TMarkupField; AValue: Integer);
    function  GetAttrPriorVal(AnAttr: TColorSchemeAttribute; AField: TMarkupField): Integer;
    procedure SetPriorEditVal(AnEdit: TEdit; AValue: Integer);
    function  GetPriorEditVal(AnEdit: TEdit): Integer;

    function  GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);
    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    procedure FillColorElementListBox;
    procedure SetColorElementsToDefaults(OnlySelected: Boolean);
    function  GetColorSchemeForLang(const LanguageName: String): String;
    procedure SetColorSchemeForLang(const LanguageName, ColorScheme: String);

    procedure SetCurrentScheme(SynClass: TCustomSynClass; const ColorScheme: String);
    procedure ApplyCurrentScheme;
    procedure UpdateCurrentScheme;

    procedure OnStatusChange(Sender: TObject; {%H-}Changes: TSynStatusChanges);
    procedure OnSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: boolean; aMarkup: TSynSelectedColor);
    procedure DoPaint(Sender : TObject);

    function GeneralPage: TCnfSyneditFrameGeneral;
    function DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo; {%H-}HandleActionProc: TSynEditMouseActionHandler): Boolean;
    procedure OnReplaceLng();

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;

    function GetTitle: String;
    procedure Setup();
    procedure UpdateScheme(languageID: integer);
    procedure ReadSettings(AOptions: TEditorOptions);
    function IndexInStringList(List: TStrings; Cmp: TCmpStrType; s: string): integer;
    procedure SetComboBoxText(AComboBox: TComboBox; const AText: String; Cmp: TCmpStrType; MaxCount: integer = 1000);
    procedure WriteSettings(AOptions: TEditorOptions);
    procedure SelectAhaColor(aha: TAdditionalHilightAttribute);
    property UnsavedColorSchemeSettings: TColorSchemeFactory read FTempColorSchemeSettings;
    property UnsavedColorSchemeDefaultNames: TStringList read FColorSchemes;

    procedure SetCurrentSchemeAll(const ColorScheme: String);

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;

    procedure InitFrame(frame: TFrame; visib: Boolean);

  end;

implementation

{$R *.lfm}

const
  COLOR_NODE_PREFIX = ' abc  ';


{-------------------------------------------------------------------------------
  DefaultToNone
 ------------------------------------------------------------------------------}
function DefaultToNone(AColor: TColor): TColor;
begin
  if AColor = clDefault then
    Result := clNone
  else
    Result := AColor;
end;


{-------------------------------------------------------------------------------
  NoneToDefault
 ------------------------------------------------------------------------------}
function NoneToDefault(AColor: TColor): TColor;
begin
  if AColor = clNone then
    Result := clDefault
  else
    Result := AColor;
end;



{ TCnfSyneditFrameColours }

{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetSettings();
begin
  OnVisible();

  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mSyneditPreview    := TSyneditPreview   (Settings.Services.FindFirstByClass(TSyneditPreview));
  mContentPreview    := TContentPreview   (Settings.Services.FindFirstByClass(TContentPreview));
  mEditorOptions     := TEditorOptions    (Settings.Services.FindFirstByClass(TEditorOptions));

  //  ReadSettings(mEditorOptions);  // Wird schon von TCnfSyneditFrameThemes aufgerufen
  Setup();
  InitFrame(mFrameColorAttrEditor, true);
  UpdateScheme(CurLanguageID);
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.GetSettings();
begin
  WriteSettings(mEditorOptions);
  mEditorOptions.Save();
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function TCnfSyneditFrameColours.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Editor', 'Farben');
begin
  ary[0] := LStr('T_CnfSyneditFrame_TreeEditor',         ary[0]);
  ary[1] := LStr('T_CnfSyneditFrameColours_TreeColours', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  InitFrame
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.InitFrame(frame: TFrame; visib: Boolean);
var
  rect: TRect;
begin
  rect := PanelColorAttribEditor.BoundsRect;

  rect.Left    := rect.Left    + 0;
  rect.Top     := rect.Top     - 35;
  rect.Width   := rect.Width   - 0;
  rect.Height  := rect.Height  - 10;

  frame.Parent     := PanelColorAttribEditor;
  frame.BoundsRect := rect;
  frame.Visible    := visib;
end;


{-------------------------------------------------------------------------------
  ColorElementTreeChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ColorElementTreeChange(Sender: TObject; Node: TTreeNode);
begin
  FindCurHighlightElement;
end;


{-------------------------------------------------------------------------------
  ColorElementTreeAdvancedCustomDrawItem
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ColorElementTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean);
var
  NodeRect: TRect;
  FullAbcWidth, AbcWidth: Integer;
  Attri: TColorSchemeAttribute;
  TextY: Integer;
  AttriIdx: LongInt;
  c: TColor;
  s: String;
  TheTree: TCustomTreeView;
begin
  DefaultDraw := (node.Data = nil) or not (stage=cdPostPaint);
  if DefaultDraw  then exit;

  TheTree := TCustomTreeView(Sender);

  Attri := TColorSchemeAttribute(node.Data);
  if Attri.IsUsingSchemeGlobals then
    Attri := Attri.GetSchemeGlobal;

  if FCurrentColorScheme = nil then exit;

  AttriIdx := GetEnumValue(TypeInfo(TAdditionalHilightAttribute), Attri.StoredName);

  // Draw node background and name
  if cdsSelected in State then begin
    TheTree.Canvas.Brush.Color := TheTree.SelectionColor;
    TheTree.Canvas.Font.Color := InvertColor(TheTree.SelectionColor);
  end else begin
    TheTree.Canvas.Brush.Color := TheTree.BackgroundColor;
    TheTree.Canvas.Font.Color := Font.Color;
  end;
  NodeRect := Node.DisplayRect(true);
  FullAbcWidth := TheTree.Canvas.TextExtent(COLOR_NODE_PREFIX).cx;
  TextY := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - TheTree.Canvas.TextHeight(Node.Text)) div 2;
  TheTree.Canvas.FillRect(NodeRect);
  TheTree.Canvas.TextOut(NodeRect.Left+FullAbcWidth, TextY, copy(Node.Text, 1+length(COLOR_NODE_PREFIX), MaxInt)); // Attri.Name);

  // Draw preview box - Background
  c := clNone;
  if (hafBackColor in  Attri.Features) and not (AttriIdx = ord(ahaCaretColor)) then
    c := Attri.Background;
  // Fallback Background-color for gutter
  if ((c = clNone) or (c = clDefault)) and
     (AttriIdx in [ord(ahaModifiedLine), ord(ahaCodeFoldingTree),
                   ord(ahaLineNumber), ord(ahaGutterSeparator)]) and
     (FCurrentColorScheme.AttributeByEnum[ahaGutter] <> nil)
  then
    c := FCurrentColorScheme.AttributeByEnum[ahaGutter].Background;
  // Fallback Background-color for text
  if (c = clNone) or (c = clDefault) then
    c := FCurrentColorScheme.DefaultAttribute.Background;
  if (c = clNone) or (c = clDefault) then
    c := ColorPreview.Color;
  TheTree.Canvas.Brush.Color := c;
  TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Special draw Modified line gutter
  if (AttriIdx = ord(ahaModifiedLine)) then begin
    TextY := NodeRect.Bottom - NodeRect.Top - 4;
    TheTree.Canvas.Brush.Color := Attri.Foreground;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+5, NodeRect.Bottom-2);
    TheTree.Canvas.Brush.Color := Attri.FrameColor;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2+ (TextY div 2), NodeRect.Left+5, NodeRect.Bottom-2);
    exit;
  end;

  // Special draw oultine color // Caret color
  if (Attri.Group = agnOutlineColors) or (AttriIdx = ord(ahaCaretColor)) then begin
    c := Attri.MarkupFoldLineColor;
    if (AttriIdx = ord(ahaCaretColor)) then
      c := Attri.Foreground;
    if c <> clNone then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+2, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+2, NodeRect.Bottom-2);
      TheTree.Canvas.MoveTo(NodeRect.Left+2+1, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+2+1, NodeRect.Bottom-2);
    end;
    NodeRect.Left := NodeRect.Left + 6;
    FullAbcWidth := FullAbcWidth - 6;
    TheTree.Canvas.Brush.Color := Attri.Background;
    TheTree.Canvas.FillRect(NodeRect.Left+2, NodeRect.Top+2, NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);
    if (AttriIdx = ord(ahaCaretColor)) then
      exit;
  end;

  // Draw preview Frame
  TheTree.Canvas.Pen.Color := Attri.FrameColor;
  if (hafFrameColor in Attri.Features) and (AttriIdx <> ord(ahaCodeFoldingTree)) and
     (Attri.FrameColor <> clDefault) and (Attri.FrameColor <> clNone)
  then
    TheTree.Canvas.Rectangle(NodeRect.Left+2, NodeRect.Top+2,
                                      NodeRect.Left+FullAbcWidth-2, NodeRect.Bottom-2);

  // Draw preview ForeGround
  if (hafForeColor in Attri.Features) //and
       //(ahaSupportedFeatures[TAdditionalHilightAttribute(AttriIdx)].BG) )       // if no BG, then FG was used
  then begin
    c := Attri.Foreground;
    if ((c = clDefault) or (c = clNone)) and not (AttriIdx = ord(ahaLineNumber)) then
      c := FCurrentColorScheme.DefaultAttribute.Foreground;
    if (c = clNone) or (c = clDefault) then
      c := ColorPreview.Font.Color;

    if AttriIdx = ord(ahaCodeFoldingTree) then begin
      // Special draw fold gutter
      TextY := NodeRect.Bottom - NodeRect.Top - 8;

      // [-]
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4, NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);

      // [+]
      inc(NodeRect.Left, TextY+2);
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-4);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-2);
      if (Attri.FrameColor <> clNone) and (Attri.FrameColor <> clDefault) then
        TheTree.Canvas.Pen.Color := Attri.FrameColor;
      TheTree.Canvas.Rectangle(NodeRect.Left+4, NodeRect.Top+4, NodeRect.Left+4+TextY, NodeRect.Bottom-4);
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.LineTo(NodeRect.Left+4+TextY-2, NodeRect.Top+4+(TextY div 2));
      TheTree.Canvas.MoveTo(NodeRect.Left+4+(TextY div 2), NodeRect.Top+6);
      TheTree.Canvas.LineTo(NodeRect.Left+4+(TextY div 2), NodeRect.Bottom-6);
      TheTree.Canvas.Brush.Style := bsSolid;
    end
    else if AttriIdx = ord(ahaGutterSeparator) then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+6, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+6, NodeRect.Bottom-2);
    end
    else if AttriIdx = ord(ahaRightMargin) then begin
      TheTree.Canvas.Pen.Color := c;
      TheTree.Canvas.MoveTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Top+2);
      TheTree.Canvas.LineTo(NodeRect.Left+FullAbcWidth-6, NodeRect.Bottom-2);
    end
    else begin
      s := 'abc';
      if AttriIdx = ord(ahaFoldedCode) then
        s:= '...';
      if (AttriIdx = ord(ahaFoldedCodeLine)) or (AttriIdx = ord(ahaHiddenCodeLine)) then
        s:= '---';
      if AttriIdx = ord(ahaLineNumber) then
        s:= '123';
      if Attri.Group = agnOutlineColors then
        s:= 'ab';
      TheTree.Canvas.Font.Color := c;
      TheTree.Canvas.Font.Style := Attri.Style;
      TheTree.Canvas.Font.Height := -(NodeRect.Bottom - NodeRect.Top - 7);
      TextY := NodeRect.Top + (NodeRect.Bottom - NodeRect.Top - canvas.TextHeight(s)) div 2;
      AbcWidth := TheTree.Canvas.TextExtent(s).cx;
      SetBkMode(TheTree.Canvas.Handle, TRANSPARENT);
      TheTree.Canvas.TextOut(NodeRect.Left+(FullAbcWidth - AbcWidth) div 2, TextY, s);
      SetBkMode(TheTree.Canvas.Handle, OPAQUE);

      TheTree.Canvas.Font.Height := Font.Height;
      TheTree.Canvas.Font.Style := [];
    end;
  end;
end;


{-------------------------------------------------------------------------------
  btnExportClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.btnExportClick(Sender: TObject);
var
  XMLConfig: TRttiXMLConfig;
  NewScheme: TColorScheme;
  NewName: String;
  l: Integer;
begin
  ExportSaveDialog.InitialDir := UserSchemeDirectory(True);
  if ExportSaveDialog.Execute then begin
    NewName := ExtractFileName(ExportSaveDialog.FileName);
    l := length(ExtractFileExt(NewName));
    if (l > 0) and (l+1 < Length(NewName)) then
      NewName := Copy(NewName, 1, Length(NewName) - l);
    l := UTF8CodepointSize(PChar(NewName));
    if l > 0 then
      NewName := UTF8UpperCase(copy(NewName, 1, l)) + copy(NewName, 1+l, length(NewName));

    XMLConfig := TRttiXMLConfig.CreateClean(ExportSaveDialog.FileName);
    XMLConfig.SetValue('Lazarus/ColorSchemes/Names/Count', 1);
    XMLConfig.SetValue('Lazarus/ColorSchemes/Names/Item1/Value', NewName);

    NewScheme := TColorScheme.Create(NewName);
    NewScheme.Free;

    InvalidateFileStateCache;
    XMLConfig.Flush;
    XMLConfig.Free;
  end;
end;


{-------------------------------------------------------------------------------
  ColorElementTreeClick
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ColorElementTreeClick(Sender: TObject);
begin
  FindCurHighlightElement;
end;


{-------------------------------------------------------------------------------
  ColorElementTreeKeyDown
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ColorElementTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  lnode: TTreeNode;
begin
  lnode := ColorElementTree.Selected;
  if (Key = VK_UP) and (lnode <> nil) then begin
    lnode := lnode.GetPrevExpanded;
    if (lnode <> nil) and (lnode.GetFirstChild <> nil) then
      lnode := lnode.GetPrevExpanded;
    if (lnode <> nil) then begin
      Key :=VK_UNKNOWN;
      ColorElementTree.Selected := lnode;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ColorPreviewMouseUp
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ColorPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  Token: String;
  Attri: TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr: TAdditionalHilightAttribute;
  NewNode: TTreeNode;
begin
  MouseXY := Point(X - (ColorPreview.CharWidth div 2), Y);
  XY := ColorPreview.PixelsToRowColumn(MouseXY);
  NewNode := nil;
  // Gutter Colors
  if X <= ColorPreview.Gutter.Width then begin
    for i := 0 to ColorPreview.Gutter.Parts.Count-1 do begin
      if ColorPreview.Gutter.Parts[i].Width > X then begin
        if ColorPreview.Gutter.Parts[i] is TSynGutterLineNumber then
          SelectAhaColor(ahaLineNumber)
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterChanges then
          SelectAhaColor(ahaModifiedLine)
        else
        if ColorPreview.Gutter.Parts[i] is TSynGutterCodeFolding then
          SelectAhaColor(ahaCodeFoldingTree)
        else
          SelectAhaColor(ahaGutter);
        exit;
      end;
      X := X - ColorPreview.Gutter.Parts[i].Width;
    end;
    exit;
  end;
  // Line Highlights
  if CurLanguageID >= 0 then
  begin
    AddAttr := mEditorOptions.HighlighterList[CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr = ahaFoldedCode then begin
      if not( (XY.X >= Length(ColorPreview.Lines[XY.Y-1]) + 4) and
              (XY.X <= Length(ColorPreview.Lines[XY.Y-1]) + 6) )
      then
        AddAttr := ahaNone;
    end;
    if AddAttr <> ahaNone then begin
      SelectAhaColor(AddAttr);
      exit;
    end;
  end;
  if (XY.Y = ColorPreview.CaretY) and
     (XY.X > Length(ColorPreview.Lines[XY.Y - 1])+1)
  then begin
    SelectAhaColor(ahaLineHighlight);
    exit;
  end;
  if FIsEditingDefaults then
    exit;
  // Pascal Highlights
  Token := '';
  Attri := nil;
  ColorPreview.GetHighlighterAttriAtRowCol(XY, Token, Attri);
  if Attri = nil then
    Attri := FCurrentHighlighter.WhitespaceAttribute;
  if Attri <> nil then begin
    NewNode := ColorElementTree.Items.GetFirstNode;
    while Assigned(NewNode) do begin
      if (NewNode.Data <> nil)
      and (TColorSchemeAttribute(NewNode.Data).StoredName = Attri.StoredName) then
        break;
      NewNode := NewNode.GetNext;
    end;
  end;
  if NewNode <> nil then begin
    NewNode.Selected := True;
    FindCurHighlightElement;
  end;
end;


{-------------------------------------------------------------------------------
  DebugNotify
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.DoColorChanged(Sender: TObject);
begin
  UpdateCurrentScheme;
end;


{-------------------------------------------------------------------------------
  ShowCurAttribute
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ShowCurAttribute;
begin
  if (FCurHighlightElement = nil) then
    Exit;
  DisableAlign;
  try
    if FCurHighlightElement.IsUsingSchemeGlobals then
      mFrameColorAttrEditor.CurHighlightElement := FCurHighlightElement.GetSchemeGlobal
    else
      mFrameColorAttrEditor.CurHighlightElement := FCurHighlightElement;
  finally
    EnableAlign;
  end;
end;


{-------------------------------------------------------------------------------
  AttrForNode
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.AttrForNode(ANode: TTreeNode): TColorSchemeAttribute;
begin
  Result := nil;
  if ANode = nil then exit;
  Result := TColorSchemeAttribute(ANode.Data);
  if (Result <> nil) and Result.IsUsingSchemeGlobals then
    Result := Result.GetSchemeGlobal;
end;


{-------------------------------------------------------------------------------
  SetAttrPriorVal
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetAttrPriorVal(AnAttr: TColorSchemeAttribute;
  AField: TMarkupField; AValue: Integer);
begin
  if AnAttr.IsUsingSchemeGlobals then
    AnAttr := AnAttr.GetSchemeGlobal;
  case AField of
    mfForeGround: AnAttr.ForePriority := AValue;
    mfBackGround: AnAttr.BackPriority := AValue;
    mfFrame:      AnAttr.FramePriority := AValue;
    mfStyle:      begin
        AnAttr.StylePriority[fsBold] := AValue;
        AnAttr.StylePriority[fsItalic] := AValue;
        AnAttr.StylePriority[fsUnderline] := AValue;
      end;
  end;
end;


{-------------------------------------------------------------------------------
  GetAttrPriorVal
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GetAttrPriorVal(AnAttr: TColorSchemeAttribute;
  AField: TMarkupField): Integer;
begin
  Result := 0;
  if AnAttr = nil then exit;
  if AnAttr.IsUsingSchemeGlobals then
    AnAttr := AnAttr.GetSchemeGlobal;
  case AField of
    mfForeGround: Result := AnAttr.ForePriority;
    mfBackGround: Result := AnAttr.BackPriority;
    mfFrame:      Result := AnAttr.FramePriority;
    mfStyle:      Result := AnAttr.StylePriority[fsBold];
  end;
end;


{-------------------------------------------------------------------------------
  SetPriorEditVal
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetPriorEditVal(AnEdit: TEdit; AValue: Integer);
begin
  AnEdit.Tag := AValue;
  AnEdit.Text := IntToStr(AValue);
end;


{-------------------------------------------------------------------------------
  GetPriorEditVal
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GetPriorEditVal(AnEdit: TEdit): Integer;
begin
  Result := StrToIntDef(AnEdit.Text, -1);
  if Result = -1 then begin
    Result := AnEdit.Tag;
    AnEdit.Text := IntToStr(Result);
  end;
end;


{-------------------------------------------------------------------------------
  FindCurHighlightElement
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.FindCurHighlightElement;
begin
  if (ColorElementTree.Selected <> nil) and
     (ColorElementTree.Selected.Parent = nil) and
     (ColorElementTree.Selected.GetFirstChild <> nil)
  then
    ColorElementTree.Selected.GetFirstChild.Selected := True;
  if (ColorElementTree.Selected = nil) or (ColorElementTree.Selected.Data = nil) then
    exit;

  if FCurHighlightElement = TColorSchemeAttribute(ColorElementTree.Selected.Data) then
    exit;

  FCurHighlightElement := TColorSchemeAttribute(ColorElementTree.Selected.Data);
  ShowCurAttribute;
end;


{-------------------------------------------------------------------------------
  FillColorElementListBox
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.FillColorElementListBox;
var
  i: Integer;
  ParentName: String;
  ParentNode: TTreeNode;
  j: TAhaGroupName;
  Attr: TColorSchemeAttribute;
  NewNode, DefNode: TTreeNode;
begin
  ColorElementTree.BeginUpdate;
  ColorElementTree.Items.Clear;

  // Create Groups
  if not FIsEditingDefaults then
    ColorElementTree.Items.Add(nil, FCurrentHighlighter.LanguageName)
  else
    ColorElementTree.Items.Add(nil, AdditionalHighlightGroupNames[agnDefault]);
  for j := low(TAhaGroupName) to high(TAhaGroupName) do
    if not(j in [agnDefault, agnLanguage]) then
      ColorElementTree.Items.Add(nil, AdditionalHighlightGroupNames[j]);

  // Fill Attributes in
  DefNode := nil;
  for i := 0 to FCurrentColorScheme.AttributeCount - 1 do begin
    Attr := FCurrentColorScheme.AttributeAtPos[i];
    if Attr.StoredName <> '' then begin
      case Attr.Group of
        agnDefault, //  continue; // default is currently not shown
        agnLanguage:
          begin
            if FIsEditingDefaults then
              ParentName := AdditionalHighlightGroupNames[agnDefault]
            else
              ParentName := FCurrentHighlighter.LanguageName;
          end;
        else
          ParentName := AdditionalHighlightGroupNames[Attr.Group];
      end;
      ParentNode := ColorElementTree.Items.FindTopLvlNode(ParentName);
      if ParentNode = nil then
        ParentNode := ColorElementTree.Items.Add(nil, ParentName);
      NewNode :=  ColorElementTree.Items.AddChild(ParentNode, COLOR_NODE_PREFIX + Attr.Caption^);
      NewNode.Data := Pointer(Attr);
      if Attr.Group = agnDefault then
        DefNode := NewNode;
    end;
  end;

  for i := 0 to ColorElementTree.Items.Count - 1 do
    ColorElementTree.Items[i].AlphaSort;
  if DefNode <> nil then
    DefNode.Index := 0;

  ColorElementTree.EndUpdate;
  ColorElementTree.FullExpand;
  if ColorElementTree.Items.GetFirstNode <> nil then
    ColorElementTree.Items.GetFirstNode.Selected := True;

  FCurHighlightElement := nil;
  FindCurHighlightElement;
end;


{-------------------------------------------------------------------------------
  SetColorElementsToDefaults
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetColorElementsToDefaults(OnlySelected: Boolean);
var
  DefaultSchemeGrp: TColorScheme;
  DefaultColorScheme: TColorSchemeLanguage;
  DefAttri: TColorSchemeAttribute;
  i: Integer;
begin
  DefaultSchemeGrp := ColorSchemeFactory.ColorSchemeGroup['LightMode'];
  if DefaultSchemeGrp = nil then
    exit;

  if FIsEditingDefaults then
    DefaultColorScheme := DefaultSchemeGrp.DefaultColors
  else
    DefaultColorScheme := DefaultSchemeGrp.ColorScheme[FCurrentColorScheme.Language];

  if OnlySelected then begin
    DefAttri := DefaultColorScheme.Attribute[FCurHighlightElement.StoredName];
    FCurHighlightElement.Assign(DefAttri);
  end else begin
    FCurrentColorScheme.Assign(DefaultColorScheme);
  end;

  // reassign tree nodes => in case
  for i := 0 to ColorElementTree.Items.Count - 1 do begin
    if (ColorElementTree.Items[i].Data <> nil) and
       (FCurrentColorScheme.IndexOfAttr
         (TColorSchemeAttribute(ColorElementTree.Items[i].Data)) < 0)
    then begin
      debugln('Error: missing Attr after assign');
      FillColorElementListBox;
      break;
    end;
  end;

  FindCurHighlightElement;
  UpdateCurrentScheme;
  ShowCurAttribute;
end;


{-------------------------------------------------------------------------------
  GetColorSchemeForLang
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GetColorSchemeForLang(const LanguageName: String): String;
begin
  if FColorSchemes = nil then
    Result := ''
  else
    Result := FColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := mEditorOptions.ReadColorScheme(LanguageName);
end;


{-------------------------------------------------------------------------------
  SetColorSchemeForLang
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetColorSchemeForLang(const LanguageName,
  ColorScheme: String);
begin
  if FColorSchemes = nil then
    FColorSchemes := TStringList.Create;
  FColorSchemes.Values[LanguageName] := ColorScheme;
end;


{-------------------------------------------------------------------------------
  SetCurrentScheme
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetCurrentScheme(SynClass: TCustomSynClass; const ColorScheme: String);
var
  SchemeGrp: TColorScheme;
  NewColorScheme: TColorSchemeLanguage;
begin
  // Modfiy directly => will be re-read form XML if canceled
  SchemeGrp := FTempColorSchemeSettings.ColorSchemeGroup[ColorScheme];
  if SchemeGrp = nil then
    exit;

  if FIsEditingDefaults then
    NewColorScheme := SchemeGrp.DefaultColors
  else
    NewColorScheme := SchemeGrp.ColorSchemeBySynClass[SynClass];
  if (NewColorScheme = FCurrentColorScheme) then
    exit;

  FCurrentColorScheme := NewColorScheme;
  if not FIsEditingDefaults then begin
    FCurrentHighlighter := FCurrentColorScheme.Highlighter;
    mFrameColorAttrEditor.CurrentColorScheme := FCurrentColorScheme;
  end;
  ApplyCurrentScheme;
  FillColorElementListBox;
end;


{-------------------------------------------------------------------------------
  SetCurrentSchemeAll
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SetCurrentSchemeAll(const ColorScheme: String);
var
  i: Integer;
  language: String;
  SynClass: TCustomSynClass;
begin
  UpdateScheme(CurLanguageID);

  for i := 0 to mEditorOptions.HighlighterList.Count - 1 do
  begin
    SynClass := mEditorOptions.HighlighterList[i].SynClass;
    language := SynClass.GetLanguageName;

    SetColorSchemeForLang(language, ColorScheme);
  end;
  SetCurrentScheme(TCustomSynClass(FCurrentHighlighter.ClassType), ColorScheme);
  ApplyCurrentScheme;
end;


{-------------------------------------------------------------------------------
  ApplyCurrentScheme
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ApplyCurrentScheme;
var
  ed: TPreviewEdit;
begin
  // there is always a colorscheme selected, except during initialization
  if FCurrentColorScheme = nil then
    exit;
  with GeneralPage do begin
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do ed.BeginUpdate;
    try
      mContentPreview.SetFirst(); ed := Nil;
      while mContentPreview.GetNext(ed) do
      begin
        ed.Highlighter := FCurrentHighlighter;
        ed.Lines.Text  := mEditorOptions.HighlighterList[CurLanguageID].SampleSource;
        ed.CaretXY     := mEditorOptions.HighlighterList[CurLanguageID].CaretXY;
        ed.TopLine     := 1;
        ed.LeftChar    := 1;
        ed.Keystrokes.Clear;
        ed.MouseActions.Clear;
        ed.AfterLoadFromFile;
        ed.Keystrokes.Clear;
        ed.MouseOptions := [emUseMouseActions];
        ed.MouseActions.Clear;
        ed.MouseActions.AddCommand(emcWheelVertScrollDown, False, mbXWheelDown, ccAny, cdDown, [], []);
        ed.MouseActions.AddCommand(emcWheelVertScrollUp,   False, mbXWheelUp,   ccAny, cdDown, [], []);
        ed.SetBookMark(1, 1, 2);
        ed.SetBookMark(2, 1, 5);
      end;
      UpdateCurrentScheme;
    finally
      mContentPreview.SetFirst(); ed := Nil;
      while mContentPreview.GetNext(ed) do ed.EndUpdate;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  UpdateCurrentScheme
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.UpdateCurrentScheme;
var
  ed: TPreviewEdit;
  hl: TSynCustomHighlighter;
  SchemeGrp: TColorScheme;
  scheme: TColorSchemeLanguage;
  LngName: String;
begin
  // there is always a colorscheme selected, except during initialization

  if FCurrentColorScheme = nil then
    exit;
  mContentPreview.SetFirst(); ed := Nil;
  while mContentPreview.GetNext(ed) do ed.BeginUpdate;
  try
    if not FIsEditingDefaults then
      FCurrentColorScheme.ApplyTo(FCurrentHighlighter);
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do
    begin
      FCurrentColorScheme.ApplyTo(ed);
      ed.Invalidate;
    end;
  finally
    mContentPreview.SetFirst(); ed := Nil;
    while mContentPreview.GetNext(ed) do ed.EndUpdate;
  end;

  mSyneditPreview.SetFirst(); ed := Nil;
  while mSyneditPreview.GetNext(ed) do ed.BeginUpdate;
  try
    mSyneditPreview.SetFirst(); ed := Nil;
    while mSyneditPreview.GetNext(ed) do
    begin
      FCurrentColorScheme.ApplyTo(ed);
      ed.Invalidate;
    end;
  finally
    mSyneditPreview.SetFirst(); ed := Nil;
    while mSyneditPreview.GetNext(ed) do ed.EndUpdate;
  end;
  mHighligterPreview.SetFirst(); hl := Nil;
  while mHighligterPreview.GetNext(hl) do hl.BeginUpdate;
  try
    mHighligterPreview.SetFirst(); hl := Nil;
    while mHighligterPreview.GetNext(hl) do
    begin
      LngName := FCurrentColorScheme.Name;

      SchemeGrp := FTempColorSchemeSettings.ColorSchemeGroup[LngName];
      if SchemeGrp = nil then Continue;

      if      hl.ClassName = 'TSynHTMLSyn'    then Scheme := SchemeGrp.ColorScheme[lshHTML]
      else if hl.ClassName = 'TSynCssSyn'     then Scheme := SchemeGrp.ColorScheme[lshCss]
      else if hl.ClassName = 'TSynJScriptSyn' then Scheme := SchemeGrp.ColorScheme[lshJScript]
      else Continue;

      scheme.ApplyTo(hl);
    end;
  finally
    mHighligterPreview.SetFirst(); hl := Nil;
    while mHighligterPreview.GetNext(hl) do hl.EndUpdate;
  end;

  ColorElementTree.Invalidate;
end;


{-------------------------------------------------------------------------------
  GeneralPage
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GeneralPage: TCnfSyneditFrameGeneral;
begin
  Result := TCnfSyneditFrameGeneral(mEditorOptions.GeneralPage);
end;


{-------------------------------------------------------------------------------
  DoSynEditMouse
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.DoSynEditMouse(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := not(AnInfo.Button in [mbXWheelDown, mbXWheelUp]);
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TCnfSyneditFrameColours.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);
  mColorPreview := Nil;

  mFrameColorAttrEditor := TCnfSyneditFrameColorAttrEditor.Create(PanelColorAttribEditor);
  PanelColorAttribEditor.OnPaint := @DoPaint;

  FTempColorSchemeSettings := TColorSchemeFactory.Create;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TCnfSyneditFrameColours.Destroy;
begin
  FreeAndNil(FTempColorSchemeSettings);
  FFileExtensions.Free;
  FColorSchemes.Free;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: DoPaint
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.DoPaint(Sender : TObject);
begin
  mFrameColorAttrEditor.Top := 0;
  mFrameColorAttrEditor.Height := PanelColorAttribEditor.Height;
end;


{-------------------------------------------------------------------------------
  GetTitle
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GetTitle: String;
begin
  Result := LStr('dlgColors', 'Colors');
end;


{-------------------------------------------------------------------------------
  Setup
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.Setup();
begin
  // Prevent the caret from moving
  ColorPreview.RegisterMouseActionSearchHandler(@DoSynEditMouse);
  FCurHighlightElement := nil;

  mFrameColorAttrEditor.Setup;
  mFrameColorAttrEditor.OnChanged := @DoColorChanged;
  mFrameColorAttrEditor.ShowPrior := False;
end;


{-------------------------------------------------------------------------------
  ReadSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.ReadSettings(AOptions: TEditorOptions);
begin
  // here we are sure that Setup has been called for every frame =>
  // we can assign events to every registered preview control

  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mContentPreview    := TContentPreview   (Settings.Services.FindFirstByClass(TContentPreview));
  mEditorOptions     := TEditorOptions    (Settings.Services.FindFirstByClass(TEditorOptions));

  if mColorPreview = Nil then
  begin
    with GeneralPage do
      mContentPreview.AddPreviewEditor(ColorPreview);
    ColorPreview.OnSpecialLineMarkup := @OnSpecialLineMarkup;
    mColorPreview := ColorPreview;
  end;

  with AOptions as TEditorOptions do
  begin
    FTempColorSchemeSettings.Assign(UserColorSchemeGroup);
  end;
end;


{-------------------------------------------------------------------------------
  UpdateScheme
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.UpdateScheme(languageID: integer);
var
  SynClass: TCustomSynClass;
  languageName: String;
begin
  CurLanguageID := languageID;
  SynClass       := mEditorOptions.HighlighterList[curLanguageID].SynClass;
  languageName   := mEditorOptions.HighlighterList[CurLanguageID].SynClass.GetLanguageName;

  SetCurrentScheme(SynClass, GetColorSchemeForLang(languageName));
  if FCurrentHighlighter = Nil then exit;
  CurLanguageID := mEditorOptions.HighlighterList.FindByClass(TCustomSynClass(FCurrentHighlighter.ClassType));

  ShowCurAttribute;
  UpdateCurrentScheme;
end;


{-------------------------------------------------------------------------------
  IndexInStringList
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.IndexInStringList(List: TStrings;
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
procedure TCnfSyneditFrameColours.SetComboBoxText(AComboBox: TComboBox;
  const AText: String; Cmp: TCmpStrType; MaxCount: integer = 1000);
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
  WriteSettings
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.WriteSettings(AOptions: TEditorOptions);
var
  i, j: Integer;
begin
  with AOptions as TEditorOptions do
  begin
    if FFileExtensions <> nil then begin
      for i := 0 to FFileExtensions.Count - 1 do begin
        j := HighlighterList.FindByName(FFileExtensions.Names[i]);
        if j >= 0 then
          HighlighterList[j].FileExtensions := FFileExtensions.ValueFromIndex[i];
      end;
    end;

    if FColorSchemes <> nil then
      for i := 0 to FColorSchemes.Count - 1 do
         WriteColorScheme(
           FColorSchemes.Names[i], FColorSchemes.Values[FColorSchemes.Names[i]]);

    // Write from userFactory
    UserColorSchemeGroup.Assign(FTempColorSchemeSettings);
  end;
end;


{-------------------------------------------------------------------------------
  SelectAhaColor
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.SelectAhaColor(aha: TAdditionalHilightAttribute);
var
  i: Integer;
begin
  for i := 0 to ColorElementTree.Items.Count - 1 do begin
    if ColorElementTree.Items[i].Data = nil then continue;
    if TColorSchemeAttribute(ColorElementTree.Items[i].Data).StoredName <>
       GetEnumName(TypeInfo(TAdditionalHilightAttribute), ord(aha))
    then
      continue;
    ColorElementTree.Items[i].Selected := True;
    break;
  end;
end;


{-------------------------------------------------------------------------------
  GetCurFileExtensions
 ------------------------------------------------------------------------------}
function TCnfSyneditFrameColours.GetCurFileExtensions(const LanguageName: String): String;
var
  i: Integer;
begin
  if FFileExtensions = nil then
    Result := ''
  else
    Result := FFileExtensions.Values[LanguageName];
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
procedure TCnfSyneditFrameColours.SetCurFileExtensions(const LanguageName,
  FileExtensions: String);
begin
  if FFileExtensions = nil then
    FFileExtensions := TStringList.Create;
  FFileExtensions.Values[LanguageName] := FileExtensions;
end;


{-------------------------------------------------------------------------------
  OnSpecialLineMarkup
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.OnSpecialLineMarkup(Sender: TObject;
  Line: Integer; var Special: boolean; aMarkup: TSynSelectedColor);
var
  e: TColorSchemeAttribute;
  AddAttr: TAdditionalHilightAttribute;
begin
  if CurLanguageID < 0 then
    exit;
  AddAttr := mEditorOptions.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
  if (AddAttr <> ahaNone) and (AddAttr <> ahaFoldedCode) then begin
    e := FCurrentColorScheme.AttributeByEnum[AddAttr];
    if e <> nil then begin
      Special := True;
      e.ApplyTo(aMarkup);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  OnStatusChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.OnStatusChange(Sender : TObject;
  Changes : TSynStatusChanges);
var
  Syn: TSynEdit;
  p: TPoint;
begin
  p := mEditorOptions.HighlighterList[CurLanguageID].CaretXY;
  Syn := Sender as TSynEdit;
  if p.y > Syn.Lines.Count then exit;
  if (Syn.CaretX = p.x) and (Syn.Carety = p.y) then exit;
  Syn.CaretXY:= p;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColours.OnReplaceLng();
begin

end;



end.
