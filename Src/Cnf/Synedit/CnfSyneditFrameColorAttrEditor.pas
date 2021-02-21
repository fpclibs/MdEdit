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
unit CnfSyneditFrameColorAttrEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, types, typinfo, FPCanvas,
  // LCL
  LCLProc, LCLType, LCLIntf, Forms, StdCtrls, ExtCtrls, Graphics, GraphUtil,
  ColorBox, Dialogs, Menus, Spin,
  // SynEdit
  SynEditTypes, SynTextDrawer,
  // IDE
  CnfSyneditOptions, Language,
  // Generel
  SysUtils;

type

  { TCnfSyneditFrameColorAttrEditor }

  TCnfSyneditFrameColorAttrEditor = class(TFrame)
    LabelBackAlpha: TLabel;
    BackAlphaSpin: TSpinEdit;
    BackGroundColorBox: TColorBox;
    BackGroundLabel: TLabel;
    CheckBoxBackGroundUseDefault: TCheckBox;
    LabelBackPrior: TLabel;
    BackPriorSpin: TSpinEdit;
    ColumnPosBevel: TPanel;
    LabelForeAlpha: TLabel;
    ForeAlphaSpin: TSpinEdit;
    ForegroundColorBox: TColorBox;
    ForeGroundLabel: TLabel;
    CheckBoxForeGroundUseDefault: TCheckBox;
    LabelForePrior: TLabel;
    ForePriorSpin: TSpinEdit;
    LabelFrameAlpha: TLabel;
    FrameAlphaSpin: TSpinEdit;
    FrameColorBox: TColorBox;
    CheckBoxFrameColorUseDefault: TCheckBox;
    FrameEdgesBox: TComboBox;
    LabelFramePrior: TLabel;
    FramePriorSpin: TSpinEdit;
    FrameStyleBox: TComboBox;
    GroupBoxColorAttributs: TGroupBox;
    LabelMarkupFoldAlpha: TLabel;
    MarkupFoldAlphaSpin: TSpinEdit;
    MarkupFoldColorBox: TColorBox;
    CheckBoxMarkupFoldColorUseDefault: TCheckBox;
    MarkupFoldStyleBox: TComboBox;
    pnlBold: TPanel;
    pnlItalic: TPanel;
    pnlUnderline: TPanel;
    CheckBoxTextBold: TCheckBox;
    RadioTextBoldInvert: TRadioButton;
    RadioTextBoldOff: TRadioButton;
    RadioTextBoldOn: TRadioButton;
    TextBoldRadioPanel: TPanel;
    CheckBoxTextItalic: TCheckBox;
    RadioTextItalicInvert: TRadioButton;
    RadioTextItalicOff: TRadioButton;
    RadioTextItalicOn: TRadioButton;
    TextItalicRadioPanel: TPanel;
    CheckBoxTextUnderline: TCheckBox;
    RadioTextUnderlineInvert: TRadioButton;
    RadioTextUnderlineOff: TRadioButton;
    RadioTextUnderlineOn: TRadioButton;
    TextUnderlineRadioPanel: TPanel;
    procedure ForeAlphaSpinChange(Sender: TObject);
    procedure ForeAlphaSpinEnter(Sender: TObject);
    procedure ForegroundColorBoxChange(Sender: TObject);
    procedure ForegroundColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure ForePriorSpinChange(Sender: TObject);
    procedure FrameEdgesBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FrameStyleBoxDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure GeneralCheckBoxOnChange(Sender: TObject);
    procedure pnlElementAttributesResize(Sender: TObject);
    procedure TextStyleRadioOnChange(Sender: TObject);

  private
    FCurHighlightElement: TColorSchemeAttribute;
    FCurrentColorScheme: TColorSchemeLanguage;
    FOnChanged: TNotifyEvent;
    FShowPrior: Boolean;
    UpdatingColor: Boolean;

    procedure SetCurHighlightElement(AValue: TColorSchemeAttribute);
    procedure DoChanged;
    procedure SetShowPrior(AValue: Boolean);
    procedure OnReplaceLng();

  public
    constructor Create(TheOwner: TComponent); override;
    procedure InitializeWnd(); override;
    procedure Setup;
    procedure UpdateAll;
    // CurrentColorScheme must be set before CurHighlightElement
    property CurHighlightElement: TColorSchemeAttribute read FCurHighlightElement write SetCurHighlightElement;
    property CurrentColorScheme: TColorSchemeLanguage read FCurrentColorScheme write FCurrentColorScheme;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property ShowPrior: Boolean read FShowPrior write SetShowPrior;

  end;


implementation

{$R *.lfm}

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



{ TCnfSyneditFrameColorAttrEditor }

{-------------------------------------------------------------------------------
  ForegroundColorBoxChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.ForegroundColorBoxChange(Sender: TObject);
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    exit;
  UpdatingColor := True;

  if Sender = ForegroundColorBox then
  begin
    FCurHighlightElement.Foreground := DefaultToNone(ForeGroundColorBox.Selected);
    CheckBoxForeGroundUseDefault.Checked := ForeGroundColorBox.Selected <> clDefault;
  end;
  if Sender = BackGroundColorBox then
  begin
    FCurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
    CheckBoxBackGroundUseDefault.Checked := BackGroundColorBox.Selected <> clDefault;
  end;
  if Sender = FrameColorBox then
  begin
    FCurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
    CheckBoxFrameColorUseDefault.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.Enabled := FrameColorBox.Selected <> clDefault;
    FrameStyleBox.Enabled := FrameColorBox.Selected <> clDefault;
  end;
  if Sender = MarkupFoldColorBox then
  begin
    FCurHighlightElement.MarkupFoldLineColor := DefaultToNone(MarkupFoldColorBox.Selected);
    CheckBoxMarkupFoldColorUseDefault.Checked := MarkupFoldColorBox.Selected <> clDefault;
    MarkupFoldStyleBox.Enabled := MarkupFoldColorBox.Selected <> clDefault;
  end;
  if Sender = FrameEdgesBox then
  begin
    FCurHighlightElement.FrameEdges := TSynFrameEdges(FrameEdgesBox.ItemIndex);
  end;
  if Sender = FrameStyleBox then
  begin
    FCurHighlightElement.FrameStyle := TSynLineStyle(FrameStyleBox.ItemIndex);
  end;
  if Sender = MarkupFoldStyleBox then
  begin
    FCurHighlightElement.MarkupFoldLineStyle := TSynLineStyle(MarkupFoldStyleBox.ItemIndex);
  end;

  UpdatingColor := False;
  DoChanged;
end;


{-------------------------------------------------------------------------------
  ForeAlphaSpinChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.ForeAlphaSpinChange(Sender: TObject);
var
  v: Integer;
begin
  if UpdatingColor then
    exit;

  UpdatingColor := True;
  v := TSpinEdit(Sender).Value;
  if (v = 256) and (Caption <> LStr('dlgEdOff', 'Off')) then
    TSpinEdit(Sender).Caption := LStr('dlgEdOff', 'Off');
  UpdatingColor := False;

  if (FCurHighlightElement = nil) then
    exit;

  if v = 256 then v := 0;

  if Sender = ForeAlphaSpin then
    FCurHighlightElement.ForeAlpha := v;
  if Sender = BackAlphaSpin then
    FCurHighlightElement.BackAlpha := v;
  if Sender = FrameAlphaSpin then
    FCurHighlightElement.FrameAlpha := v;
  if Sender = MarkupFoldAlphaSpin then
    FCurHighlightElement.MarkupFoldLineAlpha := v;

  DoChanged;
end;


{-------------------------------------------------------------------------------
  ForeAlphaSpinEnter
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.ForeAlphaSpinEnter(Sender: TObject);
begin
  UpdatingColor := True;
  If TSpinEdit(Sender).Value = 256 then
    TSpinEdit(Sender).Caption := '256';
  UpdatingColor := False;
end;


{-------------------------------------------------------------------------------
  ForegroundColorBoxGetColors
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.ForegroundColorBoxGetColors(Sender: TCustomColorBox; Items: TStrings);
var
  i: longint;
begin
  i := Items.IndexOfObject(TObject(PtrInt(clDefault)));
  if i >= 0 then begin
    Items[i] := LStr('dlgColorNotModified', 'Not modified');
    Items.Move(i, 1);
  end;
end;


{-------------------------------------------------------------------------------
  ForePriorSpinChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.ForePriorSpinChange(Sender: TObject);
var
  v: Integer;
begin
  if (FCurHighlightElement = nil) then
    exit;

  v := TSpinEdit(Sender).Value;

  if Sender = ForePriorSpin then
    FCurHighlightElement.ForePriority := v;
  if Sender = BackPriorSpin then
    FCurHighlightElement.BackPriority := v;
  if Sender = LabelFramePrior then
    FCurHighlightElement.FramePriority := v;

  DoChanged;
end;


{-------------------------------------------------------------------------------
  FrameEdgesBoxDrawItem
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.FrameEdgesBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  PCol: Integer;
begin
  if Index  < 0 then exit;;

  r.top := ARect.top + 3;
  r.bottom := ARect.bottom - 3;
  r.left := ARect.left + 5;
  r.right := ARect.Right - 5;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 1;
    PCol := pen.Color;
    Pen.Color := clGray;
    Pen.Style := psDot;
    Pen.EndCap := pecFlat;
    Rectangle(r);
    Pen.Width := 2;
    pen.Color := PCol;
    Pen.Style := psSolid;
    case Index of
      ord(sfeAround): Rectangle(r);
      ord(sfeBottom): begin
          MoveTo(r.Left, r.Bottom);
          LineTo(r.Right-1, r.Bottom);
        end;
      ord(sfeLeft): begin
          MoveTo(r.Left, r.Top);
          LineTo(r.Left, r.Bottom-1);
        end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  FrameStyleBoxDrawItem
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.FrameStyleBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  p: TPoint;
begin
  if Index  < 0 then exit;;

  with TCustomComboBox(Control).Canvas do
  begin
    FillRect(ARect);
    Pen.Width := 2;
    pen.EndCap := pecFlat;
    case Index of
      0: Pen.Style := psSolid;
      1: Pen.Style := psDash;
      2: Pen.Style := psDot;
      3: Pen.Style := psSolid;
    end;
    if Index = 3 then begin
      MoveToEx(Handle, ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2 - 2, @p);
      WaveTo(Handle, ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2 - 2, 4);
    end else begin
      MoveTo(ARect.Left + 5, (ARect.Top + ARect.Bottom) div 2);
      LineTo(ARect.Right - 5, (ARect.Top + ARect.Bottom) div 2);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  GeneralCheckBoxOnChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.GeneralCheckBoxOnChange(Sender: TObject);
var
  TheColorBox: TColorBox;
begin
  if FCurHighlightElement = nil then
    exit;

  if UpdatingColor = False then begin
    UpdatingColor := True;

    TheColorBox := nil;
    if Sender = CheckBoxForeGroundUseDefault then TheColorBox := ForegroundColorBox;
    if Sender = CheckBoxBackGroundUseDefault then TheColorBox := BackGroundColorBox;
    if Sender = CheckBoxFrameColorUseDefault then TheColorBox := FrameColorBox;
    if Sender = CheckBoxMarkupFoldColorUseDefault then TheColorBox := MarkupFoldColorBox;
    if Assigned(TheColorBox) then begin
      if TCheckBox(Sender).Checked then begin
        TheColorBox.Selected := TheColorBox.Tag;
      end
      else begin
        TheColorBox.Tag := TheColorBox.Selected;
        TheColorBox.Selected := clDefault;
      end;

      if (Sender = CheckBoxForeGroundUseDefault) and
         (DefaultToNone(ForegroundColorBox.Selected) <> FCurHighlightElement.Foreground)
      then begin
        FCurHighlightElement.Foreground := DefaultToNone(ForegroundColorBox.Selected);
        DoChanged;
      end;
      if (Sender = CheckBoxBackGroundUseDefault) and
         (DefaultToNone(BackGroundColorBox.Selected) <> FCurHighlightElement.Background)
      then begin
        FCurHighlightElement.Background := DefaultToNone(BackGroundColorBox.Selected);
        DoChanged;
      end;
      if (Sender = CheckBoxFrameColorUseDefault) and
         (DefaultToNone(FrameColorBox.Selected) <> FCurHighlightElement.FrameColor)
      then begin
        FCurHighlightElement.FrameColor := DefaultToNone(FrameColorBox.Selected);
        FrameEdgesBox.Enabled := TCheckBox(Sender).Checked;
        FrameStyleBox.Enabled := TCheckBox(Sender).Checked;
        DoChanged;
      end;
      if (Sender = CheckBoxMarkupFoldColorUseDefault) and
         (DefaultToNone(MarkupFoldColorBox.Selected) <> FCurHighlightElement.MarkupFoldLineColor)
      then begin
        FCurHighlightElement.MarkupFoldLineColor := DefaultToNone(MarkupFoldColorBox.Selected);
        MarkupFoldStyleBox.Enabled := MarkupFoldColorBox.Selected <> clDefault;
      end;
    end;

    UpdatingColor := False;
  end;

  if Sender = CheckBoxTextBold then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if CheckBoxTextBold.Checked xor (fsBold in FCurHighlightElement.Style) then
    begin
      if CheckBoxTextBold.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsBold]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsBold];
      DoChanged;
    end;
  end;

  if Sender = CheckBoxTextItalic then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if CheckBoxTextItalic.Checked xor (fsItalic in FCurHighlightElement.Style) then
    begin
      if CheckBoxTextItalic.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsItalic]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsItalic];
      DoChanged;
    end;
  end;

  if Sender = CheckBoxTextUnderline then begin
    if hafStyleMask in FCurHighlightElement.Features then
      TextStyleRadioOnChange(Sender)
    else
    if CheckBoxTextUnderline.Checked xor (fsUnderline in FCurHighlightElement.Style) then
    begin
      if CheckBoxTextUnderline.Checked then
        FCurHighlightElement.Style := FCurHighlightElement.Style + [fsUnderline]
      else
        FCurHighlightElement.Style := FCurHighlightElement.Style - [fsUnderline];
      DoChanged;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  pnlElementAttributesResize
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.pnlElementAttributesResize(Sender: TObject);
var
  MinAnchor: TControl;
  MinWidth: Integer;

  procedure CheckControl(Other: TControl);
  var w,h: Integer;
  begin
    if not Other.Visible then exit;
    h:=0;
    w:=0;
    Other.GetPreferredSize(w,h);
    if w <= MinWidth then exit;
    MinAnchor := Other;
    MinWidth := w;
  end;
begin
  MinWidth := -1;
  MinAnchor := ForeGroundLabel;
  CheckControl(ForeGroundLabel);
  CheckControl(BackGroundLabel);
  CheckControl(CheckBoxForeGroundUseDefault);
  CheckControl(CheckBoxBackGroundUseDefault);
  CheckControl(CheckBoxFrameColorUseDefault);
  CheckControl(CheckBoxMarkupFoldColorUseDefault);

  ColumnPosBevel.AnchorSide[akLeft].Control := MinAnchor;
  Constraints.MinHeight := pnlItalic.Top + pnlItalic.Height;
  Constraints.MinWidth := BackPriorSpin.Left + BackPriorSpin.Width;
end;


{-------------------------------------------------------------------------------
  TextStyleRadioOnChange
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.TextStyleRadioOnChange(Sender: TObject);

  procedure CalcNewStyle(CheckBox: TCheckBox; RadioOn, RadioOff,
                         RadioInvert: TRadioButton; fs : TFontStyle;
                         Panel: TPanel);
  begin
    if CheckBox.Checked then
    begin
      Panel.Enabled := True;
      if RadioInvert.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style + [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask - [fs];
      end
      else
      if RadioOn.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style + [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask + [fs];
      end
      else
      if RadioOff.Checked then
      begin
        FCurHighlightElement.Style     := FCurHighlightElement.Style - [fs];
        FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask + [fs];
      end
    end
    else
    begin
      Panel.Enabled := False;
      FCurHighlightElement.Style     := FCurHighlightElement.Style - [fs];
      FCurHighlightElement.StyleMask := FCurHighlightElement.StyleMask - [fs];
    end;
  end;

begin
  if FCurHighlightElement = nil then
    exit;
  if UpdatingColor or not (hafStyleMask in FCurHighlightElement.Features) then
    Exit;

  if (Sender = CheckBoxTextBold) or
     (Sender = RadioTextBoldOn) or
     (Sender = RadioTextBoldOff) or
     (Sender = RadioTextBoldInvert) then
    CalcNewStyle(CheckBoxTextBold, RadioTextBoldOn, RadioTextBoldOff,
                    RadioTextBoldInvert, fsBold, TextBoldRadioPanel);

  if (Sender = CheckBoxTextItalic) or
     (Sender = RadioTextItalicOn) or
     (Sender = RadioTextItalicOff) or
     (Sender = RadioTextItalicInvert) then
    CalcNewStyle(CheckBoxTextItalic, RadioTextItalicOn, RadioTextItalicOff,
                    RadioTextItalicInvert, fsItalic, TextItalicRadioPanel);

  if (Sender = CheckBoxTextUnderline) or
     (Sender = RadioTextUnderlineOn) or
     (Sender = RadioTextUnderlineOff) or
     (Sender = RadioTextUnderlineInvert) then
    CalcNewStyle(CheckBoxTextUnderline, RadioTextUnderlineOn, RadioTextUnderlineOff,
                    RadioTextUnderlineInvert, fsUnderline, TextUnderlineRadioPanel);


  DoChanged;
end;


{-------------------------------------------------------------------------------
  UpdateAll
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.UpdateAll;
begin
  if (FCurHighlightElement = nil) or UpdatingColor then
    Exit;
  UpdatingColor := True;
  DisableAlign;
  try
    // Adjust color captions
    LCap('dlgForecolor', CheckBoxForeGroundUseDefault);
    LCap('dlgBackColor', CheckBoxBackGroundUseDefault);
    LCap('dlgFrameColor', CheckBoxFrameColorUseDefault);
    if FCurrentColorScheme <> nil then begin
      if (FCurrentColorScheme.AttributeByEnum[ahaModifiedLine] <> nil) and
         (FCurHighlightElement.StoredName = FCurrentColorScheme.AttributeByEnum[ahaModifiedLine].StoredName)
      then begin
        LCap('dlgSavedLineColor', CheckBoxForeGroundUseDefault);
        LCap('dlgUnsavedLineColor', CheckBoxFrameColorUseDefault);
      end else
      if (FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree] <> nil) and
         (FCurHighlightElement.StoredName = FCurrentColorScheme.AttributeByEnum[ahaCodeFoldingTree].StoredName)
      then begin
        LCap('dlgGutterCollapsedColor', CheckBoxFrameColorUseDefault);
      end else
      if (FCurrentColorScheme.AttributeByEnum[ahaCaretColor] <> nil) and
         (FCurHighlightElement.StoredName = FCurrentColorScheme.AttributeByEnum[ahaCaretColor].StoredName)
      then begin
        LCap('dlgCaretForeColor', CheckBoxForeGroundUseDefault);
        LCap('dlgCaretBackColor', CheckBoxBackGroundUseDefault);
      end;
    end;

    if FCurHighlightElement.Group = agnDefault then begin
      ForegroundColorBox.Style := ForegroundColorBox.Style - [cbIncludeDefault];
      BackGroundColorBox.Style := BackGroundColorBox.Style - [cbIncludeDefault];
    end else begin
      ForegroundColorBox.Style := ForegroundColorBox.Style + [cbIncludeDefault];
      BackGroundColorBox.Style := BackGroundColorBox.Style + [cbIncludeDefault];
    end;

    // Forground
    ForeGroundLabel.Visible              := (hafForeColor in FCurHighlightElement.Features) and
                                            (FCurHighlightElement.Group = agnDefault);
    CheckBoxForeGroundUseDefault.Visible := (hafForeColor in FCurHighlightElement.Features) and
                                            not(FCurHighlightElement.Group = agnDefault);
    ForegroundColorBox.Visible           := (hafForeColor in FCurHighlightElement.Features);

    ForegroundColorBox.Selected := NoneToDefault(FCurHighlightElement.Foreground);
    if ForegroundColorBox.Selected = clDefault then
      ForegroundColorBox.Tag := ForegroundColorBox.DefaultColorColor
    else
      ForegroundColorBox.Tag := ForegroundColorBox.Selected;
    CheckBoxForeGroundUseDefault.Checked := ForegroundColorBox.Selected <> clDefault;

    ForeAlphaSpin.Visible  := ForegroundColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    LabelForeAlpha.Visible := ForeAlphaSpin.Visible;
    if FCurHighlightElement.ForeAlpha = 0 then begin
      ForeAlphaSpin.Value    := 256; // Off
      Application.ProcessMessages;
      ForeAlphaSpin.Caption  := 'Off';
      LCap('dlgEdOff', ForeAlphaSpin);
    end
    else
      ForeAlphaSpin.Value    := FCurHighlightElement.ForeAlpha;

    ForePriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    LabelForePrior.Visible := ForePriorSpin.Visible;
    ForePriorSpin.Value    := FCurHighlightElement.ForePriority;


    // BackGround
    BackGroundLabel.Visible              := (hafBackColor in FCurHighlightElement.Features) and
                                            (FCurHighlightElement.Group = agnDefault);
    CheckBoxBackGroundUseDefault.Visible := (hafBackColor in FCurHighlightElement.Features) and
                                            not(FCurHighlightElement.Group = agnDefault);
    BackGroundColorBox.Visible           := (hafBackColor in FCurHighlightElement.Features);

    BackGroundColorBox.Selected := NoneToDefault(FCurHighlightElement.Background);
    if BackGroundColorBox.Selected = clDefault then
      BackGroundColorBox.Tag := BackGroundColorBox.DefaultColorColor
    else
      BackGroundColorBox.Tag := BackGroundColorBox.Selected;
    CheckBoxBackGroundUseDefault.Checked := BackGroundColorBox.Selected <> clDefault;

    BackAlphaSpin.Visible := BackGroundColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    LabelBackAlpha.Visible := BackAlphaSpin.Visible;
    if FCurHighlightElement.BackAlpha = 0 then begin
      BackAlphaSpin.Value    := 256; // Off
      BackAlphaSpin.Caption  := 'Off';
      LCap('dlgEdOff', BackAlphaSpin);
    end
    else
      BackAlphaSpin.Value    := FCurHighlightElement.BackAlpha;

    BackPriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    LabelBackPrior.Visible := BackPriorSpin.Visible;
    BackPriorSpin.Value    := FCurHighlightElement.BackPriority;

    // Frame
    CheckBoxFrameColorUseDefault.Visible := hafFrameColor in FCurHighlightElement.Features;
    FrameColorBox.Visible                := hafFrameColor in FCurHighlightElement.Features;
    FrameEdgesBox.Visible                := hafFrameEdges in FCurHighlightElement.Features;
    FrameStyleBox.Visible                := hafFrameStyle in FCurHighlightElement.Features;

    FrameColorBox.Selected := NoneToDefault(FCurHighlightElement.FrameColor);
    if FrameColorBox.Selected = clDefault then
      FrameColorBox.Tag := FrameColorBox.DefaultColorColor
    else
      FrameColorBox.Tag := FrameColorBox.Selected;
    CheckBoxFrameColorUseDefault.Checked := FrameColorBox.Selected <> clDefault;
    FrameEdgesBox.ItemIndex := integer(FCurHighlightElement.FrameEdges);
    FrameStyleBox.ItemIndex := integer(FCurHighlightElement.FrameStyle);
    FrameEdgesBox.Enabled := CheckBoxFrameColorUseDefault.Checked;
    FrameStyleBox.Enabled := CheckBoxFrameColorUseDefault.Checked;

    FrameAlphaSpin.Visible := FrameColorBox.Visible and
                             (hafAlpha in FCurHighlightElement.Features);
    LabelFrameAlpha.Visible := FrameAlphaSpin.Visible;
    if FCurHighlightElement.FrameAlpha = 0 then begin
      FrameAlphaSpin.Value    := 256; // Off
      FrameAlphaSpin.Caption  := 'Off';
      LCap('dlgEdOff', FrameAlphaSpin);
    end
    else
      FrameAlphaSpin.Value    := FCurHighlightElement.FrameAlpha;

    FramePriorSpin.Visible  := ForegroundColorBox.Visible and FShowPrior and
                             (hafPrior in FCurHighlightElement.Features);
    LabelFramePrior.Visible := FramePriorSpin.Visible;
    FramePriorSpin.Value    := FCurHighlightElement.FramePriority;

    // Markup Fold
    CheckBoxMarkupFoldColorUseDefault.Visible := hafMarkupFoldColor in FCurHighlightElement.Features;
    MarkupFoldColorBox.Visible                := hafMarkupFoldColor in FCurHighlightElement.Features;
    LabelMarkupFoldAlpha.Visible             := hafMarkupFoldColor in FCurHighlightElement.Features;
    MarkupFoldAlphaSpin.Visible              := hafMarkupFoldColor in FCurHighlightElement.Features;
    MarkupFoldStyleBox.Visible               := hafMarkupFoldColor in FCurHighlightElement.Features;

    MarkupFoldColorBox.Selected := NoneToDefault(FCurHighlightElement.MarkupFoldLineColor);
    if MarkupFoldColorBox.Selected = clDefault then
      MarkupFoldColorBox.Tag := MarkupFoldColorBox.DefaultColorColor
    else
      MarkupFoldColorBox.Tag := MarkupFoldColorBox.Selected;
    CheckBoxMarkupFoldColorUseDefault.Checked := MarkupFoldColorBox.Selected <> clDefault;

    MarkupFoldStyleBox.ItemIndex := integer(FCurHighlightElement.MarkupFoldLineStyle);
    MarkupFoldStyleBox.Enabled := CheckBoxMarkupFoldColorUseDefault.Checked;

    if FCurHighlightElement.MarkupFoldLineAlpha = 0 then begin
      MarkupFoldAlphaSpin.Value    := 256; // Off
      MarkupFoldAlphaSpin.Caption  := 'Off';
      LCap('dlgEdOff', MarkupFoldAlphaSpin);
    end
    else
      MarkupFoldAlphaSpin.Value    := FCurHighlightElement.MarkupFoldLineAlpha;

    // Styles
    CheckBoxTextBold.Visible      := hafStyle in FCurHighlightElement.Features;
    CheckBoxTextItalic.Visible    := hafStyle in FCurHighlightElement.Features;
    CheckBoxTextUnderline.Visible := hafStyle in FCurHighlightElement.Features;

    TextBoldRadioPanel.Visible      := hafStyleMask in FCurHighlightElement.Features;
    TextItalicRadioPanel.Visible    := hafStyleMask in FCurHighlightElement.Features;
    TextUnderlineRadioPanel.Visible := hafStyleMask in FCurHighlightElement.Features;

    if hafStyleMask in FCurHighlightElement.Features then begin
      CheckBoxTextBold.Checked   := (fsBold in FCurHighlightElement.Style) or
                                    (fsBold in FCurHighlightElement.StyleMask);
      TextBoldRadioPanel.Enabled := CheckBoxTextBold.Checked;

      if not(fsBold in FCurHighlightElement.StyleMask) then
        RadioTextBoldInvert.Checked := True
      else
      if fsBold in FCurHighlightElement.Style then
        RadioTextBoldOn.Checked := True
      else
        RadioTextBoldOff.Checked := True;

      CheckBoxTextItalic.Checked   := (fsItalic in FCurHighlightElement.Style) or
                                      (fsItalic in FCurHighlightElement.StyleMask);
      TextItalicRadioPanel.Enabled := CheckBoxTextItalic.Checked;

      if not(fsItalic in FCurHighlightElement.StyleMask) then
        RadioTextItalicInvert.Checked := True
      else
      if fsItalic in FCurHighlightElement.Style then
        RadioTextItalicOn.Checked := True
      else
        RadioTextItalicOff.Checked := True;

      CheckBoxTextUnderline.Checked := (fsUnderline in FCurHighlightElement.Style) or
                                  (fsUnderline in FCurHighlightElement.StyleMask);
      TextUnderlineRadioPanel.Enabled := CheckBoxTextUnderline.Checked;

      if not(fsUnderline in FCurHighlightElement.StyleMask) then
        RadioTextUnderlineInvert.Checked := True
      else
      if fsUnderline in FCurHighlightElement.Style then
        RadioTextUnderlineOn.Checked := True
      else
        RadioTextUnderlineOff.Checked := True;
    end
    else
    begin
      CheckBoxTextBold.Checked      := fsBold in FCurHighlightElement.Style;
      CheckBoxTextItalic.Checked    := fsItalic in FCurHighlightElement.Style;
      CheckBoxTextUnderline.Checked := fsUnderline in FCurHighlightElement.Style;
    end;

    UpdatingColor := False;
  finally
    EnableAlign;
  end;
  pnlElementAttributesResize(nil);
end;


{-------------------------------------------------------------------------------
  SetCurHighlightElement
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.SetCurHighlightElement(AValue: TColorSchemeAttribute);
begin
  if FCurHighlightElement = AValue then Exit;
  FCurHighlightElement := AValue;
  UpdateAll;
end;


{-------------------------------------------------------------------------------
  DoChanged
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;


{-------------------------------------------------------------------------------
  SetShowPrior
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.SetShowPrior(AValue: Boolean);
begin
  if FShowPrior = AValue then Exit;
  FShowPrior := AValue;
  UpdateAll;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TCnfSyneditFrameColorAttrEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);
  FShowPrior := False;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.InitializeWnd();
begin

end;


{-------------------------------------------------------------------------------
  Setup
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.Setup;
begin
  UpdatingColor := False;
  ColumnPosBevel.Height := 1;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TCnfSyneditFrameColorAttrEditor.OnReplaceLng();
begin
  LCap('T_CnfSyneditFrameColorAttrEditor_GroupBoxColorAttributs',            GroupBoxColorAttributs);
  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxForeGroundUseDefault',      CheckBoxForeGroundUseDefault);
  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxBackGroundUseDefault',      CheckBoxBackGroundUseDefault);

  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxFrameColorUseDefault',      CheckBoxFrameColorUseDefault);
  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxMarkupFoldColorUseDefault', CheckBoxMarkupFoldColorUseDefault);

  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxTextUnderline',             CheckBoxTextUnderline);
  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxTextBold',                  CheckBoxTextBold);
  LCap('T_CnfSyneditFrameColorAttrEditor_CheckBoxTextItalic',                CheckBoxTextItalic);

  LCap('T_General_LabelAlpha',  LabelForeAlpha);
  LCap('T_General_LabelAlpha',  LabelBackAlpha);
  LCap('T_General_LabelAlpha',  LabelFrameAlpha);
  LCap('T_General_LabelAlpha',  LabelMarkupFoldAlpha);

  LCap('T_General_LabelPrior',  LabelForePrior);
  LCap('T_General_LabelPrior',  LabelBackPrior);
  LCap('T_General_LabelPrior',  LabelFramePrior);

  LCap('T_General_RadioOn',     RadioTextUnderlineOn);
  LCap('T_General_RadioOff',    RadioTextUnderlineOff);
  LCap('T_General_RadioInvert', RadioTextUnderlineInvert);

  LCap('T_General_RadioOn',     RadioTextBoldOn);
  LCap('T_General_RadioOff',    RadioTextBoldOff);
  LCap('T_General_RadioInvert', RadioTextBoldInvert);

  LCap('T_General_RadioOn',     RadioTextItalicOn);
  LCap('T_General_RadioOff',    RadioTextItalicOff);
  LCap('T_General_RadioInvert', RadioTextItalicInvert);

end;




end.


