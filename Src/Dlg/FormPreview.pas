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
 *  Sources: Convert parts of HtmlViewer unit to fpcLibs                       *
 *           Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
unit FormPreview;

interface

uses
{$ifdef LCL}
  LclIntf, LclType, PrintersDlgs, HtmlMisc,
{$endif}
{$ifdef MsWindows}
  Windows,
{$endif}
{$ifndef NoMetafile}
  MetaFilePrinter,
  vwPrint,
{$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Printers,
  StdCtrls, ExtCtrls, Buttons,
  HTMLView, FormPrintStatus;

const
   crZoom = 40;
   crHandDrag = 41;
   ZOOMFACTOR = 1.5;

type

  TFormPreview = class(TForm)
    ToolBarPanel: TPanel;
    GridBut: TSpeedButton;
    ZoomCursorBut: TSpeedButton;
    HandCursorBut: TSpeedButton;
    OnePageBut: TSpeedButton;
    TwoPageBut: TSpeedButton;
    PrintBut: TBitBtn;
    NextPageBut: TBitBtn;
    PrevPageBut: TBitBtn;
    CloseBut: TBitBtn;
    ZoomBox: TComboBox;
    StatBarPanel: TPanel;
    CurPageLabel: TPanel;
    ZoomLabel: TPanel;
    Panel1: TPanel;
    HintLabel: TLabel;
    MoveButPanel: TPanel;
    FirstPageSpeed: TSpeedButton;
    PrevPageSpeed: TSpeedButton;
    NextPageSpeed: TSpeedButton;
    LastPageSpeed: TSpeedButton;
    PageNumSpeed: TSpeedButton;
    ScrollBox1: TScrollBox;
    ContainPanel: TPanel;
    PagePanel: TPanel;
    PB1: TPaintBox;
    PagePanel2: TPanel;
    PB2: TPaintBox;
    PrintDialog: TPrintDialog;
    FitPageBut: TSpeedButton;
    FitWidthBut: TSpeedButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    UnitsBox: TComboBox;
    Bevel7: TBevel;
    procedure CloseButClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ScrollBox1Resize(Sender: TObject);
    procedure PBPaint(Sender: TObject);
    procedure GridButClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ZoomBoxChange(Sender: TObject);
    procedure TwoPageButClick(Sender: TObject);
    procedure NextPageButClick(Sender: TObject);
    procedure PrevPageButClick(Sender: TObject);
    procedure FirstPageSpeedClick(Sender: TObject);
    procedure LastPageSpeedClick(Sender: TObject);
    procedure ZoomCursorButClick(Sender: TObject);
    procedure HandCursorButClick(Sender: TObject);
    procedure PB1MouseDown(Sender: TObject; {%H-}Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PB1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PB1MouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PrintButClick(Sender: TObject);
    procedure PageNumSpeedClick(Sender: TObject);
    procedure OnePageButMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FitPageButClick(Sender: TObject);
    procedure FitWidthButClick(Sender: TObject);
    procedure UnitsBoxChange(Sender: TObject);

  private
    Viewer: ThtmlViewer;
    function GetPageCount: Integer;

  protected
    FCurPage      : integer;
    OldHint       : TNotifyEvent;
    DownX, DownY  : integer;
    Moving        : boolean;
{$ifndef NoMetafile}
    MFPrinter     : TMetaFilePrinter;
    procedure     DrawMetaFile(PB: TPaintBox; mf: TMetaFile);
{$endif}
    procedure     OnHint(Sender: TObject);
    procedure     SetCurPage(Val: Integer);
    procedure     CheckEnable;
    property      CurPage: Integer read FCurPage write SetCurPage;
    property      PageCount: Integer read GetPageCount;

  public
    Zoom          : double;
    constructor CreateIt(AOwner: TComponent; AViewer: ThtmlViewer; var Abort: boolean);
    destructor Destroy; override;

  end;


implementation

uses
   FormGopage;

{$ifdef LCL}
  {$R *.lfm}
{$else}
  {$R *.dfm}
{$endif}
{$R GRID.RES}


{ TFormPreview }

{-------------------------------------------------------------------------------
  CreateIt
 ------------------------------------------------------------------------------}
constructor TFormPreview.CreateIt(AOwner: TComponent; AViewer: ThtmlViewer; var Abort: boolean);
var
  StatusForm: TFormPrnStatus;
begin
  inherited Create(AOwner);
  ZoomBox.ItemIndex := 0;
  UnitsBox.ItemIndex := 0;
{$ifdef MsWindows}
  Screen.Cursors[crZoom] := LoadCursor(hInstance, 'ZOOM_CURSOR');
  Screen.Cursors[crHandDrag] := LoadCursor(hInstance, 'HAND_CURSOR');
{$else}
  Screen.Cursors[crZoom] := LoadCursorFromLazarusResource('ZOOM_CURSOR');
  Screen.Cursors[crHandDrag] := LoadCursorFromLazarusResource('HAND_CURSOR');
{$endif}
  ZoomCursorButClick(nil);
  Viewer := AViewer;
{$ifndef NoMetafile}
  MFPrinter := TMetaFilePrinter.Create(Self);
{$endif}
  StatusForm := TFormPrnStatus.Create(Self);
  try
{$ifndef NoMetafile}
    StatusForm.DoPreview(Viewer, MFPrinter, Abort);
{$endif}
  finally
    StatusForm.Free;
  end;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TFormPreview.Destroy;
begin
  inherited;
end;


{-------------------------------------------------------------------------------
  CloseButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.CloseButClick(Sender: TObject);
begin
   Close;
end;


{-------------------------------------------------------------------------------
  FormClose
 ------------------------------------------------------------------------------}
procedure TFormPreview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   CloseAction := caFree;
   Application.OnHint := OldHint;
{$ifndef NoMetafile}
   MFPrinter.Free;
{$endif}
end;


{-------------------------------------------------------------------------------
  ScrollBox1Resize
 ------------------------------------------------------------------------------}
procedure TFormPreview.ScrollBox1Resize(Sender: TObject);
const
   BORD = 20;
var
  z        : double;
  tmp      : integer;
  TotWid   : integer;
begin
  case ZoomBox.ItemIndex of
     0  : FitPageBut.Down  := True;
     1  : FitWidthBut.Down := True;
  else
     begin
        FitPageBut.Down  := False;
        FitWidthBut.Down := False;
     end;
  end;

  if ZoomBox.ItemIndex = -1 then
     ZoomBox.ItemIndex := 0;

  Case ZoomBox.ItemIndex of

     0: z := ((ScrollBox1.ClientHeight - BORD) / PixelsPerInch) / (MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY);
     1: z := ((ScrollBox1.ClientWidth - BORD) / PixelsPerInch) / (MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX);
     2: z := Zoom;
     3: z := 0.25;
     4: z := 0.50;
     5: z := 0.75;
     6: z := 1.00;
     7: z := 1.25;
     8: z := 1.50;
     9: z := 2.00;
     10: z := 3.00;
     11: z := 4.00;
  else
     z := 1;
  end;

  if ZoomBox.ItemIndex<>0 then OnePageBut.Down := True;

  PagePanel.Height := TRUNC(PixelsPerInch * z * MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY);
  PagePanel.Width  := TRUNC(PixelsPerInch * z * MFPrinter.PaperWidth  / MFPrinter.PixelsPerInchX);

  PagePanel2.Visible := TwoPageBut.Down;
  if TwoPageBut.Down then
     begin
        PagePanel2.Width  := PagePanel.Width;
        PagePanel2.Height := PagePanel.Height;
     end;

  TotWid := PagePanel.Width + BORD;
  if TwoPageBut.Down then
     TotWid := TotWid + PagePanel2.Width + BORD;

  // Resize the Contain Panel
  tmp := PagePanel.Height + BORD;
  if tmp < ScrollBox1.ClientHeight then
     tmp := ScrollBox1.ClientHeight-1;
  ContainPanel.Height := tmp;

  tmp := TotWid;
  if tmp < ScrollBox1.ClientWidth then
     tmp := ScrollBox1.ClientWidth-1;
  ContainPanel.Width := tmp;

  // Center the Page Panel
  if PagePanel.Height + BORD < ContainPanel.Height then
     PagePanel.Top := ContainPanel.Height div 2 - PagePanel.Height div 2
  else
     PagePanel.Top := BORD div 2;
  PagePanel2.Top := PagePanel.Top;

  if TotWid < ContainPanel.Width then
     PagePanel.Left := ContainPanel.Width div 2 - (TotWid - BORD) div 2
  else
     PagePanel.Left := BORD div 2;
  PagePanel2.Left := PagePanel.Left + PagePanel.Width + BORD;

  {Make sure the scroll bars are hidden if not needed}
  if (PagePanel.Width +BORD <= ScrollBox1.Width) and
     (PagePanel.Height +BORD <= ScrollBox1.Height) then
    begin
    ScrollBox1.HorzScrollBar.Visible := False;
    ScrollBox1.VertScrollBar.Visible := False;
    end
  else
    begin
    ScrollBox1.HorzScrollBar.Visible := True;
    ScrollBox1.VertScrollBar.Visible := True;
    end;

  // Set the Zoom Variable
  Zoom := z;
  ZoomLabel.Caption := Format('%1.0n', [z * 100]) + '%';
end;

{$ifndef NoMetafile}

{-------------------------------------------------------------------------------
  DrawMetaFile
 ------------------------------------------------------------------------------}
procedure TFormPreview.DrawMetaFile(PB: TPaintBox; mf: TMetaFile);
begin
  PB.Canvas.Draw(0, 0, mf);
end;

{$endif}


{-------------------------------------------------------------------------------
  PBPaint
 ------------------------------------------------------------------------------}
procedure TFormPreview.PBPaint(Sender: TObject);
var
  PB       : TPaintBox;
  x1, y1   : integer;
  x, y     : integer;
  Factor   : double;
  Draw     : boolean;
  Page     : integer;
begin
  PB := Sender as TPaintBox;

  if PB = PB1 then
    begin
      Draw := CurPage < MFPrinter.LastAvailablePage;
      Page := CurPage;
    end
  else
    begin
        // PB2
      Draw := TwoPageBut.Down and (CurPage+1 < MFPrinter.LastAvailablePage);
      Page := CurPage + 1;
    end;

  SetMapMode(PB.Canvas.Handle, MM_ANISOTROPIC);
  SetWindowExtEx(PB.Canvas.Handle, MFPrinter.PaperWidth, MFPrinter.PaperHeight, nil);
  SetViewportExtEx(PB.Canvas.Handle, PB.Width, PB.Height, nil);
  SetWindowOrgEx(PB.Canvas.Handle, -MFPrinter.OffsetX, -MFPrinter.OffsetY, nil);
  if Draw then
    DrawMetaFile(PB, MFPrinter.MetaFiles[Page]);

  if GridBut.Down then
    begin
      SetWindowOrgEx(PB.Canvas.Handle, 0, 0, nil);
      PB.Canvas.Pen.Color := clLtGray;
      if UnitsBox.ItemIndex = 0 then
        Factor := 1.0
      else Factor := 2.54;

      for x := 1 to Round(MFPrinter.PaperWidth / MFPrinter.PixelsPerInchX * Factor) do
        begin
          x1 := Round(MFPrinter.PixelsPerInchX * x / Factor);
          PB.Canvas.MoveTo(x1, 0);
          PB.Canvas.LineTo(x1, MFPrinter.PaperHeight);
        end;

    for y := 1 to Round(MFPrinter.PaperHeight / MFPrinter.PixelsPerInchY * Factor) do
      begin
        y1 := Round(MFPrinter.PixelsPerInchY * y / Factor);
        PB.Canvas.MoveTo(0, y1);
        PB.Canvas.LineTo(MFPrinter.PaperWidth, y1);
      end;
    end;
end;


{-------------------------------------------------------------------------------
  GridButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.GridButClick(Sender: TObject);
begin
  PB1.Invalidate;
  PB2.Invalidate;
end;


{-------------------------------------------------------------------------------
  OnHint
 ------------------------------------------------------------------------------}
procedure TFormPreview.OnHint(Sender: TObject);
begin
  HintLabel.Caption := Application.Hint;
end;


{-------------------------------------------------------------------------------
  FormShow
 ------------------------------------------------------------------------------}
procedure TFormPreview.FormShow(Sender: TObject);
begin
  CurPage := 0;
  OldHint := Application.OnHint;
  Application.OnHint := @OnHint;
  CheckEnable;
 {$ifdef delphi7_plus}
  PagePanel.ParentBackground := False;
  PagePanel2.ParentBackground := False;
 {$endif}
  ScrollBox1Resize(Nil);   {make sure it gets sized}
end;


{-------------------------------------------------------------------------------
  SetCurPage
 ------------------------------------------------------------------------------}
procedure TFormPreview.SetCurPage(Val: integer);
var
  tmp : integer;
begin
  FCurPage := Val;
  tmp := 0;
  if MFPrinter <> nil then
     tmp := MFPrinter.LastAvailablePage;
  CurPageLabel.Caption := Format('Page %d of %d', [Val+1, tmp]);
  PB1.Invalidate;
  PB2.Invalidate;
end;


{-------------------------------------------------------------------------------
  ZoomBoxChange
 ------------------------------------------------------------------------------}
procedure TFormPreview.ZoomBoxChange(Sender: TObject);
begin
  ScrollBox1Resize(nil);
  ScrollBox1Resize(nil);
end;


{-------------------------------------------------------------------------------
  TwoPageButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.TwoPageButClick(Sender: TObject);
begin
  ZoomBox.ItemIndex := 0;
  ScrollBox1Resize(nil);
end;


{-------------------------------------------------------------------------------
  NextPageButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.NextPageButClick(Sender: TObject);
begin
  CurPage := CurPage + 1;
  CheckEnable;
end;


{-------------------------------------------------------------------------------
  PrevPageButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.PrevPageButClick(Sender: TObject);
begin
  CurPage := CurPage - 1;
  CheckEnable;
end;


{-------------------------------------------------------------------------------
  CheckEnable
 ------------------------------------------------------------------------------}
procedure TFormPreview.CheckEnable;
begin
  NextPageBut.Enabled    := CurPage+1 < MFPrinter.LastAvailablePage;
  PrevPageBut.Enabled    := CurPage > 0;

  NextPageSpeed.Enabled  := NextPageBut.Enabled;
  PrevPageSpeed.Enabled  := PrevPageBut.Enabled;

  FirstPageSpeed.Enabled := PrevPageBut.Enabled;
  LastPageSPeed.Enabled  := NextPageBut.Enabled;

  PageNumSpeed.Enabled   := MFPrinter.LastAvailablePage > 1;
end;


{-------------------------------------------------------------------------------
  FirstPageSpeedClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.FirstPageSpeedClick(Sender: TObject);
begin
  CurPage := 0;
  CheckEnable;
end;


{-------------------------------------------------------------------------------
  LastPageSpeedClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.LastPageSpeedClick(Sender: TObject);
begin
  CurPage := MFPrinter.LastAvailablePage-1;
  CheckEnable;
end;


{-------------------------------------------------------------------------------
  ZoomCursorButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.ZoomCursorButClick(Sender: TObject);
begin
  PB1.Cursor := crZoom;
  PB2.Cursor := crZoom;
end;


{-------------------------------------------------------------------------------
  HandCursorButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.HandCursorButClick(Sender: TObject);
begin
  PB1.Cursor := crHandDrag;
  PB2.Cursor := crHandDrag;
end;


{-------------------------------------------------------------------------------
  PB1MouseDown
 ------------------------------------------------------------------------------}
procedure TFormPreview.PB1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  sx, sy : single;
  nx, ny : integer;
begin
  if ZoomCursorBut.Down then
    begin
      sx := X / PagePanel.Width;
      sy := Y / PagePanel.Height;

      if (ssLeft  in Shift) and (Zoom < 20.0) then Zoom := Zoom * ZOOMFACTOR;
      if (ssRight in Shift) and (Zoom > 0.1) then Zoom := Zoom / ZOOMFACTOR;
      ZoomBox.ItemIndex := 2;
      ScrollBox1Resize(nil);

      nx := TRUNC(sx * PagePanel.Width);
      ny := TRUNC(sy * PagePanel.Height);
      ScrollBox1.HorzScrollBar.Position := nx - ScrollBox1.Width div 2;
      ScrollBox1.VertScrollBar.Position := ny - ScrollBox1.Height div 2;
    end;

  if HandCursorBut.Down then
    begin
      DownX  := X;
      DownY  := Y;
      Moving := True;
    end;
end;


{-------------------------------------------------------------------------------
  PB1MouseMove
 ------------------------------------------------------------------------------}
procedure TFormPreview.PB1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if Moving then
      begin
         ScrollBox1.HorzScrollBar.Position := ScrollBox1.HorzScrollBar.Position + (DownX - X);
         ScrollBox1.VertScrollBar.Position := ScrollBox1.VertScrollBar.Position + (DownY - Y);
      end;
end;


{-------------------------------------------------------------------------------
  PB1MouseUp
 ------------------------------------------------------------------------------}
procedure TFormPreview.PB1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Moving := False;
end;


{-------------------------------------------------------------------------------
  PrintButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.PrintButClick(Sender: TObject);
begin
  PrintWithDialog(Self, PrintDialog, Viewer, PageCount);
end;


{-------------------------------------------------------------------------------
  PageNumSpeedClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.PageNumSpeedClick(Sender: TObject);
var
  gp : TFormGoPage;
begin
  gp := TFormGoPage.Create(Self);
  gp.PageNum.MaxValue := MFPrinter.LastAvailablePage;
  gp.PageNum.Value := CurPage + 1;

  if gp.ShowModal = mrOK then
     begin
        CurPage := gp.PageNum.Value - 1;
        CheckEnable;
     end;
  gp.Free;
end;


{-------------------------------------------------------------------------------
  OnePageButMouseUp
 ------------------------------------------------------------------------------}
procedure TFormPreview.OnePageButMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ZoomBox.ItemIndex := 0;
  ScrollBox1Resize(nil);
end;


{-------------------------------------------------------------------------------
  FitPageButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.FitPageButClick(Sender: TObject);
begin
  ZoomBox.ItemIndex := 0;
  ZoomBoxChange(nil);
end;


{-------------------------------------------------------------------------------
  FitWidthButClick
 ------------------------------------------------------------------------------}
procedure TFormPreview.FitWidthButClick(Sender: TObject);
begin
  ZoomBox.ItemIndex := 1;
  ZoomBoxChange(nil);
end;


{-------------------------------------------------------------------------------
  UnitsBoxChange
 ------------------------------------------------------------------------------}
procedure TFormPreview.UnitsBoxChange(Sender: TObject);
begin
if GridBut.down then
  begin
  PB1.Invalidate;
  PB2.Invalidate;
  end;
end;


{-------------------------------------------------------------------------------
  GetPageCount
 ------------------------------------------------------------------------------}
function TFormPreview.GetPageCount: Integer;
begin
  Result := MFPrinter.LastAvailablePage;
end;



end.
