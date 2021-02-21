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
 *  Sources: Andreas Peter Luft, January 10 2021                               *
 *                                                                             *
 *******************************************************************************
}
unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources,

  // LCL
  Forms, Controls, Graphics, StdCtrls, Buttons, ExtCtrls, ComCtrls, Menus,
  LCLIntf, LCLPlatformDef, Clipbrd, LCLVersion,

  // LazUtils
  FPCAdds, LazFileUtils,

  // Application Version
  AppVersion;

type

  { TScrollingText }

  TScrollingText = class(TGraphicControl)

  private
    FActive: boolean;
    FActiveLine: integer;
    FBuffer: TBitmap;
    FEndLine: integer;
    FLineHeight: integer;
    FLines: TStrings;
    FNumLines: integer;
    FOffset: integer;
    FOffsetHeight: integer;
    FStartLine: integer;
    FStepSize: integer;
    FTimer: TTimer;
    function ActiveLineIsURL: boolean;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);

  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: boolean read FActive write SetActive;
    property Lines: TStrings read FLines write FLines;
    property OffsetHeight: integer read FOffsetHeight write FOffsetHeight;

  end;


  { TFormAbout }

  TFormAbout = class(TForm)

    CloseButton: TButton;
    BuildDateLabel: TLABEL;
    DocumentationLabel: TLabel;
    DocumentationURLLabel: TLabel;
    FPCVersionLabel: TLabel;
    ImageList1: TImageList;
    LabelAppName: TLabel;
    LogoImage: TImage;
    miVerToClipboard: TMenuItem;
    OfficialLabel: TLabel;
    OfficialURLLabel: TLabel;
    PanelAbout: TPanel;
    VersionPage: TTabSheet;
    ButtonPanel: TPanel;
    PlatformLabel: TLabel;
    PopupMenu1: TPopupMenu;
    VersionLabel: TLABEL;
    RevisionLabel: TLabel;
    Notebook: TPageControl;
    AboutPage: TTabSheet;
    DisclaimerPage: TTabSheet;
    AcknowledgementsPage:TTabSheet;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormAboutCreate(Sender:TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure miVerToClipboardClick(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure URLLabelMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);

  private
    About: TScrollingText;
    Disclaimer: TScrollingText;
    Acknowledgements: TScrollingText;
    procedure LoadAbout;
    procedure LoadDisclaimer;
    procedure LoadAcknowledgements;

  end;


function ShowFormAbout: TModalResult;
function GetAppVersionString: string;
function GetAppRevision: string;

var
  OFormAbout: TFormAbout;
  LazarusRevisionStr: string;


implementation

{$R *.lfm}

uses
  FormBase;

{-------------------------------------------------------------------------------
  ShowFormAbout
 ------------------------------------------------------------------------------}
function ShowFormAbout: TModalResult;
var
  FormAbout: TFormAbout;
begin
  FormAbout := TFormAbout.Create(nil);
  Result    := FormAbout.ShowModal;
  FormAbout.Free;
end;


{-------------------------------------------------------------------------------
  GetAppVersionString
 ------------------------------------------------------------------------------}
function GetAppVersionString: string;
begin
  Result := VersionMajor.ToString + '.' + VersionMinor.ToString + '.' + VersionRelease.ToString;
end;


{-------------------------------------------------------------------------------
  GetAppRevision
 ------------------------------------------------------------------------------}
function GetAppRevision: string;
begin
  Result:=VersionRelease.ToString;
end;



{ TFormAbout }

{-------------------------------------------------------------------------------
  FormAboutCreate
 ------------------------------------------------------------------------------}
procedure TFormAbout.FormAboutCreate(Sender:TObject);

  { The compiler generated date string is always of the form y/m/d.
    This function gives it a string respresentation according to the
    shortdateformat }
  function GetLocalizedBuildDate(): string;
  var
    BuildDate: string;
    SlashPos1, SlashPos2: integer;
    Date: TDateTime;
  begin
    BuildDate := {$I %date%};
    SlashPos1 := Pos('/',BuildDate);
    SlashPos2 := SlashPos1 +
      Pos('/', Copy(BuildDate, SlashPos1+1, Length(BuildDate)-SlashPos1));
    Date := EncodeDate(StrToWord(Copy(BuildDate,1,SlashPos1-1)),
      StrToWord(Copy(BuildDate,SlashPos1+1,SlashPos2-SlashPos1-1)),
      StrToWord(Copy(BuildDate,SlashPos2+1,Length(BuildDate)-SlashPos2)));
    Result := FormatDateTime('yyyy-mm-dd', Date);
  end;

begin
  Notebook.PageIndex      := 0;
  Caption                 := 'About '        + OAppName;
  VersionLabel.Caption    := 'Version: '     + GetAppVersionString;
  RevisionLabel.Caption   := 'Revision: '    + GetFileVersion;
  BuildDateLabel.Caption  := 'Date: '        + GetCompiledDate;
  FPCVersionLabel.Caption := 'Version FPC: ' + {$I %FPCVERSION%} +
                             '  Laz: '       + GetLCLVersion;
  PlatformLabel.Caption   := 'Target-CPU: '  + GetCPU +
                             '  Platform: '  + GetOS;

  VersionPage.Caption                 := 'Version';
  AboutPage.Caption                   := 'About';
  DisclaimerPage.Caption              := 'Disclaimer';
  DisclaimerPage.DoubleBuffered       := True;
  miVerToClipboard.Caption            := 'Clipboard';
  AcknowledgementsPage.Caption        := 'Acknowledgements';
  AcknowledgementsPage.DoubleBuffered := True;

  Constraints.MinWidth  := 460;
  Constraints.MinHeight := 380;
  Width  := 460;
  Height := 380;

  OfficialLabel.Caption         := 'Website: ';
  OfficialURLLabel.Caption      := 'https://github.com/FpcLibs/MdEdit';
  DocumentationLabel.Caption    := 'Documentation: ';
  DocumentationURLLabel.Caption := 'https://github.com/FpcLibs/MdEdit-docs';

  LoadAbout;
  LoadDisclaimer;
  LoadAcknowledgements;

  CloseButton.Caption     := 'Close';
  Acknowledgements.Active := False;
  Disclaimer.Active       := False;
end;


{-------------------------------------------------------------------------------
  CloseButtonClick
 ------------------------------------------------------------------------------}
procedure TFormAbout.CloseButtonClick(Sender: TObject);
begin
  Close;
end;


{-------------------------------------------------------------------------------
  FormClose
 ------------------------------------------------------------------------------}
procedure TFormAbout.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Acknowledgements.Active := False;
  Disclaimer.Active       := False;
end;


{-------------------------------------------------------------------------------
  miVerToClipboardClick
 ------------------------------------------------------------------------------}
procedure TFormAbout.miVerToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := 'Lazarus ' + GetAppVersionString + ' r' + LazarusRevisionStr +
    ' FPC ' + {$I %FPCVERSION%} + ' ' + PlatformLabel.Caption;
end;


{-------------------------------------------------------------------------------
  NotebookPageChanged
 ------------------------------------------------------------------------------}
procedure TFormAbout.NotebookPageChanged(Sender: TObject);
begin
  About.Active            := False;
  Disclaimer.Active       := False;
  Acknowledgements.Active := False;
  if Assigned(About) then
    About.Active := NoteBook.ActivePage = AboutPage;
  if Assigned(Disclaimer) then
    Disclaimer.Active := NoteBook.ActivePage = DisclaimerPage;
  if Assigned(Acknowledgements) then
    Acknowledgements.Active := NoteBook.ActivePage = AcknowledgementsPage;
end;


{-------------------------------------------------------------------------------
  URLLabelMouseDown
 ------------------------------------------------------------------------------}
procedure TFormAbout.URLLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OpenURL(TLabel(Sender).Caption);
end;


{-------------------------------------------------------------------------------
  URLLabelMouseLeave
 ------------------------------------------------------------------------------}
procedure TFormAbout.URLLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor     := crDefault;
end;


{-------------------------------------------------------------------------------
  URLLabelMouseEnter
 ------------------------------------------------------------------------------}
procedure TFormAbout. URLLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor     := crHandPoint;
end;


{-------------------------------------------------------------------------------
  LoadAbout
 ------------------------------------------------------------------------------}
procedure TFormAbout.LoadAbout;
var
  res     : TLResource;
  resName : String = 'About';
  Stream  : TStringStream;
begin
  AboutPage.ControlStyle := AboutPage.ControlStyle - [csOpaque];
  About         := TScrollingText.Create(AboutPage);
  About.Name    := resName;
  About.Parent  := AboutPage;
  About.Align   := alClient;
  About.OffsetHeight := 50;

  res := LazarusResources.Find(resName);
  if Not (res = nil) then
  begin
    Stream := TStringStream.Create(res.Value);
    About.Lines.LoadFromStream(Stream);
  end
  else
  About.Lines.Text := resName;
end;


{-------------------------------------------------------------------------------
  LoadDisclaimer
 ------------------------------------------------------------------------------}
procedure TFormAbout.LoadDisclaimer;
var
  res     : TLResource;
  resName : String = 'Disclaimer';
  Stream  : TStringStream;
begin
  DisclaimerPage.ControlStyle := DisclaimerPage.ControlStyle - [csOpaque];
  Disclaimer         := TScrollingText.Create(DisclaimerPage);
  Disclaimer.Name    := resName;
  Disclaimer.Parent  := DisclaimerPage;
  Disclaimer.Align   := alClient;

  res := LazarusResources.Find(resName);
  if Not (res = nil) then
  begin
    Stream := TStringStream.Create(res.Value);
    Disclaimer.Lines.LoadFromStream(Stream);
  end
  else
    Disclaimer.Lines.Text := resName;
end;


{-------------------------------------------------------------------------------
  LoadAcknowledgements
 ------------------------------------------------------------------------------}
procedure TFormAbout.LoadAcknowledgements;
var
  res     : TLResource;
  resName : String = 'Acknowledgements';
  Stream  : TStringStream;
begin
  Acknowledgements         := TScrollingText.Create(AcknowledgementsPage);
  Acknowledgements.Name    := resName;
  Acknowledgements.Parent  := AcknowledgementsPage;
  Acknowledgements.Align   := alClient;

  res := LazarusResources.Find(resName);
  if Not (res = nil) then
  begin
    Stream := TStringStream.Create(res.Value);
    Acknowledgements.Lines.LoadFromStream(Stream);
  end
  else
    Acknowledgements.Lines.Text := resName;
end;



{ TScrollingText }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  OnPaint := @DrawScrollingText;
  FLines := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:=@DoTimer;
  FTimer.Interval:=30;
  FBuffer := TBitmap.Create;

  FStepSize := 1;
  FStartLine := 0;
  FOffset := -1;
  FOffsetHeight := 0;
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TScrollingText.Destroy;
begin
  FLines.Free;
  FTimer.Free;
  FBuffer.Free;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  SetActive
 ------------------------------------------------------------------------------}
procedure TScrollingText.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
    Init;
  FTimer.Enabled:=Active;
end;


{-------------------------------------------------------------------------------
  Init
 ------------------------------------------------------------------------------}
procedure TScrollingText.Init;
begin
  FBuffer.Width  := Width;
  FBuffer.Height := Height - FOffsetHeight;
  FLineHeight    := FBuffer.Canvas.TextHeight('X');
  FNumLines      := FBuffer.Height div FLineHeight;

  if FOffset = -1 then
    FOffset := round(Double(FBuffer.Height) / 3);

  with FBuffer.Canvas do
  begin
    Brush.Color := clWindow; // clAppWorkspace;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, FBuffer.Height);
  end;
end;


{-------------------------------------------------------------------------------
  DrawScrollingText
 ------------------------------------------------------------------------------}
procedure TScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0,FBuffer);
end;


{-------------------------------------------------------------------------------
  DoTimer
 ------------------------------------------------------------------------------}
procedure TScrollingText.DoTimer(Sender: TObject);
var
  w: integer;
  s: string;
  i: integer;
begin
  if not Active then
    Exit;

  Dec(FOffset, FStepSize);

  if FOffSet < 0 then
    FStartLine := -FOffset div FLineHeight
  else
    FStartLine := 0;

  FEndLine := FStartLine + FNumLines + 1;
  if FEndLine > FLines.Count - 1 then
    FEndLine := FLines.Count - 1;

  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  for i := FEndLine downto FStartLine do
  begin
    s := Trim(FLines[i]);

    //reset buffer font
    FBuffer.Canvas.Font.Style := [];
    FBuffer.Canvas.Font.Color := clWindowText;

    //skip empty lines
    if Length(s) > 0 then
    begin
      //check for bold format token
      if s[1] = '#' then
      begin
        s := copy(s, 2, Length(s) - 1);
        FBuffer.Canvas.Font.Style := [fsBold];
      end
      else
      begin
        //check for url
        if Pos('http://', s) = 1 then
        begin
          if i = FActiveLine then
          begin
            FBuffer.Canvas.Font.Style := [fsUnderline];
            FBuffer.Canvas.Font.Color := clRed;
          end
          else
            FBuffer.Canvas.Font.Color := clBlue;
         end;
        if Pos('https://', s) = 1 then
        begin
          if i = FActiveLine then
          begin
            FBuffer.Canvas.Font.Style := [fsUnderline];
            FBuffer.Canvas.Font.Color := clRed;
          end
          else
            FBuffer.Canvas.Font.Color := clBlue;
         end;
      end;

      w := FBuffer.Canvas.TextWidth(s);
      FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
    end;
  end;
  if FStartLine > FLines.Count - 1 then
    FOffset := FBuffer.Height;
  Invalidate;
end;


{-------------------------------------------------------------------------------
  ActiveLineIsURL
 ------------------------------------------------------------------------------}
function TScrollingText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < FLines.Count) then
  begin
    Result := Pos('http://',  FLines[FActiveLine]) = 1;
    if Not Result then
      Result := Pos('https://', FLines[FActiveLine]) = 1;
  end
  else
    Result := False;
end;


{-------------------------------------------------------------------------------
  DoOnChangeBounds
 ------------------------------------------------------------------------------}
procedure TScrollingText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  Init;
end;


{-------------------------------------------------------------------------------
  MouseDown
 ------------------------------------------------------------------------------}
procedure TScrollingText.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
    OpenURL(FLines[FActiveLine]);
end;


{-------------------------------------------------------------------------------
  MouseMove
 ------------------------------------------------------------------------------}
procedure TScrollingText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  FActiveLine := (Y - FOffset) div FLineHeight;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < FLines.Count) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;



initialization

  lcl_revision_func := @GetAppRevision;

  {$I Ressources/FormAbout/FormAbout.lrs}

end.

