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
unit FormMain;

{$mode objfpc}{$H+}


interface

uses
  {$IFDEF WINDOWS}
    Windows, FormPreview, Messages,
  {$ENDIF}

  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, ActnList, PairSplitter, HtmlView, { AnchorDockPanel, }
  PrintersDlgs, SynEdit, SynEditTypes, SynHighlighterHTML, SynEditMarkupHighAll,
  SynEditHighlighter, DateUtils,
  StdCtrls, Clipbrd, StrUtils, TextStrings,

  // LCL
  LCLProc, LCLIntf, LCLType, LResources, LazFileUtils,

  MarkdownProcessor, MarkdownUtils, Printers, Buttons,

  FormBase, FormDebug, HTMLUn2, HtmlGlobals, FormPrintStatus, Types,
  FormAbout, Language,

  AppSettings, CnfSpellFrameDictionarys, FormSpellCheckDlg,
  CnfLocalizeFrameLng, CnfViewerFrameStyleViewer, CnfExportFrameStylePdf,
  CnfViewerFrameStylePrint, CnfExportFrameStyleHtml,
  CnfViewerFrameStylePrintHF, CnfExportFrameStylePdfHF,
  CnfSynedit, CnfSyneditPreview,

  SynSpellcheckHighlighter, SynSpellToken, SynHighlighterMd, SynEditMarkup,
  CheckBoxThemed, SynSpellMarkup;

{$IFDEF WINDOWS}
const
   ID_ABOUT = WM_USER + 1;
{$ENDIF}

type

  TBtnEnableEvent  = ^TAction;
  TBtnExecuteEvent = procedure(Sender: TObject) of object;

  TFocusEnableStruct = record

    CanUndo    : TBtnEnableEvent;
    CanRedo    : TBtnEnableEvent;
    CanCopy    : TBtnEnableEvent;
    CanCut     : TBtnEnableEvent;
    CanPaste   : TBtnEnableEvent;
    CanFind    : TBtnEnableEvent;
    CanReplace : TBtnEnableEvent;
  end;

  TFocusExecuteStruct = record

    OnUndo    : TBtnExecuteEvent;
    OnRedo    : TBtnExecuteEvent;
    OnCopy    : TBtnExecuteEvent;
    OnCut     : TBtnExecuteEvent;
    OnPaste   : TBtnExecuteEvent;
    OnFind    : TBtnExecuteEvent;
    OnReplace : TBtnExecuteEvent;

  end;

  TOffsetStruct = record

    MdView    : Double;
    ViewMd    : Double;
    MdHtml    : Double;
    HtmlMd    : Double;
    ViewHt    : Double;
    HtView    : Double;
  end;



  { TMyFindDialog }

  TMyFindDialog = class(TFindDialog)

    public

      procedure GetChildProc( Child: TComponent);
      procedure InitColors;

  end;


  { TFormMain }

  TFormMain = class(TFormBase)

    ActionOpenAgain: TAction;
    ActionDebug: TAction;
    ActionStrgCopy: TAction;
    ActionDoFind: TAction;
    ActionDumy: TAction;
    ButtonOffset: TButton;
    CheckBoxOffset: TCheckBoxThemed;
    ImageListBookmarks: TImageList;
    ImageListMenuBars: TImageList;
    ActionListApp: TActionList;
    ActionOpen: TAction;
    ActionFind: TAction;
    ActionCopy: TAction;
    ActionCut: TAction;
    ActionPaste: TAction;
    ActionPropEditor: TAction;
    ActionPropStyle: TAction;
    ActionSpelling: TAction;
    ActionReplace: TAction;
    ActionThesaurus: TAction;
    ActionExportPdf: TAction;
    ActionSave: TAction;
    ActionExportHtml: TAction;
    ActionHelpMd: TAction;
    ActionHelpEditor: TAction;
    ActionHelpAbout: TAction;
    ActionSaveAs: TAction;
    ActionPrinter: TAction;
    ActionPrintPreview: TAction;
    ActionPrint: TAction;
    ActionClose: TAction;
    ActionUndo: TAction;
    ActionRedo: TAction;
    ActionNew: TAction;

    MainMenuApp: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItemOpenFile0: TMenuItem;
    MenuItemOpenAgain: TMenuItem;
    MenuItemPopupInsert: TMenuItem;
    MenuItemPopupCut: TMenuItem;
    MenuItemPopupCopy: TMenuItem;
    MenuItemPopupNl1: TMenuItem;
    MenuItemPopupSpell9: TMenuItem;
    MenuItemPopupSpell8: TMenuItem;
    MenuItemPopupSpell7: TMenuItem;
    MenuItemPopupSpell6: TMenuItem;
    MenuItemPopupSpell5: TMenuItem;
    MenuItemPopupSpell4: TMenuItem;
    MenuItemPopupSpell3: TMenuItem;
    MenuItemPopupSpell2: TMenuItem;
    MenuItemPopupSpell1: TMenuItem;
    MenuItemDebug: TMenuItem;

    MenuItemCopy: TMenuItem;
    MenuItemSearch: TMenuItem;
    MenuItemSplitt4: TMenuItem;
    MenuItemRedo: TMenuItem;
    MenuItemUndo: TMenuItem;
    MenuItemPropStyle: TMenuItem;
    MenuItemSplitt5: TMenuItem;
    MenuItemSplitt1: TMenuItem;
    MenuItemSplitt2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuItemSplitt6: TMenuItem;
    MenuItemSplitt7: TMenuItem;
    MenuItemSplitt3: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItemPrintPriview: TMenuItem;
    MenuItemPrinter: TMenuItem;
    MenuItemHelpAbout: TMenuItem;
    MenuItemHelpEdit: TMenuItem;
    MenuItemHelpMd: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExpPDF: TMenuItem;
    MenuItemExpHTML: TMenuItem;
    MenuItemSpell: TMenuItem;
    MenuItemThes: TMenuItem;
    MenuItemProp: TMenuItem;
    MenuItemPropEditor: TMenuItem;
    MenuItemReplace: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemSaveAs: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemSave: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItemEdit: TMenuItem;
    MenuItemNew: TMenuItem;

    CoolBarApp: TCoolBar;
    PageControlView: TPageControl;
    PageControlApp: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PopupMenu1: TPopupMenu;
    SaveDialogHtml: TSaveDialog;
    SaveDialogPdf: TSaveDialog;
    SynEditMd: TSynEdit;
    SynEditHtml: TSynEdit;
    SynHTMLSynHtml: TSynHTMLSyn;
    TabSheet1: TTabSheet;
    TabSheetView: TTabSheet;
    TabSheetHtml: TTabSheet;
    TimerSynEditMd: TTimer;

    ToolBarTools: TToolBar;
    ToolBarFile: TToolBar;
    ToolBarEdit: TToolBar;
    ToolBarSettings: TToolBar;

    ToolButtonNew: TToolButton;
    ToolButtonCut: TToolButton;
    ToolButtonPaste: TToolButton;
    ToolButtonSearch: TToolButton;
    ToolButtonSplitt3: TToolButton;
    ToolButtonFind: TToolButton;
    ToolButtonReplace: TToolButton;
    ToolButtonSpelling: TToolButton;
    ToolButtonThesaurus: TToolButton;
    ToolButtonSplitt4: TToolButton;
    ToolButtonExpPDF: TToolButton;
    ToolButtonExpHTML: TToolButton;
    ToolButtonSplitt1: TToolButton;
    ToolButtonSettingsEdit: TToolButton;
    ToolButtonSrttingsStyle: TToolButton;
    ToolButtonSplitt5: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSaveAs: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolButtonRedo: TToolButton;
    ToolButtonSplitt2: TToolButton;
    ToolButtonCopy: TToolButton;

    FindDialogApp: TFindDialog;
    ReplaceDialogApp: TReplaceDialog;
    OpenDialogApp1: TOpenDialog;
    PageSetupDialogApp: TPageSetupDialog;
    PrintDialogApp: TPrintDialog;
    PrinterSetupDialogApp: TPrinterSetupDialog;
    SaveDialogApp1: TSaveDialog;
    StatusBarApp: TStatusBar;

    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionCutExecute(Sender: TObject);
    procedure ActionDebugExecute(Sender: TObject);
    procedure ActionDoFindExecute(Sender: TObject);
    procedure ActionExportHtmlExecute(Sender: TObject);
    procedure ActionExportPdfExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionHelpEditorExecute(Sender: TObject);
    procedure ActionHelpMdExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenAgainExecute(Sender: TObject);
    procedure ActionOpenAgainFileExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionPrinterExecute(Sender: TObject);
    procedure ActionPrintExecute(Sender: TObject);
    procedure ActionPrintPreviewExecute(Sender: TObject);
    procedure ActionPropEditorExecute(Sender: TObject);
    procedure ActionPropStyleExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionSpellingExecute(Sender: TObject);
    procedure ActionStrgCopyExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure ButtonOffsetClick(Sender: TObject);
    procedure CheckBoxOffsetChange(Sender: TObject);
    procedure CoolBarAppResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure MenuItemPopupSpell1Click(Sender: TObject);
    procedure MenuItemPopupSpell2Click(Sender: TObject);
    procedure MenuItemPopupSpell3Click(Sender: TObject);
    procedure MenuItemPopupSpell4Click(Sender: TObject);
    procedure MenuItemPopupSpell5Click(Sender: TObject);
    procedure MenuItemPopupSpell6Click(Sender: TObject);
    procedure MenuItemPopupSpell7Click(Sender: TObject);
    procedure MenuItemPopupSpell8Click(Sender: TObject);
    procedure MenuItemPopupSpell9Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SynEditHtmlEnter(Sender: TObject);
    procedure SynEditMdChange(Sender: TObject);
    procedure SynEditMdChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    procedure SynEditMdEnter(Sender: TObject);
    procedure SynEditMdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SynEditMdPaint(Sender: TObject; {%H-}ACanvas: TCanvas);
    procedure TabSheetHtmlShow(Sender: TObject);
    procedure TabSheetViewShow(Sender: TObject);
    procedure TimerSynEditMdTimer(Sender: TObject);
    procedure SynEditHtmlPaint(Sender: TObject; {%H-}ACanvas: TCanvas);
    procedure HtmlViewerPaint(Sender: TObject);

  private
    SynMarkup: TSynEditMarkupHighlightAllCaret;
    HtmlViewer:   THtmlViewer;
    HtmlPrinter:  THtmlViewer;
    HtmlExporter: THtmlViewer;

    mCoolBarAppHigh : Integer;
    mFlagTimerMd    : Boolean;
    mFlagHtmlShow   : Boolean;
    mFlagModified   : Boolean;
    mFileName       : String;
    mFilePath       : String;
    mFileExtd       : String;
    mNewFileName    : String;
    mNewFilePath    : String;
    mNewFileExtd    : String;
    mFileEmpty      : String;
    mHtmlFileName   : String;
    mHtmlFilePath   : String;
    mHtmlFileExtd   : String;
    mPdfFileName    : String;
    mPdfFilePath    : String;
    mPdfFileExtd    : String;

    mFocusExActive : TFocusExecuteStruct;
    mFocusExDummy  : TFocusExecuteStruct;
    mFocusExMd     : TFocusExecuteStruct;
    mFocusExHtml   : TFocusExecuteStruct;
    mFocusExView   : TFocusExecuteStruct;

    mFocusEnActiveMd   : TFocusEnableStruct;
    mFocusEnActiveHtml : TFocusEnableStruct;
    mFocusEnActiveView : TFocusEnableStruct;
    mFocusEnDummy      : TFocusEnableStruct;
    mFocusEnApp        : TFocusEnableStruct;

    mFlagWholeWord   : Boolean;
    mFlagMatchCase   : Boolean;
    mFlagSearchFount : Boolean;
    mLastSearchText  : AnsiString;

    mFlagFindDialog    : Boolean;
    mFlagReplaceDialog : Boolean;

    mOffset : TOffsetStruct;

    mSpellBgn, mSpellLen, mSpellRow: Integer;
    mAppCnf : TAppSettings;
    mMark   : TSynSpellMarkup;
    mP      : TPoint;
    mSpellSuggests : TStrings;
    mSpellToken    : TSynSpellToken;
    mSynSpellSyn   : TSynCustomHighlighter;
    mSynEditorBridge: TEditorConfigBridge;

    mChangeStamp: Int64;

  public

  private
    {$IFDEF WINDOWS}
    procedure AddAboutItem();
    procedure OnSysAbout(var Msg: TWMSysCommand) ; message WM_SYSCOMMAND;
    {$ENDIF}
    procedure CheckCommandLine();
    procedure SynEditHtmlScroll();
    procedure SynEditMdScroll();
    procedure ShowFileName;
    procedure ResetNewFile();
    procedure HtmlViewShow();

    function DoCheckSave(): TModalResult;
    function DoSaveAs():    TModalResult;
    function DoSaveAsDlg(): Boolean;
    function DoSaveMgr():   TModalResult;
    function DoSave():      TModalResult;
    function DoSaveFile():  Boolean;
    function DoOpen():      Boolean;
    function DoLoad():      Boolean;

    procedure InitFocus();
    procedure BtnExecuteEventDummy(Sender: TObject);
    procedure BtnExecuteMdUndo(Sender: TObject);
    procedure BtnExecuteMdRedo(Sender: TObject);
    procedure BtnExecuteHtmlUndo(Sender: TObject);
    procedure BtnExecuteHtmlRedo(Sender: TObject);
    procedure BtnExecuteMdCopy(Sender: TObject);
    procedure BtnExecuteHtmlCopy(Sender: TObject);
    procedure BtnExecuteViewCopy(Sender: TObject);
    procedure BtnExecuteMdCut(Sender: TObject);
    procedure BtnExecuteHtmlCut(Sender: TObject);
    procedure BtnExecuteViewCut(Sender: TObject);
    procedure BtnExecuteMdPaste(Sender: TObject);
    procedure BtnExecuteHtmlPaste(Sender: TObject);
    procedure BtnExecuteViewPaste(Sender: TObject);
    procedure BtnExecuteMdFind(Sender: TObject);
    procedure BtnExecuteHtmlFind(Sender: TObject);
    procedure BtnExecuteViewFind(Sender: TObject);
    procedure BtnExecuteMdReplace(Sender: TObject);
    procedure BtnExecuteHtmlReplace(Sender: TObject);
    procedure BtnExecuteViewReplace(Sender: TObject);

    procedure FocusMdFindReplace();
    procedure FocusHtmlFindReplace();
    procedure FocusViewFindReplace();
    procedure OnMdFind(Sender: TObject);
    procedure OnHtmlFind(Sender: TObject);
    procedure OnViewFind(Sender: TObject);
    procedure OnFindDialogShow(Sender: TObject);
    procedure OnFindDialogClose(Sender: TObject);
    procedure OnReplaceDialogShow(Sender: TObject);
    procedure OnReplaceDialogClose(Sender: TObject);
    procedure OnMdReplaceFind(Sender: TObject);
    procedure OnMdReplace(Sender: TObject);
    procedure OnHtmlReplaceFind(Sender: TObject);
    procedure OnHtmlReplace(Sender: TObject);
    procedure OnViewReplaceFind(Sender: TObject);
    procedure OnViewReplace(Sender: TObject);
    procedure HtmlToPdf(Sender: TObject);

    procedure DoChangeMd(Sender: TObject);
    procedure Spell1SynEditReplace(pos: Integer);
    procedure DoSettingsNotify(Sender: TObject);
    procedure ActionSetHighTxtExecute(Sender: TObject);

    procedure OnHtmlViewerEnter(Sender: TObject);
    procedure OnHtmlViewerImageRequest(Sender: TObject; const SRC: ThtString; var Stream: TStream);
    procedure OnHtmlViewerFooter  (Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure OnHtmlViewerHeader  (Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure OnHtmlPrinterFooter (Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure OnHtmlPrinterHeader (Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure OnHtmlExporterFooter(Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure OnHtmlExporterHeader(Sender: TObject; HFViewer: THtmlViewer; NumPage: Integer; {%H-}LastPage: Boolean; var {%H-}XL, {%H-}XR: Integer; var {%H-}StopPrinting: Boolean);
    procedure ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
    procedure DlgAboutShow;
    procedure SettingsRecentFilesAdd;

    procedure HtmlViewerInit();
    procedure HtmlPrinterInit();
    procedure HtmlExporterInit();

    procedure HtmlPrinterShow;
    procedure OnReplaceLng();

  public

  end;


var
  OFormMain : TFormMain;
  MStream   : TMemoryStream = nil;
  md        : TMarkdownProcessor = nil;

  {HTML for Html-Viewer header and footer}
  ViewerHead: ThtString = '';
  ViewerFoot: ThtString = '';

  {HTML for Html-export header and footer}
  ExportHtmlHead: ThtString = '';
  ExportHtmlFoot: ThtString = '';

  {HTML for print header and footer for pages even odd}
  PrintEvenHead: ThtString = '';
  PrintCss:      ThtString = '';
  PrintEvenFoot: ThtString = '';
  PrintOddHead:  ThtString = '';
  PrintOddFoot:  ThtString = '';

  {HTML for Pdf-export header and footer for pages even odd}
  ExportPdfCss:      ThtString = '';
  ExportPdfEvenHead: ThtString = '';
  ExportPdfEvenFoot: ThtString = '';
  ExportPdfOddHead:  ThtString = '';
  ExportPdfOddFoot:  ThtString = '';

  HelpDocMdURL: String = 'https://github.com/FpcLibs/MdEdit-docs';
  HelpDocEditorURL: String = 'https://github.com/FpcLibs/MdEdit';



implementation

{$R *.lfm}


{-------------------------------------------------------------------------------
  IsDarkTheme : is based on the GUI colors
 ------------------------------------------------------------------------------}
function IsDarkTheme: boolean;
const
  cMax = $A0;
var
  N: TColor;
begin
  N:= ColorToRGB(clWindow);
  Result:= (Red(N)<cMax) and (Green(N)<cMax) and (Blue(N)<cMax);
end;


{-------------------------------------------------------------------------------
  ReplaceStr
 ------------------------------------------------------------------------------}
function ReplaceStr(const S, FromStr, ToStr: String): String;
var
  I: integer;
begin
  Result := S;
  I := Pos(FromStr, S);
  if I > 0 then
  begin
    Delete(Result, I, Length(FromStr));
    Insert(ToStr, Result, I);
  end;
end;



{ TMyFindDialog }

{-------------------------------------------------------------------------------
  InitColors
 ------------------------------------------------------------------------------}
procedure TMyFindDialog.InitColors;
begin
  if not Assigned(FFindForm) then
    FFindForm := CreateForm();

  if FFindForm = Nil then exit;
  FFindForm.Color := clForm;
  FFindForm.GetChildren(@GetChildProc, FFindForm);
 end;


{-------------------------------------------------------------------------------
  GetChildProc
 ------------------------------------------------------------------------------}
procedure TMyFindDialog.GetChildProc( Child: TComponent);
begin
  if (TClass(Child) =      TPanel) then TPanel(Child).Color := clForm;
  if (TClass(Child) = TRadioGroup) then TPanel(Child).Color := clForm;
end;



{ TFormMain }

{-------------------------------------------------------------------------------
  FormCreate
 ------------------------------------------------------------------------------}
procedure TFormMain.FormCreate(Sender: TObject);
var
  RootPath : UnicodeString;
  mgr: TSynEditMarkupManager;
  cnfPath: String;
begin

  inherited;

  ViewerHead         := ViewerHead;
  ViewerFoot         := ViewerFoot;
  ExportHtmlHead     := ViewerHead;
  ExportHtmlFoot     := ViewerFoot;
  PrintCss           := PrintCss;
  PrintEvenHead      := PrintEvenHead;
  PrintEvenFoot      := PrintEvenHead;
  PrintOddHead       := PrintEvenHead;
  PrintOddFoot       := PrintEvenHead;
  ExportPdfCss       := PrintCss;
  ExportPdfEvenHead  := PrintEvenHead;
  ExportPdfEvenFoot  := PrintEvenHead;
  ExportPdfOddHead   := PrintEvenHead;
  ExportPdfOddFoot   := PrintEvenHead;

  OLng.AddListener(@OnReplaceLng);

  Application.CreateForm( TFormDebug,  OFormDebug);
  OFormDebug.Append:='Bla Bla';

  HtmlViewerInit();

  mChangeStamp := 0;

  OpenDialogApp1.InitialDir := ExtractFilePath(ParamStr(0));

  mAppCnf := TAppSettings.Create(Self);
  mAppCnf.Services.AddService(TContentPreview.Create());
  mAppCnf.Services.AddService(TSyneditPreview.Create());
  mAppCnf.Services.AddService(THighligterPreview.Create());

  cnfPath := GetPrimaryConfigPath();
  mAppCnf.FileName := LazFileUtils.CreateAbsolutePath(OAppName + '.cnf', cnfPath);
  mAppCnf.AddNotify(@DoSettingsNotify);
  mAppCnf.Restore;

  mSynEditorBridge := TEditorConfigBridge.Create(mAppCnf);

  mAppCnf.AddFrame(TFrameDictionarys.Create(Nil));

  mAppCnf.AddFrame(TFrameStyleViewer.Create(Nil));
  mAppCnf.AddFrame(TFrameStyleExportHtml.Create(Nil));
  mAppCnf.AddFrame(TFrameStyleExportPdf.Create(Nil));
  mAppCnf.AddFrame(TFrameStyleExportPdfHF.Create(Nil));
  mAppCnf.AddFrame(TFrameStylePrint.Create(Nil));
  mAppCnf.AddFrame(TFrameStylePrintHF.Create(Nil));

  mAppCnf.AddFrame(TFrameCnfLocalizeLng.Create(Nil));

  OFormSpellCheckDlg := TFormSpellCheckDlg.Create(Self);
  OFormSpellCheckDlg.Visible := False;
  OFormSpellCheckDlg.Settings := mAppCnf;

  HtmlViewer.OnPaint := @HtmlViewerPaint;

  InitFocus();

  md := TMarkdownProcessor.createDialect(mdCommonMark);
  md.UnSafe := false;

  mCoolBarAppHigh  := CoolBarApp.Height;

  RootPath := UnicodeString(GetTempDir);

  // Start highlighting of equal words
  SynMarkup := TSynEditMarkupHighlightAllCaret(
    SynEditMd.MarkupByClass[TSynEditMarkupHighlightAllCaret]);
  SynMarkup.MarkupInfo.Background :=  TColor($EFE0E0);
  SynMarkup.WaitTime := 1500; // time in milliseconds
  SynMarkup.Trim := True;
  SynMarkup.FullWord:= True; // will only match the full word
  SynEditMd.HighlightAllColor.Background := clLtGray;
  SynEditMd.LineHighlightColor.Background:= TColor($E0FFFF);

  mSpellToken := TSynSpellToken.Create;
  mSynSpellSyn := TSynSpellMdSyn.Create(Self);
  SynEditMd.Highlighter := mSynSpellSyn;

  mSpellToken.Clear;
  mSpellToken.Highlighter := mSynSpellSyn;

  mMark := TSynSpellMarkup.Create(SynEditMd);
  mMark.SpellToken := mSpellToken;
  mgr   := SynEditMd.MarkupManager;
  mgr.AddMarkUp(mMark);
  mSpellSuggests := TTextStrings.Create;

  ActionSetHighTxtExecute(Self);

  SynEditMd.OnChange := @DoChangeMd;

  HtmlViewer.Clear;
  HtmlViewer.DefBackground  := clWhite;
  HtmlViewer.DefFontColor   := clBlack;
  HtmlViewer.DefFontName    := 'Helvetica';
  HtmlViewer.DefFontSize    := 10;
  HtmlViewer.DefPreFontName := 'Lucida Console';
  HtmlViewer.DefPreFontName := 'Courier';
  HtmlViewer.ServerRoot     := RootPath;
//  HtmlViewer.OnHotSpotTargetClick:=@HtmlViewerHotSpotTargetClick;
//  HtmlViewer.OnHotSpotClick:=@HtmlViewerHotSpotClick;
  HtmlViewer.OnImageRequest := @OnHtmlViewerImageRequest;

  MStream := TMemoryStream.Create;

  ActionNewExecute(nil);
  HtmlViewer.Repaint;
  SynEditHtml.Text := md.process(SynEditMd.Text);
  HtmlViewShow();
  TimerSynEditMd.Enabled := True;
  mFlagTimerMd  := False;
  mFlagHtmlShow := False;
  mFlagModified := False;
  mFileEmpty    := 'Markdown';
  ShowFileName;

  mHtmlFilePath := '';
  mHtmlFileName := '';
  mHtmlFileExtd := '.html';

  FindDialogApp.CloseDialog;
  ReplaceDialogApp.CloseDialog;
  mLastSearchText := '';

  CheckBoxOffset.Checked := false;
  CheckBoxOffsetChange(self);

  CheckCommandLine();

  DoSettingsNotify(Self);

  // Hack unti Lazarus 2.0.8: Set correct DarkMode Backround Colors
  TMyFindDialog(FindDialogApp).InitColors;
  TMyFindDialog(TFindDialog(ReplaceDialogApp)).InitColors;


  ButtonOffset.BringToFront;
  CheckBoxOffset.BringToFront;

   {$IFDEF WINDOWS}
  AddAboutItem();
  {$ENDIF}
end;


{-------------------------------------------------------------------------------
  FormDestroy
 ------------------------------------------------------------------------------}
procedure TFormMain.FormDestroy(Sender: TObject);
begin
   if assigned(md) then md.Free;

   with mAppCnf do begin
     Write('Position/Left',   Left);
     Write('Position/Top',    Top);
     Write('Position/Width',  Width);
     Write('Position/Height', Height);

     Write('Position/Splitter', PairSplitter1.Position);
   end;
   mAppCnf.Save();
end;


{-------------------------------------------------------------------------------
  HtmlViewerInit
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlViewerInit();
begin
  HtmlViewer := THTMLViewer.Create(TabSheetView);
  with HtmlViewer do
  begin
    Parent := TabSheetView;
    Left := 0;
    Height := 499;
    Top := 0;
    Width := 380;
    Align := alClient;
    TabOrder := 0;
    BorderStyle := htSingle;
    HistoryMaxCount := 0;
    NoSelect := False;
    PrintMarginBottom := 2;
    PrintMarginLeft := 2;
    PrintMarginRight := 2;
    PrintMarginTop := 2;
    PrintScale := 1;
    OnEnter :=  @OnHtmlViewerEnter;
    OnImageRequest := @OnHtmlViewerImageRequest;
    OnPrintHTMLHeader := @OnHtmlViewerHeader;
    OnPrintHTMLFooter := @OnHtmlViewerFooter;
    OnProcessing := @ProcessingHandler;
  end;
end;


{-------------------------------------------------------------------------------
  HtmlPrinterInit
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlPrinterInit();
begin
  HtmlPrinter := THTMLViewer.Create(TabSheetView);
  with HtmlPrinter do
  begin
    Parent := TabSheetView;
    Visible := False;
    Left := 0;
    Height := 499;
    Top := 0;
    Width := 380;
    Align := alClient;
    TabOrder := 0;
    BorderStyle := htSingle;
    HistoryMaxCount := 0;
    NoSelect := False;
    PrintMarginBottom := 2;
    PrintMarginLeft := 2;
    PrintMarginRight := 2;
    PrintMarginTop := 2;
    PrintScale := 1;
    OnImageRequest := @OnHtmlViewerImageRequest;
    OnPrintHTMLHeader := @OnHtmlPrinterHeader;
    OnPrintHTMLFooter := @OnHtmlPrinterFooter;
    ServerRoot := UnicodeString(GetTempDir);;
  end;
end;


{-------------------------------------------------------------------------------
  HtmlExporterInit
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlExporterInit();
begin
  HtmlExporter := THTMLViewer.Create(TabSheetView);
  with HtmlExporter do
  begin
    Parent := TabSheetView;
    Visible := False;
    Left := 0;
    Height := 499;
    Top := 0;
    Width := 380;
    Align := alClient;
    TabOrder := 0;
    BorderStyle := htSingle;
    HistoryMaxCount := 0;
    NoSelect := False;
    PrintMarginBottom := 2;
    PrintMarginLeft := 2;
    PrintMarginRight := 2;
    PrintMarginTop := 2;
    PrintScale := 1;
    OnImageRequest := @OnHtmlViewerImageRequest;
    OnPrintHTMLHeader := @OnHtmlExporterHeader;
    OnPrintHTMLFooter := @OnHtmlExporterFooter;
  end;
end;


{-------------------------------------------------------------------------------
  CheckCommandLine
 ------------------------------------------------------------------------------}
procedure TFormMain.CheckCommandLine();
var
  ary : Array of String;
  i: Integer;
begin
   if ParamCount < 1 then exit;

   SetLength(ary, ParamCount);
    for i := 0 to ParamCount-1 do
  begin
    ary[i] := ParamStr(i+1);
  end;
  FormDropFiles(self, ary)
end;


{-------------------------------------------------------------------------------
  FormDropFiles
 ------------------------------------------------------------------------------}
procedure TFormMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i     : Integer;
  nAnz  : Integer;
  sFN   : String;
  sExt  : String;
  flagFile : Boolean;
begin
  sExt := '';
  nAnz := Length(FileNames);
  flagFile := False;

  for i := 0 to nAnz - 1 do
  begin
    sFN  := FileNames[i];
    sExt := ExtractFileExt (sFN);
    sExt := AnsiLowerCase(sExt);

    if (sExt='.md') or (sExt='.ad') or (sExt='.txt') then
    begin
      flagFile := True;
      break;
    end
  end;

  if flagFile = False then
  begin
    ShowMessage(LStr('T_FormMain_MsgErrType', 'Fehler: unbekannter Datentyp!'));
    exit;
  end;

  case DoCheckSave() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;

  mNewFilePath := ExtractFilePath(sFN);
  mNewFileName := ExtractFileName(sFN);
  mNewFileExtd := ExtractFileExt (sExt);

  if Not DoLoad() then
  begin
     mNewFilePath := ''; mNewFileName := ''; mNewFileExtd := '';

     ShowMessage(LStr('T_FormMain_MsgErrOpen', 'Fehler: Die Datei konnte nicht geladen werden!'));
     exit;
  end;
  mFlagModified := false;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  FormShow
 ------------------------------------------------------------------------------}
procedure TFormMain.FormShow(Sender: TObject);
begin
  OFormMain.BeginFormUpdate;
  with mAppCnf do begin
    Left   := Read('Position/Left',   Left);
    Top    := Read('Position/Top',    Top);
    Width  := Read('Position/Width',  Width);
    Height := Read('Position/Height', Height);

    PairSplitter1.Position := Read('Position/Splitter', PairSplitter1.Position);
  end;
  DoSettingsNotify(Self);

  OFormMain.EndFormUpdate;
  OFormMain.Repaint;
end;


{-------------------------------------------------------------------------------
  DoChangeMd
 ------------------------------------------------------------------------------}
procedure TFormMain.DoChangeMd(Sender: TObject);
begin
  mMark.FlagDirty := true;
  mFlagModified   := true;
  ShowFileName;
end;


{-------------------------------------------------------------------------------
  FormCloseQuery
 ------------------------------------------------------------------------------}
procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  case DoCheckSave() of
    mrCancel: begin ResetNewFile(); CanClose := false; end;
  end;
end;


{-------------------------------------------------------------------------------
  Event SynEditMdChange
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdChange(Sender: TObject);
begin
  TimerSynEditMd.Enabled := False;
  TimerSynEditMd.Enabled := True;

  if Length(SynEditMd.Text) > 1 then
   begin
     mFocusEnActiveMd.CanFind^.Enabled := true;
   end
   else mFocusEnActiveMd.CanFind^.Enabled := false;
end;


{-------------------------------------------------------------------------------
  Event SynEditMdChangeUpdating
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdChangeUpdating(ASender: TObject; AnUpdating: Boolean);
begin
  // workaround because SynEdit.OnChange isn't triggerd
  if AnUpdating then mChangeStamp := SynEditMd.ChangeStamp
  else if mChangeStamp <> SynEditMd.ChangeStamp then SynEditMdChange(ASender);
end;


{-------------------------------------------------------------------------------
  Event SynEditMdEnter
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdEnter(Sender: TObject);
begin
  SynEditMdPaint(Sender, Nil);
  FocusMdFindReplace();
end;


{-------------------------------------------------------------------------------
  Event SynEditMdMouseDown
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Button := Button; Shift := Shift;
  mP.x :=  X;
  mP.y :=  Y;
end;


{-------------------------------------------------------------------------------
  Event SynEditMdPaint
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdPaint(Sender: TObject; ACanvas: TCanvas);
begin
  if SynEditMd.Modified and Not mFlagModified then
  begin
     mFlagModified := true;
     ShowFileName;
  end;
  if Not SynEditMd.Focused then exit;

  mFocusExActive := mFocusExMd;

  mFocusEnActiveMd   := mFocusEnApp;
  mFocusEnActiveHtml := mFocusEnDummy;
  mFocusEnActiveView := mFocusEnDummy;

  mFocusEnActiveMd.CanUndo^.Enabled  := SynEditMd.CanUndo;
  mFocusEnActiveMd.CanRedo^.Enabled  := SynEditMd.CanRedo;
  mFocusEnActiveMd.CanCopy^.Enabled  := SynEditMd.SelAvail;
  mFocusEnActiveMd.CanCut^.Enabled   := SynEditMd.SelAvail;
  if (Clipboard.HasFormat(CF_TEXT)) And (SynEditMd.CanPaste) then
  begin
    mFocusEnActiveMd.CanPaste^.Enabled := true;
  end
  else mFocusEnActiveMd.CanPaste^.Enabled := false;

  if Length(SynEditMd.Text) > 2 then
  begin
    mFocusEnActiveMd.CanFind^.Enabled    := true;
    mFocusEnActiveMd.CanReplace^.Enabled := true;
  end
  else
  begin
     mFocusEnActiveMd.CanFind^.Enabled    := false;
     mFocusEnActiveMd.CanReplace^.Enabled := false;
  end;

  if SynEditMd.SelAvail then
  begin
    SynEditHtml.SelStart := 1;
    SynEditHtml.SelEnd   := 1;
    HtmlViewer.SelStart  := 1;
    HtmlViewer.SelLength := 0;
  end;

  SynEditMdScroll();
end;


{-------------------------------------------------------------------------------
  Event SynEditMdScroll
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditMdScroll();
var
  SInfo:  TagScrollInfo;
  vRange: Integer;
  nPos:   Double;
  cPos:   Double;
begin

  SInfo.cbSize := SizeOf(TScrollInfo);
  SInfo.fMask  := SIF_ALL;
  GetScrollInfo(SynEditMd.Handle, SB_VERT, SInfo);

  vRange := HtmlViewer.VScrollBarRange;

  nPos := SInfo.nMax - SInfo.nPage;
  if nPos < 1 then nPos := 1;
  nPos := (SInfo.nPos-1) / nPos;
  if nPos < 0 then nPos := 0;

  HtmlViewer.VScrollBarPosition := Round(vRange * nPos * mOffset.MdView);

  cPos := SynEditHtml.Lines.Count - SynEditHtml.LinesInWindow + 1;
  if cPos < 0 then cPos := 0;
  SynEditHtml.TopLine := Round(cPos * nPos * mOffset.MdHtml);

  UpdateScrollBars;
end;


{-------------------------------------------------------------------------------
  Event HtmlViewerPaint
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditHtmlEnter(Sender: TObject);
begin
  SynEditHtmlPaint(Sender, Nil);
  FocusHtmlFindReplace();
end;


{-------------------------------------------------------------------------------
  Event ProcessingHandler
 ------------------------------------------------------------------------------}
procedure TFormMain.ProcessingHandler(Sender: TObject; ProcessingOn: Boolean);
begin
  if ProcessingOn then
  begin    {disable various buttons and menuitems during processing}

  end;
end;


{-------------------------------------------------------------------------------
  Event SynEditHtmlPaint
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditHtmlPaint(Sender: TObject; ACanvas: TCanvas);
begin
  if Not SynEditHtml.Focused then exit;

  mFocusExActive := mFocusExHtml;

  mFocusEnActiveHtml := mFocusEnApp;
  mFocusEnActiveView := mFocusEnDummy;
  mFocusEnActiveMd   := mFocusEnDummy;

  mFocusEnActiveHtml.CanUndo^.Enabled  := SynEditHtml.CanUndo;
  mFocusEnActiveHtml.CanRedo^.Enabled  := SynEditHtml.CanRedo;
  mFocusEnActiveHtml.CanCopy^.Enabled  := SynEditHtml.SelAvail;
  mFocusEnActiveHtml.CanCut^.Enabled   := SynEditHtml.SelAvail;

  if (Clipboard.HasFormat(CF_TEXT)) And (SynEditHtml.CanPaste) then
  begin
    mFocusEnActiveHtml.CanPaste^.Enabled := true;
  end
  else mFocusEnActiveHtml.CanPaste^.Enabled := false;

  if SynEditHtml.Text.Length > 2 then
  begin
    mFocusEnActiveHtml.CanFind^.Enabled    := true;
    mFocusEnActiveHtml.CanReplace^.Enabled := true;
  end
  else
  begin
     mFocusEnActiveHtml.CanFind^.Enabled    := false;
     mFocusEnActiveHtml.CanReplace^.Enabled := false;
  end;

  if SynEditHtml.SelAvail then
  begin
    SynEditMd.SelStart   := 1;
    SynEditMd.SelEnd     := 1;
    HtmlViewer.SelStart  := 1;
    HtmlViewer.SelLength := 0;
  end;

  SynEditHtmlScroll();
end;


{-------------------------------------------------------------------------------
  Event HtmlViewerScroll
 ------------------------------------------------------------------------------}
procedure TFormMain.SynEditHtmlScroll();
var
  SInfo:  TagScrollInfo;
  vRange: Integer;
  nPos:   Double;
  mPos:   Double;
begin
  SInfo.cbSize := SizeOf(TScrollInfo);
  SInfo.fMask  := SIF_ALL;
  GetScrollInfo(SynEditHtml.Handle, SB_VERT, SInfo);

  vRange := HtmlViewer.VScrollBarRange;

  nPos := SInfo.nMax - SInfo.nPage;
  if nPos < 1 then nPos := 1;
  nPos := (SInfo.nPos-1) / nPos;
  if nPos < 0 then nPos := 0;

  HtmlViewer.VScrollBarPosition := Round(vRange * nPos * mOffset.HtView);

  if Not SynEditMd.Focused then
  begin
    mPos := SynEditMd.Lines.Count - SynEditMd.LinesInWindow + 1;
    if mPos < 0 then mPos := 0;
    SynEditMd.TopLine := Round(mPos * nPos * mOffset.HtmlMd);
  end;
  UpdateScrollBars;
end;


{-------------------------------------------------------------------------------
  Event HtmlViewerEnter
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlViewerEnter(Sender: TObject);
var
  l : Integer;
begin
  if Not HtmlViewer.Focused  then exit;

  FocusViewFindReplace();
  mFocusExActive := mFocusExView;

  mFocusEnActiveView := mFocusEnApp;
  mFocusEnActiveMd   := mFocusEnDummy;
  mFocusEnActiveHtml := mFocusEnDummy;

  mFocusEnActiveView.CanUndo^.Enabled  := false;
  mFocusEnActiveView.CanRedo^.Enabled  := false;
  mFocusEnActiveView.CanCopy^.Enabled  := (HtmlViewer.SelLength <> 0);
  mFocusEnActiveView.CanCut^.Enabled   := false;
  mFocusEnActiveView.CanPaste^.Enabled := false;
  if (Clipboard.HasFormat(CF_TEXT)) And false then
  begin
    mFocusEnActiveView.CanPaste^.Enabled := true;
  end
  else mFocusEnActiveView.CanPaste^.Enabled := false;
  l := Length(HtmlViewer.Text);
  if l > 490 then
  begin
    mFocusEnActiveView.CanFind^.Enabled := true;
  end
  else
  begin
     mFocusEnActiveView.CanFind^.Enabled := false;
  end;
  mFocusEnActiveView.CanReplace^.Enabled := false;

  if (HtmlViewer.SelLength <> 0) then
  begin
    SynEditHtml.SelStart := 1;
    SynEditHtml.SelEnd   := 1;
    SynEditMd.SelStart   := 1;
    SynEditMd.SelEnd     := 1;
  end;
end;


{-------------------------------------------------------------------------------
  Event HtmlViewerPaint
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlViewerPaint(Sender: TObject);
var
  vPos:   Integer;
  vRange: Integer;
  nPos:   Double;
  mPos:   Double;
  cPos:   Double;
begin
  if mFlagHtmlShow then
  begin
     mFlagHtmlShow := false;
     SynEditHtmlScroll();
     exit;
  end;
  if Not HtmlViewer.Focused then exit;

  vPos   := HtmlViewer.VScrollBarPosition;
  vRange := HtmlViewer.VScrollBarRange;

  nPos := vRange;
  if nPos < 1 then nPos := 1;
  nPos := vPos / nPos;
  if nPos < 0 then nPos := 0;

  mPos := SynEditMd.Lines.Count - SynEditMd.LinesInWindow + 1;
  if mPos < 0 then mPos := 0;
  SynEditMd.TopLine := Round(mPos * nPos * mOffset.ViewMd);

  cPos := SynEditHtml.Lines.Count - SynEditHtml.LinesInWindow + 1;
  if cPos < 0 then cPos := 0;
  SynEditHtml.TopLine := Round(cPos * nPos * mOffset.ViewHt);

  OnHtmlViewerEnter(nil);
  UpdateScrollBars;
end;


{-------------------------------------------------------------------------------
  Event TimerSynEditMdTimer
 ------------------------------------------------------------------------------}
procedure TFormMain.TimerSynEditMdTimer(Sender: TObject);
begin
  if (SynEditMd.Modified) and (not mFlagTimerMd) then
  begin
     mFlagTimerMd := true;
     SynEditHtml.Text := md.process(SynEditMd.Text);
     SynEditMd.Modified := false;
     SynEditHtml.Modified := true;
     HtmlViewShow();
     mFlagTimerMd := false;
     SynEditHtml.ClearUndo;
     SynEditMdScroll();
  end;
end;


{-------------------------------------------------------------------------------
  Event TabSheetHtmlShow
 ------------------------------------------------------------------------------}
procedure TFormMain.TabSheetHtmlShow(Sender: TObject);
begin
  mFocusExActive  := mFocusExHtml;
  mCoolBarAppHigh := CoolBarApp.Height;
  FocusHtmlFindReplace();
  if SynEditHtml.IsVisible then SynEditHtml.SetFocus;
end;


{-------------------------------------------------------------------------------
  Event TabSheetViewShow
 ------------------------------------------------------------------------------}
procedure TFormMain.TabSheetViewShow(Sender: TObject);
begin
  mFocusExActive := mFocusExView;
  HtmlViewShow();

  FocusViewFindReplace();
  if mFlagReplaceDialog then BtnExecuteViewReplace(Self);
end;


{-------------------------------------------------------------------------------
  Event HtmlViewerImageRequest
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlViewerImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
var
  Filename : String;
  uri      : String;
begin
  Stream := nil;
  uri := String(HtmlViewer.URL);
  uri := ExtractFilePath(uri);
  FileName := IfThen(FileExists(SRC), String(SRC), uri + String(SRC));
  if FileExists(FileName) then
  begin
    MStream.LoadFromFile(FileName);
    Stream := MStream;
  end;
end;


{-------------------------------------------------------------------------------
  Event CoolBarAppResize
 ------------------------------------------------------------------------------}
procedure TFormMain.CoolBarAppResize(Sender: TObject);
begin
  PageControlApp.Top    := CoolBarApp.Height;
  PageControlApp.Height := PageControlApp.Height + mCoolBarAppHigh - CoolBarApp.Height;
  mCoolBarAppHigh       := CoolBarApp.Height;
end;


{-------------------------------------------------------------------------------
  Event ActionNewExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionNewExecute(Sender: TObject);
begin
  case DoCheckSave() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;
  ResetNewFile();
  mFilePath := '';
  mFileName := '';
  mFileExtd := '';

  SynEditMd.ClearAll;
  SynEditMd.ClearUndo;
  TSynMdSyn(mSynSpellSyn).ClearRangeList;
  mFlagModified := false;
  mMark.FlagDirty := true;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  Event ActionOpenAgainExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionOpenAgainExecute(Sender: TObject);
var
  idItem   : Integer;
  count    : Integer;
  NeuesSubmenue : TMenuItem;
  fileName, filePath : string;
begin
  count := mAppCnf.Read('RecentFiles/Items', 0);

  MenuItemOpenAgain.Clear;

  for idItem := 1 To count do
  begin
    fileName := mAppCnf.Read('RecentFiles/Item_' + idItem.ToString + '/Name', '');
    filePath := mAppCnf.Read('RecentFiles/Item_' + idItem.ToString + '/File', '');

    NeuesSubmenue := TMenuItem.Create(Self);
    NeuesSubmenue.Hint    := filePath;
    NeuesSubmenue.Caption := fileName;
    NeuesSubmenue.OnClick := @ActionOpenAgainFileExecute;
    MenuItemOpenAgain.Add(NeuesSubmenue);
  end;
  if count = 0 then
  begin
    NeuesSubmenue := TMenuItem.Create(Self);
    NeuesSubmenue.Caption := ' ';
    NeuesSubmenue.Enabled:= False;
    MenuItemOpenAgain.Add(NeuesSubmenue);
  end;
end;


{-------------------------------------------------------------------------------
  Event ActionOpenExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionOpenAgainFileExecute(Sender: TObject);
begin
  case DoCheckSave() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;

  if Not FileExists(TMenuItem(Sender).Hint) then
  begin
    ShowMessage(LStr('T_FormMain_MsgErrOpen', 'Fehler: Die Datei konnte nicht geladen werden!'));
     exit;
  end;

  mNewFileName := ExtractFileName(TMenuItem(Sender).Hint);
  mNewFilePath := ExtractFilePath(TMenuItem(Sender).Hint);

  if Not DoLoad() then
  begin
     mNewFilePath := ''; mNewFileName := '';  mNewFileExtd := '';

     ShowMessage(LStr('T_FormMain_MsgErrOpen', 'Fehler: Die Datei konnte nicht geladen werden!'));
     exit;
  end;
  mFlagModified      := false;
  mMark.FlagDirty    := true;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  Event ActionOpenExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionOpenExecute(Sender: TObject);
begin
  case DoCheckSave() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;

  mNewFilePath := mFilePath;
  mNewFileName := '';

  if Not DoOpen() then
  begin  ResetNewFile(); exit; end;

  if Not DoLoad() then
  begin
     mNewFilePath := ''; mNewFileName := '';  mNewFileExtd := '';

     ShowMessage(LStr('T_FormMain_MsgErrOpen', 'Fehler: Die Datei konnte nicht geladen werden!'));

     exit;
  end;
  mFlagModified      := false;
  mMark.FlagDirty    := true;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  Event ActionSaveAsExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionSaveAsExecute(Sender: TObject);
begin
  case DoSaveAs() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;

  mFlagModified      := false;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  Event ActionSaveExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionSaveExecute(Sender: TObject);
begin
  mNewFilePath := mFilePath;
  mNewFileName := mFileName;

  case DoSaveMgr() of
    mrCancel: begin ResetNewFile(); exit; end;
  end;

  mFlagModified      := false;
  SynEditMd.Modified := true;
  ShowFileName;
  TimerSynEditMdTimer(nil);
end;


{-------------------------------------------------------------------------------
  Event ActionUndoExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionUndoExecute(Sender: TObject);
begin
  mFocusExActive.OnUndo(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionRedoExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionRedoExecute(Sender: TObject);
begin
  mFocusExActive.OnRedo(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionCopyExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionCopyExecute(Sender: TObject);
begin
  mFocusExActive.OnCopy(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionCloseExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;


{-------------------------------------------------------------------------------
  Event ActionCutExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionCutExecute(Sender: TObject);
begin
  mFocusExActive.OnCut(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionDebugExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionDebugExecute(Sender: TObject);
begin
  OFormDebug.Show;
  OFormDebug.Visible := True;
end;


{-------------------------------------------------------------------------------
  Event ActionPasteExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPasteExecute(Sender: TObject);
begin
  mFocusExActive.OnPaste(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionDoFindExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionDoFindExecute(Sender: TObject);
begin
  if FindDialogApp <> Nil then
    if Assigned(FindDialogApp.OnFind) then FindDialogApp.OnFind(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionFindExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionFindExecute(Sender: TObject);
begin
  mFocusExActive.OnFind(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionSpellingExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionSpellingExecute(Sender: TObject);
begin
  OFormSpellCheckDlg.ShowModal;
end;


{-------------------------------------------------------------------------------
  Event ActionStrgCopyExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionStrgCopyExecute(Sender: TObject);
begin
  ActionCopyExecute(Sender);
end;


{-------------------------------------------------------------------------------
  Event ActionReplaceExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionReplaceExecute(Sender: TObject);
begin
  mFocusExActive.OnReplace(Sender);
end;


{-------------------------------------------------------------------------------
  Event ButtonOffsetClick
 ------------------------------------------------------------------------------}
procedure TFormMain.ButtonOffsetClick(Sender: TObject);
begin
  CheckBoxOffset.Checked := true;
  CheckBoxOffsetChange(Sender);
end;


{-------------------------------------------------------------------------------
  Event CheckBoxOffsetChange
 ------------------------------------------------------------------------------}
procedure TFormMain.CheckBoxOffsetChange(Sender: TObject);
var
  SInfo:  TagScrollInfo;
  vPos:   Integer;
  vRange: Integer;
  nPos:   Double;
  mPos:   Double;
  cPos:   Double;
  lPos:   Double;
begin
  if CheckBoxOffset.Checked then
  begin
    // HtmlViewer

    vPos   := HtmlViewer.VScrollBarPosition;
    vRange := HtmlViewer.VScrollBarRange;

    nPos := vRange;
    if nPos < 1 then nPos := 1;
    nPos := vPos / nPos;
    if nPos < 0 then nPos := 0;

    mPos := SynEditMd.Lines.Count - SynEditMd.LinesInWindow + 1;
    if mPos < 0 then mPos := 0;
    lPos := SynEditMd.TopLine;
    mOffset.ViewMd := (lPos + 1) / (mPos * nPos + 1);

    cPos := SynEditHtml.Lines.Count - SynEditHtml.LinesInWindow + 1;
    if cPos < 0 then cPos := 0;
    lPos := SynEditHtml.TopLine;
    mOffset.ViewHt := (lPos + 1) / (cPos * nPos + 1);

    // SynEditHtml

    SInfo.cbSize := SizeOf(TScrollInfo);
    SInfo.fMask  := SIF_ALL;
    GetScrollInfo(SynEditHtml.Handle, SB_VERT, SInfo);

    vRange := HtmlViewer.VScrollBarRange;

    nPos := SInfo.nMax - SInfo.nPage;
    if nPos < 1 then nPos := 1;
    nPos := (SInfo.nPos-1) / nPos;
    if nPos < 0 then nPos := 0;
    lPos := HtmlViewer.VScrollBarPosition;
    mOffset.HtView := (lPos + 1) / (vRange * nPos + 1);

    mPos := SynEditMd.Lines.Count - SynEditMd.LinesInWindow + 1;
    if mPos < 0 then mPos := 0;
    lPos := SynEditMd.TopLine;
    mOffset.HtmlMd := (lPos + 1) / (mPos * nPos + 1);

    // SynEditMd

    SInfo.cbSize := SizeOf(TScrollInfo);
    SInfo.fMask  := SIF_ALL;
    GetScrollInfo(SynEditMd.Handle, SB_VERT, SInfo);

    vRange := HtmlViewer.VScrollBarRange;

    nPos := SInfo.nMax - SInfo.nPage;
    if nPos < 1 then nPos := 1;
    nPos := (SInfo.nPos-1) / nPos;
    if nPos < 0 then nPos := 0;
    lPos := HtmlViewer.VScrollBarPosition;
    mOffset.MdView := (lPos + 1) / (vRange * nPos + 1);

    cPos := SynEditHtml.Lines.Count - SynEditHtml.LinesInWindow + 1;
    if cPos < 0 then cPos := 0;
    lPos := SynEditHtml.TopLine;
    mOffset.MdHtml := (lPos + 1) / (cPos * nPos + 1);
  end
  else
  begin
    mOffset.MdHtml := 1.0;
    mOffset.MdView := 1.0;
    mOffset.HtmlMd := 1.0;
    mOffset.HtView := 1.0;
    mOffset.ViewMd := 1.0;
    mOffset.ViewHt := 1.0;
  end;
end;


{-------------------------------------------------------------------------------
  Event ActionExportHtmlExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionExportHtmlExecute(Sender: TObject);
var
  filename : String;
  str      : String;
  index    : Integer;
  fsOut    : TFileStream;
begin
  if FileExists(mHtmlFilePath + mHtmlFileName) then
  begin
    SaveDialogHtml.FileName   := mHtmlFileName;
  end
  else
  begin
    if FileExists(mFilePath + mFileName) then
    begin
      filename := Copy(mFileName,1, length(mFileName) - length(mFileExtd) );
      SaveDialogHtml.FileName   := filename + '.html';
    end;
  end;
  if DirectoryExists(mHtmlFilePath) then
  begin
    SaveDialogHtml.InitialDir := mHtmlFilePath;
  end
  else
  begin
    if DirectoryExists(mFilePath) then
    begin
      SaveDialogHtml.InitialDir := mFilePath;
    end
  end;

  SaveDialogHtml.DefaultExt := mHtmlFileExtd;
  case mHtmlFileExtd of
    '.htm':  Index := 1;
    '.html': Index := 2;
  end;
  SaveDialogHtml.FilterIndex := Index;
  SaveDialogHtml.Options := [ofOverwritePrompt, ofEnableSizing];


  if Not SaveDialogHtml.Execute then Exit;
  filename      := SaveDialogHtml.Filename;
  mHtmlFilePath := ExtractFilePath(filename);
  mHtmlFileName := ExtractFileName(filename);
  mHtmlFileExtd := LowerCase( ExtractFileExt (filename) );

  try

    fsOut := TFileStream.Create(mHtmlFilePath + mHtmlFileName, fmCreate);
    str := String(ExportHtmlHead);
    fsOut.Write(str[1], length(str));
    SynEditHtml.Lines.SaveToStream(fsOut);
    str := String(ExportHtmlFoot);
    fsOut.Write(str[1], length(str));
    fsOut.Free;
  except
    on E: EInOutError do
  end;
end;


{-------------------------------------------------------------------------------
  Event ActionExportPdfExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionExportPdfExecute(Sender: TObject);
var
  filename : String;
  str      : String;
  index    : Integer;
  fsOut    : TFileStream;
begin
  if FileExists(mPdfFilePath + mPdfFileName) then
  begin
    SaveDialogPdf.FileName   := mPdfFileName;
  end
  else
  begin
    if FileExists(mFilePath + mFileName) then
    begin
      filename := Copy(mFileName,1, length(mFileName) - length(mFileExtd) );
      SaveDialogPdf.FileName   := filename + '.pdf';
    end;
  end;
  if DirectoryExists(mPdfFilePath) then
  begin
    SaveDialogPdf.InitialDir := mPdfFilePath;
  end
  else
  begin
    if DirectoryExists(mFilePath) then
    begin
      SaveDialogPdf.InitialDir := mFilePath;
    end
  end;

  SaveDialogPdf.DefaultExt := mPdfFileExtd;
  case mHtmlFileExtd of
    '.pdf':  Index := 1;
  end;
  SaveDialogPdf.FilterIndex := Index;
  SaveDialogPdf.Options := [ofOverwritePrompt, ofEnableSizing];


  if Not SaveDialogPdf.Execute then Exit;
  filename     := SaveDialogPdf.Filename;
  mPdfFilePath := ExtractFilePath(filename);
  mPdfFileName := ExtractFileName(filename);
  mPdfFileExtd := LowerCase( ExtractFileExt (filename) );

  try
    fsOut := TFileStream.Create(mPdfFilePath + mPdfFileName, fmCreate);
    str := '<html>'+LineEnding+String(ExportPdfCss)+LineEnding+'<body>'+LineEnding;
    fsOut.Write(str[1], length(str));
    SynEditHtml.Lines.SaveToStream(fsOut);
    str := '</body></html>';

    fsOut.Write(str[1], length(str));
    fsOut.Free;
  except
    on E: EInOutError do
  end;
end;


{-------------------------------------------------------------------------------
  HtmlPrinterShow
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlPrinterShow;
begin
  HtmlPrinter.URL := UnicodeString(mFilePath + mFileName);
  HtmlPrinter.LoadFromString(
    '<html>'  + LineEnding + PrintCss + LineEnding +
    '<body>'  + LineEnding + UnicodeString(SynEditHtml.Text) +
    '</body>' + LineEnding + '</html>');
end;


{-------------------------------------------------------------------------------
  Event ActionPropPageExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPrintPreviewExecute(Sender: TObject);
{$IFDEF WINDOWS}
var
  pf    : TFormPreview;
  Abort : boolean;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  if HtmlPrinter <> nil then
  begin
    HtmlPrinter.Free;
    HtmlPrinter := Nil;
  end;
  HtmlPrinterInit;

  HtmlPrinterShow;
  Abort := False;

  if HtmlPrinter <> nil then
  pf := TFormPreview.CreateIt(Self, HtmlPrinter, Abort);
  begin
    try
      pf.ShowModal;
    finally
      pf.Free;
      HtmlPrinter.Free;
      HtmlPrinter := Nil;
    end;
  end;
  {$ENDIF}
end;


{-------------------------------------------------------------------------------
  Event ActionPrintExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPrintExecute(Sender: TObject);
begin
  if HtmlPrinter <> nil then
  begin
    HtmlPrinter.Free;
    HtmlPrinter := Nil;
  end;
  HtmlPrinterInit;
  HtmlPrinterShow;

  if HtmlPrinter <> nil then
    PrintWithDialog(Self, PrintDialogApp, HtmlPrinter);
end;


{-------------------------------------------------------------------------------
  Event ActionPrinterExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPrinterExecute(Sender: TObject);
begin
  PrinterSetupDialogApp.Execute;
end;


{-------------------------------------------------------------------------------
  Event ActionPropEditorExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPropEditorExecute(Sender: TObject);
begin
  mAppCnf.Show;
end;


{-------------------------------------------------------------------------------
  Event ActionPropStyleExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionPropStyleExecute(Sender: TObject);
begin
  mAppCnf.Show(7);
end;


{-------------------------------------------------------------------------------
  Event ActionHelpAboutExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  DlgAboutShow;
end;


{-------------------------------------------------------------------------------
  Event ActionHelpEditorExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionHelpEditorExecute(Sender: TObject);
begin
  OpenURL(HelpDocEditorURL);
end;


{-------------------------------------------------------------------------------
  Event ActionHelpMdExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionHelpMdExecute(Sender: TObject);
begin
  OpenURL(HelpDocMdURL);
end;


{-------------------------------------------------------------------------------
  Event Spell1SynEditReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.Spell1SynEditReplace(pos: Integer);
var
  startPoint, endPoint, curserPoint: TPoint;
  str: String;
begin
  if pos >= mSpellSuggests.Count then exit;
  str := mSpellSuggests[pos];

  startPoint.x  := mSpellBgn;
  startPoint.y  := mSpellRow;
  endPoint.x    := mSpellBgn + mSpellLen;
  endPoint.y    := mSpellRow;

  curserPoint.y := mSpellRow;
  curserPoint.x := mSpellBgn + Length(str);
  curserPoint   := SynEditMd.LogicalToPhysicalPos(curserPoint);

  SynEditMd.CaretXY := curserPoint;

  SynEditMd.TextBetweenPoints[startPoint, endPoint] := str;
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell1Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell1Click(Sender: TObject);
begin
  Spell1SynEditReplace(0);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell2Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell2Click(Sender: TObject);
begin
  Spell1SynEditReplace(1);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell3Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell3Click(Sender: TObject);
begin
  Spell1SynEditReplace(2);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell4Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell4Click(Sender: TObject);
begin
  Spell1SynEditReplace(3);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell5Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell5Click(Sender: TObject);
begin
  Spell1SynEditReplace(4);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell6Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell6Click(Sender: TObject);
begin
  Spell1SynEditReplace(5);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell7Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell7Click(Sender: TObject);
begin
  Spell1SynEditReplace(6);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell8Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell8Click(Sender: TObject);
begin
  Spell1SynEditReplace(7);
end;


{-------------------------------------------------------------------------------
  Event MenuItemPopupSpell9Click
 ------------------------------------------------------------------------------}
procedure TFormMain.MenuItemPopupSpell9Click(Sender: TObject);
begin
  Spell1SynEditReplace(8);
end;

{-------------------------------------------------------------------------------
  Event PopupMenu1Popup
 ------------------------------------------------------------------------------}
procedure TFormMain.PopupMenu1Popup(Sender: TObject);
var
  p: TPoint;
  s: String;
  passed: Boolean;
begin
  p := SynEditMd.PixelsToLogicalPos(mP);
  MenuItemPopupSpell1.Caption := p.x.ToString + ' x ' + p.y.ToString;

  MenuItemPopupSpell1.Visible := False;
  MenuItemPopupSpell2.Visible := False;
  MenuItemPopupSpell3.Visible := False;
  MenuItemPopupSpell4.Visible := False;
  MenuItemPopupSpell5.Visible := False;
  MenuItemPopupSpell6.Visible := False;
  MenuItemPopupSpell7.Visible := False;
  MenuItemPopupSpell8.Visible := False;
  MenuItemPopupSpell9.Visible := False;

  MenuItemPopupNl1.Visible:=False;

  if  (p.x <= 0) then exit;

  mSpellBgn := 0; mSpellLen := 0; mSpellRow := p.y; passed := False;
  s := mMark.WrongWordAt(p, mSpellBgn, mSpellLen, passed);

  if ( (mSpellLen = 0) Or (passed) ) then exit;

  mSpellSuggests := TTextStrings.Create;
  mMark.SuggestWords(s, mSpellSuggests);

  MenuItemPopupSpell1.Visible   := True;
  MenuItemPopupNl1.Visible      := True;

  if mSpellSuggests.Count > 0 then
  begin
    MenuItemPopupSpell1.Caption := mSpellSuggests[0];
    MenuItemPopupSpell1.Enabled := True;
  end
  else
  begin
    MenuItemPopupSpell1.Caption :='(Keine Vorschläge)';
    MenuItemPopupSpell1.Enabled :=False;
  end;
  if mSpellSuggests.Count > 1 then
  begin
    MenuItemPopupSpell2.Caption := mSpellSuggests[1];
    MenuItemPopupSpell2.Visible := True;
  end;
  if mSpellSuggests.Count > 2 then
  begin
    MenuItemPopupSpell3.Caption := mSpellSuggests[2];
    MenuItemPopupSpell3.Visible := True;
  end;
  if mSpellSuggests.Count > 3 then
  begin
    MenuItemPopupSpell4.Caption := mSpellSuggests[3];
    MenuItemPopupSpell4.Visible := True;
  end;
  if mSpellSuggests.Count > 4 then
  begin
    MenuItemPopupSpell5.Caption := mSpellSuggests[4];
    MenuItemPopupSpell5.Visible := True;
  end;
  if mSpellSuggests.Count > 5 then
  begin
    MenuItemPopupSpell6.Caption := mSpellSuggests[5];
    MenuItemPopupSpell6.Visible := True;
  end;
  if mSpellSuggests.Count > 6 then
  begin
    MenuItemPopupSpell7.Caption := mSpellSuggests[6];
    MenuItemPopupSpell7.Visible := True;
  end;
  if mSpellSuggests.Count > 7 then
  begin
    MenuItemPopupSpell8.Caption := mSpellSuggests[7];
    MenuItemPopupSpell8.Visible := True;
  end;
  if mSpellSuggests.Count > 8 then
  begin
    MenuItemPopupSpell9.Caption := mSpellSuggests[8];
    MenuItemPopupSpell9.Visible := True;
  end;
end;


{-------------------------------------------------------------------------------
  Event ActionSetHighTxtExecute
 ------------------------------------------------------------------------------}
procedure TFormMain.ActionSetHighTxtExecute(Sender: TObject);
begin

  SynEditMd.Highlighter := Nil;
  if Not (TSynSpellMdSyn = Nil) then
  begin
    mSynSpellSyn.Destroy;
    mSynSpellSyn := Nil;
  end;
  mSynSpellSyn := TSynSpellMdSyn.Create(Self);
  TSynSpellMdSyn(mSynSpellSyn).SpellToken := mSpellToken;

  mSpellToken.Clear;
  mSpellToken.Highlighter := mSynSpellSyn;
  mSpellToken.Attributes(['Text']);
  mSpellToken.Initialize;
  SynEditMd.Highlighter := TSynCustomHighlighter(mSynSpellSyn);

end;


{-------------------------------------------------------------------------------
  Operator mod
 ------------------------------------------------------------------------------}
operator mod(const a, b: double) c: double; inline;
begin
  c:= a - b * Int(a / b);
end;


{-------------------------------------------------------------------------------
  Event OnHtmlViewerHeader
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlViewerHeader(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  if (double(NumPage) mod 2.0) <> 0.0 then
  begin
    S := ReplaceStr(String(PrintOddHead), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end
  else
  begin
    S := ReplaceStr(String(PrintEvenHead), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end;
  HFViewer.LoadFromString(ThtString(S));
end;


{-------------------------------------------------------------------------------
  Event OnHtmlViewerFooter
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlViewerFooter(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  if (double(NumPage) mod 2.0) <> 0.0 then
  begin
    S := ReplaceStr(String(PrintOddFoot), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end
  else
  begin
    S := ReplaceStr(String(PrintEvenFoot), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end;
  HFViewer.LoadFromString(ThtString(S));
end;


{-------------------------------------------------------------------------------
  Event OnHtmlPrinterHeader
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlPrinterHeader(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  if (double(NumPage) mod 2.0) <> 0.0 then
  begin
    S := ReplaceStr(String(PrintOddHead), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end
  else
  begin
    S := ReplaceStr(String(PrintEvenHead), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end;
  HFViewer.LoadFromString(ThtString(S));
end;

{-------------------------------------------------------------------------------
  Event OnHtmlPrinterFooter
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlPrinterFooter(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  if (double(NumPage) mod 2.0) <> 0.0 then
  begin
    S := ReplaceStr(String(PrintOddFoot), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end
  else
  begin
    S := ReplaceStr(String(PrintEvenFoot), '#filename', mFileName);
    S := ReplaceStr(S, '#page', IntToStr(NumPage));
  end;
  HFViewer.LoadFromString(ThtString(S));
end;


{-------------------------------------------------------------------------------
  Event OnHtmlExporterHeader
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlExporterHeader(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  S := ReplaceStr(String(ExportHtmlHead), '#filename', mFileName);
  S := ReplaceStr(S, '#page', IntToStr(NumPage));

  HFViewer.LoadFromString(ThtString(S));
end;

{-------------------------------------------------------------------------------
  Event OnHtmlExporterFooter
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlExporterFooter(Sender: TObject;
  HFViewer: THtmlViewer; NumPage: Integer; LastPage: Boolean; var XL,
  XR: Integer; var StopPrinting: Boolean);
var
  S: String;
begin
  S := ReplaceStr(String(ExportHtmlFoot), '#filename', mFileName);
  S := ReplaceStr(S, '#page', IntToStr(NumPage));

  HFViewer.LoadFromString(ThtString(S));
end;


{-------------------------------------------------------------------------------
  HtmlViewShow
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlViewShow();
begin
   if SynEditHtml.Modified then
   begin
     HtmlViewer.URL := UnicodeString(mFilePath + mFileName);
     HtmlViewer.LoadFromString( LineEnding + ViewerHead + LineEnding +
       UnicodeString(SynEditHtml.Text) + LineEnding +
       ViewerFoot);
     SynEditHtml.Modified := false;
   end;
   mFlagHtmlShow := true;
end;


{-------------------------------------------------------------------------------
  ResetNewFile
 ------------------------------------------------------------------------------}
procedure TFormMain.ResetNewFile();
begin
  mNewFilePath := '';
  mNewFileName := '';
  mNewFileExtd := '';
end;


{-------------------------------------------------------------------------------
  DoSaveAs
 ------------------------------------------------------------------------------}
function TFormMain.DoSaveAs(): TModalResult;
label
  DialogStart;
begin
  Result := mrCancel;

  DialogStart:

  if Not DoSaveAsDlg() then
  begin
    ResetNewFile(); exit;
  end;

  case DoSave() of
    mrYes:    begin  ResetNewFile(); Goto DialogStart; end;
    mrCancel: begin  ResetNewFile(); exit; end;
  end;
  Result := mrYes;
end;


{-------------------------------------------------------------------------------
  DoSaveAsDlg
 ------------------------------------------------------------------------------}
function TFormMain.DoSaveAsDlg(): Boolean;
var
  filename : string;
  index : Integer;
begin
  Result := false;

  if FileExists(mFilePath + mFileName) then
  begin
    SaveDialogApp1.FileName   := mFileName;
  end;
  if DirectoryExists(mFilePath) then
  begin
    SaveDialogApp1.InitialDir := mFilePath;
  end;

  SaveDialogApp1.DefaultExt := mFileExtd;
  case mFileExtd of
    '.md':  Index := 1;
    '.ad':  Index := 2;
    '.txt': Index := 3;
  end;
  SaveDialogApp1.FilterIndex := Index;
  SaveDialogApp1.Options := [ofOverwritePrompt, ofEnableSizing];

  if SaveDialogApp1.Execute then
  begin
    filename     := SaveDialogApp1.Filename;
    mNewFilePath := ExtractFilePath(filename);
    mNewFileName := ExtractFileName(filename);
    mNewFileExtd := LowerCase( ExtractFileExt (filename) );
    Result       := true;
  end
end;


{-------------------------------------------------------------------------------
  DoSaveMgr
 ------------------------------------------------------------------------------}
function TFormMain.DoSaveMgr(): TModalResult;
label
  DialogStart;
begin
  Result := mrCancel;

  if mFileName = '' then
  begin
    DialogStart:

    case DoSaveAs() of
      mrCancel: begin ResetNewFile(); exit; end;
    end;
    Result := mrYes;
    exit;
  end;

  case DoSave() of
    mrYes:    begin ResetNewFile(); Goto DialogStart; end;
    mrCancel: begin ResetNewFile(); exit; end;
  end;
  Result := mrYes;
end;


{-------------------------------------------------------------------------------
  DoCheckSave
 ------------------------------------------------------------------------------}
function TFormMain.DoCheckSave(): TModalResult;
var
  res : TModalResult;
label
  DialogStart;
begin
  Result := mrCancel;

  if mFlagModified = true then
  begin
    res := QuestionDlg(

      LStr('T_FormMain_MsgSaveAsHead',     'Datei speichern unter ...'),
      LStr('T_FormMain_MsgSaveAsBody',     'Die Änderungen sind noch nicht gespeichert.') + LineEnding + LineEnding +
      LStr('T_FormMain_MsgSaveAsQuestion', 'Möchten Sie die Änderungen jetzt speichern?'),
      mtWarning, [
        mrYes,    LStr('T_FormMain_MsgYes', '&Ja'),
        mrNo,     LStr('T_FormMain_MsgNo',  '&Nein'),
        mrCancel, LStr('T_FormMain_MsgCancel',  '&Abbrechen')
      ], 0
    );
    case res of
      mrYes:
        begin
           DialogStart:

           case DoSaveMgr() of
             mrCancel: begin ResetNewFile(); exit; end;
           end;
        end;
      mrCancel:
        begin ResetNewFile(); exit; end;
      mrNo:
        begin Result := mrYes; exit; end;
    end;
  end;
  Result := mrYes;
end;


{-------------------------------------------------------------------------------
  DoSave
 ------------------------------------------------------------------------------}
function TFormMain.DoSave(): TModalResult;
var
  Attributes : Word;
begin
  if FileExists(mNewFilePath + mNewFileName) then
  begin
    Attributes := FileGetAttr(mNewFilePath + mNewFileName);
    if (Attributes and SysUtils.faReadOnly) = faReadOnly then
    begin
      Result := QuestionDlg(

        LStr('T_FormMain_MsgReadOnlyHead',     'Datei speichern'),
        LStr('T_FormMain_MsgReadOnlyBody',     'Die Datei ist schreibgeschützt.') + LineEnding + LineEnding +
        LStr('T_FormMain_MsgReadOnlyQuestion', 'Möchten Sie die Änderungen unter einem anderen Namen speichern?'),
        mtWarning, [
          mrYes, LStr('T_FormMain_MsgYes', '&Ja'),
          mrCancel, LStr('T_FormMain_MsgCancel',  '&Abbrechen')
        ], 0
      );
      exit;
    end;
  end;
  if DoSaveFile then begin Result := mrOK; exit; end;

  Result := QuestionDlg(

    LStr('T_FormMain_MsgSaveErrHead',     'Datei speichern'),
    LStr('T_FormMain_MsgSaveErrBody',     'Fehler beim Speichern der Datei.') + LineEnding + LineEnding +
    LStr('T_FormMain_MsgSaveErrQuestion', 'Möchten Sie die Änderungen unter einem anderen Namen speichern?'),
    mtWarning, [
      mrYes, LStr('T_FormMain_MsgYes', '&Ja'),
      mrCancel, LStr('T_FormMain_MsgCancel',  '&Abbrechen')
    ], 0
  );
end;


{-------------------------------------------------------------------------------
  DoSave
 ------------------------------------------------------------------------------}
function TFormMain.DoSaveFile(): Boolean;
begin
  Result := false;

  Try
    SynEditMd.Lines.SaveToFile(mNewFilePath + mNewFileName);
  except
    exit;
  end;

  mFilePath := mNewFilePath;
  mFileName := mNewFileName;
  mFileExtd := mNewFileExtd;
  mFlagModified := false;
  ShowFileName;
  SettingsRecentFilesAdd;
  Result := true;
end;


{-------------------------------------------------------------------------------
  DoOpen
 ------------------------------------------------------------------------------}
function TFormMain.DoOpen(): Boolean;
var
  filename : string;
  Index    : Integer;
begin
  Result := false;

  SaveDialogApp1.InitialDir := '';

  if DirectoryExists(mFilePath) then
  begin
    SaveDialogApp1.InitialDir := mFilePath;
  end
  else
  begin
    SaveDialogApp1.InitialDir := GetUserDir;
  end;

  SaveDialogApp1.DefaultExt := mFileExtd;
  case mFileExtd of
    '.md':  Index := 1;
    '.ad':  Index := 2;
    '.txt': Index := 3;
  end;

  SaveDialogApp1.FilterIndex := Index;
  SaveDialogApp1.Options := [ofEnableSizing];

  if OpenDialogApp1.Execute then
  begin
    Update;
    filename     := OpenDialogApp1.Filename;
    mNewFilePath := ExtractFilePath(filename);
    mNewFileName := ExtractFileName(filename);
    mNewFileExtd := LowerCase( ExtractFileExt(filename) );

    Result := true;
  end
end;


{-------------------------------------------------------------------------------
  DoLoad
 ------------------------------------------------------------------------------}
function TFormMain.DoLoad(): Boolean;
begin
  Result := false;

  TSynMdSyn(mSynSpellSyn).ClearRangeList;
  Try
    SynEditMd.Lines.LoadFromFile(mNewFilePath + mNewFileName);
  except
    exit;
  end;
  mFilePath := mNewFilePath;
  mFileName := mNewFileName;
  mFileExtd := mNewFileExtd;
  SettingsRecentFilesAdd;
  Result := true;
end;


{-------------------------------------------------------------------------------
  ShowFileName
 ------------------------------------------------------------------------------}
procedure TFormMain.ShowFileName;
var
  fn : String;
begin
  fn := '';
  if mFlagModified then fn := '*';
  fn := fn + mFileName;
  if mFileName = '' then fn := fn + mFileEmpty;
  TabSheet1.Caption := fn;
  Self.Caption := fn + ' - MdEdit';
end;


{-------------------------------------------------------------------------------
  Event FocusMdFindReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.FocusMdFindReplace();
var
  sOpt : TFindOptions;
begin
  if SynEditMd.SelText <> '' then
  begin
    mLastSearchText := SynEditMd.SelText;
  end;
  sOpt := [frHideUpDown, frHideEntireScope];

  if FindDialogApp = Nil then
  begin
    FindDialogApp:=TFindDialog.Create(Self);
    TMyFindDialog(FindDialogApp).InitColors;
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);

  FindDialogApp.Options  := sOpt;
  FindDialogApp.FindText := mLastSearchText;
  FindDialogApp.Options  := sOpt;
  FindDialogApp.OnShow   := @OnFindDialogShow;
  FindDialogApp.OnClose  := @OnFindDialogClose;
  FindDialogApp.OnFind   := @OnMdFind;

  mFlagSearchFount := False;
  if SynEditMd.SelText <> '' then
  begin
    mLastSearchText := SynEditMd.SelText;
  end;
  sOpt := [frHideUpDown, frHideEntireScope, frHidePromptOnReplace];
  if ReplaceDialogApp = Nil then
  begin
    ReplaceDialogApp := TReplaceDialog.Create(Self);
    TMyFindDialog(TFindDialog(ReplaceDialogApp)).InitColors;
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);
  if (Not mFlagReplaceDialog) and (SynEditMd.SelText <> '') then mFlagSearchFount := True;
  ReplaceDialogApp.Options   := sOpt;
  ReplaceDialogApp.FindText  := mLastSearchText;
  ReplaceDialogApp.OnShow    := @OnReplaceDialogShow;
  ReplaceDialogApp.OnClose   := @OnReplaceDialogClose;
  ReplaceDialogApp.OnFind    := @OnMdReplaceFind;
  ReplaceDialogApp.OnReplace := @OnMdReplace;

  if mFlagFindDialog then FindDialogApp.Execute;
  if mFlagReplaceDialog then ReplaceDialogApp.Execute;
end;


{-------------------------------------------------------------------------------
  Event FocusHtmlFindReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.FocusHtmlFindReplace();
var
  sOpt : TFindOptions;
begin
  if SynEditHtml.SelText <> '' then
  begin
    mLastSearchText := SynEditHtml.SelText;
  end;
  sOpt := [frHideUpDown, frHideEntireScope];

  if FindDialogApp = Nil then
  begin
    FindDialogApp:=TFindDialog.Create(Self);
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);

  TMyFindDialog(FindDialogApp).InitColors;
  FindDialogApp.Options  := sOpt;
  FindDialogApp.FindText := mLastSearchText;
  FindDialogApp.Options  := sOpt;
  FindDialogApp.OnShow   := @OnFindDialogShow;
  FindDialogApp.OnClose  := @OnFindDialogClose;
  FindDialogApp.OnFind   := @OnHtmlFind;

  mFlagSearchFount := False;
  if SynEditHtml.SelText <> '' then
  begin
    mLastSearchText := SynEditHtml.SelText;
  end;
  sOpt := [frHideUpDown, frHideEntireScope, frHidePromptOnReplace];
  if ReplaceDialogApp = Nil then
  begin
    ReplaceDialogApp := TReplaceDialog.Create(Self);
    TMyFindDialog(TFindDialog(ReplaceDialogApp)).InitColors;
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);
  if (Not mFlagReplaceDialog) and (SynEditHtml.SelText <> '') then mFlagSearchFount := True;
  ReplaceDialogApp.Options   := sOpt;
  ReplaceDialogApp.FindText  := mLastSearchText;
  ReplaceDialogApp.OnShow    := @OnReplaceDialogShow;
  ReplaceDialogApp.OnClose   := @OnReplaceDialogClose;
  ReplaceDialogApp.OnFind    := @OnHtmlReplaceFind;
  ReplaceDialogApp.OnReplace := @OnHtmlReplace;

  if mFlagFindDialog then FindDialogApp.Execute;
  if mFlagReplaceDialog then ReplaceDialogApp.Execute;
end;


{-------------------------------------------------------------------------------
  Event FocusViewFindReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.FocusViewFindReplace();
var
  sOpt : TFindOptions;
begin
  if HtmlViewer.SelText <> '' then
  begin
    mLastSearchText := AnsiString(HtmlViewer.SelText);
  end;
  sOpt := [frHideUpDown, frHideEntireScope];

  if FindDialogApp = Nil then
  begin
    FindDialogApp:=TMyFindDialog.Create(Self);
    FindDialogApp.Options := sOpt;
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);
  FindDialogApp.FindText := mLastSearchText;
  FindDialogApp.Options  := sOpt;
  FindDialogApp.OnShow   := @OnFindDialogShow;
  FindDialogApp.OnClose  := @OnFindDialogClose;
  FindDialogApp.OnFind   := @OnViewFind;

  mFlagSearchFount := False;
  if HtmlViewer.SelText <> '' then
  begin
    mLastSearchText := AnsiString(HtmlViewer.SelText);
  end;
  sOpt := [frHideUpDown, frHideEntireScope, frHidePromptOnReplace];
  if ReplaceDialogApp = Nil then
  begin
    ReplaceDialogApp := TReplaceDialog.Create(Self);
  end;
  if mFlagWholeWord then Include(sOpt, frWholeWord);
  if mFlagMatchCase then Include(sOpt, frMatchCase);
  if (Not mFlagReplaceDialog) and (HtmlViewer.SelText <> '') then mFlagSearchFount := True;
  ReplaceDialogApp.Options   := sOpt;
  ReplaceDialogApp.FindText  := mLastSearchText;
  ReplaceDialogApp.OnShow    := @OnReplaceDialogShow;
  ReplaceDialogApp.OnClose   := @OnReplaceDialogClose;
  ReplaceDialogApp.OnFind    := @OnViewReplaceFind;
  ReplaceDialogApp.OnReplace := @OnViewReplace;

  if mFlagFindDialog then FindDialogApp.Execute;
  if mFlagReplaceDialog then ReplaceDialogApp.Execute;
end;


{-------------------------------------------------------------------------------
  InitFocus
 ------------------------------------------------------------------------------}
procedure TFormMain.InitFocus();
begin
  mFocusExDummy.OnUndo     := @BtnExecuteEventDummy;
  mFocusExDummy.OnRedo     := @BtnExecuteEventDummy;
  mFocusExDummy.OnCopy     := @BtnExecuteEventDummy;
  mFocusExDummy.OnCut      := @BtnExecuteEventDummy;
  mFocusExDummy.OnPaste    := @BtnExecuteEventDummy;
  mFocusExDummy.OnFind     := @BtnExecuteEventDummy;
  mFocusExDummy.OnReplace  := @BtnExecuteEventDummy;

  mFocusExMd.OnUndo        := @BtnExecuteMdUndo;
  mFocusExMd.OnRedo        := @BtnExecuteMdRedo;
  mFocusExMd.OnCopy        := @BtnExecuteMdCopy;
  mFocusExMd.OnCut         := @BtnExecuteMdCut;
  mFocusExMd.OnPaste       := @BtnExecuteMdPaste;
  mFocusExMd.OnFind        := @BtnExecuteMdFind;
  mFocusExMd.OnReplace     := @BtnExecuteMdReplace;

  mFocusExHtml.OnUndo      := @BtnExecuteHtmlUndo;
  mFocusExHtml.OnRedo      := @BtnExecuteHtmlRedo;
  mFocusExHtml.OnCopy      := @BtnExecuteHtmlCopy;
  mFocusExHtml.OnCut       := @BtnExecuteHtmlCut;
  mFocusExHtml.OnPaste     := @BtnExecuteHtmlPaste;
  mFocusExHtml.OnFind      := @BtnExecuteHtmlFind;
  mFocusExHtml.OnReplace   := @BtnExecuteHtmlReplace;

  mFocusExView.OnUndo      := @BtnExecuteEventDummy;
  mFocusExView.OnRedo      := @BtnExecuteEventDummy;
  mFocusExView.OnCopy      := @BtnExecuteViewCopy;
  mFocusExView.OnCut       := @BtnExecuteViewCut;
  mFocusExView.OnPaste     := @BtnExecuteViewPaste;
  mFocusExView.OnFind      := @BtnExecuteViewFind;
  mFocusExView.OnReplace   := @BtnExecuteViewReplace;

  mFocusExActive := mFocusExMd;

  mFocusEnDummy.CanUndo    := @ActionDumy;
  mFocusEnDummy.CanRedo    := @ActionDumy;
  mFocusEnDummy.CanCopy    := @ActionDumy;
  mFocusEnDummy.CanCut     := @ActionDumy;
  mFocusEnDummy.CanPaste   := @ActionDumy;
  mFocusEnDummy.CanFind    := @ActionDumy;
  mFocusEnDummy.CanReplace := @ActionDumy;

  mFocusEnApp.CanUndo      := @ActionUndo;
  mFocusEnApp.CanRedo      := @ActionRedo;
  mFocusEnApp.CanCopy      := @ActionCopy;
  mFocusEnApp.CanCut       := @ActionCut;
  mFocusEnApp.CanPaste     := @ActionPaste;
  mFocusEnApp.CanFind      := @ActionFind;
  mFocusEnApp.CanReplace   := @ActionReplace;

  mFocusEnActiveMd   := mFocusEnApp;
  mFocusEnActiveHtml := mFocusEnDummy;
  mFocusEnActiveView := mFocusEnDummy;

  mFocusEnApp.CanUndo^.Enabled    := False;
  mFocusEnApp.CanRedo^.Enabled    := False;
  mFocusEnApp.CanCopy^.Enabled    := False;
  mFocusEnApp.CanCut^.Enabled     := False;
  mFocusEnApp.CanPaste^.Enabled   := False;
  mFocusEnApp.CanFind^.Enabled    := False;
  mFocusEnApp.CanReplace^.Enabled := False;
end;


{-------------------------------------------------------------------------------
  Btn events
 ------------------------------------------------------------------------------}
procedure TFormMain.BtnExecuteEventDummy(Sender: TObject);
begin
end;

procedure TFormMain.BtnExecuteMdUndo(Sender: TObject);
begin
  SynEditMd.Undo;
end;

procedure TFormMain.BtnExecuteMdRedo(Sender: TObject);
begin
  SynEditMd.Redo;
end;

procedure TFormMain.BtnExecuteHtmlUndo(Sender: TObject);
begin
  SynEditHtml.Undo;
end;

procedure TFormMain.BtnExecuteHtmlRedo(Sender: TObject);
begin
  SynEditHtml.Redo;
end;

procedure TFormMain.BtnExecuteMdCopy(Sender: TObject);
begin
  SynEditMd.CopyToClipboard;
end;

procedure TFormMain.BtnExecuteHtmlCopy(Sender: TObject);
begin
  SynEditHtml.CopyToClipboard;
end;

procedure TFormMain.BtnExecuteViewCopy(Sender: TObject);
begin
  HtmlViewer.CopyToClipboard;
end;

procedure TFormMain.BtnExecuteMdCut(Sender: TObject);
begin
  SynEditMd.CopyToClipboard;
  SynEditMd.ClearSelection;
end;

procedure TFormMain.BtnExecuteHtmlCut(Sender: TObject);
begin
  SynEditHtml.CopyToClipboard;
  SynEditHtml.ClearSelection;
end;

procedure TFormMain.BtnExecuteViewCut(Sender: TObject);
begin
  HtmlViewer.CopyToClipboard;
end;

procedure TFormMain.BtnExecuteMdPaste(Sender: TObject);
begin
  SynEditMd.PasteFromClipboard;
end;

procedure TFormMain.BtnExecuteHtmlPaste(Sender: TObject);
begin
  SynEditHtml.PasteFromClipboard;
end;

procedure TFormMain.BtnExecuteViewPaste(Sender: TObject);
begin
//  HtmlViewer.PasteFromClipboard;
end;

procedure TFormMain.BtnExecuteMdFind(Sender: TObject);
begin
  FocusMdFindReplace();
  if (ReplaceDialogApp <> Nil) then ReplaceDialogApp.CloseDialog;
  FindDialogApp.Execute;
end;

procedure TFormMain.BtnExecuteHtmlFind(Sender: TObject);
begin
  FocusHtmlFindReplace();
  if (ReplaceDialogApp <> Nil) then ReplaceDialogApp.CloseDialog;
  FindDialogApp.Execute;
end;

procedure TFormMain.BtnExecuteViewFind(Sender: TObject);
begin
  FocusViewFindReplace();
  if (ReplaceDialogApp <> Nil) then ReplaceDialogApp.CloseDialog;
  FindDialogApp.Execute;
end;

procedure TFormMain.BtnExecuteMdReplace(Sender: TObject);
begin
  FocusMdFindReplace();
  if (FindDialogApp <> Nil) then FindDialogApp.CloseDialog;
  ReplaceDialogApp.Execute;
end;

procedure TFormMain.BtnExecuteHtmlReplace(Sender: TObject);
begin
  FocusHtmlFindReplace();
  if (FindDialogApp <> Nil) then FindDialogApp.CloseDialog;
  ReplaceDialogApp.Execute;
end;

procedure TFormMain.BtnExecuteViewReplace(Sender: TObject);
begin
  FocusViewFindReplace();
  if (FindDialogApp <> Nil) then FindDialogApp.CloseDialog;
  ReplaceDialogApp.Execute;
end;

procedure TFormMain.OnFindDialogShow(Sender: TObject);
begin
  mFlagFindDialog := True;
end;

procedure TFormMain.OnFindDialogClose(Sender: TObject);
begin
  mFlagFindDialog := False;
end;

procedure TFormMain.OnReplaceDialogShow(Sender: TObject);
begin
  mFlagReplaceDialog := True;
end;

procedure TFormMain.OnReplaceDialogClose(Sender: TObject);
begin
  mFlagReplaceDialog := False;
end;


{-------------------------------------------------------------------------------
  Event OnMdFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnMdFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  FindDialogApp = Nil then Exit;

  str := FindDialogApp.FindText;
  opt := [];
  mFlagMatchCase := (frMatchCase in FindDialogApp.Options);
  mFlagWholeWord := (frWholeWord in FindDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := SynEditMd.SearchReplace(str, '', opt);
  if res = 0 then
  begin
     SynEditMd.CaretX := 0;
     SynEditMd.CaretY := 0;
     res := SynEditMd.SearchReplace(str, '', opt);
  end;
end;


{-------------------------------------------------------------------------------
  Event OnHtmlFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  FindDialogApp = Nil then Exit;

  str := FindDialogApp.FindText;
  opt := [];
  mFlagMatchCase := (frMatchCase in FindDialogApp.Options);
  mFlagWholeWord := (frWholeWord in FindDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := SynEditHtml.SearchReplace(str, '', opt);
  if res = 0 then
  begin
     SynEditHtml.CaretX := 0;
     SynEditHtml.CaretY := 0;
     res := SynEditHtml.SearchReplace(str, '', opt);
  end;
end;


{-------------------------------------------------------------------------------
  Event OnViewFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnViewFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Boolean;
begin
  if  FindDialogApp = Nil then Exit;

  str := FindDialogApp.FindText;
  opt := [];
  mFlagMatchCase := (frMatchCase in FindDialogApp.Options);
  mFlagWholeWord := (frWholeWord in FindDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := HtmlViewer.Find(UnicodeString(str), mFlagMatchCase);
  if res = false then
  begin
     HtmlViewer.Position  := 0;
     HtmlViewer.SelStart  := 1;
     HtmlViewer.SelLength := 0;
     res := HtmlViewer.Find(UnicodeString(str), mFlagMatchCase);
  end;
end;


{-------------------------------------------------------------------------------
  Event OnMdReplaceFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnMdReplaceFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  ReplaceDialogApp = Nil then Exit;

  str := ReplaceDialogApp.FindText;
  opt := [];
  mFlagSearchFount := True;
  mFlagMatchCase := (frMatchCase in ReplaceDialogApp.Options);
  mFlagWholeWord := (frWholeWord in ReplaceDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := SynEditMd.SearchReplace(str, '', opt);
  if res = 0 then
  begin
     SynEditMd.CaretX := 0;
     SynEditMd.CaretY := 0;
     res := SynEditMd.SearchReplace(str, '', opt);
     if res = 0 then mFlagSearchFount := False;
  end;
end;


{-------------------------------------------------------------------------------
  Event OnMdReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.OnMdReplace(Sender: TObject);
Var
  str : String;
  rep : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  ReplaceDialogApp = Nil then Exit;

  str := ReplaceDialogApp.FindText;
  rep := ReplaceDialogApp.ReplaceText;
  opt := [];
  if frMatchCase in ReplaceDialogApp.Options then Include(opt, ssoMatchCase);
  if frWholeWord in ReplaceDialogApp.Options then Include(opt, ssoWholeWord);

  if frReplaceAll in ReplaceDialogApp.Options then
  begin
    SynEditMd.CaretX := 0;
    SynEditMd.CaretY := 0;
    Include(opt, ssoReplaceAll);
    res := SynEditMd.SearchReplace(str, rep, opt);
  end
  else
  begin
    if mFlagSearchFount = False then
    begin
      OnMdReplaceFind(Sender);
    end
    else
    begin
      Include(opt, ssoReplace);
      Include(opt, ssoBackwards);
      res := SynEditMd.SearchReplace(str, rep, opt);
      if res = 0 then
      begin
        SynEditMd.CaretX := 0;
        SynEditMd.CaretY := 0;
        res := SynEditMd.SearchReplace(str, rep, opt);
      end
      else
      begin
        SynEditMd.CaretX := SynEditMd.BlockEnd.x;
        SynEditMd.CaretY := SynEditMd.BlockEnd.y;
      end;
      mFlagSearchFount := False;
      OnMdReplaceFind(Sender);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Event OnHtmlReplaceFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlReplaceFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  ReplaceDialogApp = Nil then Exit;

  str := ReplaceDialogApp.FindText;
  opt := [];
  mFlagSearchFount := True;
  mFlagMatchCase := (frMatchCase in ReplaceDialogApp.Options);
  mFlagWholeWord := (frWholeWord in ReplaceDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := SynEditHtml.SearchReplace(str, '', opt);
  if res = 0 then
  begin
     SynEditHtml.CaretX := 0;
     SynEditHtml.CaretY := 0;
     res := SynEditHtml.SearchReplace(str, '', opt);
     if res = 0 then mFlagSearchFount := False;
  end;
end;


{-------------------------------------------------------------------------------
  Event OnHtmlReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.OnHtmlReplace(Sender: TObject);
Var
  str : String;
  rep : String;
  opt : TSynSearchOptions;
  res : Integer;
begin
  if  ReplaceDialogApp = Nil then Exit;

  str := ReplaceDialogApp.FindText;
  rep := ReplaceDialogApp.ReplaceText;
  opt := [];
  if frMatchCase in ReplaceDialogApp.Options then Include(opt, ssoMatchCase);
  if frWholeWord in ReplaceDialogApp.Options then Include(opt, ssoWholeWord);

  if frReplaceAll in ReplaceDialogApp.Options then
  begin
    SynEditHtml.CaretX := 0;
    SynEditHtml.CaretY := 0;
    Include(opt, ssoReplaceAll);
    res := SynEditHtml.SearchReplace(str, rep, opt);
    if res > 0 then
    begin
      OnHtmlReplaceFind(Sender);
    end;
  end
  else
  begin
    if mFlagSearchFount = False then
    begin
      OnHtmlReplaceFind(Sender);
    end
    else
    begin
      Include(opt, ssoReplace);
      Include(opt, ssoBackwards);
      res := SynEditHtml.SearchReplace(str, rep, opt);
      if res = 0 then
      begin
        SynEditHtml.CaretX := 0;
        SynEditHtml.CaretY := 0;
        res := SynEditHtml.SearchReplace(str, rep, opt);
      end
      else
      begin
        SynEditHtml.CaretX := SynEditHtml.BlockEnd.x;
        SynEditHtml.CaretY := SynEditHtml.BlockEnd.y;
      end;
      mFlagSearchFount := False;
      OnHtmlReplaceFind(Sender);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  Event OnViewReplaceFind
 ------------------------------------------------------------------------------}
procedure TFormMain.OnViewReplaceFind(Sender: TObject);
Var
  str : String;
  opt : TSynSearchOptions;
  res : Boolean;
begin
  if  ReplaceDialogApp = Nil then Exit;

  str := ReplaceDialogApp.FindText;
  opt := [];
  mFlagSearchFount := True;
  mFlagMatchCase := (frMatchCase in ReplaceDialogApp.Options);
  mFlagWholeWord := (frWholeWord in ReplaceDialogApp.Options);
  if mFlagWholeWord then Include(opt, ssoWholeWord);
  if mFlagMatchCase then Include(opt, ssoMatchCase);
  res := HtmlViewer.Find(UnicodeString(str), mFlagMatchCase);
  if res = False then
  begin
    HtmlViewer.Position  := 0;
    HtmlViewer.SelStart  := 1;
    HtmlViewer.SelLength := 0;
    res := HtmlViewer.Find(UnicodeString(str), mFlagMatchCase);
     if res = False then mFlagSearchFount := False;
  end;
end;


{-------------------------------------------------------------------------------
  Event OnViewReplace
 ------------------------------------------------------------------------------}
procedure TFormMain.OnViewReplace(Sender: TObject);
begin
  // is Read only
end;


{-------------------------------------------------------------------------------
  HtmlToPdf
 ------------------------------------------------------------------------------}
procedure TFormMain.HtmlToPdf(Sender: TObject);
begin
{
var
  Size: TSize;
  Viewer: THtmlViewer;
  pdfForm: TForm;
  pdf: TPdfDocumentGDI;
begin
  pdf := TPdfDocumentGDI.Create;
  pdfForm := TForm.Create(nil);
  Viewer := THtmlViewer.Create(pdfForm);
  Viewer.Parent := pdfForm;
  Viewer.DefBackground := clWhite;
  Viewer.LoadFromFile(htmlFile);
  Size.cx := Round(HTMLWidthFromViewPort(viewer.Text) * pdf.ScreenLogPixels / 25.4);
  Size := viewer.FullDisplaySize(Size.cx);
  pdf.AddPage;
  pdf.DefaultPageWidth := Size.cx;
  pdf.DefaultPageHeight := Size.cy;
  Viewer.Draw(pdf.VCLCanvas, 0, Size.cx, Size.cx, Size.cy);
  pdf.SaveToFile(ChangeFileExt(htmlFile, '-HTML-To-SYNPDF.pdf'));
  pdf.Free;
  pdfForm.Free;
}
end;

procedure TFormMain.DoSettingsNotify(Sender: TObject);
var
  theme: String;
begin
  with mAppCnf do
  begin
    theme := Read('EditorOptions/Themes/Value', 'LightMode');
    if Not (theme = 'DarkMode') then theme := 'LightMode';

    ViewerHead         := UnicodeString(Read('Stylesheets/Viewer/' + theme + '/Head/Value', String(ViewerHead  )));
    ViewerFoot         := UnicodeString(Read('Stylesheets/Viewer/' + theme + '/Foot/Value', String(ViewerFoot  )));

    ExportHtmlHead     := UnicodeString(Read('Stylesheets/ExportHtml/Head/Value',    String(ExportHtmlHead     )));
    ExportHtmlFoot     := UnicodeString(Read('Stylesheets/ExportHtml/Foot/Value',    String(ExportHtmlFoot     )));
    PrintCss           := UnicodeString(Read('Stylesheets/Print/Css/Value',          String(PrintCss           )));
    PrintEvenHead      := UnicodeString(Read('Stylesheets/Print/EvenHead/Value',     String(PrintEvenHead      )));
    PrintEvenFoot      := UnicodeString(Read('Stylesheets/Print/EvenFoot/Value',     String(PrintEvenFoot      )));
    PrintOddHead       := UnicodeString(Read('Stylesheets/Print/OddHead/Value',      String(PrintOddHead       )));
    PrintOddFoot       := UnicodeString(Read('Stylesheets/Print/OddFoot/Value',      String(PrintOddFoot       )));
    ExportPdfCss       := UnicodeString(Read('Stylesheets/ExportPdf/Css/Value',      String(ExportPdfCss       )));
    ExportPdfEvenHead  := UnicodeString(Read('Stylesheets/ExportPdf/EvenHead/Value', String(ExportPdfEvenHead  )));
    ExportPdfEvenFoot  := UnicodeString(Read('Stylesheets/ExportPdf/EvenFoot/Value', String(ExportPdfEvenFoot  )));
    ExportPdfOddHead   := UnicodeString(Read('Stylesheets/ExportPdf/OddHead/Value',  String(ExportPdfOddHead   )));
    ExportPdfOddFoot   := UnicodeString(Read('Stylesheets/ExportPdf/OddFoot/Value',  String(ExportPdfOddFoot   )));
  end;
  SynEditHtml.Modified := true;

  mSynEditorBridge.EditorOptions.SetMarkupColors(SynEditHtml);
  mSynEditorBridge.EditorOptions.GetSynEditSettings(SynEditHtml);

  mSynEditorBridge.EditorOptions.SetMarkupColors(SynEditMd);
  mSynEditorBridge.EditorOptions.GetSynEditSettings(SynEditMd);

  SynEditMd.Options := SynEditMd.Options - [eoTrimTrailingSpaces];

  mSpellToken.ColorSchame;

  HtmlViewShow();
end;

procedure TFormMain.SettingsRecentFilesAdd;
var
  idNew    : Integer;
  idOld    : Integer;
  count    : Integer;
  max      : Integer;
  fileName, filePath : string;
  fNewName, fNewPath : string;
begin
  idNew := 0;
  idOld := 1;

  count := mAppCnf.Read('RecentFiles/Items', 0);
  max   := mAppCnf.Read('RecentFiles/Max',  20);

  fNewName := mFileName;
  fNewPath := LazFileUtils.CreateAbsolutePath(mFileName, mFilePath);

  for idOld := 1 To count+1 do
  begin
    fileName := mAppCnf.Read('RecentFiles/Item_' + idOld.ToString + '/Name', '');
    filePath := mAppCnf.Read('RecentFiles/Item_' + idOld.ToString + '/File', '');
    if LazFileUtils.CreateAbsolutePath(mFileName, mFilePath) = filePath then
    begin
      Continue;
    end;
    Inc(idNew);
    mAppCnf.Write('RecentFiles/Item_' + idNew.ToString + '/Name', fNewName);
    mAppCnf.Write('RecentFiles/Item_' + idNew.ToString + '/File', fNewPath);
    fNewName := fileName;
    fNewPath := filePath;
    if idNew = max then break;
  end;
  mAppCnf.Write('RecentFiles/Items', idNew);
  mAppCnf.Write('RecentFiles/Max',     max);
end;


{-------------------------------------------------------------------------------
  Event DlgAboutShow
 ------------------------------------------------------------------------------}
procedure TFormMain.DlgAboutShow;
begin
   Application.CreateForm(TFormAbout, OFormAbout);
   OFormAbout.Visible:=false;
   OFormAbout.ShowModal;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFormMain.OnReplaceLng();
begin
  LCap('T_FormMain_Title',               Self);
  LCap('T_FormMain_TabSheet1',           TabSheet1);
  LCap('T_FormMain_BtnOffset',           ButtonOffset);
  LCap('T_FormMain_TabSheetView',        TabSheetView);
  LCap('T_FormMain_TabSheetHtml',        TabSheetHtml);

  LCap('T_FormMain_MenuActFile',         MenuItemFile);
  LCap('T_FormMain_MenuActNew',          ActionNew);
  LCap('T_FormMain_MenuActOpen',         ActionOpen);
  LCap('T_FormMain_MenuActOpenAgain',    ActionOpenAgain);
  LCap('T_FormMain_MenuActOpenFile0',    MenuItemOpenFile0);
  LCap('T_FormMain_MenuActSave',         ActionSave);
  LCap('T_FormMain_MenuActSaveAs',       ActionSaveAs);
  LCap('T_FormMain_MenuActPrinter',      ActionPrinter);
  LCap('T_FormMain_MenuActPrintPriview', ActionPrintPreview);
  LCap('T_FormMain_MenuActPrint',        ActionPrint);
  LCap('T_FormMain_MenuActClose',        ActionClose);
  LCap('T_FormMain_MenuActEdit',         MenuItemEdit);
  LCap('T_FormMain_MenuActUndo',         ActionUndo);
  LCap('T_FormMain_MenuActRedo',         ActionRedo);
  LCap('T_FormMain_MenuActSearch',       ActionFind);
  LCap('T_FormMain_MenuActReplace',      ActionReplace);
  LCap('T_FormMain_MenuActCopy',         ActionCopy);
  LCap('T_FormMain_MenuActCut',          ActionCut);
  LCap('T_FormMain_MenuActPaste',        ActionPaste);
  LCap('T_FormMain_MenuActProp',         MenuItemProp);
  LCap('T_FormMain_MenuActPropEditor',   ActionPropEditor);
  LCap('T_FormMain_MenuActPropStyle',    ActionPropStyle);
  LCap('T_FormMain_MenuAct5',            MenuItem5);
  LCap('T_FormMain_MenuActSpell',        ActionSpelling);
  LCap('T_FormMain_MenuActThes',         ActionThesaurus);
  LCap('T_FormMain_MenuActExport',       MenuItemExport);
  LCap('T_FormMain_MenuActExpPDF',       ActionExportPDF);
  LCap('T_FormMain_MenuActExpHTML',      ActionExportHTML);
  LCap('T_FormMain_MenuActDebug',        ActionDebug);
  LCap('T_FormMain_MenuActHelp',         MenuItemHelp);
  LCap('T_FormMain_MenuActHelpMd',       ActionHelpMd);
  LCap('T_FormMain_MenuActHelpEdit',     ActionHelpEditor);
  LCap('T_FormMain_MenuActHelpAbout',    ActionHelpAbout);

  LHnt('T_FormMain_HintActNew',          ActionNew);
  LHnt('T_FormMain_HintActOpen',         ActionOpen);
  LHnt('T_FormMain_HintActSave',         ActionSave);
  LHnt('T_FormMain_HintActSaveAs',       ActionSaveAs);
  LHnt('T_FormMain_HintActPrinter',      ActionPrinter);
  LHnt('T_FormMain_HintActPrintPriview', ActionPrintPreview);
  LHnt('T_FormMain_HintActPrint',        ActionPrint);
  LHnt('T_FormMain_HintActClose',        ActionClose);
  LHnt('T_FormMain_HintActUndo',         ActionUndo);
  LHnt('T_FormMain_HintActRedo',         ActionRedo);
  LHnt('T_FormMain_HintActSearch',       ActionFind);
  LHnt('T_FormMain_HintActReplace',      ActionReplace);
  LHnt('T_FormMain_HintActCopy',         ActionCopy);
  LHnt('T_FormMain_HintActCut',          ActionCut);
  LHnt('T_FormMain_HintActPaste',        ActionPaste);
  LHnt('T_FormMain_HintActPropEditor',   ActionPropEditor);
  LHnt('T_FormMain_HintActPropStyle',    ActionPropStyle);
  LHnt('T_FormMain_HintActSpell',        ActionSpelling);
  LHnt('T_FormMain_HintActThes',         ActionThesaurus);
  LHnt('T_FormMain_HintActExpPDF',       ActionExportPDF);
  LHnt('T_FormMain_HintActExpHTML',      ActionExportHTML);
  LHnt('T_FormMain_HintActDebug',        ActionDebug);
  LHnt('T_FormMain_HintActHelpMd',       ActionHelpMd);
  LHnt('T_FormMain_HintActHelpEdit',     ActionHelpEditor);
  LHnt('T_FormMain_HintActHelpAbout',    ActionHelpAbout);

  OpenDialogApp1.Title   := LStr('T_FormMain_OpenDialogApp',    OpenDialogApp1.Title);
  SaveDialogApp1.Title   := LStr('T_FormMain_SaveDialogApp',    SaveDialogApp1.Title);
  FindDialogApp.Title    := LStr('T_FormMain_FindDialogApp',    FindDialogApp.Title);
  ReplaceDialogApp.Title := LStr('T_FormMain_ReplaceDialogApp', ReplaceDialogApp.Title);

end;



{$IFDEF WINDOWS}

(* -----------------------------------------------------------------------------
 *
 @ NAME:    OnSysAbout()
 *
 @ INFO:
 * Evnet for showing the About dialog.
 *
 * ---------------------------------------------------------------------------*)
procedure TFormMain.OnSysAbout(var Msg : TWMSysCommand);
begin
   if Msg.CmdType = ID_ABOUT then
   begin
     DlgAboutShow;
   end
   else inherited;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    AddAboutItem()
 *
 @ INFO:
 * Sorgt dafür, das im System-Menü der Menüeintrag About hinzugefugt wird.
 *
 * ---------------------------------------------------------------------------*)
procedure TFormMain.AddAboutItem();
var
   AboutCaption : LPCSTR;
   SysMenu : HMenu;
begin
   AboutCaption := PChar(LStr('T_FormMain_AboutApp', 'About') + ' ' + Caption);
   // Get system menu
   SysMenu := GetSystemMenu(Handle, FALSE);
   // Add Seperator
   AppendMenu(SysMenu, MF_SEPARATOR, 0, '');
   // add menu entry
   AppendMenu(SysMenu, MF_STRING, ID_ABOUT, AboutCaption);
end;

{$ENDIF}


end.

