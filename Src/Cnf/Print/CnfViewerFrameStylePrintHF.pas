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
unit CnfViewerFrameStylePrintHF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazFileUtils, LResources,

  FrameSettingsBase, Dialogs, Buttons, ExtCtrls, PairSplitter, ComCtrls,
  SynEdit, SynHighlighterMulti, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterCss, CnfDialogComboBoxItems, CnfSyneditPreview, Language;

type

  { TFrameStylePrintHF }

  TFrameStylePrintHF = class(TFrameSettingsBase)
    ButtonEvenFootExport: TButton;
    ButtonEvenFootLoad: TButton;
    ButtonOddHeadExport: TButton;
    ButtonOddFootExport: TButton;
    ButtonEvenHeadExport: TButton;
    ButtonOddHeadLoad: TButton;
    ButtonOddFootLoad: TButton;
    ButtonEvenHeadLoad: TButton;
    ComboBoxEvenFootFiles: TComboBox;
    ComboBoxOddHeadFiles: TComboBox;
    ComboBoxOddFootFiles: TComboBox;
    ComboBoxEvenHeadFiles: TComboBox;
    LabelEven: TLabel;
    LabelOdd: TLabel;
    OpenDialogEvenFoot: TOpenDialog;
    OpenDialogOddHead: TOpenDialog;
    OpenDialogOddFoot: TOpenDialog;
    OpenDialogEvenHead: TOpenDialog;
    PageControlOddHeadFoot: TPageControl;
    PageControlEvenHeadFoot: TPageControl;
    PairSplitter1: TPairSplitter;
    PairSplitterSideEven: TPairSplitterSide;
    PairSplitterSideOdd: TPairSplitterSide;
    PanelEvenFoot: TPanel;
    PanelOddHead: TPanel;
    PanelOddFoot: TPanel;
    PanelEvenHead: TPanel;
    SaveDialogEvenFoot: TSaveDialog;
    SaveDialogOddHead: TSaveDialog;
    SaveDialogOddFoot: TSaveDialog;
    SaveDialogEvenHead: TSaveDialog;
    SynCssSyn1: TSynCssSyn;
    SynEditEvenFoot: TSynEdit;
    SynEditOddHead: TSynEdit;
    SynEditOddFoot: TSynEdit;
    SynEditEvenHead: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynMultiSyn1: TSynMultiSyn;
    TabSheetEvenFoot: TTabSheet;
    TabSheetOddHead: TTabSheet;
    TabSheetOddFoot: TTabSheet;
    TabSheetEvenHead: TTabSheet;

    procedure ButtonEvenFootExportClick(Sender: TObject);
    procedure ButtonEvenFootLoadClick(Sender: TObject);
    procedure ButtonEvenHeadExportClick(Sender: TObject);
    procedure ButtonEvenHeadLoadClick(Sender: TObject);
    procedure ComboBoxEvenFootFilesChange(Sender: TObject);
    procedure ComboBoxEvenHeadFilesChange(Sender: TObject);

    procedure ButtonOddFootExportClick(Sender: TObject);
    procedure ButtonOddFootLoadClick(Sender: TObject);
    procedure ButtonOddHeadExportClick(Sender: TObject);
    procedure ButtonOddHeadLoadClick(Sender: TObject);
    procedure ComboBoxOddFootFilesChange(Sender: TObject);
    procedure ComboBoxOddHeadFilesChange(Sender: TObject);

  private
    mCBoxEvenFoot: TComboBoxItems;
    mCBoxEvenHead: TComboBoxItems;
    mCBoxOddFoot:  TComboBoxItems;
    mCBoxOddHead:  TComboBoxItems;

    mHighligterPreview: THighligterPreview;
    mSyneditPreview:    TSyneditPreview;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  InitializeWnd(); override;
    procedure  Initialize();
    Function   DefaultEvenHead(): String;
    Function   DefaultEvenFoot(): String;
    Function   DefaultOddHead(): String;
    Function   DefaultOddFoot(): String;

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;

  private
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}

{ TFrameStylePrintHF }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameStylePrintHF.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  mCBoxEvenFoot := Nil;
  mCBoxEvenHead := Nil;
  mCBoxOddFoot  := Nil;
  mCBoxOddHead  := Nil;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameStylePrintHF.Destroy;
begin
  mCBoxEvenFoot.Free;
  mCBoxEvenHead.Free;
  mCBoxOddFoot.Free;
  mCBoxOddHead.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.InitializeWnd;
begin
  SynEditEvenHead.Highlighter := SynMultiSyn1;
  SynEditEvenFoot.Highlighter := SynMultiSyn1;
  SynEditOddHead.Highlighter := SynMultiSyn1;
  SynEditOddFoot.Highlighter := SynMultiSyn1;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.Initialize();
begin
  if mCBoxEvenFoot = Nil then mCBoxEvenFoot := TComboBoxItems.Create
  else exit;
  mCBoxEvenFoot.Settings := Settings;
  mCBoxEvenFoot.Path     := 'Stylesheets/Print/RecentEvenFootFiles';
  mCBoxEvenFoot.ComboBox := ComboBoxEvenFootFiles;
  mCBoxEvenFoot.LoadDialog := OpenDialogEvenFoot;
  mCBoxEvenFoot.SaveDialog := SaveDialogEvenFoot;
  mCBoxEvenFoot.OnLoadFile := @SynEditEvenFoot.Lines.LoadFromFile;
  mCBoxEvenFoot.OnSaveFile := @SynEditEvenFoot.Lines.SaveToFile;
  mCBoxEvenFoot.Extension  := '.html';

  if mCBoxEvenHead = Nil then mCBoxEvenHead := TComboBoxItems.Create;
  mCBoxEvenHead.Settings := Settings;
  mCBoxEvenHead.Path     := 'Stylesheets/Print/RecentEvenHeadFiles';
  mCBoxEvenHead.ComboBox := ComboBoxEvenHeadFiles;
  mCBoxEvenHead.LoadDialog := OpenDialogEvenHead;
  mCBoxEvenHead.SaveDialog := SaveDialogEvenHead;
  mCBoxEvenHead.OnLoadFile := @SynEditEvenHead.Lines.LoadFromFile;
  mCBoxEvenHead.OnSaveFile := @SynEditEvenHead.Lines.SaveToFile;
  mCBoxEvenHead.Extension  := '.html';

  if mCBoxOddFoot = Nil then mCBoxOddFoot := TComboBoxItems.Create;
  mCBoxOddFoot.Settings := Settings;
  mCBoxOddFoot.Path     := 'Stylesheets/Print/RecentOddFootFiles';
  mCBoxOddFoot.ComboBox := ComboBoxOddFootFiles;
  mCBoxOddFoot.LoadDialog := OpenDialogOddFoot;
  mCBoxOddFoot.SaveDialog := SaveDialogOddFoot;
  mCBoxOddFoot.OnLoadFile := @SynEditOddFoot.Lines.LoadFromFile;
  mCBoxOddFoot.OnSaveFile := @SynEditOddFoot.Lines.SaveToFile;
  mCBoxOddFoot.Extension  := '.html';

  if mCBoxOddHead = Nil then mCBoxOddHead := TComboBoxItems.Create;
  mCBoxOddHead.Settings := Settings;
  mCBoxOddHead.Path     := 'Stylesheets/Print/RecentOddHeadFiles';
  mCBoxOddHead.ComboBox := ComboBoxOddHeadFiles;
  mCBoxOddHead.LoadDialog := OpenDialogOddHead;
  mCBoxOddHead.SaveDialog := SaveDialogOddHead;
  mCBoxOddHead.OnLoadFile := @SynEditOddHead.Lines.LoadFromFile;
  mCBoxOddHead.OnSaveFile := @SynEditOddHead.Lines.SaveToFile;
  mCBoxOddHead.Extension  := '.html';

  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mSyneditPreview    := TSyneditPreview   (Settings.Services.FindFirstByClass(TSyneditPreview));

  mHighligterPreview.AddPreviewHighligter(SynHTMLSyn1);
  mHighligterPreview.AddPreviewHighligter(SynCssSyn1);
  mHighligterPreview.AddPreviewHighligter(SynJScriptSyn1);

  mSyneditPreview.AddPreviewEditor(SynEditEvenHead);
  mSyneditPreview.AddPreviewEditor(SynEditEvenFoot);
  mSyneditPreview.AddPreviewEditor(SynEditOddHead);
  mSyneditPreview.AddPreviewEditor(SynEditOddFoot);
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.SetSettings();
begin
  if Not Settings.HasPath('Stylesheets/Print/EvenHead/Value', True) then
    Settings.Write('Stylesheets/Print/EvenHead/Value', DefaultEvenHead());

  if Not Settings.HasPath('Stylesheets/Print/EvenFoot/Value', True) then
    Settings.Write('Stylesheets/Print/EvenFoot/Value', DefaultEvenFoot());

  if Not Settings.HasPath('Stylesheets/Print/OddHead/Value', True) then
    Settings.Write('Stylesheets/Print/OddHead/Value', DefaultOddHead());

  if Not Settings.HasPath('Stylesheets/Print/OddFoot/Value', True) then
    Settings.Write('Stylesheets/Print/OddFoot/Value', DefaultOddFoot());

  Initialize();

  SynEditEvenHead.Text := Settings.Read('Stylesheets/Print/EvenHead/Value', SynEditEvenHead.Text);
  SynEditEvenFoot.Text := Settings.Read('Stylesheets/Print/EvenFoot/Value', SynEditEvenFoot.Text);
  SynEditOddHead.Text  := Settings.Read('Stylesheets/Print/OddHead/Value',  SynEditOddHead.Text);
  SynEditOddFoot.Text  := Settings.Read('Stylesheets/Print/OddFoot/Value',  SynEditOddFoot.Text);

  PairSplitter1.Position := Settings.Read('Stylesheets/Print/Position/SplitterHf', PairSplitter1.Position);

  mCBoxEvenHead.FileSet('');
  mCBoxEvenFoot.FileSet('');
  mCBoxOddHead.FileSet('');
  mCBoxOddFoot.FileSet('');
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.GetSettings();
begin
  Settings.Write('Stylesheets/Print/EvenHead/Value', SynEditEvenHead.Text);
  Settings.Write('Stylesheets/Print/EvenFoot/Value', SynEditEvenFoot.Text);
  Settings.Write('Stylesheets/Print/OddHead/Value',  SynEditOddHead.Text);
  Settings.Write('Stylesheets/Print/OddFoot/Value',  SynEditOddFoot.Text);

  Settings.Write('Stylesheets/Print/Position/SplitterHf', PairSplitter1.Position);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameStylePrintHF.GetTreePath: TStrArray;
var
  ary: Array[0..2] of String = ('Stylesheets', 'Printing', 'Kopf-Fußzeilen');
begin
  ary[0] := LStr('T_FrameStyle_TreeStylesheets', ary[0]);
  ary[1] := LStr('T_FrameStylePrint_TreePrint',  ary[1]);
  ary[2] := LStr('T_FrameStylePrintHF_TreeHF',   ary[2]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  DefaultEvenHead
 ------------------------------------------------------------------------------}
Function  TFrameStylePrintHF.DefaultEvenHead(): String;
begin
  Result := LoadResource('PrintEvenHead')
end;


{-------------------------------------------------------------------------------
  DefaultEvenFoot
 ------------------------------------------------------------------------------}
Function  TFrameStylePrintHF.DefaultEvenFoot(): String;
begin
  Result := LoadResource('PrintEvenFoot');
end;


{-------------------------------------------------------------------------------
  DefaultOddHead
 ------------------------------------------------------------------------------}
Function  TFrameStylePrintHF.DefaultOddHead(): String;
begin
  Result := LoadResource('PrintOddHead')
end;


{-------------------------------------------------------------------------------
  DefaultOddFoot
 ------------------------------------------------------------------------------}
Function  TFrameStylePrintHF.DefaultOddFoot(): String;
begin
  Result := LoadResource('PrintOddFoot');
end;


{-------------------------------------------------------------------------------
  ButtonEvenHeadLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonEvenHeadLoadClick(Sender: TObject);
begin
  mCBoxEvenHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonEvenHeadExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonEvenHeadExportClick(Sender: TObject);
begin
  mCBoxEvenHead.FileSave();
end;


{----------------------------------------------Printing---------------------------------
  ComboBoxEvenHeadFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ComboBoxEvenHeadFilesChange(Sender: TObject);
begin
  mCBoxEvenHead.FileChange();
end;


{-------------------------------------------------------------------------------
  ButtonEvenFootLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonEvenFootLoadClick(Sender: TObject);
begin
  mCBoxEvenFoot.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonEvenFootExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonEvenFootExportClick(Sender: TObject);
begin
  mCBoxEvenFoot.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxEvenFootFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ComboBoxEvenFootFilesChange(Sender: TObject);
begin
  mCBoxEvenFoot.FileChange();
end;


{-------------------------------------------------------------------------------
  ButtonOddHeadLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonOddHeadLoadClick(Sender: TObject);
begin
  mCBoxOddHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonOddHeadExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonOddHeadExportClick(Sender: TObject);
begin
  mCBoxOddHead.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxOddHeadFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ComboBoxOddHeadFilesChange(Sender: TObject);
begin
  mCBoxOddHead.FileChange();
end;


{-------------------------------------------------------------------------------
  ButtonOddFootLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonOddFootLoadClick(Sender: TObject);
begin
  mCBoxOddFoot.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonOddFootExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ButtonOddFootExportClick(Sender: TObject);
begin
  mCBoxOddFoot.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxOddFootFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.ComboBoxOddFootFilesChange(Sender: TObject);
begin
  mCBoxOddFoot.FileChange();
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameStylePrintHF.OnReplaceLng();
begin
  LCap('T_FrameStylePrintHF_LabelEven',     LabelEven);
  LCap('T_FrameStylePrintHF_LabelOdd',      LabelOdd);
  LCap('T_FrameStyleGeneral_TabSheetHead',  TabSheetEvenHead);
  LCap('T_FrameStyleGeneral_TabSheetFoot',  TabSheetEvenFoot);
  LCap('T_FrameStyleGeneral_TabSheetHead',  TabSheetOddHead);
  LCap('T_FrameStyleGeneral_TabSheetFoot',  TabSheetOddFoot);
  LCap('T_General_BtnLoad',                 ButtonEvenHeadLoad);
  LCap('T_General_BtnExport',               ButtonEvenHeadExport);
  LCap('T_General_BtnLoad',                 ButtonEvenFootLoad);
  LCap('T_General_BtnExport',               ButtonEvenFootExport);
  LCap('T_General_BtnLoad',                 ButtonOddHeadLoad);
  LCap('T_General_BtnExport',               ButtonOddHeadExport);
  LCap('T_General_BtnLoad',                 ButtonOddFootLoad);
  LCap('T_General_BtnExport',               ButtonOddFootExport);
end;



initialization

{$I Resources/CnfViewerFrameStylePrintHF.lrs}

end.

