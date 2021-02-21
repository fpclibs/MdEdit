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
unit CnfViewerFrameStyleViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazFileUtils,

  FrameSettingsBase, Dialogs, Buttons, ExtCtrls, ComCtrls, LResources,
  SynEdit, SynHighlighterMulti, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterCss, CnfDialogComboBoxItems, CnfSyneditPreview, Language;

type

  { TFrameStyleViewer }

  TFrameStyleViewer = class(TFrameSettingsBase)
    ButtonFootExport: TButton;
    ButtonFootLoad: TButton;
    ButtonHeadExport: TButton;
    ButtonHeadLoad: TButton;
    ComboBoxFootFiles: TComboBox;
    ComboBoxHeadFiles: TComboBox;
    LabelStylesheet: TLabel;
    LabelTheme: TLabel;
    OpenDialogFoot: TOpenDialog;
    OpenDialogHead: TOpenDialog;
    PageControlHeadFoot: TPageControl;
    PanelFoot: TPanel;
    PanelHead: TPanel;
    SaveDialogFoot: TSaveDialog;
    SaveDialogHead: TSaveDialog;
    SynCssSyn1: TSynCssSyn;
    SynEditFoot: TSynEdit;
    SynEditHead: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynMultiSyn1: TSynMultiSyn;
    TabSheetFoot: TTabSheet;
    TabSheetHead: TTabSheet;
    procedure ButtonFootExportClick(Sender: TObject);
    procedure ButtonFootLoadClick(Sender: TObject);
    procedure ButtonHeadExportClick(Sender: TObject);
    procedure ButtonHeadLoadClick(Sender: TObject);
    procedure ComboBoxFootFilesChange(Sender: TObject);
    procedure ComboBoxHeadFilesChange(Sender: TObject);

  private
     mCBoxFoot: TComboBoxItems;
     mCBoxHead: TComboBoxItems;
     mTheme:    String;
     mDlgDarkMode:  String;
     mDlgLightMode: String;

     mHighligterPreview: THighligterPreview;
     mSyneditPreview:    TSyneditPreview;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  InitializeWnd(); override;
    procedure  Initialize();
    procedure  ThemeSettings();

    procedure SetSettings(); override;
    procedure GetSettings(); override;
    Function  GetTreePath: TStrArray override;
    Function  DefaultHead(): String;
    Function  DefaultFoot(): String;

    procedure OnVisible(); override;

  private
    procedure SetLabelTheme();
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}

{ TFrameStyleViewer }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameStyleViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  mCBoxFoot := Nil;
  mCBoxHead := Nil;
  mDlgDarkMode  := 'DarkMode';
  mDlgLightMode := 'LightMode';

end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameStyleViewer.Destroy;
begin
  mCBoxFoot.Free;
  mCBoxHead.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.InitializeWnd();
begin
  SynEditHead.Highlighter := SynMultiSyn1;
  SynEditFoot.Highlighter := SynMultiSyn1;

  inherited InitializeWnd();
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.Initialize();
begin
  if mCBoxFoot = Nil then mCBoxFoot := TComboBoxItems.Create
  else exit;
  mCBoxFoot.Settings := Settings;
  mCBoxFoot.Path     := 'Stylesheets/Viewer/RecentFootFiles';
  mCBoxFoot.ComboBox := ComboBoxFootFiles;
  mCBoxFoot.LoadDialog := OpenDialogFoot;
  mCBoxFoot.SaveDialog := SaveDialogFoot;
  mCBoxFoot.OnLoadFile := @SynEditFoot.Lines.LoadFromFile;
  mCBoxFoot.OnSaveFile := @SynEditFoot.Lines.SaveToFile;
  mCBoxFoot.Extension  := '.html';

  if mCBoxHead = Nil then mCBoxHead := TComboBoxItems.Create;
  mCBoxHead.Settings := Settings;
  mCBoxHead.Path     := 'Stylesheets/Viewer/RecentHeadFiles';
  mCBoxHead.ComboBox := ComboBoxHeadFiles;
  mCBoxHead.LoadDialog := OpenDialogHead;
  mCBoxHead.SaveDialog := SaveDialogHead;
  mCBoxHead.OnLoadFile := @SynEditHead.Lines.LoadFromFile;
  mCBoxHead.OnSaveFile := @SynEditHead.Lines.SaveToFile;
  mCBoxHead.Extension  := '.html';

  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mSyneditPreview    := TSyneditPreview   (Settings.Services.FindFirstByClass(TSyneditPreview));

  mHighligterPreview.AddPreviewHighligter(SynHTMLSyn1);
  mHighligterPreview.AddPreviewHighligter(SynCssSyn1);
  mHighligterPreview.AddPreviewHighligter(SynJScriptSyn1);

  mSyneditPreview.AddPreviewEditor(SynEditHead);
  mSyneditPreview.AddPreviewEditor(SynEditFoot);

  ThemeSettings();

  mCBoxHead.FileSet('');
  mCBoxFoot.FileSet('');
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.SetSettings();
begin
  Initialize();
end;


{-------------------------------------------------------------------------------
  @NAME: OnVisible
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.OnVisible();
begin
  if Visible then ThemeSettings();
end;


{-------------------------------------------------------------------------------
  ThemeSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ThemeSettings();
var
  theme: String;
begin
  theme := Settings.Read('EditorOptions/Themes/Value', 'LightMode');
  if Not (theme = 'DarkMode') then theme := 'LightMode';

  if mTheme = theme then exit;
  mTheme := theme;

  if Not Settings.HasPath('Stylesheets/Viewer/' + mTheme + '/Head/Value', True) then
    Settings.Write('Stylesheets/Viewer/' + mTheme + '/Head/Value', DefaultHead());

  if Not Settings.HasPath('Stylesheets/Viewer/' + mTheme + '/Foot/Value', True) then
    Settings.Write('Stylesheets/Viewer/' + mTheme + '/Foot/Value', DefaultFoot());

  SynEditHead.Text := Settings.Read('Stylesheets/Viewer/' + mTheme + '/Head/Value', SynEditHead.Text);
  SynEditFoot.Text := Settings.Read('Stylesheets/Viewer/' + mTheme + '/Foot/Value', SynEditFoot.Text);

  SetLabelTheme();
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.GetSettings();
begin
  ThemeSettings();

  Settings.Write('Stylesheets/Viewer/' + mTheme + '/Head/Value', SynEditHead.Text);
  Settings.Write('Stylesheets/Viewer/' + mTheme + '/Foot/Value', SynEditFoot.Text);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameStyleViewer.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Stylesheets', 'Viewer');
begin
  ary[0] := LStr('T_FrameStyle_TreeStylesheets',   ary[0]);
  ary[1] := LStr('T_FrameStyleViewer_TreeViewer',  ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  DefaultHead
 ------------------------------------------------------------------------------}
Function  TFrameStyleViewer.DefaultHead(): String;
begin
  if mTheme = 'LightMode' then
    Result := LoadResource('ViewerHeadLightMode')
  else
    Result := LoadResource('ViewerHeadDarkMode');
end;


{-------------------------------------------------------------------------------
  DefaultFoot
 ------------------------------------------------------------------------------}
Function  TFrameStyleViewer.DefaultFoot(): String;
begin
  if mTheme = 'LightMode' then
    Result := LoadResource('ViewerFootLightMode')
  else
    Result := LoadResource('ViewerFootDarkMode');
end;


{-------------------------------------------------------------------------------
  ButtonHeadLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ButtonHeadLoadClick(Sender: TObject);
begin
  mCBoxHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonHeadExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ButtonHeadExportClick(Sender: TObject);
begin
  mCBoxHead.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxHeadFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ComboBoxHeadFilesChange(Sender: TObject);
begin
  mCBoxHead.FileChange();
end;


{-------------------------------------------------------------------------------
  ButtonFootLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ButtonFootLoadClick(Sender: TObject);
begin
  mCBoxFoot.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonFootExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ButtonFootExportClick(Sender: TObject);
begin
  mCBoxFoot.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxFootFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.ComboBoxFootFilesChange(Sender: TObject);
begin
  mCBoxFoot.FileChange();
end;


{-------------------------------------------------------------------------------
  SetLabelTheme
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.SetLabelTheme();
begin
  if Not (mTheme = 'DarkMode') then
    LabelTheme.Caption:='(' + mDlgLightMode + ')'
  else
    LabelTheme.Caption:='(' + mDlgDarkMode + ')';
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameStyleViewer.OnReplaceLng();
begin
  LCap('T_FrameStyleViewer_LabelStylesheet', LabelStylesheet);
  LCap('T_FrameStyleGeneral_TabSheetHead',   TabSheetHead);
  LCap('T_FrameStyleGeneral_TabSheetFoot',   TabSheetFoot);
  LCap('T_General_BtnLoad',                  ButtonHeadLoad);
  LCap('T_General_BtnExport',                ButtonHeadExport);
  LCap('T_General_BtnLoad',                  ButtonFootLoad);
  LCap('T_General_BtnExport',                ButtonFootExport);

  mDlgDarkMode  := LStr('T_FrameStyleViewer_LabelDarkMode',  mDlgDarkMode);
  mDlgLightMode := LStr('T_FrameStyleViewer_LabelLightMode', mDlgLightMode);

  SetLabelTheme();
end;



initialization

{$I Resources/CnfViewerFrameStyleViewer.lrs}

end.

