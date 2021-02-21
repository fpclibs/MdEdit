{
 ******************************************++***********************************
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
unit CnfExportFrameStyleHtml;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazFileUtils,

  FrameSettingsBase, Dialogs, Buttons, ExtCtrls, ComCtrls, LResources,
  SynEdit, SynHighlighterMulti, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterCss, CnfDialogComboBoxItems, CnfSyneditPreview, Language;

type

  { TFrameStyleExportHtml }

  TFrameStyleExportHtml = class(TFrameSettingsBase)
    ButtonFootExport: TButton;
    ButtonFootLoad: TButton;
    ButtonHeadExport: TButton;
    ButtonHeadLoad: TButton;
    ComboBoxFootFiles: TComboBox;
    ComboBoxHeadFiles: TComboBox;
    LabelStylesheet: TLabel;
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

     mHighligterPreview: THighligterPreview;
     mSyneditPreview:    TSyneditPreview;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  InitializeWnd(); override;
    procedure  Initialize();
    Function   DefaultHead(): String;
    Function   DefaultFoot(): String;

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;

  private
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}

{ TFrameStyleExportHtml }


{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameStyleExportHtml.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  mCBoxFoot := Nil;
  mCBoxHead := Nil;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameStyleExportHtml.Destroy;
begin
  mCBoxFoot.Free;
  mCBoxHead.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.InitializeWnd;
begin
  SynEditHead.Highlighter := SynMultiSyn1;
  SynEditFoot.Highlighter := SynMultiSyn1;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.Initialize();
begin
  if mCBoxFoot = Nil then mCBoxFoot := TComboBoxItems.Create
  else exit;
  mCBoxFoot.Settings   := Settings;
  mCBoxFoot.Path       := 'Stylesheets/ExportHtml/RecentFootFiles';
  mCBoxFoot.ComboBox   := ComboBoxFootFiles;
  mCBoxFoot.LoadDialog := OpenDialogFoot;
  mCBoxFoot.SaveDialog := SaveDialogFoot;
  mCBoxFoot.OnLoadFile := @SynEditFoot.Lines.LoadFromFile;
  mCBoxFoot.OnSaveFile := @SynEditFoot.Lines.SaveToFile;
  mCBoxFoot.Extension  := '.html';

  if mCBoxHead = Nil then mCBoxHead := TComboBoxItems.Create;
  mCBoxHead.Settings   := Settings;
  mCBoxHead.Path       := 'Stylesheets/ExportHtml/RecentHeadFiles';
  mCBoxHead.ComboBox   := ComboBoxHeadFiles;
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
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.SetSettings();
begin
  if Not Settings.HasPath('Stylesheets/ExportHtml/Head/Value', True) then
    Settings.Write('Stylesheets/ExportHtml/Head/Value', DefaultHead());

  if Not Settings.HasPath('Stylesheets/ExportHtml/Foot/Value', True) then
    Settings.Write('Stylesheets/ExportHtml/Foot/Value', DefaultFoot());

  Initialize();

  SynEditHead.Text := Settings.Read('Stylesheets/ExportHtml/Head/Value', SynEditHead.Text);
  SynEditFoot.Text := Settings.Read('Stylesheets/ExportHtml/Foot/Value', SynEditFoot.Text);
  mCBoxHead.FileSet('');
  mCBoxFoot.FileSet('');
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.GetSettings();
begin
  Settings.Write('Stylesheets/ExportHtml/Head/Value', SynEditHead.Text);
  Settings.Write('Stylesheets/ExportHtml/Foot/Value', SynEditFoot.Text);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameStyleExportHtml.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Stylesheets', 'Export HTML');
begin
  ary[0] := LStr('T_FrameStyle_TreeStylesheets',          ary[0]);
  ary[1] := LStr('T_FrameStyleExportHtml_TreeExportHtml', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  DefaultHead
 ------------------------------------------------------------------------------}
Function  TFrameStyleExportHtml.DefaultHead(): String;
begin
  Result := LoadResource('ExportHtmlHead')
end;


{-------------------------------------------------------------------------------
  DefaultFoot
 ------------------------------------------------------------------------------}
Function  TFrameStyleExportHtml.DefaultFoot(): String;
begin
  Result := LoadResource('ExportHtmlFoot');
end;


{-------------------------------------------------------------------------------
  ButtonHeadLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ButtonHeadLoadClick(Sender: TObject);
begin
  mCBoxHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonHeadExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ButtonHeadExportClick(Sender: TObject);
begin
  mCBoxHead.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxHeadFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ComboBoxHeadFilesChange(Sender: TObject);
begin
  mCBoxHead.FileChange();
end;


{-------------------------------------------------------------------------------
  ButtonFootLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ButtonFootLoadClick(Sender: TObject);
begin
  mCBoxFoot.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonFootExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ButtonFootExportClick(Sender: TObject);
begin
  mCBoxFoot.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxFootFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.ComboBoxFootFilesChange(Sender: TObject);
begin
  mCBoxFoot.FileChange();
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportHtml.OnReplaceLng();
begin
  LCap('T_FrameStyleExportHtml_LabelStylesheet', LabelStylesheet);
  LCap('T_FrameStyleGeneral_TabSheetHead',       TabSheetHead);
  LCap('T_FrameStyleGeneral_TabSheetFoot',       TabSheetFoot);
  LCap('T_General_BtnLoad',                      ButtonHeadLoad);
  LCap('T_General_BtnExport',                    ButtonHeadExport);
  LCap('T_General_BtnLoad',                      ButtonFootLoad);
  LCap('T_General_BtnExport',                    ButtonFootExport);
end;



initialization

{$I Resources/CnfExportFrameStyleHtml.lrs}

end.

