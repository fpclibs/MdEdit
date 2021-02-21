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
unit CnfExportFrameStylePdf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazFileUtils, LResources,

  FrameSettingsBase, Dialogs, Buttons, ExtCtrls, SynEdit, Language,
  SynHighlighterMulti, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterCss, CnfDialogComboBoxItems, CnfSyneditPreview;

type

  { TFrameStyleExportPdf }

  TFrameStyleExportPdf = class(TFrameSettingsBase)
    ButtonCssLoad: TButton;
    ButtonCssExport: TButton;
    ComboBoxCssFiles: TComboBox;
    ImageListFramePrint: TImageList;
    LabelStylesheet: TLabel;
    OpenDialogCss: TOpenDialog;
    SaveDialogCss: TSaveDialog;
    SynCssSyn1: TSynCssSyn;
    SynEditCss: TSynEdit;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynMultiSyn1: TSynMultiSyn;
    procedure ButtonCssExportClick(Sender: TObject);
    procedure ButtonCssLoadClick(Sender: TObject);
    procedure ComboBoxCssFilesChange(Sender: TObject);

  private
     mCBoxHead: TComboBoxItems;
     mHighligterPreview: THighligterPreview;
     mSyneditPreview:    TSyneditPreview;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  InitializeWnd(); override;
    procedure  Initialize();
    Function   DefaultStyle(): String;

  public
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;

  private
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}

{ TFrameStyleExportPdf }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameStyleExportPdf.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  mCBoxHead := Nil;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameStyleExportPdf.Destroy;
begin
  mCBoxHead.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.InitializeWnd;
begin
  SynEditCss.Highlighter := SynMultiSyn1;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.Initialize();
begin
  if mCBoxHead = Nil then mCBoxHead := TComboBoxItems.Create;
  mCBoxHead.Settings   := Settings;
  mCBoxHead.Path       := 'Stylesheets/ExportPdf/RecentHeadFiles';
  mCBoxHead.ComboBox   := ComboBoxCssFiles;
  mCBoxHead.LoadDialog := SaveDialogCss;
  mCBoxHead.SaveDialog := SaveDialogCss;
  mCBoxHead.OnLoadFile := @SynEditCss.Lines.LoadFromFile;
  mCBoxHead.OnSaveFile := @SynEditCss.Lines.SaveToFile;
  mCBoxHead.Extension  := '.html';

  mHighligterPreview := THighligterPreview(Settings.Services.FindFirstByClass(THighligterPreview));
  mSyneditPreview    := TSyneditPreview   (Settings.Services.FindFirstByClass(TSyneditPreview));

  mHighligterPreview.AddPreviewHighligter(SynHTMLSyn1);
  mHighligterPreview.AddPreviewHighligter(SynCssSyn1);
  mHighligterPreview.AddPreviewHighligter(SynJScriptSyn1);

  mSyneditPreview.AddPreviewEditor(SynEditCss);
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.SetSettings();
begin
  if Not Settings.HasPath('Stylesheets/ExportPdf/Style/Value', True) then
    Settings.Write('Stylesheets/ExportPdf/Style/Value', DefaultStyle());

  Initialize();

  SynEditCss.Text := Settings.Read('Stylesheets/ExportPdf/Style/Value', SynEditCss.Text);
  mCBoxHead.FileSet('');
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.GetSettings();
begin
  Settings.Write('Stylesheets/ExportPdf/Style/Value', SynEditCss.Text);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameStyleExportPdf.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Stylesheets', 'Export PDF');
begin
  ary[0] := LStr('T_FrameStyle_TreeStylesheets',        ary[0]);
  ary[1] := LStr('T_FrameStyleExportPdf_TreeExportPdf', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  DefaultStyle
 ------------------------------------------------------------------------------}
Function  TFrameStyleExportPdf.DefaultStyle(): String;
begin
  Result := LoadResource('ExportPdfStyle')
end;


{-------------------------------------------------------------------------------
  ButtonCssLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.ButtonCssLoadClick(Sender: TObject);
begin
  mCBoxHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonCssExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.ButtonCssExportClick(Sender: TObject);
begin
  mCBoxHead.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxCssFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.ComboBoxCssFilesChange(Sender: TObject);
begin
  mCBoxHead.FileChange();
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameStyleExportPdf.OnReplaceLng();
begin
  LCap('T_FrameStyleExportPdf_LabelStylesheet',  LabelStylesheet);
  LCap('T_General_BtnLoad',                      ButtonCssLoad);
  LCap('T_General_BtnExport',                    ButtonCssExport);
end;



initialization

{$I Resources/CnfExportFrameStylePdf.lrs}

end.

