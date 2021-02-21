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
unit CnfViewerFrameStylePrint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazFileUtils, Language,

  FrameSettingsBase, Dialogs, Buttons, ExtCtrls, SynEdit, LResources,
  SynHighlighterMulti, SynHighlighterHTML, SynHighlighterJScript,
  SynHighlighterCss, CnfDialogComboBoxItems, CnfSyneditPreview;

type

  { TFrameStylePrint }

  TFrameStylePrint = class(TFrameSettingsBase)
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

{ TFrameStylePrint }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameStylePrint.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  mCBoxHead := Nil;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameStylePrint.Destroy;
begin
  mCBoxHead.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  InitializeWnd
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.InitializeWnd;
begin
  SynEditCss.Highlighter := SynMultiSyn1;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.Initialize();
begin
  if mCBoxHead = Nil then mCBoxHead := TComboBoxItems.Create;
  mCBoxHead.Settings   := Settings;
  mCBoxHead.Path       := 'Stylesheets/Print/RecentHeadFiles';
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
procedure TFrameStylePrint.SetSettings();
begin
  if Not Settings.HasPath('Stylesheets/Print/Style/Value', True) then
    Settings.Write('Stylesheets/Print/Style/Value', DefaultStyle());

  Initialize();

  SynEditCss.Text := Settings.Read('Stylesheets/Print/Style/Value', SynEditCss.Text);
  mCBoxHead.FileSet('');
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.GetSettings();
begin
  Settings.Write('Stylesheets/Print/Style/Value', SynEditCss.Text);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameStylePrint.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Stylesheets', 'Printing');
begin
  ary[0] := LStr('T_FrameStyle_TreeStylesheets', ary[0]);
  ary[1] := LStr('T_FrameStylePrint_TreePrint',  ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  DefaultStyle
 ------------------------------------------------------------------------------}
Function  TFrameStylePrint.DefaultStyle(): String;
begin
  Result := LoadResource('PrintStyle')
end;


{-------------------------------------------------------------------------------
  ButtonCssLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.ButtonCssLoadClick(Sender: TObject);
begin
  mCBoxHead.FileLoad();
end;


{-------------------------------------------------------------------------------
  ButtonCssExportClick
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.ButtonCssExportClick(Sender: TObject);
begin
  mCBoxHead.FileSave();
end;


{-------------------------------------------------------------------------------
  ComboBoxCssFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.ComboBoxCssFilesChange(Sender: TObject);
begin
  mCBoxHead.FileChange();
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameStylePrint.OnReplaceLng();
begin
  LCap('T_FrameStylePrint_LabelStylesheet',  LabelStylesheet);
  LCap('T_General_BtnLoad',                  ButtonCssLoad);
  LCap('T_General_BtnExport',                ButtonCssExport);
end;



initialization

{$I Resources/CnfViewerFrameStylePrint.lrs}

end.

