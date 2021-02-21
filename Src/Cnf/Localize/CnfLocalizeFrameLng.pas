unit CnfLocalizeFrameLng;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, StdCtrls, ExtCtrls, Dialogs,
  // Cnf
  Storage, FrameSettingsBase, CnfDialogComboBoxItems,
  Language, SysUtils;

type

  { TFrameCnfLocalizeLng }

  TFrameCnfLocalizeLng = class(TFrameSettingsBase)
    ButtonExport: TButton;
    ButtonLoad: TButton;
    CheckBoxUseExternLng: TCheckBox;
    ComboBoxLng: TComboBox;
    ComboBoxSelectExternLngFiles: TComboBox;
    LabelSelectLng: TLabel;
    LabelSelectExternLngFiles: TLabel;
    MemoInfoText: TMemo;
    OpenDialog: TOpenDialog;
    Panel2: TPanel;
    SaveDialog: TSaveDialog;
    procedure ButtonExportClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure CheckBoxUseExternLngChange(Sender: TObject);
    procedure ComboBoxLngChange(Sender: TObject);
    procedure ComboBoxSelectExternLngFilesChange(Sender: TObject);

  private
    mCBox: TComboBoxItems;
    mLngIndex: Integer;

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure  Initialize();

    procedure SetSettings(); override;
    procedure SetCancel(); override;
    procedure GetSettings(); override;
    Function  GetTreePath: TStrArray override;

  private
    procedure LngFileLoad(const actfile: string);
    procedure LngFileSave(const actfile: string);
    procedure InitLanguage();
    procedure SetLanguage(LngCode: string);
    procedure SetLanguage(Index: integer);
    procedure OnReplaceLng();

  end;


const

  CrLf = LineEnding;


implementation

{$R *.lfm}


{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameCnfLocalizeLng.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);

  MemoInfoText.Caption :=
  'Please help to translate this application in to your language. '            +
  'Export the Lng.xml in to the application directory. '                       +
  'With Excel you can edit tis file comfortably. '                      + CrLf +
  ''                                                                    + CrLf +
  'Please send back your translated file to FpcLibs '                          +
  '(fpclibs@online.de). It will add into next release.'                 + CrLf +
  ''                                                                    + CrLf +
  'Infos:'                                                              + CrLf +
  'In the Table add a new column and on the head cell insert a new '           +
  'language-code. For example:'                                         + CrLf +
  '("cz_CZ" "en_AU" or what ever else)'                                 + CrLf ;

  mLngIndex := 0;
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameCnfLocalizeLng.Destroy;
begin
  mCBox.Free;

  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  Initialize
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.Initialize();
begin
  if mCBox = Nil then mCBox := TComboBoxItems.Create
  else Exit;
  mCBox.Settings   := Settings;
  mCBox.Path       := 'Localize/Language/LngFiles';
  mCBox.ComboBox   := ComboBoxSelectExternLngFiles;
  mCBox.LoadDialog := OpenDialog;
  mCBox.SaveDialog := SaveDialog;
  mCBox.OnLoadFile := @LngFileLoad;
  mCBox.OnSaveFile := @LngFileSave;
  mCBox.Extension  := '.xml';
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    InitLanguage()
 *
 @ INFO:    Füllt die Dropdown-Box mit den verfügbaren Sprachen
 * ---------------------------------------------------------------------------*)
procedure TFrameCnfLocalizeLng.InitLanguage();
var
   Lng: TLanguage;
   i: integer;
begin
   Lng := GetLngInstance();

   ComboBoxLng.Items.Clear;
   i:=0;
   for i:=0 to Lng.Count-1 do
   begin
      ComboBoxLng.Items.Add(IntToStr(i+1) + '.   ' + Lng.Name[i] + '  (' + Lng.Code[i] + ')');
   end;
   ComboBoxLng.ItemIndex:=mLngIndex;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetLanguage()
 @ PARAM:   LngCode: String mit dem Language Code
 *
 * ---------------------------------------------------------------------------*)
procedure TFrameCnfLocalizeLng.SetLanguage(LngCode: string);
begin
   OLng.SetUsedLng(LngCode);

   if Not ( CheckBoxUseExternLng.Checked           And
      (ComboBoxSelectExternLngFiles.ItemIndex > 0) And
      OLng.ParseLngFile() ) then
   begin
     OLng.ParseResFile();
     CheckBoxUseExternLng.Checked:= False;
     ComboBoxSelectExternLngFiles.ItemIndex := 0;
   end;
   OLng.SendEventToListener();
   InitLanguage();
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetLanguage()
 @ PARAM:   Index: String mit dem Language Code
 *
 * ---------------------------------------------------------------------------*)
procedure TFrameCnfLocalizeLng.SetLanguage(Index: integer);
begin
   mLngIndex := Index;
   if Index >= OLng.Count then
   begin
     SetLanguage('en_GB');
   end;
   if Index < OLng.Count then
   begin
     SetLanguage(OLng.Code[Index]);
   end;
end;


{-------------------------------------------------------------------------------
  SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.SetSettings();
begin
  Initialize();
  mCBox.FileSet('');

  OLng.ParseResFile();
  InitLanguage();

  mLngIndex := Settings.Read('Localize/Language/Index', 1);
  CheckBoxUseExternLng.Checked:= Settings.Read('Localize/Language/ExternFile/Checked', False);
  ComboBoxSelectExternLngFiles.ItemIndex := Settings.Read('Localize/Language/ExternFile/Index', 0);
  OLng.SetLngFile(ComboBoxSelectExternLngFiles.Items[ComboBoxSelectExternLngFiles.ItemIndex]);

  SetLanguage(mLngIndex);
end;


{-------------------------------------------------------------------------------
  SetCancel
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.SetCancel();
begin
  SetSettings();
end;


{-------------------------------------------------------------------------------
  GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.GetSettings();
begin
  Settings.Write('Localize/Language/Index', mLngIndex);
  Settings.Write('Localize/Language/ExternFile/Checked', CheckBoxUseExternLng.Checked);
  Settings.Write('Localize/Language/ExternFile/Index', ComboBoxSelectExternLngFiles.ItemIndex);
end;


{-------------------------------------------------------------------------------
  GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameCnfLocalizeLng.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Localize', 'Language');
begin
  ary[0] := LStr('T_FrameCnfLocalize_TreeLocalize',    ary[0]);
  ary[1] := LStr('T_FrameCnfLocalizeLng_TreeLanguage', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  ComboBoxLngChange
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.ComboBoxLngChange(Sender: TObject);
begin
  if ComboBoxLng.ItemIndex < 0 then exit;
  SetLanguage(ComboBoxLng.ItemIndex);
end;


{-------------------------------------------------------------------------------
  ButtonLoadClick
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.ButtonLoadClick(Sender: TObject);
begin
  mCBox.FileLoad();
end;


{-------------------------------------------------------------------------------
  CheckBoxUseExternLngChange
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.CheckBoxUseExternLngChange(Sender: TObject);
begin
  if CheckBoxUseExternLng.Checked then
  begin
    mCBox.FileChange();
  end
  else
  begin
    OLng.ParseResFile();
    InitLanguage();
    ComboBoxLngChange(Self);
  end;
end;


{-------------------------------------------------------------------------------
  LngFileLoad
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.LngFileLoad(const actfile: string);
begin
  OLng.LoadLngFile(actfile);
  OLng.SendEventToListener();
  InitLanguage();
  CheckBoxUseExternLng.Checked := True;
end;


{-------------------------------------------------------------------------------
  ButtonExportClick
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.ButtonExportClick(Sender: TObject);
begin
  mCBox.FileSave();
end;


{-------------------------------------------------------------------------------
  LngFileSave
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.LngFileSave(const actfile: string);
begin
   OLng.ExportLngFile(actfile);
   OLng.SendEventToListener();
end;


{-------------------------------------------------------------------------------
  ComboBoxSelectExternLngFilesChange
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.ComboBoxSelectExternLngFilesChange(
  Sender: TObject);
begin
  mCBox.FileChange();
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameCnfLocalizeLng.OnReplaceLng();
begin
  LCap('T_FrameCnfLocalizeLng_SelectLng',      LabelSelectLng);
  LCap('T_FrameCnfLocalizeLng_UseExternLng',   CheckBoxUseExternLng);
  LCap('T_FrameCnfLocalizeLng_SelectExtFiles', LabelSelectExternLngFiles);
  LCap('T_General_BtnLoad',                    ButtonLoad);
  LCap('T_General_BtnExport',                  ButtonExport);
  LCap('T_FrameCnfLocalizeLng_InfoText',       MemoInfoText);
end;



end.

