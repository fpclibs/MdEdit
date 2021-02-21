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
unit FormSpellCheckDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  FormSettingsDlgBase, CheckBoxThemed, Language;

type

  { TFormSpellCheckDlg }

  TFormSpellCheckDlg = class(TFormSettingsDlgBase)

    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxCamelCase: TCheckBoxThemed;
    CheckBoxSpellCheckActive: TCheckBoxThemed;
    CheckBoxUpperCase: TCheckBoxThemed;
    ComboBoxActiveLng: TComboBox;
    GroupBoxSpellCheck: TGroupBox;
    LabelActiveLng: TLabel;
    Panel2: TPanel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure CheckBoxCamelCaseChange(Sender: TObject);
    procedure CheckBoxUpperCaseChange(Sender: TObject);
    procedure CheckBoxSpellCheckActiveChange(Sender: TObject);
    procedure ComboBoxActiveLngChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
    procedure OnReplaceLng();

  end;


var
  OFormSpellCheckDlg: TFormSpellCheckDlg;

implementation

{$R *.lfm}


{ TFormSpellCheckDlg }

{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.FormCreate(Sender: TObject);
begin
  inherited;

  OLng.AddListener(@OnReplaceLng);
end;


{-------------------------------------------------------------------------------
  @NAME: FormShow
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.FormShow(Sender: TObject);
var
  items, itemId : Integer;
  dicfile, actfile: string;
begin
  with mSettings do begin
    Left   := Read('SpellCheckDlg/Left',   Left);
    Top    := Read('SpellCheckDlg/Top',    Top);
    Width  := Read('SpellCheckDlg/Width',  Width);
    Height := Read('SpellCheckDlg/Height', Height);
  end;
  actfile := Settings.Read('SpellCheckDlg/ActiveDig', '');
  CheckBoxCamelCase.Checked := Settings.Read('SpellCheckDlg/CamelCase', False);
  CheckBoxUpperCase.Checked := Settings.Read('SpellCheckDlg/UpperCase', False);

  ComboBoxActiveLng.Text:='';
  ComboBoxActiveLng.Items.Clear;
  items := Settings.Read('DictHunspell/Items', 0);
  for itemId := 1 To items do
  begin
    dicfile := Settings.Read('DictHunspell/Item_' + itemId.ToString + '/File', '');
    ComboBoxActiveLng.AddItem(dicfile, Nil);
    if itemId = 1 then ComboBoxActiveLng.Text:= dicfile;
    if actfile = dicfile then ComboBoxActiveLng.Text:= dicfile;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: FormClose
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with mSettings do begin
    Write('SpellCheckDlg/Left',   Left);
    Write('SpellCheckDlg/Top',    Top);
    Write('SpellCheckDlg/Width',  Width);
    Write('SpellCheckDlg/Height', Height);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonOkClick
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.ButtonOkClick(Sender: TObject);
begin
  with mSettings do begin
    Write('SpellCheckDlg/ActiveDig', ComboBoxActiveLng.Text);
    Write('SpellCheckDlg/AutoSpell', CheckBoxSpellCheckActive.Checked);
    Write('SpellCheckDlg/CamelCase', CheckBoxCamelCase.Checked);
    Write('SpellCheckDlg/UpperCase', CheckBoxUpperCase.Checked);
  end;
  DoSettingsNotify(Self);
  Close;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckBoxSpellCheckActiveChange
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.CheckBoxSpellCheckActiveChange(Sender: TObject);
begin
  if CheckBoxSpellCheckActive.Checked then
  begin
    ComboBoxActiveLng.Enabled   := True;
    LabelActiveLng.Enabled      := True;
    CheckBoxCamelCase.Enabled   := True;
    CheckBoxUpperCase.Enabled := True;
  end
  else
  begin
    ComboBoxActiveLng.Enabled   := False;
    LabelActiveLng.Enabled      := False;
    CheckBoxCamelCase.Enabled   := False;
    CheckBoxUpperCase.Enabled := False;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonCancelClick
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.ButtonCancelClick(Sender: TObject);
begin
  CLose;
end;


{-------------------------------------------------------------------------------
  @NAME: CheckBoxCamelCaseChange
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.CheckBoxCamelCaseChange(Sender: TObject);
begin

end;


{-------------------------------------------------------------------------------
  @NAME: CheckBoxUpperCaseChange
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.CheckBoxUpperCaseChange(Sender: TObject);
begin

end;


{-------------------------------------------------------------------------------
  @NAME: ComboBoxActiveLngChange
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.ComboBoxActiveLngChange(Sender: TObject);
begin

end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFormSpellCheckDlg.OnReplaceLng();
begin
  LCap('T_FormSpellCheckDlg_Title',                    Self);
  LCap('T_FormSpellCheckDlg_CheckBoxSpellCheckActive', CheckBoxSpellCheckActive);
  LCap('T_FormSpellCheckDlg_CheckBoxCamelCase',        CheckBoxCamelCase);
  LCap('T_FormSpellCheckDlg_CheckBoxUpperCase',        CheckBoxUpperCase);
  LCap('T_FormSpellCheckDlg_LabelActiveLng',           LabelActiveLng);
  LCap('T_General_BtnOk',                              ButtonOk);
  LCap('T_General_BtnCancel',                          ButtonCancel);
end;



end.

