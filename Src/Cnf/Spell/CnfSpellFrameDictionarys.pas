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
unit CnfSpellFrameDictionarys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls,

  FrameSettingsBase, Dialogs, Buttons, Language;

type

  { TFrameDictionarys }

  TFrameDictionarys = class(TFrameSettingsBase)
    BitBtnUsrDw: TBitBtn;
    BitBtnHunUp: TBitBtn;
    BitBtnHunDw: TBitBtn;
    BitBtnUsrUp: TBitBtn;
    ButtonHunAdd: TButton;
    ButtonHunEdit: TButton;
    ButtonHunDel: TButton;
    ButtonUsrAdd: TButton;
    ButtonUsrEdit: TButton;
    ButtonUsrDel: TButton;
    ImageListFrameDict: TImageList;
    LabelDicHunspell: TLabel;
    LabelDicUser: TLabel;
    ListBoxHun: TListBox;
    ListBoxUsr: TListBox;
    OpenDialogUsr: TOpenDialog;
    OpenDialogHun: TOpenDialog;
    procedure BitBtnHunDwClick(Sender: TObject);
    procedure BitBtnHunUpClick(Sender: TObject);
    procedure BitBtnUsrDwClick(Sender: TObject);
    procedure BitBtnUsrUpClick(Sender: TObject);
    procedure ButtonUsrAddClick(Sender: TObject);
    procedure ButtonUsrEditClick(Sender: TObject);
    procedure ButtonUsrDelClick(Sender: TObject);
    procedure ButtonHunAddClick(Sender: TObject);
    procedure ButtonHunEditClick(Sender: TObject);
    procedure ButtonHunDelClick(Sender: TObject);
    procedure ListBoxHunDblClick(Sender: TObject);
    procedure ListBoxHunSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure ListBoxUsrDblClick(Sender: TObject);
    procedure ListBoxUsrSelectionChange(Sender: TObject; {%H-}User: boolean);

  public
    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
    procedure SetSettings();override;
    procedure GetSettings();override;
    Function  GetTreePath: TStrArray override;

  private
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}


{ TFrameDictionarys }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TFrameDictionarys.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OLng.AddListener(@OnReplaceLng);
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TFrameDictionarys.Destroy;
begin
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: SetSettings
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.SetSettings();
var
  items, itemId : Integer;
  dicfile  : string;
begin
  ListBoxHunSelectionChange(Self, false);
  ListBoxUsrSelectionChange(Self, false);

  ListBoxHun.Items.Clear;
  items := Settings.Read('DictHunspell/Items', 0);
  for itemId := 1 To items do
  begin
    dicfile := Settings.Read('DictHunspell/Item_' + itemId.ToString + '/File', '');
    ListBoxHun.AddItem(dicfile, Nil);
  end;

  ListBoxUsr.Items.Clear;
  items := Settings.Read('DictUser/Items', 0);
  for itemId := 1 To items do
  begin
    dicfile := Settings.Read('DictUser/Item_' + itemId.ToString + '/File', '');
    ListBoxUsr.AddItem(dicfile, Nil);
  end;
  ListBoxHunSelectionChange(Self, false);
  ListBoxUsrSelectionChange(Self, false);
end;


{-------------------------------------------------------------------------------
  @NAME: GetSettings
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.GetSettings();
var
  itemId   : Integer;
  dicfile  : string;
  fileName : string;
begin
  itemId := 1;
  Settings.Write('DictHunspell/Items', ListBoxHun.Items.Count);
  for itemId := 1 To ListBoxHun.Items.Count do
  begin
    dicfile:= ListBoxHun.Items[itemId-1];
    fileName := ExtractFileName(dicfile);
    Settings.Write('DictHunspell/Item_' + itemId.ToString + '/File', dicfile);
    Settings.Write('DictHunspell/Item_' + itemId.ToString + '/Name', fileName);
  end;

  itemId := 1;
  Settings.Write('DictUser/Items', ListBoxUsr.Items.Count);
   for itemId := 1 To ListBoxUsr.Items.Count do
  begin
    dicfile:= ListBoxUsr.Items[itemId-1];
    fileName := ExtractFileName(dicfile);
    Settings.Write('DictUser/Item_' + itemId.ToString + '/File', dicfile);
    Settings.Write('DictUser/Item_' + itemId.ToString + '/Name', fileName);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: GetTreePath
 ------------------------------------------------------------------------------}
Function  TFrameDictionarys.GetTreePath: TStrArray;
var
  ary: Array[0..1] of String = ('Rechtschreibung', 'Wöterbücher');
begin
  ary[0] := LStr('T_FrameSpelling_TreeSpelling',        ary[0]);
  ary[1] := LStr('T_FrameSpellingDictionarys_TreeDict', ary[1]);
  Result := ary;
end;


{-------------------------------------------------------------------------------
  @NAME: ListBoxHunDblClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ListBoxHunDblClick(Sender: TObject);
begin
  ButtonHunEditClick(Sender);
end;


{-------------------------------------------------------------------------------
  @NAME: ListBoxHunSelectionChange
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ListBoxHunSelectionChange(Sender: TObject; User: boolean);
var
  itemId: Integer;
begin
  if ListBoxHun.SelCount > 0 then
  begin
    ButtonHunEdit.Enabled := True;
    ButtonHunDel.Enabled  := True;

    for itemId := 0 To ListBoxHun.Items.Count - 1 do
    begin
      if ListBoxHun.Selected[itemId] then
      begin
        if itemId > 0 then BitBtnHunUp.Enabled := True
        else BitBtnHunUp.Enabled := False;
        if itemId < (ListBoxHun.Count-1) then BitBtnHunDw.Enabled := True
        else BitBtnHunDw.Enabled := False;
        break;
      end;
    end;
  end
  else
  begin
    ButtonHunEdit.Enabled := False;
    ButtonHunDel.Enabled  := False;
    BitBtnHunUp.Enabled   := False;
    BitBtnHunDw.Enabled   := False;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonHunAddClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonHunAddClick(Sender: TObject);
var
  filename : string;
begin
  OpenDialogHun.FilterIndex := 1;
  OpenDialogHun.Options := [ofEnableSizing];

  if OpenDialogHun.Execute then
  begin
    filename     := OpenDialogHun.Filename;
//    mNewFilePath := ExtractFilePath(filename);
//    mNewFileName := ExtractFileName(filename);
//    mNewFileExtd := LowerCase( ExtractFileExt(filename) );
    ListBoxHun.AddItem(filename, Nil);
  end
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonHunEditClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonHunEditClick(Sender: TObject);
var
  dicfile  : string;
  itemId   : Integer;
  filePath : string;
  fileName : string;
//  fileExtd : string;
begin
  if ListBoxHun.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxHun.Items.Count - 1 do
    begin
      if ListBoxHun.Selected[itemId] then
      begin
        dicfile:= ListBoxHun.Items[itemId];
        break;
      end;
    end;
  end;
  filePath := ExtractFilePath(dicfile);
  fileName := ExtractFileName(dicfile);

  OpenDialogHun.FileName := fileName;
  OpenDialogHun.InitialDir:= filePath;
  OpenDialogHun.FilterIndex := 1;
  OpenDialogHun.Options := [ofEnableSizing];

  if OpenDialogHun.Execute then
  begin
    dicfile     := OpenDialogHun.Filename;
//    filePath := ExtractFilePath(filename);
//    fileName := ExtractFileName(filename);
//    fileExtd := LowerCase( ExtractFileExt(filename) );
    ListBoxHun.Items[itemId] := dicfile;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ComboBoxActiveLngChange
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonHunDelClick(Sender: TObject);
begin
  ListBoxHun.DeleteSelected;
  ListBoxHunSelectionChange(Self, false);
end;


{-------------------------------------------------------------------------------
  @NAME: BitBtnHunUpClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.BitBtnHunUpClick(Sender: TObject);
var
  itemId: Integer;
begin
  if ListBoxHun.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxHun.Items.Count - 1 do
    begin
      if ListBoxHun.Selected[itemId] then
      begin
        if itemId > 0 then
        begin
          ListBoxHun.Items.Move(itemId, itemId-1);
          ListBoxHun.Selected[itemId-1] := true;
        end;
        break;
      end;
    end;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: BitBtnHunDwClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.BitBtnHunDwClick(Sender: TObject);
var
  itemId: Integer;
begin
  if ListBoxHun.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxHun.Items.Count - 1 do
    begin
      if ListBoxHun.Selected[itemId] then
      begin
        if itemId < (ListBoxHun.Count-1) then
        begin
          ListBoxHun.Items.Move(itemId, itemId+1);
          ListBoxHun.Selected[itemId+1] := true;
        end;
        break;
      end;
    end;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: ListBoxUsrDblClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ListBoxUsrDblClick(Sender: TObject);
begin
  ButtonUsrEditClick(Sender);
end;


{-------------------------------------------------------------------------------
  @NAME: ListBoxUsrSelectionChange
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ListBoxUsrSelectionChange(Sender: TObject; User: boolean);
var
  itemId: Integer;
begin
  if ListBoxUsr.SelCount > 0 then
  begin
    ButtonUsrEdit.Enabled := True;
    ButtonUsrDel.Enabled  := True;

    for itemId := 0 To ListBoxUsr.Items.Count - 1 do
    begin
      if ListBoxUsr.Selected[itemId] then
      begin
        if itemId > 0 then BitBtnUsrUp.Enabled := True
        else BitBtnUsrUp.Enabled := False;
        if itemId < (ListBoxUsr.Count-1) then BitBtnUsrDw.Enabled := True
        else BitBtnUsrDw.Enabled := False;
        break;
      end;
    end;
  end
  else
  begin
    ButtonUsrEdit.Enabled := False;
    ButtonUsrDel.Enabled  := False;
    BitBtnUsrUp.Enabled   := False;
    BitBtnUsrDw.Enabled   := False;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonUsrAddClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonUsrAddClick(Sender: TObject);
var
  filename : string;
begin
  OpenDialogUsr.FilterIndex := 1;
  OpenDialogUsr.Options := [ofEnableSizing];

  if OpenDialogUsr.Execute then
  begin
    filename     := OpenDialogUsr.Filename;
//    mNewFilePath := ExtractFilePath(filename);
//    mNewFileName := ExtractFileName(filename);
//    mNewFileExtd := LowerCase( ExtractFileExt(filename) );
    ListBoxUsr.AddItem(filename, Nil);
  end
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonUsrEditClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonUsrEditClick(Sender: TObject);
var
  dicfile  : string;
  itemId   : Integer;
  filePath : string;
  fileName : string;
//  fileExtd : string;
begin
  if ListBoxUsr.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxUsr.Items.Count - 1 do
    begin
      if ListBoxUsr.Selected[itemId] then
      begin
        dicfile:= ListBoxUsr.Items[itemId];
        break;
      end;
    end;
  end;
  filePath := ExtractFilePath(dicfile);
  fileName := ExtractFileName(dicfile);

  OpenDialogUsr.FileName := fileName;
  OpenDialogUsr.InitialDir:= filePath;
  OpenDialogUsr.FilterIndex := 1;
  OpenDialogUsr.Options := [ofEnableSizing];

  if OpenDialogUsr.Execute then
  begin
    dicfile     := OpenDialogUsr.Filename;
//    filePath := ExtractFilePath(filename);
//    fileName := ExtractFileName(filename);
//    fileExtd := LowerCase( ExtractFileExt(filename) );
    ListBoxUsr.Items[itemId] := dicfile;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonUsrDelClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.ButtonUsrDelClick(Sender: TObject);
begin
  ListBoxUsr.DeleteSelected;
  ListBoxUsrSelectionChange(Self, false);
end;


{-------------------------------------------------------------------------------
  @NAME: BitBtnUsrUpClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.BitBtnUsrUpClick(Sender: TObject);
var
  itemId: Integer;
begin
  if ListBoxUsr.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxUsr.Items.Count - 1 do
    begin
      if ListBoxUsr.Selected[itemId] then
      begin
        if itemId > 0 then
        begin
          ListBoxUsr.Items.Move(itemId, itemId-1);
          ListBoxUsr.Selected[itemId-1] := true;
        end;
        break;
      end;
    end;
  end
end;


{-------------------------------------------------------------------------------
  @NAME: BitBtnUsrDwClick
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.BitBtnUsrDwClick(Sender: TObject);
var
  itemId: Integer;
begin
  if ListBoxUsr.SelCount > 0 then
  begin
    for itemId := 0 To ListBoxUsr.Items.Count - 1 do
    begin
      if ListBoxUsr.Selected[itemId] then
      begin
        if itemId < (ListBoxUsr.Count-1) then
        begin
          ListBoxUsr.Items.Move(itemId, itemId+1);
          ListBoxUsr.Selected[itemId+1] := true;
        end;
        break;
      end;
    end;
  end
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFrameDictionarys.OnReplaceLng();
begin
  LCap('T_FrameDictionarys_LabelDicHunspell', LabelDicHunspell);
  LCap('T_FrameDictionarys_ButtonHunAdd',     LabelDicUser);

  LCap('T_General_BtnAdd',  ButtonHunAdd);
  LCap('T_General_BtnEdit', ButtonHunEdit);
  LCap('T_General_BtnDel',  ButtonHunDel);
  LCap('T_General_BtnAdd',  ButtonUsrAdd);
  LCap('T_General_BtnEdit', ButtonUsrEdit);
  LCap('T_General_BtnDel',  ButtonUsrDel);
end;



end.

