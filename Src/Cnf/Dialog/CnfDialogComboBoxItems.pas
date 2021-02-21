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
unit CnfDialogComboBoxItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, LazFileUtils, StdCtrls, Dialogs, Storage,
  Language, SysUtils;

type
  TChangeEvent = procedure(const filename: String) of object;

  { TCnfSyneditFrameThemes }

  TComboBoxItems = class

  type

  private
    mPath: String;
    mComboBox: TComboBox;
    mSettings: TAppStorage;
    mExt:      String;
    mItemText: String;

    FLoadFile: TChangeEvent;
    FSaveFile: TChangeEvent;

    mOpenDlg:  TOpenDialog;
    mSaveDlg:  TSaveDialog;

  public
    property Path: String read mPath write mPath;
    property ComboBox: TComboBox   read mComboBox write mComboBox;
    property Settings: TAppStorage read mSettings write mSettings;
    property Extension: String read mExt write mExt;

    property OnLoadFile: TChangeEvent read FLoadFile write FLoadFile;
    property OnSaveFile: TChangeEvent read FSaveFile write FSaveFile;

    property LoadDialog: TOpenDialog read mOpenDlg write mOpenDlg;
    property SaveDialog: TSaveDialog read mSaveDlg write mSaveDlg;

  public
    constructor Create;
    destructor Destroy; override;

    procedure FileSet(actfile: string);
    procedure FileAdd(filePath, fileName: String);
    procedure FileDel(filePath, fileName: String);
    procedure FileChange();
    procedure FileLoad();
    procedure FileSave();

  private
    procedure OnReplaceLng();

  end;


implementation

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TComboBoxItems.Create;
begin
  mPath     := '';
  FLoadFile := Nil;
  mOpenDlg  := Nil;
  mSaveDlg  := Nil;
  mItemText := 'letzte Vorlagen ...';
  mComboBox := Nil;

  OLng.AddListener(@OnReplaceLng);
end;


{-------------------------------------------------------------------------------
  Destructor
 ------------------------------------------------------------------------------}
destructor TComboBoxItems.Destroy;
begin

end;


{-------------------------------------------------------------------------------
  FileSet
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileSet(actfile: string);
var
  items, itemId : Integer;
  dicfile: string;
begin

  ComboBox.Text:='';
  ComboBox.Items.Clear;

  ComboBox.AddItem(mItemText, Nil);
  ComboBox.ItemIndex := 0;

  items := Settings.Read(mPath + '/Items', 0);
  for itemId := 1 To items do
  begin
    dicfile := Settings.Read(mPath + '/Item_' + itemId.ToString + '/File', '');
    ComboBox.AddItem(dicfile, Nil);
    if actfile = dicfile then ComboBox.ItemIndex := itemId;
  end;
end;


{-------------------------------------------------------------------------------
  FileAdd
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileAdd(filePath, fileName: String);
var
  idNew : Integer;
  idOld : Integer;
  count : Integer;
  max   : Integer;
  fOldName, fOldPath : string;
  fNewName, fNewPath : string;
begin
  idNew := 0;
  idOld := 1;

  count := Settings.Read(Path + '/Items', 0);
  max   := Settings.Read(Path + '/Max',  20);

  fNewName := fileName;
  fNewPath := LazFileUtils.CreateAbsolutePath(fileName, filePath);

  for idOld := 1 To count+1 do
  begin
    fOldName := Settings.Read(Path + '/Item_' + idOld.ToString + '/Name', '');
    fOldPath := Settings.Read(Path + '/Item_' + idOld.ToString + '/File', '');
    if LazFileUtils.CreateAbsolutePath(fileName, filePath) = fOldPath then
    begin
      Continue;
    end;
    Inc(idNew);
    Settings.Write(Path + '/Item_' + idNew.ToString + '/Name', fNewName);
    Settings.Write(Path + '/Item_' + idNew.ToString + '/File', fNewPath);
    fNewName := fOldName;
    fNewPath := fOldPath;
    if idNew = max then break;
  end;
  Settings.Write(Path + '/Items', idNew);
  Settings.Write(Path + '/Max',     max);
end;


{-------------------------------------------------------------------------------
  FileDel
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileDel(filePath, fileName: String);
var
  idNew : Integer;
  idOld : Integer;
  count : Integer;
  max   : Integer;
  fOldName, fOldPath : string;
begin
  idNew := 0;
  idOld := 1;

  count := Settings.Read(Path + '/Items', 0);
  max   := Settings.Read(Path + '/Max',  20);

  for idOld := 1 To count do
  begin
    fOldName := Settings.Read(Path + '/Item_' + idOld.ToString + '/Name', '');
    fOldPath := Settings.Read(Path + '/Item_' + idOld.ToString + '/File', '');
    if LazFileUtils.CreateAbsolutePath(fileName, filePath) = fOldPath then
    begin
      Continue;
    end;
    Inc(idNew);
    Settings.Write(Path + '/Item_' + idNew.ToString + '/Name', fOldName);
    Settings.Write(Path + '/Item_' + idNew.ToString + '/File', fOldPath);
  end;

  for idOld := count downto idNew+1 do
  begin
    Settings.Delete(Path + '/Item_' + idOld.ToString + '/');
  end;
  if idNew <> count then Dec(idOld);

  Settings.Write(Path + '/Items',   idOld);
  Settings.Write(Path + '/Max',       max);
end;


{-------------------------------------------------------------------------------
  ComboBoxFootFilesChange
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileChange();
var
  newfile  : string;
  filePath : string;
  fileName : string;
begin
  if ComboBox.ItemIndex > 0 then
  begin
    newfile:= ComboBox.Items[ComboBox.ItemIndex];

    filePath := ExtractFilePath(newfile);
    fileName := ExtractFileName(newfile);

    Try
      if FileExists(newfile) then
      begin
        if Assigned(FLoadFile) then
        begin
          FLoadFile(newfile);
        end;
     end
      else
      begin
        ShowMessage(LStr('T_CnfDialogComboBoxItems_FileErrFind',
          'Fehler: Die Datei konnte nicht gefunden werden!'));
        FileDel(filePath, fileName);
        FileSet('');
      end;

    except
      ShowMessage(LStr('T_CnfDialogComboBoxItems_FileErrLoad',
        'Fehler: Die Datei konnte nicht geladen werden!'));
      FileDel(filePath, fileName);
      FileSet('');
      exit;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  FileLoad
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileLoad();
var
  newfile : string;
  filePath : string;
  fileName : string;
begin
  newfile := '';
  if ComboBox.ItemIndex > 0 then
  begin
    newfile := ComboBox.Items[ComboBox.ItemIndex];
  end;
  filePath := ExtractFilePath(newfile);
  fileName := ExtractFileName(newfile);

  if Not Assigned(mOpenDlg) then exit;

  mOpenDlg.FileName := fileName;
  mOpenDlg.InitialDir:= filePath;
  mOpenDlg.FilterIndex := 1;
  mOpenDlg.Options := [ofEnableSizing];

  if mOpenDlg.Execute then
  begin
    newfile  := mOpenDlg.Filename;
    filePath := ExtractFilePath(newfile);
    fileName := ExtractFileName(newfile);

    Try
      if Assigned(FLoadFile) then FLoadFile(newfile);
    except
      exit;
    end;

    FileAdd(filePath, fileName);
    FileSet(newfile);
  end;
end;


{-------------------------------------------------------------------------------
  FileSave
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.FileSave();
var
  newfile  : string;
  filePath : string;
  fileName : string;
  fileExtd : string;
begin
  newfile := '';
  if ComboBox.ItemIndex > 0 then
  begin
    newfile:= ComboBox.Items[ComboBox.ItemIndex];
  end;
  filePath := ExtractFilePath(newfile);
  fileName := ExtractFileName(newfile);

  if Not Assigned(mSaveDlg) then exit;

  mSaveDlg.FileName := fileName;
  mSaveDlg.InitialDir:= filePath;
  mSaveDlg.FilterIndex := 1;
  mSaveDlg.Options := [ofEnableSizing];

  if mSaveDlg.Execute then
  begin
    newfile  := mSaveDlg.Filename;
    filePath := ExtractFilePath(newfile);
    fileName := ExtractFileName(newfile);
    fileExtd := LowerCase( ExtractFileExt(filename) );

    if fileExtd = '' then newfile := newfile + mExt;
    Try
      if Assigned(FSaveFile) then FSaveFile(newfile);
    except
      exit;
    end;

    FileAdd(filePath, fileName);
    FileSet(newfile);
  end;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TComboBoxItems.OnReplaceLng();
begin
   mItemText := LStr('T_CnfDialogComboBoxItems_ItemText', mItemText);

   if Not (mComboBox = Nil) then FileSet(ComboBox.Items[ComboBox.ItemIndex]);
end;



end.

