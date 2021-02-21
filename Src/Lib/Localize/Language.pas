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

 *******************************************************************************

    Diese Datei ist Teil des Programms MdEdit.

    Ist Teil Language Bibliothek. Stellt die nach außen Sichtbaren
    Klassenfunktionen zur dynamischen Sprachunschaltung zur Verfügung.

 *******************************************************************************
}
unit Language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, LangParse, LazMethodList, Controls,
  Menus, ActnList, LResources;

type
  TLStr = function (token: String; fallback: String): String of object;
  TLCls = procedure (token: String; control: TObject) of object;
  TLCHt = procedure (token: String; control: TObject) of object;

  TLCap = procedure (token: String; control: TObject) of object;
  TLHnt = procedure (token: String; control: TObject) of object;


type
  TNotify = procedure() of object;

  pString = ^string;

  {TMapString}

  TMapString = class(TFPStringHashTable)
  private
    { private declarations }
    function HasKey(key: string): boolean;

  end;


  {TLanguage}

  TLanguage = class(TObject)
  private
    { private declarations }
    FLngHash: TFPStringHashTable;
    FLngFile: string;
    FUsedLng: string;
    FLngList: TObjectList;
    FListeners: TMethodList;
    FAppLocation: string;

    function LoadResFile(): TStream;
    function GetCode(Index: Integer): string;
    function GetName(Index: Integer): string;
    function GetCount(): Integer;
    function LoadFromLazarusResource(const ResName: String): TStringStream;
    function GetApplicationPath(): string;

  public
    { public declarations }
    constructor Create();
    procedure SetLngFile(const FileName: string);
    procedure SetUsedLng(const UsedLng: string);
    function GetUsedLng(): string;
    function GetCodeIndex(): integer;
    function ParseLngFile(): boolean;
    function ParseResFile(): boolean;
    function LngFound(): boolean;
    function HasTag(key: string): boolean;
    function FindTag(key: string): string;
    function Assign(Text: string; key: string): string;
    procedure AddTag(key: string; Value: string);
    property Count: Integer read GetCount;
    property Code[Index: Integer]: string read GetCode;
    property Name[Index: Integer]: string read GetName;
    procedure AddListener(const Handler: TNotify);
    procedure SendEventToListener();
    procedure ExportLngFile(lngFile: string);
    procedure ExportLngFile();
    function LoadLngFile(lngFile: string): Boolean;
    property AppLocation: string read FAppLocation write FAppLocation;

    function GetString({%H-}token, fallback: String): String;
    procedure GetCaption({%H-}token: String; control: TObject);
    procedure GetHint({%H-}token: String; control: TObject);

  end;


var
   OLng: TLanguage = nil;

   LStr: TLStr = Nil;
   LCap: TLCap = Nil;
   LHnt: TLHnt = Nil;


function GetLngInstance(): TLanguage;


implementation


{TLanguage}

{-------------------------------------------------------------------------------
  Class Create in TLanguage
 ------------------------------------------------------------------------------}
constructor TLanguage.Create();
begin
  If Assigned(OLng) then
    Raise Exception.Create('Please use the global variable OLanguage');
  inherited Create;
  FListeners := TMethodList.Create;
  FLngHash := TFPStringHashTable.Create;

  LStr := @GetString;
  LCap := @GetCaption;
  LHnt := @GetHint;
end;


{-------------------------------------------------------------------------------
  Class Singleton Instance of TLanguage
 ------------------------------------------------------------------------------}
function GetLngInstance() : TLanguage;
begin
  If(OLng = nil) then
  raise Exception.Create('OLanguage not created during initialization.');
  Result := OLng;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetLngFile()
 @ PARAM:   FileName: Name der Datei, die als Sprachdatei geladen werden soll
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.SetLngFile(const FileName: string);
begin
  FLngFile := FileName;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetUsedLng()
 @ PARAM:   UsedLng: Name der Sprache die benutzt werden soll
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * übereinstimmen
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.SetUsedLng(const UsedLng: string);
begin
  // TODO: prüfen, ob Sprache existiert
  FUsedLng := UsedLng;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetUsedLng()
 @ PARAM:   UsedLng: Name der Sprache die benutzt werden soll
 *
 @ INFO:
 * Gind den Language Code der gerade benutzten Sprache zurück.
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetUsedLng(): string;
begin
  Result := FUsedLng;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetUsedLng()
 @ PARAM:   UsedLng: Name der Sprache die benutzt werden soll
 *
 @ INFO:
 * Gind den Language Code der gerade benutzten Sprache zurück.
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetCodeIndex(): integer;
var
  i: integer;
begin
  Result := -1;
  for i:=0 to OLng.Count - 1 do
    if Code[i] = FUsedLng then Result := i;
end;


{-------------------------------------------------------------------------------
  GetString
 ------------------------------------------------------------------------------}
function TLanguage.GetString(token, fallback: String): String;
begin
  if not LngFound() then exit;
  Result := OLng.Assign(fallback, token);
end;


{-------------------------------------------------------------------------------
  GetCaption
 ------------------------------------------------------------------------------}
procedure TLanguage.GetCaption(token: String; control: TObject);
var
  obj: TClass;
begin
  if not LngFound() then exit;
  if control = Nil  then exit;

  if control.ClassType = TAction.ClassType then
  begin
    TAction(control).Caption := OLng.Assign(TAction(control).Caption, token);
    exit;
  end;
  if control.ClassType = TMenuItem.ClassType then
  begin
    TMenuItem(control).Caption := OLng.Assign(TMenuItem(control).Caption, token);
    exit;
  end;
  obj := control.ClassType;
  while(Not(obj = Nil)) do
  begin
    if obj = TControl.ClassType then
      TControl(control).Caption := OLng.Assign(TControl(control).Caption, token);
    obj := obj.ClassParent();
  end;
end;


{-------------------------------------------------------------------------------
  GetHint
 ------------------------------------------------------------------------------}
procedure TLanguage.GetHint(token: String; control: TObject);
var
  obj: TClass;
begin
  if not LngFound() then exit;
  if control = Nil  then exit;

  if control.ClassType = TAction then
  begin
    TAction(control).Hint := OLng.Assign(TAction(control).Hint, token);
    exit;
  end;
  if control.ClassType = TMenuItem then
  begin
    TMenuItem(control).Hint := OLng.Assign(TMenuItem(control).Hint, token);
    exit;
  end;
  obj := control.ClassType;
  while(Not(obj = Nil)) do
  begin
    if obj = TControl.ClassType then
      TControl(control).Hint := OLng.Assign(TControl(control).Hint, token);
    obj := obj.ClassParent();
  end;
 end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    ParseLngFile()
 @ RESULT:  Bei Erfolg true sonst false
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * übereinstimmen
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.ParseLngFile(): boolean;
var
  OLangParse: TLangParse;
begin
  if FLngHash <> nil then FLngHash.Free;

  FLngHash := TFPStringHashTable.Create;
  OLangParse := TLangParse.Create(FUsedLng, FLngHash);
  FLngList := OLangParse.GetLanguages();

  Result := LoadLngFile(FLngFile);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    ParseResFile()
 @ RESULT:  Bei Erfolg true sonst false
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * übereinstimmen
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.ParseResFile(): boolean;
var
  OLangParse: TLangParse;
  Stream: TStream;
begin
  if FLngHash <> nil then FLngHash.Free;

  FLngHash := TFPStringHashTable.Create;
  OLangParse := TLangParse.Create(FUsedLng, FLngHash);
  FLngList := OLangParse.GetLanguages();

  Stream := LoadResFile();
  if Stream = nil then
  begin
    Result := false; exit;
  end;
  Stream.Position := 0;
  OLangParse.SetStream(Stream);
  OLangParse.ParseXml();
  Stream.Free;

  FLngList := OLangParse.GetLanguages();
  Result := true;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    LoadFile()
 @ RESULT:  Stream der geoeffneten Datei
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.LoadResFile(): TStream;
begin
  Result := LoadFromLazarusResource('LngXml');
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    LngFound()
 @ RESULT:  Bei Erfolg true sonst false
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * uebereinstimmen
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.LngFound(): boolean;
begin
  Result := false;
  if FLngHash.Count > 0 then;
  begin
    Result := true;
  end
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    HasTag()
 @ PARAM:   key: Name der Sprache die benutzt werden soll
 @ RESULT:  bei Erfolg true, sonst false
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * übereinstimmen
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.HasTag(key: string): boolean;
begin
  Result := TMapString(FLngHash).HasKey(key);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    FindTag()
 @ PARAM:   key: Name der Sprache die benutzt werden soll
 @ RESULT:  String der Sprache der unter dem Key gefunden wurde
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.FindTag(key: string): string;
begin
  Result := FLngHash.Items[key];
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    Assign()
 @ PARAM:   String des aktuellen Caption
 @ PARAM:   key: Name der Sprache die benutzt werden soll
 @ RESULT:  Bei erfolgter Zuweisung String des Keys sonst alten Caption
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.Assign(Text: string; key: string): string;
begin
  Result := Text;
  if Not OLng.HasTag(key) then exit;
  Result := OLng.FindTag(key);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    AddTag()
 @ PARAM:   key: Name der Sprache die benutzt werden soll
 @ PARAM:   Value: Inhalt als String
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.AddTag(key: string; Value: string);
begin
  FLngHash.Add(key, Value);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetCode()
 @ PARAM:   Index: Nummer der Sprache
 @ RESULT:  Gibt den Language-Code [de_DE, usw.] zurück
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetCode(Index: Integer): string;
var
  StrList: TStringList;
begin
  StrList := TStringList(FLngList.Items[0]);
  if StrList.Count <= Index then begin Result := ''; exit; end;
  Result :=StrList[index];
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetName()
 @ PARAM:   Index: Nummer der Sprache
 @ RESULT:  String mit dem Namen der Sprache
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetName(Index: Integer): string;
var
  StrList: TStringList;
begin
  StrList := TStringList(FLngList.Items[1]);
  if StrList.Count <= Index then begin Result := ''; exit; end;
  Result :=StrList[index];
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetCount()
 @ RESULT:  Gibt die Anzahl der verfügbaren Sprachen zurück
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetCount(): Integer;
var
  StrList: TStringList;
begin
  StrList := TStringList(FLngList.Items[0]);
  Result  := StrList.Count;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    AddListener()
 @ PARAM:   Handler: Referenz auf die Funktion, die der Listener-Liste
 *          hinzugefügt werden soll
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.AddListener(const Handler: TNotify);
begin
  FListeners.Add(TMethod(Handler));
  TNotify(TMethod(Handler))();
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SentEventToListener()
 *
 @ INFO:
 @ Ruft alle Fuktionen auf, die in der Listener-Liste enthalten sind.
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.SendEventToListener();
var
  i: integer;
  Handler: TMethod;
begin
  for i := 0 to FListeners.Count -1 do
  begin
    Handler := FListeners[i];
    TNotify(Handler)();
  end;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:  ExportLngFile()
 @ PARAM: fileName: Name und Zielverzeichnis der zu exportierenden Datei
 *
 @ INFO:
 * Exportiert die Language-XML-Sruktur in eine Datei.
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.ExportLngFile(lngFile: string);
var
  fileName: string;
  filePath: string;
  MemStream: TMemoryStream;
  StrStream: TStringStream;
begin
  filePath := ExtractFilePath(lngFile);
  fileName := ExtractFileName(lngFile);

  SetLngFile(lngFile);
  if FileExists(filePath + fileName) then
   begin
      ShowMessage('Error: File already exists:' + #13#10#13#10 + filePath + fileName);
      exit;
   end;

   try
      StrStream := LoadFromLazarusResource('LngXml');
      MemStream := TMemoryStream.Create;
      StrStream.Position:=0;
      MemStream.CopyFrom(StrStream, StrStream.Size);
      MemStream.SaveToFile(filePath + fileName);
   except
      begin
         ShowMessage('Error: File can not saved:' + #13#10#13#10 + filePath + fileName);
         exit;
      end;
   end;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME: ExportLngFile()
 *
 @ INFO:
 * Exportiert die einkompilierte Lng.xml in das Verzeichnis der Applikation.
 *
 * ---------------------------------------------------------------------------*)
procedure TLanguage.ExportLngFile();
var
  fileName: string;
  filePath: string;
begin
  filePath := ExtractFilePath(FLngFile);
  fileName := ExtractFileName(FLngFile);
  if filePath = '' then filePath := GetApplicationPath();

  ExportLngFile(filePath + fileName);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   LoadLngFile()
 @ PARAM:  fileName: Name und Quellverzeichnis der zu ladenden Datei
 @ RESULT: True wenn OK
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.LoadLngFile(lngFile: string): Boolean;
var
  Stream: TMemoryStream;
  fileName: string;
  filePath: string;
  OLangParse: TLangParse;
begin
  if FLngHash <> nil then FLngHash.Free;

  FLngHash := TFPStringHashTable.Create;
  OLangParse := TLangParse.Create(FUsedLng, FLngHash);

  FLngList := OLangParse.GetLanguages();
  filePath := ExtractFilePath(lngFile);
  fileName := ExtractFileName(lngFile);

  if not FileExists(filePath + fileName) then
  begin
    Result:=false; exit;
  end;

  try
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(filePath + fileName);
  except
    Stream.Free;
    // TODO: Message: Beim Laden der Datei ... ist ein Fehler aufgetreten.
    Result := False; exit;
  end;
  if Stream = nil then
  begin
    Result := false; exit;
  end;
  FLngFile := filePath + fileName;

  Stream.Position := 0;
  OLangParse.SetStream(Stream);
  OLangParse.ParseXml();
  Stream.Free;

  Result := True;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetApplicationPath()
 @ RESULT: Gibt einen String mit dem Pfad der Anwendung zurück.
 *
 @ INFO:
 * Unter Linux, Windows oder MAC gibt es unterschidliche Strategien um an den
 * Pfad zu gelangen.
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.GetApplicationPath(): string;
begin
  {$IFDEF BSD}
  Result := ExtractFileDir(ParamStr(0)) + '/';
  {$ELSE}
  Result := AppLocation;
  {$ENDIF}
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    LoadFromLazarusResource()
 @ PARAM:   ResName: Name des Resource-Elements in der Resource-Datei
 @ RESULT:  Stream der Ressource
 *
 @ INFO:
 * Das Resource-Konzept von Windows wird von Lazrus so nicht unterstützt, da
 * auf anderen Plattformen keine Resourcen an das Binary gelinkt werden können.
 * Unter Lazarus werden die Resoucen mit einkompiliert. Die Resourcen befinden
 * sich in "Ressources.lrs"
 *
 * Einbinden der "Ressources.lrs":
 *
 * initialization
 *    {$I Resources.lrs}
 *
 * ---------------------------------------------------------------------------*)
function TLanguage.LoadFromLazarusResource(const ResName: String): TStringStream;
var
  Stream: TLazarusResourceStream;
  F: TStringStream;
begin
  Stream := nil;
  // TODO: Vereinfachen und gleich TMomoryStream zurückgeben
  try
    Stream := TLazarusResourceStream.Create(ResName, nil);
    Stream.Position:=0;
    F:= TStringStream.Create('');
    Stream.SaveToStream(F);
    F.Position:=0;
  finally
    Stream.Free;
  end;
  Result :=  F;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    HasKey()
 @ PARAM:   key: String des Key-Words
 @ RESULT:  Wenn der Key existiert, dann true, sonst false
 *
 @ INFO:
 * Diese Funktion ist ein Workaround, da es diese Funktionalität in
 * TFPStringHashTable nicht gibt.
 *
 * ---------------------------------------------------------------------------*)
function TMapString.HasKey(key: string): boolean;
var
  chn: TFPObjectList;
  i: Longword;
begin
  chn := Chain(HashFunction(Key, HashTableSize));
  if (not Assigned(chn)) or (chn.count <= 0) then
  begin
    Result := false; exit;
  end;

  for i := 0 to chn.Count - 1 do
  if THTCustomNode(chn[i]).HasKey(key) then
  begin
    Result := true; exit;
  end;
  Result := false;
end;



initialization

  OLng := TLanguage.Create;

  {$I Resources/Language.lrs}

end.

