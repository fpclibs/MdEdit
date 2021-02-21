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

  FormBase ist die Basisklasse dieser Anwendung, von der alle weiteren Forms
  abgeleitet sind. Die Hauptaufgabe ist das Persistieren aller Einstellungen.

 *******************************************************************************
}
unit FormBase;

{$mode objfpc}{$H+}

interface

uses
   Classes, Forms, Dialogs, Contnrs, SysUtils, LazFileUtils,
   FileUtil, LazUTF8, md5,

  {$IFDEF WINDOWS}
  WinDirs,
  {$ENDIF}

  Language;

type
   ConfigType = (Init, User);

   TAnsiString = class(TObject)

      constructor Create();
      constructor Create(Value : AnsiString);

   private
      { private declarations }
      FText : AnsiString;

   public
      { public declarations }
      property Text: AnsiString read FText write FText;
   end;


   TBoolean = class(TObject)

      constructor Create();
      constructor Create(Value : boolean);
      constructor Create(Value : AnsiString);

   private
      { private declarations }
      FFlag : boolean;
      procedure SetText(Value : AnsiString);
      function GetText() : AnsiString;

   public
      { public declarations }
      property Flag: boolean read FFlag write FFlag;
      property Text: AnsiString read GetText write SetText;

   end;


   { TFormBase }

   TFormBase = class(TForm)

      procedure FormCreate(Sender: TObject);

   private
      { private declarations }
      function CreateDirWide(const NewDir: string): Boolean;
      procedure LoadLanguage();

   protected

   public
      { public declarations }
      AppLocation: string; static;

      ShouldDestroy: Boolean; static;
      Data: TFPObjectHashTable; static;
      procedure Close();

      function Md5ConfigDir(): String;
      function EncodeUrl(url: string): string;
      function DecodeUrl(url: string): string;
      Function GetConfigDir({%H-}global : Boolean) : String;
      function GetPrimaryConfigPath(): string;
   end;


type
  THash = TFPObjectHashTable;
  TAnsi = TAnsiString;
  TBool = TBoolean;

   function SysToUTF8( str: String ): String;

var
  OFormBase : TFormBase;
  OAppName  : String =  '';

implementation

{$R *.lfm}

uses FormMain;


{$IFDEF VER3}
{-------------------------------------------------------------------------------
@ NAME:    SysToUTF8
@ PARAM:   str: String
@ RESULT:  String des Eingangsparaneters str
*
@ INFO:
* Platzhalter-Funktion ab LCL 1.6.0
 ------------------------------------------------------------------------------}
function SysToUTF8( str: String ): String;
begin
   Result := str;
end;
{$ENDIF}


{ TAnsiString }

{-------------------------------------------------------------------------------
  Class Create
 ------------------------------------------------------------------------------}
constructor TAnsiString.Create();
begin
   Text := '';
end;


{-------------------------------------------------------------------------------
  Class Create in TAnsiString
 ------------------------------------------------------------------------------}
constructor TAnsiString.Create(value : AnsiString);
begin
   Text := value;
end;


{ TBoolean }

{-------------------------------------------------------------------------------
  Class Create in TBoolean
 ------------------------------------------------------------------------------}
constructor TBoolean.Create();
begin
   Text := '';
end;


{-------------------------------------------------------------------------------
  Class Create in TBoolean
 ------------------------------------------------------------------------------}
constructor TBoolean.Create(Value : boolean);
begin
   Flag := Value;
end;


{-------------------------------------------------------------------------------
  Class Create in TBoolean
 ------------------------------------------------------------------------------}
constructor TBoolean.Create(Value : AnsiString);
begin
   SetText(Value);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetText()
 @ PARAM:   Value: 'true' als String
 *
 @ INFO:
 * Wird ein anderes Schlüsselwort als 'true' übergeben, dann wird das Flag
 * 'false' gesetzt.
 *
 * ---------------------------------------------------------------------------*)
procedure TBoolean.SetText(Value : AnsiString);
begin
   if (Value = 'true') or (Value = 'True') or (Value = 'TRUE') then
   begin
      Flag := true;
   end
   else
   begin
      Flag := false;
   end;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetText()
 @ RESULT:  Gibt 'true' bzw. 'false' als String zurück.
 *
 * ---------------------------------------------------------------------------*)
function TBoolean.GetText() : AnsiString;
begin
   if Flag = true then begin Result := 'true' end
   else begin Result := 'false' end;
end;


{ TFormBase }

{-------------------------------------------------------------------------------
  Event FormCreate in TFormBase
 ------------------------------------------------------------------------------}
procedure TFormBase.FormCreate(Sender: TObject);
begin
   if Data = nil then
   begin
      LoadLanguage();
   end
end;


{-------------------------------------------------------------------------------
  Event Close in TFormBase
 ------------------------------------------------------------------------------}
procedure TFormBase.Close();
begin
   inherited;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    CreateDirWide()
 @ PARAM:   NewDir: Name des Verzeichnisses, das erstellt werden soll
 @ RESULT:  Boolean, der angibt, ob das Erstellen erfolgreich war
 *
 @ INFO:
 * Außer ASCII können UTF8 Zeichen verwendet werden
 *
 * ---------------------------------------------------------------------------*)
function TFormBase.CreateDirWide(const NewDir: string): Boolean;
begin
  Result := LazFileUtils.CreateDirUtf8(NewDir);
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   LoadLanguage()
 *
 * ---------------------------------------------------------------------------*)
procedure TFormBase.LoadLanguage();
var
   Lng: TLanguage;

begin
   Lng := GetLngInstance();
   Lng.SetLngFile('Lng.xml');
   Lng.SetUsedLng('de_DE');
   Lng.AppLocation:=AppLocation;
   lng.ParseLngFile();
   lng.LngFound();
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetPrimaryConfigPath()
 @ RESULT:  Gibt eine Verzeichnis-Path als String zurück.
 *
 @ INFO:
 * GetPrimaryConfigPath ermittelt den Path, abhängig vin der Plattform,
 * in dem Programme ihre Configuration ablegen könen.
 *
 * ---------------------------------------------------------------------------*)
function TFormBase.GetPrimaryConfigPath(): string;
var
   attr : Longint;
   path : String;
begin
   Result := Application.ExeName;
   if OAppName = '' then
   begin
     OAppName := LazFileUtils.ExtractFileNameWithoutExt(
                   LazFileUtils.ExtractFileNameOnly(
                     LazFileUtils.ChompPathDelim(LazUTF8.SysToUTF8(Result))));
   end;
   Result := GetConfigDir(False)  + OAppName;
   path := LazFileUtils.CreateAbsolutePath(Md5ConfigDir(), Result);

   attr := LazFileUtils.FileGetAttrUtf8(Result);
   if  attr = -1 then
   begin
      if not CreateDirWide(Result) then
      begin
        Result := '';
        exit;
      end;
   end;
   attr := LazFileUtils.FileGetAttrUtf8(path);
   if  attr = -1 then
   begin
      if not CreateDirWide(path) then
      begin
        Result := '';
        exit;
      end;
   end;
   Result := path;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   Md5ConfigDir()
 *
 * ---------------------------------------------------------------------------*)
function TFormBase.Md5ConfigDir(): String;
begin
   Result := MD5Print(MD5String( Application.Location ));
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   EncodeUrl
 *
 * ---------------------------------------------------------------------------*)
function TFormBase.EncodeUrl(url: string): string;
var
  x: integer;
  sBuff: string;
const
  SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-'];
begin
  // Init
  sBuff := '';

  for x := 1 to Length(url) do
  begin
    // Check if we have a safe char
    if url[x] in SafeMask then
    begin
      // Append all other chars
      sBuff := sBuff + url[x];
    end
    else if url[x] = ' ' then
    begin
      // Append space
      sBuff := sBuff + '+';
    end
    else
    begin
      // Convert to hex
      sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
    end;
  end;
  Result := sBuff;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   DecodeUrl
 *
 * ---------------------------------------------------------------------------*)
function TFormBase.DecodeUrl(url: string): string;
var
  x: integer;
  ch: string;
  sVal: string;
  Buff: string;
begin
  //Init
  Buff := '';
  x := 1;
  while x <= Length(url) do
  begin
    // Get single char
    ch := url[x];

    if ch = '+' then
    begin
      // Append space
      Buff := Buff + ' ';
    end
    else if ch <> '%' then
    begin
      // Append other chars
      Buff := Buff + ch;
    end
    else
    begin
      // Get value
      sVal := Copy(url, x + 1, 2);
      // Convert sval to int then to char
      Buff := Buff + char(StrToInt('$' + sVal));
      //Inc counter by 2
      Inc(x, 2);
    end;
    // Inc counter
    Inc(x);
  end;
  Result := Buff;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   GetConfigDir
 *
 * ---------------------------------------------------------------------------*)
Function TFormBase.GetConfigDir(global : Boolean) : String;
begin
  Result := '';
  {$IFDEF WINDOWS}
  if global then
      Result := GetWindowsSpecialDir(CSIDL_COMMON_APPDATA)
   else
      Result := GetWindowsSpecialDir(CSIDL_LOCAL_APPDATA);
  {$ENDIF}
  {$IFDEF LINUX}
  Result := GetEnvironmentVariable('HOME');
  if (Result<>'') then
    Result := IncludeTrailingPathDelimiter(Result) + '.';
  {$ENDIF}

   if (Result = '') then
   begin
      Result := IncludeTrailingPathDelimiter(Application.Location);
      if (Result <> '') then
      begin
         Result := ExtractFilePath(LazFileUtils.ChompPathDelim(LazUTF8.SysToUTF8(Result)));
      end;
   end;
end;



end.


