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

    A Unit to connect to the hunspell library and check some spelling.
    First, create the class, it will try and find a library to load.
    Check ErrorMessage.
    Then call SetDictionary(), with a full filename of the dictionary to use.
    If GoodToGo is true, you can call Spell() and Suggests()
    otherwise, look in ErrorString for what went wrong.

    Look in FindLibrary() for default locations of Library.

 *******************************************************************************
}
unit Hunspell;

{$MODE objfpc}{$H+}

interface
uses Classes, DynLibs, LConvEncoding;

type

  THunspell_create    = function(aff_file: PChar; dict_file: PChar): Pointer; cdecl;
  THunspell_destroy   = procedure(spell: Pointer); cdecl;
  THunspell_spell     = function(spell: Pointer; word: PChar): Boolean; cdecl;
  THunspell_suggest   = function(spell: Pointer; out slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_analyze   = function(spell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_stem      = function(spell: Pointer; var slst: PPChar; word: PChar): Integer; cdecl;
  THunspell_free_list = procedure(spell: Pointer; var slst: PPChar; n: integer); cdecl;
  THunspell_get_dic_encoding = function(spell: Pointer): PChar; cdecl;
  THunspell_add       = function(spell: Pointer; word: PChar): Integer; cdecl;
  THunspell_remove    = function(spell: Pointer; word: PChar): Integer; cdecl;

   { THunspell }

  THunspell = class
  private
    Speller: Pointer;
        { Loads indicated library, returns False and sets ErrorMessage if something wrong }
    function LoadHunspellLibrary(LibraryName: AnsiString): Boolean;
  public
            { set to True if speller is ready to accept requests }
    GoodToGo : boolean;
            { empty if OK, contains an error message if something goes wrong }
    ErrorMessage : ANSIString;
            { Will have a full name to library if correctly loaded at create }
    LibraryFullName : string;
            { Will have a "first guess" as to where dictionaries are, poke another name in
            and call FindDictionary() if default did not work }
    constructor Create();
    destructor Destroy; override;
            { Returns True if word spelt correctly }
    function Spell(Word: string): boolean;
            { Returns with List full of suggestions how to spell Word }
    procedure Suggest(Word: string; List: TStrings);
            { untested }
    procedure Add(Word: string);
            { untested }
    procedure Remove(Word: string);
            { returns a full library name or '' if it cannot find anything suitable }
    function FindLibrary(out FullName : AnsiString) : boolean;
            { returns true if it successfully set the indicated dictionary }
    function SetDictionary(const FullDictName: string) : boolean;
    function SetNewLibrary(const LibName : string) : boolean;

  private
    function EncodeWord(Word: string): String;
    function DecodeWord(Word: string): String;
  end;


var Hunspell_create: THunspell_create;
var Hunspell_destroy: THunspell_destroy;
var Hunspell_spell: Thunspell_spell;
var Hunspell_suggest: Thunspell_suggest;
var Hunspell_analyze: Thunspell_analyze;
var Hunspell_stem: Thunspell_stem;
var Hunspell_get_dic_encoding: Thunspell_get_dic_encoding;
var Hunspell_add: THunspell_add;
var Hunspell_free_list: THunspell_free_list;
var Hunspell_remove: THunspell_remove;
var HunLibLoaded: Boolean = False;
var HunLibHandle: THandle;


implementation

uses
  LazUTF8, SysUtils, {$ifdef linux}Process, {$else} Forms, {$endif} LazFileUtils;


{ THunspell }

{-------------------------------------------------------------------------------
  @NAME: EncodeWord
 ------------------------------------------------------------------------------}
function THunspell.EncodeWord(Word: string): String;
begin
  Result := UTF8ToCP1252(Word);
end;


{-------------------------------------------------------------------------------
  @NAME: DecodeWord
 ------------------------------------------------------------------------------}
function THunspell.DecodeWord(Word: string): String;
begin
  Result := CP1252ToUTF8(Word);
end;


{-------------------------------------------------------------------------------
  @NAME: LoadHunspellLibrary
 ------------------------------------------------------------------------------}
function THunspell.LoadHunspellLibrary(libraryName: Ansistring): Boolean;
begin
  Result := false;
  HunLibHandle := LoadLibrary(PAnsiChar(libraryName));
  if HunLibHandle = NilHandle then
    ErrorMessage := 'Failed to load library ' + libraryName
  else begin
    Result := True;
    Hunspell_create := THunspell_create(GetProcAddress(HunLibHandle, 'Hunspell_create'));
    if not Assigned(Hunspell_create) then Result := False;
    Hunspell_destroy := Thunspell_destroy(GetProcAddress(HunLibHandle, 'Hunspell_destroy'));
    if not Assigned(Hunspell_destroy) then Result := False;
    Hunspell_spell := THunspell_spell(GetProcAddress(HunLibHandle, 'Hunspell_spell'));
    if not Assigned(Hunspell_spell) then Result := False;
    Hunspell_suggest := THunspell_suggest(GetProcAddress(HunLibHandle, 'Hunspell_suggest'));
    if not Assigned(Hunspell_suggest) then Result := False;
    Hunspell_analyze := THunspell_analyze(GetProcAddress(HunLibHandle, 'Hunspell_analyze'));  // not used here
    if not Assigned(Hunspell_analyze) then Result := False;
    Hunspell_stem := THunspell_stem(GetProcAddress(HunLibHandle, 'Hunspell_stem'));           // not used here
    if not Assigned(Hunspell_stem) then Result := False;
    Hunspell_get_dic_encoding := THunspell_get_dic_encoding(GetProcAddress(HunLibHandle, 'Hunspell_get_dic_encoding'));   // not used here
    if not Assigned(Hunspell_get_dic_encoding) then Result := False;
    Hunspell_free_list := THunspell_free_list(GetProcAddress(HunLibHandle, 'Hunspell_free_list'));
    if not Assigned(Hunspell_free_list) then Result := False;
    Hunspell_add := THunspell_add(GetProcAddress(HunLibHandle, 'Hunspell_add'));
    if not Assigned(Hunspell_add) then Result := False;
    Hunspell_remove := THunspell_remove(GetProcAddress(HunLibHandle, 'Hunspell_remove'));
    if not Assigned(Hunspell_remove) then Result := False;
    HunLibLoaded := Result;
  end;
  if ErrorMessage = '' then
    if not Result then ErrorMessage := 'Failed to find functions in ' + LibraryName;
end;


{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor THunspell.Create();
begin
  ErrorMessage := '';
  if Not FindLibrary(LibraryFullName) then begin
    ErrorMessage := 'Cannot find Hunspell library';
    exit();
  end;
  LoadHunspellLibrary(LibraryFullName);    // will flag any errors it finds
  Speller := nil;           // we are not GoodToGo yet, need a dictionary ....
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor THunspell.Destroy;
begin
  if (HunLibHandle <> 0) and HunLibLoaded then
  begin
    if Speller <> nil then hunspell_destroy(Speller);
    Speller := nil;
    if HunLibHandle <> 0 then FreeLibrary(HunLibHandle);
    HunLibLoaded := false;
  end;
  inherited Destroy;
end;


{-------------------------------------------------------------------------------
  @NAME: Spell
 ------------------------------------------------------------------------------}
function THunspell.Spell(Word: string): boolean;
begin
  Result := hunspell_spell(Speller, PChar(EncodeWord(Word)))
end;


{-------------------------------------------------------------------------------
  @NAME: Suggest
 ------------------------------------------------------------------------------}
procedure THunspell.Suggest(Word: string; List: TStrings);
var
  i, len: Integer;
  SugList, Words: PPChar;
begin
  List.clear;
  try
    len   := hunspell_suggest(Speller, SugList, PChar(EncodeWord(Word)));
    Words := SugList;
    for i := 1 to len do
    begin
        // In das richtige Format konvertieren
        List.Add(DecodeWord(Words^));
        Inc({%H-}PtrInt(Words), sizeOf(Pointer));
    end;
  finally
    Hunspell_free_list(Speller, SugList, len);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: Add
 ------------------------------------------------------------------------------}
procedure THunspell.Add(Word: string);
begin
  Hunspell_add(Speller, Pchar(EncodeWord(Word)));
end;


{-------------------------------------------------------------------------------
  @NAME: Remove
 ------------------------------------------------------------------------------}
procedure THunspell.Remove(Word: string);
begin
  Hunspell_remove(Speller, Pchar(EncodeWord(Word)));
end;


{-------------------------------------------------------------------------------
  @NAME: FindLibrary
 ------------------------------------------------------------------------------}
function THunspell.FindLibrary(out FullName : ANSIString):boolean;
var
  {$ifdef LINUX} I : integer = 1; {$endif}
  Info : TSearchRec;
  Mask : ANSIString;
begin
  Result := False;
  Mask := '';
  {$IFDEF LINUX}
  // Assumes ldconfig always returns same format, better than searching several dirs
  if RunCommand('/bin/bash',['-c','ldconfig -p | grep hunspell'], FullName) then begin
      while UTF8Pos(' ', FullName, I) <> 0 do inc(I);
      if I=1 then exit();
      UTF8Delete(FullName, 1, I-1);
      UTF8Delete(FullName, UTF8Pos(#10, FullName, 1), 1);
      Result := True;
  end;
  exit();
  {$ENDIF}
  {$IFDEF WINDOWS}

   // Check if 32 bit Build
  {$DEFINE UNIV_OR_32BIT}
  {$IFNDEF DARWIN}  {Assume OS X library is universal 32/64-bit}
    {$IFDEF CPUX64}
      {$UNDEF UNIV_OR_32BIT}
    {$ENDIF}
    {$IFDEF CPU64}
      {$UNDEF UNIV_OR_32BIT}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIV_OR_32BIT}      // Check if 32 bit Build
  Mask := '*hunspell32*.dll';
  {$ELSE}
  Mask := '*hunspell64*.dll';
  {$ENDIF}

 {
  if sizeof(pointer) = 4 then // Check if 32 bit Build
  begin
    Mask := '*hunspell32*.dll';
  end;
  if sizeof(pointer) = 8 then // Check if 64 bit Build
  begin
    Mask := '*hunspell64*.dll';
  end;
 }

  // Look for a dll in application home dir.
  FullName := ExtractFilePath(Application.ExeName);
  {$endif}
  {$ifdef DARWIN}
  Mask := 'libhunspell*';
  FullName := '/usr/lib/';
  {$endif}
  if FindFirst(FullName + Mask, faAnyFile and faDirectory, Info)=0 then
  begin
    FullName := FullName + Info.name;
    Result   := True;
  end;
  FindClose(Info);
end;


{-------------------------------------------------------------------------------
  @NAME: SetDictionary
 ------------------------------------------------------------------------------}
function THunspell.SetDictionary(const FullDictName: string) : boolean;
var
  FullAff : string;
begin
  FullAff := FullDictName;
  UTF8Delete(FullAff, UTF8Length(FullAff) - 2, 3);
  FullAff := FullAff + 'aff';
  if Speller <> Nil then
      hunspell_destroy(Speller);
  Speller := hunspell_create(PChar(FullAff), PChar(FullDictName));
  GoodToGo := Speller <> Nil;
  if not GoodToGo then
      ErrorMessage := 'Failed to set Dictionary ' + FullDictName;
  Result := GoodToGo;
end;


{-------------------------------------------------------------------------------
  @NAME: SetNewLibrary
 ------------------------------------------------------------------------------}
function THunspell.SetNewLibrary(const LibName: string): boolean;
begin
  LibraryFullName := LibName;
  Result := LoadHunspellLibrary(LibraryFullName);
end;



end.
