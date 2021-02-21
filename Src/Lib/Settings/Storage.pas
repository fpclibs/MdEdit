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
unit Storage;

{$mode objfpc}{$H+}

interface

uses
  Classes, TypInfo, Laz2_XMLCfg, laz2_XMLRead, SysUtils;

type
  TAppStorage = class(TXMLConfig)

  public
    function  Read(const Ident: string; DefaultValue: Boolean): Boolean; overload;
    function  Read(const Ident, DefaultValue: string): string; overload;
    function  Read(const Ident: string; DefaultValue: Longint): Longint; overload;
    procedure Read(const APath: String; out ARect: TRect; const ADefault: TRect); overload;

    procedure Write(const Ident, Value: string); overload;
    procedure Write(const Ident: string; Value: Longint); overload;
    procedure Write(const Ident: string; Value: Boolean); overload;

    procedure WriteOrDefault(const APath, AValue, DefValue: String); overload;
    procedure WriteOrDefault(const APath: String; AValue, DefValue: Integer); overload;
    procedure WriteOrDefault(const APath: String; AValue, DefValue: Boolean); overload;
    procedure WriteOrDefault(const APath: String; const AValue, DefValue: TRect); overload;

    procedure Delete(const APath: string);
    procedure DeleteValue(const APath: string);
    procedure Save();
    procedure Restore();

    procedure WriteObject(Path: String; Obj: TPersistent;
      DefObject: TPersistent= nil; OnlyProperty: String= '');
    procedure ReadObject(Path: String; Obj: TPersistent;
      DefObject: TPersistent= nil; OnlyProperty: String= '');

  protected
    procedure WriteProperty(Path: String; Instance: TPersistent;
      PropInfo: Pointer; DefInstance: TPersistent = nil;
      OnlyProperty: String= '');
    procedure ReadProperty(Path: String; Instance: TPersistent;
      PropInfo: Pointer; DefInstance: TPersistent = nil;
      OnlyProperty: String= '');

    function DecodeSpecialChars ( text: String ): String;
    function EncodeSpecialChars ( text: String ): String;

  end;


implementation


{-------------------------------------------------------------------------------
  Read Boolean
 ------------------------------------------------------------------------------}
function TAppStorage.Read(const Ident: string; DefaultValue: Boolean): Boolean;
begin
  Result := GetValue(Ident, DefaultValue);
end;


{-------------------------------------------------------------------------------
  Read string
 ------------------------------------------------------------------------------}
function TAppStorage.Read(const Ident, DefaultValue: string): string;
begin
  Result := DecodeSpecialChars(GetValue(Ident, EncodeSpecialChars(DefaultValue)));
end;


{-------------------------------------------------------------------------------
  Read Longint
 ------------------------------------------------------------------------------}
function TAppStorage.Read(const Ident: string; DefaultValue: Longint): Longint;
begin
  Result := GetValue(Ident, DefaultValue);
end;


{-------------------------------------------------------------------------------
  Read TRect
 ------------------------------------------------------------------------------}
procedure TAppStorage.Read(const APath: String; out ARect: TRect; const ADefault: TRect);
begin
  ARect.Left   := Read(APath + 'Left',   ADefault.Left);
  ARect.Top    := Read(APath + 'Top',    ADefault.Top);
  ARect.Right  := Read(APath + 'Right',  ADefault.Right);
  ARect.Bottom := Read(APath + 'Bottom', ADefault.Bottom);
end;


{-------------------------------------------------------------------------------
  Write string
 ------------------------------------------------------------------------------}
procedure TAppStorage.Write(const Ident, Value: string);
begin
   SetValue(Ident, EncodeSpecialChars(Value));
end;


{-------------------------------------------------------------------------------
  Write Longint
 ------------------------------------------------------------------------------}
procedure TAppStorage.Write(const Ident: string; Value: Longint);
begin
  SetValue(Ident, Value);
end;


{-------------------------------------------------------------------------------
  Write Boolean
 ------------------------------------------------------------------------------}
procedure TAppStorage.Write(const Ident: string; Value: Boolean);
begin
  SetValue(Ident, Value);
end;


{-------------------------------------------------------------------------------
  WriteOrDefault
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteOrDefault(const APath, AValue, DefValue: String);
begin
  if AValue = DefValue then

    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;


{-------------------------------------------------------------------------------
  WriteOrDefault
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteOrDefault(const APath: String; AValue, DefValue: Integer);
begin
  if AValue = DefValue then

    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;


{-------------------------------------------------------------------------------
  WriteOrDefault
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteOrDefault(const APath: String; AValue, DefValue: Boolean);
begin
  if AValue = DefValue then

    DeleteValue(APath)
  else
    SetValue(APath, AValue);
end;


{-------------------------------------------------------------------------------
  WriteOrDefault
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteOrDefault(const APath: String; const AValue, DefValue: TRect);
begin
  WriteOrDefault(APath + 'Left',   AValue.Left,   DefValue.Left);
  WriteOrDefault(APath + 'Top',    AValue.Top,    DefValue.Top);
  WriteOrDefault(APath + 'Right',  AValue.Right,  DefValue.Right);
  WriteOrDefault(APath + 'Bottom', AValue.Bottom, DefValue.Bottom);
end;


{-------------------------------------------------------------------------------
  Delete
 ------------------------------------------------------------------------------}
procedure TAppStorage.Delete(const APath: string);
begin
  DeletePath(APath);
end;


{-------------------------------------------------------------------------------
  DeleteValue
 ------------------------------------------------------------------------------}
procedure TAppStorage.DeleteValue(const APath: string);
begin
  Inherited DeleteValue(APath);
end;


{-------------------------------------------------------------------------------
  Save
 ------------------------------------------------------------------------------}
procedure TAppStorage.Save();
begin
   Flush();
end;


{-------------------------------------------------------------------------------
  Restore
 ------------------------------------------------------------------------------}
procedure TAppStorage.Restore();
begin
  Clear();
  if Not FileExists(Filename) then exit;

  ReadXMLFile(doc, Filename);
end;


{-------------------------------------------------------------------------------
  WriteObject
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteObject(Path: String; Obj: TPersistent;
  DefObject: TPersistent; OnlyProperty: String = '');
var
  PropCount, i : integer;
  PropList   : PPropList;
begin
  PropCount := GetPropList(Obj, PropList);
  if PropCount>0 then begin
    try
      for i := 0 to PropCount-1 do
        WriteProperty(Path, Obj, PropList^[i], DefObject, OnlyProperty);
    finally
      Freemem(PropList);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ReadObject
 ------------------------------------------------------------------------------}
procedure TAppStorage.ReadObject(Path: String; Obj: TPersistent; DefObject: TPersistent;
  OnlyProperty: String);
var
  PropCount, i : integer;
  PropList   : PPropList;
begin
  PropCount := GetPropList(Obj,PropList);
  if PropCount>0 then begin
    try
      for i := 0 to PropCount-1 do
        ReadProperty(Path, Obj, PropList^[i], DefObject, OnlyProperty);
    finally
      Freemem(PropList);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  WriteProperty
 ------------------------------------------------------------------------------}
procedure TAppStorage.WriteProperty(Path: String; Instance: TPersistent;
  PropInfo: Pointer; DefInstance: TPersistent; OnlyProperty: String= '');
type
  tset = set of 0..31;
var
  i: Integer;
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
  Ident: String;
  IntToIdentFn: TIntToIdent;
  SetType: Pointer;
  FloatValue, DefFloatValue: Extended;
  StrValue, DefStrValue: String;
  BoolValue, DefBoolValue: boolean;
begin
  if not (Assigned(PPropInfo(PropInfo)^.GetProc) and
          Assigned(PPropInfo(PropInfo)^.SetProc)) then
    exit;

  PropType := PPropInfo(PropInfo)^.PropType;
  Path := Path + PPropInfo(PropInfo)^.Name;
  if (OnlyProperty <> '') and (OnlyProperty <> PPropInfo(PropInfo)^.Name) then
    exit;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        Value := GetOrdProp(Instance, PropInfo);
        if (DefInstance <> nil) then
          DefValue := GetOrdProp(DefInstance, PropInfo);
        if (DefInstance <> nil)  and (Value = DefValue) then
          DeleteValue(Path)
        else
        begin
          case PropType^.Kind of
            tkInteger:
              begin                      // Check if this integer has a string identifier
                IntToIdentFn := FindIntToIdent(PPropInfo(PropInfo)^.PropType);
                if Assigned(IntToIdentFn) and IntToIdentFn(Value, Ident{%H-}) then
                  Write(Path, Ident) // Integer can be written a human-readable identifier
                else
                  Write(Path, Value); // Integer has to be written just as number
              end;
            tkChar:
              Write(Path, Chr(Value));
            tkWChar:
              Write(Path, Value);
            tkSet:
              begin
                SetType := GetTypeData(PropType)^.CompType;
                Ident := '';
                for i := 0 to 31 do
                  if (i in tset(Value)) then begin
                    if Ident <> '' then Ident := Ident + ',';
                    Ident := Ident + GetEnumName(PTypeInfo(SetType), i);
                  end;
                Write(Path, Ident);
              end;
            tkEnumeration:
              Write(Path, GetEnumName(PropType, Value));
          end;
        end;
      end;
    tkFloat:
      begin
        FloatValue := GetFloatProp(Instance, PropInfo);
        if (DefInstance <> nil) then
         DefFloatValue := GetFloatProp(DefInstance, PropInfo);
        if (DefInstance <> nil)  and (DefFloatValue = FloatValue) then
          DeleteValue(Path)
        else
          Write(Path, FloatToStr(FloatValue));
      end;
    tkSString, tkLString, tkAString:
      begin
        StrValue := GetStrProp(Instance, PropInfo);
        if (DefInstance <> nil) then
           DefStrValue := GetStrProp(DefInstance, PropInfo);
        if (DefInstance <> nil)  and (DefStrValue = StrValue) then
          DeleteValue(Path)
        else
          Write(Path, StrValue);
      end;
    tkBool:
      begin
        BoolValue := GetOrdProp(Instance, PropInfo)<>0;
        if (DefInstance <> nil) then
          DefBoolValue := GetOrdProp(DefInstance, PropInfo)<>0;
        if (DefInstance <> nil) and (BoolValue = DefBoolValue) then
          DeleteValue(Path)
        else
          Write(Path, BoolValue);
      end;
  end;
end;


{-------------------------------------------------------------------------------
  ReadProperty
 ------------------------------------------------------------------------------}
procedure TAppStorage.ReadProperty(Path: String; Instance: TPersistent; PropInfo: Pointer;
  DefInstance: TPersistent; OnlyProperty: String);
type
  tset = set of 0..31;
var
  i, j: Integer;
  PropType: PTypeInfo;
  Value, DefValue: LongInt;
  Ident, s: String;
  IdentToIntFn: TIdentToInt;
  SetType: Pointer;
  FloatValue, DefFloatValue: Extended;
  StrValue, DefStrValue: String;
  BoolValue, DefBoolValue: boolean;
begin
  // do not stream properties without getter and setter
  if not (Assigned(PPropInfo(PropInfo)^.GetProc) and
          Assigned(PPropInfo(PropInfo)^.SetProc)) then
    exit;

  PropType := PPropInfo(PropInfo)^.PropType;
  Path := Path + PPropInfo(PropInfo)^.Name;
  if (OnlyProperty <> '') and (OnlyProperty <> PPropInfo(PropInfo)^.Name) then
    exit;
  if DefInstance = nil then
    DefInstance := Instance;

  case PropType^.Kind of
    tkInteger, tkChar, tkEnumeration, tkSet, tkWChar:
      begin
        DefValue := GetOrdProp(DefInstance, PropInfo);
        case PropType^.Kind of
          tkInteger:
            begin                      // Check if this integer has a string identifier
              Ident := Read(Path, IntToStr(DefValue));
              IdentToIntFn := FindIdentToInt(PPropInfo(PropInfo)^.PropType);
              if TryStrToInt(Ident, Value) then
                SetOrdProp(Instance, PropInfo, Value)
              else if Assigned(IdentToIntFn) and IdentToIntFn(Ident, Value) then
                SetOrdProp(Instance, PropInfo, Value)
              else
                SetOrdProp(Instance, PropInfo, DefValue)
            end;
          tkChar:
            begin
              Ident := Read(Path, chr(DefValue));
              if Length(Ident) > 0 then
                SetOrdProp(Instance, PropInfo, ord(Ident[1]))
              else
                SetOrdProp(Instance, PropInfo, DefValue);
            end;
          tkWChar:
            SetOrdProp(Instance, PropInfo, Read(Path, DefValue));
          tkSet:
            begin
              SetType := GetTypeData(PropType)^.CompType;
              Ident := Read(Path, '-');
              If Ident = '-' then
                Value := DefValue
              else begin
                Value := 0;
                while length(Ident) > 0 do begin
                  i := Pos(',', Ident);
                  if i < 1 then
                    i := length(Ident) + 1;
                  s := copy(Ident, 1, i-1);
                  Ident := copy(Ident, i+1, length(Ident));
                  j := GetEnumValue(PTypeInfo(SetType), s);
                  if j <> -1 then
                    include(tset(Value), j)
                  else Begin
                    Value := DefValue;
                    break;
                  end;
                end;
              end;
              SetOrdProp(Instance, PropInfo, Value);
            end;
          tkEnumeration:
            begin
              Ident := Read(Path, '-');
              If Ident = '-' then
                Value := DefValue
              else
                Value := GetEnumValue(PropType, Ident);
              if Value <> -1 then
                SetOrdProp(Instance, PropInfo, Value)
              else
                SetOrdProp(Instance, PropInfo, DefValue);
            end;
        end;
      end;
    tkFloat:
      begin
        DefFloatValue := GetFloatProp(DefInstance, PropInfo);
        Ident := Read(Path, FloatToStr(DefFloatValue));
        if TryStrToFloat(Ident, FloatValue) then
          SetFloatProp(Instance, PropInfo, FloatValue)
        else
          SetFloatProp(Instance, PropInfo, DefFloatValue)
      end;
    tkSString, tkLString, tkAString:
      begin
        DefStrValue := GetStrProp(DefInstance, PropInfo);
        StrValue := Read(Path, DefStrValue);
        SetStrProp(Instance, PropInfo, StrValue)
      end;
    tkBool:
      begin
        DefBoolValue := GetOrdProp(DefInstance, PropInfo) <> 0;
        BoolValue := Read(Path, DefBoolValue);
        SetOrdProp(Instance, PropInfo, ord(BoolValue));
      end;
  end;
end;

// DoDo:

{-------------------------------------------------------------------------------
  Encode/Decode
 ------------------------------------------------------------------------------}
Var
  {%H-}TextSpecialCharsRow: array[0..35] of String = (
  '&', '<', '>', '"',
  #00, #01, #02, #03, #04, #05, #06, #07,
  #08, #09, #10, #11, #12, #13, #14, #15,
  #16, #17, #18, #19, #20, #21, #22, #23,
  #24, #25, #26, #27, #28, #29, #30, #31);

  {%H-}TextSpecialCharsHex: array[0..35] of String = (
     '&amp;',   '&lt;',   '&gt;',  '&quot;',
     '&#x0;',  '&#x1;',  '&#x2;',  '&#x3;',  '&#x4;',  '&#x5;',  '&#x6;',  '&#x7;',
     '&#x8;',  '&#x9;',  '&#xA;',  '&#xB;',  '&#xC;',  '&#xD;',  '&#xE;',  '&#xF;',
    '&#x10;', '&#x11;', '&#x12;', '&#x13;', '&#x14;', '&#x15;', '&#x16;', '&#x17;',
    '&#x18;', '&#x19;', '&#x1A;', '&#x1B;', '&#x1C;', '&#x1D;', '&#x1E;', '&#x1F;');

const
  {%H-}QuotStr = '&quot;';
  {%H-}AmpStr  = '&amp;';
  {%H-}ltStr   = '&lt;';
  {%H-}gtStr   = '&gt;';


{-------------------------------------------------------------------------------
  EncodeSpecialChars
 ------------------------------------------------------------------------------}
function TAppStorage.EncodeSpecialChars ( text: String ): String;
begin
//  text := StringReplace(text, '&', AmpStr,  [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, '<', ltStr,   [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, '>', gtStr,   [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, '"', QuotStr, [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text,  #9, '&#x9;', [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text, #10, '&#xA;', [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text, #13, '&#xD;', [rfReplaceAll, rfIgnoreCase]);

  Result := text;
end;


{-------------------------------------------------------------------------------
  DecodeSpecialChars
 ------------------------------------------------------------------------------}
function TAppStorage.DecodeSpecialChars ( text: String ): String;
begin
//  text := StringReplace(text, AmpStr,  '&', [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, ltStr,   '<', [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, gtStr,   '>', [rfReplaceAll, rfIgnoreCase]);
//  text := StringReplace(text, QuotStr, '"', [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text, '&#x9;',  #9, [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text, '&#xA;', #10, [rfReplaceAll, rfIgnoreCase]);
  text := StringReplace(text, '&#xD;', #13, [rfReplaceAll, rfIgnoreCase]);

  Result := text;
end;


end.

