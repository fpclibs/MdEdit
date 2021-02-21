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

    Ist Teil Language Bibliothek. Diese Klasse stellt alle Funktionen bereit,
    die zum Parsen der Lng.xml nötig sind. Die Lng.xml kann konfortaben
    mit Excel editiert werden.

 *******************************************************************************
}
unit LangParse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, XMLRead, DOM;

  // Lconvencoding;

type

  {TLangParse}

  TLangParse = class(TObject)

  private { private declarations }

    FLngHash: TFPStringHashTable;
    FUsedLng: string;
    FLngList: TObjectList;

    FIdentifiers: TStringList;
    FLanguages:   TStringList;

    FStream: TStream;

    mKey, mItem: String;
    mFlagNext: Boolean;


    procedure CollectEntries(Node: TDOMNode; SList: TStringList);

  public  { public declarations }

    constructor Create();
    constructor Create(const UsedLng: string; const LngHash:  TFPStringHashTable);
    procedure SetLngHash(const LngHash: TFPStringHashTable);
    function  GetLngHash(): TFPStringHashTable;
    function GetLanguages(): TObjectList;
    procedure SetUsedLng(const UsedLng: string);
    procedure SetStream(const Stream: TStream);
    function ParseXml(): boolean;
    function BuildXml(): string;

  private
    procedure Iterator(Item: String; const Key: string; var Continue: Boolean);


  end;


implementation


{TLangParse}

{-------------------------------------------------------------------------------
  Class Create in TLangParse
 ------------------------------------------------------------------------------}
constructor TLangParse.Create();
begin
  FIdentifiers := TStringList.Create();
  FLanguages   := TStringList.Create();

  FLngList := TObjectList.Create();
  FLngList.Add(FIdentifiers);
  FLngList.Add(FLanguages);
end;


{-------------------------------------------------------------------------------
  Class Create in TLangParse
 ------------------------------------------------------------------------------}
constructor TLangParse.Create(const UsedLng: string; const LngHash: TFPStringHashTable);
begin
  Create();
  FLngHash := LngHash;
  FUsedLng := UsedLng;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetLngHash()
 @ PARAM:   LngHash: Die Hash-Tabelle, die mit den Texten der jeweiligen
 *          Sprache gefüllt werden soll.
 *
 * ---------------------------------------------------------------------------*)
procedure TLangParse.SetLngHash(const LngHash: TFPStringHashTable);
begin
  FLngHash := LngHash;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetLngHash()
 @ RESULT:  Die Hash-Tabelle, die mit den Texten der jeweiligen
 *          Sprache gefüllt ist, oder leer ist.
 *
 * ---------------------------------------------------------------------------*)
function TLangParse.GetLngHash(): TFPStringHashTable;
begin
  Result := FLngHash;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    GetLanguages()
 @ RESULT:  Gibt die Liste der gefundenen Sprachen zurück.
 *
 * ---------------------------------------------------------------------------*)
function TLangParse.GetLanguages(): TObjectList;
begin
  Result := FLngList;
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
procedure TLangParse.SetUsedLng(const UsedLng: string);
begin
  FUsedLng := UsedLng;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    SetStream()
 @ PARAM:   Stream: Stream des xml-language-Files
 *
 @ INFO:
 * Der Name der Sprache muss mit dem Namen im Spaltenkopf der xml Datei
 * übereinstimmen
 *
 * ---------------------------------------------------------------------------*)
procedure TLangParse.SetStream(const Stream: TStream);
begin
  FStream := Stream;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    ParseXml()
 @ RESULT:  Gibt True im Fehlerfall zurueck.
 *
 @ INFO:
 *
 * ---------------------------------------------------------------------------*)
function TLangParse.ParseXml(): boolean;
var
  Doc: TXMLDocument;
  PassNode, CellNode, DataNode: TDOMNode;
  Count, Column: integer;
  Key, Value: string;
begin
  ReadXMLFile(Doc, FStream);
  Column := 0;

  PassNode := Doc.DocumentElement.FindNode('Worksheet');
  if PassNode = nil then begin Result := false; exit; end;
  PassNode := PassNode.FindNode('Table');
  if PassNode = nil then begin Result := false; exit; end;

  PassNode := PassNode.FindNode('Row');
  while PassNode <> nil do
  begin
    if PassNode.NodeName = 'Row' then
    begin
      Count := 0;
      CellNode := PassNode.FirstChild;
      while CellNode <> nil do
      begin
         inc(Count);
         DataNode := CellNode.FirstChild;
         while DataNode <> nil do
         begin
//           ShowMessage(DataNode.NodeName);
           if DataNode.NodeName = '#text' then
           begin
             Value := String(DataNode.NodeValue);

             if Count = 1 then
             begin
               Key := Value;
               if Value = 'Identifier' then
               begin
                 Column := -1;
                 CollectEntries(CellNode, FIdentifiers);
               end;
               if Value = 'T_Languages' then
               begin
                 CollectEntries(CellNode, FLanguages);
               end;

             end;
             if (Column > 0) and (Column = Count) then
             begin
                FLngHash.Add(Key, AnsiToUTF8(Value));
             end;
             if (Column = -1) and (Value = FUsedLng) then
             begin
               Column := Count;
             end;
           end;
           DataNode := DataNode.FirstChild;

         end;
         CellNode := CellNode.NextSibling;
      end;
    end;
    Count := 0;
    PassNode := PassNode.NextSibling;
  end;
  Result := false;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:    CollectEntries()
 @ PARAM:   Node:  Knoten der ersten Spalte (Key) der Zeile
 @ PARAM:   SList: Liste der Strings, mit dem Inhalt der Zeile
 *
 * INFO:
 * Sammelt aus allen weiteren Spalten die Texte der Zeile ein.
 *
 * ---------------------------------------------------------------------------*)
procedure TLangParse.CollectEntries(Node: TDOMNode; SList: TStringList);
Var
   DataNode: TDOMNode;
begin
  Node := Node.NextSibling;
  while Node <> nil do
  begin
    DataNode := Node.FirstChild;
    while DataNode <> nil do
    begin
      if DataNode.NodeName = '#text' then
      begin
        SList.Add(AnsiToUTF8(String(DataNode.NodeValue)));
      end;
      DataNode := DataNode.FirstChild;
    end;
    Node := Node.NextSibling;
  end;
end;


(* -----------------------------------------------------------------------------
 *
 @ NAME:   BuildXml()
 @ RESULT: Gibt einen xml-String zurück.
 *
 * TODO:   Soll zukünftig die Struktur für alle Sprachen als XML exportieren
 *         Zur Zeit ist das nur für die aktive Sprache möglich.
 * INFO:
 * Sammelt aus allen weiteren Spalten die Texte der Zeilen ein.
 *
 * ---------------------------------------------------------------------------*)
function TLangParse.BuildXml(): string;
var
  cnt, i: Integer;
  str: String;
begin
  str := '';
  cnt := FLngHash.Count;
  for i:=0 to cnt -1 do
  begin
    FLngHash.Iterate(@Iterator);
    str := str + mKey + ' = ' + mItem + LineEnding;
  end;
  Result := str;
end;

procedure TLangParse.Iterator(Item: String; const Key: string; var Continue: Boolean);
begin
  mKey  := Key;
  mItem := Item;
  mFlagNext := Continue;
end;



end.
