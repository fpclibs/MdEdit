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
unit CnfSyneditPreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, Controls,

  SynEditHighlighter;



{ TSyneditPreview }

type
  TPreviewEdit = TSynEdit;
  TPreviewEditors = array of TPreviewEdit;

  TBookmarkNumRange = 0..9;


  TSyneditPreview = class(TObject)

  private { Members }
    mEditors                : TPreviewEditors;
    mCouter                 : Integer;
    mDefaultBookmarkImages  : TImageList;

  private { Getter/Setter }
     function GetCount(): Integer;

  public  { Properties }
    property Items: TPreviewEditors read mEditors write mEditors;

  public  { Class Publics }
    constructor Create();

    procedure AddPreviewEditor(AEditor: TPreviewEdit);
    procedure SetFirst();
    function GetNext(Var AEditor: TPreviewEdit): Boolean;
    function IsEof(): Boolean;
    property Count: Integer read GetCount;

  private { Class Privates }
    function DefaultBookmarkImages: TImageList;

end;



{ TContentPreview }

type
  TContentPreview = Class(TSyneditPreview);


{ THighligterPreview }

type
  TPreviewHighligter = TSynCustomHighlighter;
  TPreviewHighligters = array of TPreviewHighligter;


  THighligterPreview = class(TObject)

  private { Members }
    mHighligters            : TPreviewHighligters;
    mCouter                 : Integer;

  private { Getter/Setter }
    function GetCount(): Integer;


  public  { Properties }
    property Items: TPreviewHighligters read mHighligters write mHighligters;
    property Count: Integer read GetCount;

  public  { Class Publics }
    constructor Create();

    procedure AddPreviewHighligter(aHighligter: TPreviewHighligter);
    procedure SetFirst();
    function GetNext(Var aHighligter: TPreviewHighligter): Boolean;
    function IsEof(): Boolean;

end;




implementation


{ TSyneditPreview }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TSyneditPreview.Create();
begin
  mEditors := Nil;
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  AddPreviewEditor
 ------------------------------------------------------------------------------}
procedure TSyneditPreview.AddPreviewEditor(AEditor: TPreviewEdit);
begin
  SetLength(mEditors, Length(mEditors) + 1);
  mEditors[Length(mEditors)-1] := AEditor;
  if AEditor.BookMarkOptions.BookmarkImages = nil then
    AEditor.BookMarkOptions.BookmarkImages := DefaultBookmarkImages;
end;


{-------------------------------------------------------------------------------
  DefaultBookmarkImages
 ------------------------------------------------------------------------------}
function TSyneditPreview.DefaultBookmarkImages: TImageList;
var
  i: integer;
begin
  if mDefaultBookmarkImages = nil then
  begin
    mDefaultBookmarkImages := TImageList.Create(Nil);
    mDefaultBookmarkImages.Width := 11;
    mDefaultBookmarkImages.Height := 11;
    for i in TBookmarkNumRange do
      mDefaultBookmarkImages.AddResourceName(HInstance, 'bookmark' + IntToStr(i));
  end;
  Result := mDefaultBookmarkImages;
end;


{-------------------------------------------------------------------------------
  SetFirst
 ------------------------------------------------------------------------------}
procedure TSyneditPreview.SetFirst();
begin
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  GetNext
 ------------------------------------------------------------------------------}
function TSyneditPreview.GetNext(Var AEditor: TPreviewEdit): Boolean;
begin
  Inc(mCouter);
  Result := false;
  AEditor := Nil;

  if Length(mEditors) <= mCouter then exit;

  AEditor := mEditors[mCouter];
  if AEditor <> Nil then Result := true;
end;


{-------------------------------------------------------------------------------
  IsEof
 ------------------------------------------------------------------------------}
function TSyneditPreview.IsEof(): Boolean;
begin
  Result := false;
  if Length(mEditors) > mCouter then Result := true;
end;


{-------------------------------------------------------------------------------
  GetCount
 ------------------------------------------------------------------------------}
function TSyneditPreview.GetCount(): Integer;
begin
  Result := Length(mEditors);
end;




{ THighligterPreview }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor THighligterPreview.Create();
begin
  mHighligters := Nil;
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  AddPreviewHighligter
 ------------------------------------------------------------------------------}
procedure THighligterPreview.AddPreviewHighligter(aHighligter: TPreviewHighligter);
begin
  SetLength(mHighligters, Length(mHighligters) + 1);
  mHighligters[Length(mHighligters)-1] := aHighligter;
end;


{-------------------------------------------------------------------------------
  SetFirst
 ------------------------------------------------------------------------------}
procedure THighligterPreview.SetFirst();
begin
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  GetNext
 ------------------------------------------------------------------------------}
function THighligterPreview.GetNext(Var aHighligter: TPreviewHighligter): Boolean;
begin
  Inc(mCouter);
  Result := false;
  aHighligter := Nil;

  if Length(mHighligters) <= mCouter then exit;

  aHighligter := mHighligters[mCouter];
  if aHighligter <> Nil then Result := true;
end;


{-------------------------------------------------------------------------------
  IsEof
 ------------------------------------------------------------------------------}
function THighligterPreview.IsEof(): Boolean;
begin
  Result := false;
  if Length(mHighligters) > mCouter then Result := true;
end;


{-------------------------------------------------------------------------------
  GetCount
 ------------------------------------------------------------------------------}
function THighligterPreview.GetCount(): Integer;
begin
  Result := Length(mHighligters);
end;



end.

