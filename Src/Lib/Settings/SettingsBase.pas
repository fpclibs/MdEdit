{
 ******************************************++***********************************
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
unit SettingsBase;

interface

uses
  Classes, SysUtils, Storage, Forms, FormSettings,
  FrameSettingsInterface, fgl;


{ TAppServices }

type

  TServiceObject  = TObject;
  TServiceObjects = array of TServiceObject;

  TAppServices = class(TObject)

  private { Members }
    mServices : TServiceObjects;
    mCouter   : Integer;

  private { Getter/Setter }

  public  { Properties }
    property Items: TServiceObjects read mServices write mServices;

  public  { Class Publics }
    constructor Create();

    procedure AddService(AObject: TServiceObject);
    procedure SetFirst();
    function GetNext(Var AObject: TServiceObject): Boolean;
    function IsEof(): Boolean;
    function FindFirstByClass(AClass: TClass): TObject;

  end;


{ TSettings }

type

  TNotifyList = specialize TFPGList<TNotifyEvent>;

  TSettings = class(TAppStorage)

  private   { Members }
    mFormSettings: TFormSettings;
    mNotifyList:   TNotifyList;
    mServices:     TAppServices;

  public    { Properties }
    property Services: TAppServices read mServices;

  public    { Class Publics }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;

    Procedure AddFrame(newFrame: ISettingsInterface);
    procedure AddNotify(notify: TNotifyEvent);

    procedure Show;
    procedure Show(index: Integer);

  protected { Class Protected }
    procedure DoSettingsNotify(Sender: TObject);

  end;



implementation


{ TSettings }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TSettings.Create(AOwner: TComponent);
begin
  mServices     := TAppServices.Create();
  mNotifyList   := TNotifyList.Create;
  mFormSettings := TFormSettings.Create(Nil);
  mFormSettings.OnSettingsNotify := @DoSettingsNotify;
  inherited;
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor TSettings.Destroy;
begin
  inherited;
end;


{-------------------------------------------------------------------------------
  @NAME: AddFrame
 ------------------------------------------------------------------------------}
Procedure TSettings.AddFrame(newFrame: ISettingsInterface);
begin
  mFormSettings.Settings := Self;
  mFormSettings.AddFrame(newFrame);
end;


{-------------------------------------------------------------------------------
  @NAME: Show
 ------------------------------------------------------------------------------}
procedure TSettings.Show;
begin
  mFormSettings.Settings := Self;
  mFormSettings.Show;
end;


{-------------------------------------------------------------------------------
  @NAME: Show
 ------------------------------------------------------------------------------}
procedure TSettings.Show(index: Integer);
begin
  mFormSettings.Settings := Self;
  mFormSettings.SetActiveFrame(index);
  mFormSettings.Show;
end;


{-------------------------------------------------------------------------------
  @NAME: DoSettingsNotify
 ------------------------------------------------------------------------------}
procedure TSettings.DoSettingsNotify(Sender: TObject);
var
  i: Integer;
  notify: TNotifyEvent;
begin
  for i := 0 to mNotifyList.Count-1 do
  begin
    notify := mNotifyList.Items[i];
    if Assigned(notify) then begin
      notify(Self);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: AddNotify
 ------------------------------------------------------------------------------}
procedure TSettings.AddNotify(notify: TNotifyEvent);
begin
  mNotifyList.Add(notify);
end;



{ TAppServices }

{-------------------------------------------------------------------------------
  Constructor
 ------------------------------------------------------------------------------}
constructor TAppServices.Create();
begin
  mServices := Nil;
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  AddPreviewHighligter
 ------------------------------------------------------------------------------}
procedure TAppServices.AddService(aObject: TServiceObject);
begin
  SetLength(mServices, Length(mServices) + 1);
  mServices[Length(mServices)-1] := aObject;
end;


{-------------------------------------------------------------------------------
  SetFirst
 ------------------------------------------------------------------------------}
procedure TAppServices.SetFirst();
begin
  mCouter := -1;
end;


{-------------------------------------------------------------------------------
  GetNext
 ------------------------------------------------------------------------------}
function TAppServices.GetNext(Var aObject: TServiceObject): Boolean;
begin
  Inc(mCouter);
  Result := false;
  aObject := Nil;

  if Length(mServices) <= mCouter then exit;

  aObject := mServices[mCouter];
  if aObject <> Nil then Result := true;
end;


{-------------------------------------------------------------------------------
  IsEof
 ------------------------------------------------------------------------------}
function TAppServices.IsEof(): Boolean;
begin
  Result := false;
  if Length(mServices) > mCouter then Result := true;
end;


{-------------------------------------------------------------------------------
  GetByClass
 ------------------------------------------------------------------------------}
function TAppServices.FindFirstByClass(AClass: TClass): TObject;
var
  i, cnt: Integer;
begin
  cnt := Length(mServices);
  Result := Nil;

  for i := 0 to cnt-1 do
  begin
    if mServices[i].ClassType = AClass.ClassType then Result := mServices[i];
  end;
end;



end.

