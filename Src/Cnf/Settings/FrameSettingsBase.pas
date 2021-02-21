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
unit FrameSettingsBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, FrameSettingsInterface, SettingsBase,
  Storage, LResources;

type
  TStrArray = Array Of String;


type
  TFrameSettingsBase = class(TFrame, ISettingsInterface)

  private
    mAppCnf: TSettings;

  protected
    procedure SetSettings(appCnf: TSettings); Virtual;

  public
    constructor Create(TheOwner: TComponent); override;

    property Settings: TSettings read mAppCnf write SetSettings;
    procedure GetSettings; Virtual; Abstract;
    procedure SetSettings; Virtual; Abstract;
    procedure SetCancel; Virtual;
    Function  GetTreePath: TStrArray; Virtual; Abstract;
    procedure Free;
    procedure OnVisible(); Virtual;

  private
    procedure _Settings(value: TAppStorage);
    function _Settings: TAppStorage;

    procedure _BoundsRect(value: TRect);
    function _BoundsRect: TRect;

    procedure _Parent(value: TWinControl);
    function _Parent: TWinControl;

    procedure _Visible(value: Boolean);
    function _Visible: Boolean;

  protected
    function LoadResource(const ResName: String): String;

  end;


implementation


{ TFrameSettingsBase }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TFrameSettingsBase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase.SetSettings(appCnf: TSettings);
begin
  mAppCnf := appCnf;
  SetSettings;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase.Free;
begin
  inherited Free;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase._Settings(value: TAppStorage);
begin
  Settings := TSettings(value);
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
function TFrameSettingsBase._Settings: TAppStorage;
begin
  Result := Settings;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase._BoundsRect(value: TRect);
begin
  BoundsRect := value;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
function TFrameSettingsBase._BoundsRect: TRect;
begin
  Result := BoundsRect;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase._Parent(value: TWinControl);
begin
  Parent := value;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
function TFrameSettingsBase._Parent: TWinControl;
begin
  Result := Parent;
end;


{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase._Visible(value: Boolean);
begin
  Visible := value;
  OnVisible();
end;

{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
function TFrameSettingsBase._Visible: Boolean;
begin
  Result := Visible;
end;


{-------------------------------------------------------------------------------
  LoadResource
 ------------------------------------------------------------------------------}
function TFrameSettingsBase.LoadResource(const ResName: String): String;
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
  Result :=  F.DataString;
end;


{-------------------------------------------------------------------------------
  @NAME: OnVisible
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase.OnVisible();
begin
end;


{-------------------------------------------------------------------------------
  @NAME: SetCancel
 ------------------------------------------------------------------------------}
procedure TFrameSettingsBase.SetCancel();
begin

end;



end.

