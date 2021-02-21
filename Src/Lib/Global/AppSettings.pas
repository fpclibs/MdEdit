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
unit AppSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SettingsBase;


{ TAppSettings }

Type
  TAppSettings = class(TSettings)

  private { Members }
    mInstance: Integer;
    {%H-}mServices: TAppServices;

  private { Getter/Setter }

  public  { Class Publics }
    constructor Create(AOwner: TComponent); override;
    constructor Create();
    destructor  Destroy; override;

  end;


implementation

var
  OAppSettings: TAppSettings;


{ TAppSettings }

{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TAppSettings.Create(AOwner: TComponent);
begin
  inherited;

  OAppSettings := Self;
  mInstance    := 1;
end;


{-------------------------------------------------------------------------------
  Create
 ------------------------------------------------------------------------------}
constructor TAppSettings.Create();
begin
  Self := OAppSettings;
  Inc(mInstance);
end;


{-------------------------------------------------------------------------------
  Destroy
 ------------------------------------------------------------------------------}
destructor TAppSettings.Destroy;
begin
  Dec(mInstance);
  if mInstance < 1 then inherited;
end;


end.

