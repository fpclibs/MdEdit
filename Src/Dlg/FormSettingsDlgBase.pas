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
unit FormSettingsDlgBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Storage, fgl;

type

  TNotifyList = specialize TFPGList<TNotifyEvent>;

  { TFormSettingsDlgBase }

  TFormSettingsDlgBase = class(TForm)

  private

  protected
    mSettings: TAppStorage;
    mNotifyList: TNotifyList;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Settings: TAppStorage read mSettings write mSettings;
    procedure AddNotify(notify: TNotifyEvent);

  protected
    procedure DoSettingsNotify(Sender: TObject);

  end;

var
  OFormSettingsDlgBase: TFormSettingsDlgBase;

implementation

{$R *.lfm}


{ TFormSettingsDlgBase }

{-------------------------------------------------------------------------------
  @NAME: Create
 ------------------------------------------------------------------------------}
constructor TFormSettingsDlgBase.Create(AOwner: TComponent);
begin
  mNotifyList := TNotifyList.Create;
  inherited;
end;


{-------------------------------------------------------------------------------
  @NAME: Destroy
 ------------------------------------------------------------------------------}
destructor TFormSettingsDlgBase.Destroy;
begin
  inherited;
end;


{-------------------------------------------------------------------------------
  @NAME: DoSettingsNotify
 ------------------------------------------------------------------------------}
procedure TFormSettingsDlgBase.DoSettingsNotify(Sender: TObject);
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
procedure TFormSettingsDlgBase.AddNotify(notify: TNotifyEvent);
begin
  mNotifyList.Add(notify);
end;



end.

