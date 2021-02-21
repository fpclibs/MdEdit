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
unit FormDebug;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fgl;

type

  TNotifyList = specialize TFPGList<TNotifyEvent>;


  { TFormDebug }

  TFormDebug = class(TForm)

    ButtonSend: TButton;
    ComboBoxDebug: TComboBox;
    MemoDebug: TMemo;

    procedure ButtonSendClick(Sender: TObject);
    procedure ComboBoxDebugKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);

  private
    mCommand: String;
    mNotifyList: TNotifyList;

  public
    procedure MemoAppend(str: String);
    property Command: String read mCommand write mCommand;
    property Append: String write MemoAppend;
    procedure AddNotify(notify: TNotifyEvent);

  end;


var
  OFormDebug: TFormDebug;


implementation

{$R *.lfm}

{ TFormDebug }


{-------------------------------------------------------------------------------
  FormCreate
 ------------------------------------------------------------------------------}
procedure TFormDebug.FormCreate(Sender: TObject);
begin
  mNotifyList := TNotifyList.Create;
end;


{-------------------------------------------------------------------------------
  ButtonSendClick
 ------------------------------------------------------------------------------}
procedure TFormDebug.ButtonSendClick(Sender: TObject);
var
  i: Integer;
  notify: TNotifyEvent;
begin
  mCommand := ComboBoxDebug.Text;
  MemoDebug.Append('< ' + mCommand);
  ComboBoxDebug.Text := '';
  ComboBoxDebug.Items.Insert(0, mCommand);
  for i := 0 to mNotifyList.Count-1 do
  begin
    notify := mNotifyList.Items[i];
    if Assigned(notify) then begin
      notify(Self);
    end;
  end;
end;


{-------------------------------------------------------------------------------
  ComboBoxDebugKeyPress
 ------------------------------------------------------------------------------}
procedure TFormDebug.ComboBoxDebugKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    ButtonSendClick(Sender);
  end;
end;


{-------------------------------------------------------------------------------
  MemoAppend
 ------------------------------------------------------------------------------}
procedure TFormDebug.MemoAppend(str: String);
begin
  if self.Visible = False then exit;
  MemoDebug.Append('> ' + str);
end;


{-------------------------------------------------------------------------------
  AddNotify
 ------------------------------------------------------------------------------}
procedure TFormDebug.AddNotify(notify: TNotifyEvent);
begin
  mNotifyList.Add(notify);
end;



end.

