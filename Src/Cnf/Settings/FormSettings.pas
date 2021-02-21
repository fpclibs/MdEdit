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
unit FormSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Graphics, Dialogs, PairSplitter, ExtCtrls,
  StdCtrls, ComCtrls, Storage, Language,

  FrameSettingsInterface, SysUtils;

type

  { TFormSettings }

  TFormSettings = class(TForm)

    ButtonOk: TButton;
    ButtonCancel: TButton;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    TreeViewSettings: TTreeView;

    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeViewSettingsCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      {%H-}State: TCustomDrawState; var {%H-}DefaultDraw: Boolean);
    procedure TreeViewSettingsSelectionChanged(Sender: TObject);

  private
    mFrame: ISettingsInterface;
    mNFont, mBFont: TFont;
    mAppCnf: TAppStorage;
    FSettingsNotify: TNotifyEvent;

  private
    procedure InitTreeView;
    procedure InitFrame(frame: ISettingsInterface; visib: Boolean);
    procedure SetSettings(appCnf: TAppStorage);
    function  GetPathNode(ary: TStringArray; var tn: TTreeNode): Integer;

  public
    property Settings: TAppStorage read mAppCnf write SetSettings;
    Procedure AddFrame(newFrame: ISettingsInterface);
    property OnSettingsNotify: TNotifyEvent read FSettingsNotify write FSettingsNotify;
    procedure SetActiveFrame(index: Integer);

  private
    procedure OnReplaceLng();

  end;


implementation

{$R *.lfm}


{ TFormSettings }

{-------------------------------------------------------------------------------
  @NAME: FormCreate
 ------------------------------------------------------------------------------}
procedure TFormSettings.FormCreate(Sender: TObject);
begin
  mFrame := Nil;

  OLng.AddListener(@OnReplaceLng);

  InitTreeView;
end;


{-------------------------------------------------------------------------------
  @NAME: AddFrame
 ------------------------------------------------------------------------------}
Procedure TFormSettings.AddFrame(newFrame: ISettingsInterface);
var
  tn: TTreeNode;
  ary: TStringArray;
  i, j: Integer;
begin
  if TreeViewSettings.Items.Count < 1 then InitFrame(newFrame, true)
  else InitFrame(newFrame, false);

  ary := newFrame.GetTreePath;
  tn := Nil;

  j := GetPathNode(ary, tn);
  if tn = Nil then
  begin
    tn := TreeViewSettings.Items.Add(Nil, ary[0]);
    tn.Data:={%H-}Pointer(newFrame);
  end;
  for i:=j+1 to Length(ary)-1 do
  begin
    tn := TreeViewSettings.Items.AddChild(tn, ary[i]);
    tn.Data:={%H-}Pointer(newFrame);
  end;

  TreeViewSettings.FullExpand;
end;


{-------------------------------------------------------------------------------
  @NAME: GetPathNode
 ------------------------------------------------------------------------------}
function TFormSettings.GetPathNode(ary: TStringArray; var tn: TTreeNode): Integer;
var
  i,j: Integer;
begin
  tn := Nil;
  Result := 0;
  for j:=0 to Length(ary)-2 do
  begin
    for i:=0 to TreeViewSettings.Items.Count-1 do
    begin
      if TreeViewSettings.Items[i].Text = ary[j] then
       begin
         tn := TreeViewSettings.Items[i];
         Result := j;
       end;
    end;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: InitFrame
 ------------------------------------------------------------------------------}
procedure TFormSettings.InitFrame(frame: ISettingsInterface; visib: Boolean);
var
  rect: TRect;
begin
  rect := Panel1.BoundsRect;
  rect.Width  := rect.Width  - 10;

  frame.Settings := mAppCnf;
  frame.BoundsRect := rect;
  frame.Parent := Panel1;
  frame.Visible := visib;
  if visib then mFrame := frame;
end;


{-------------------------------------------------------------------------------
  @NAME: FormClose
 ------------------------------------------------------------------------------}
procedure TFormSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction );
begin
  with mAppCnf do begin
    Write('DlgSettings/Position/Left', Left);
    Write('DlgSettings/Position/Top', Top);
    Write('DlgSettings/Position/Width', Width);
    Write('DlgSettings/Position/Height', Height);
    Write('DlgSettings/Position/Splitter', PairSplitter1.Position);
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: SetSettings
 ------------------------------------------------------------------------------}
procedure TFormSettings.SetSettings(appCnf: TAppStorage);
var
  last, frame: ISettingsInterface;
  i: Integer;
begin
  mAppCnf := appCnf;

  with mAppCnf do begin
    Left   := Read('DlgSettings/Position/Left',   Left);
    Top    := Read('DlgSettings/Position/Top',    Top);
    Width  := Read('DlgSettings/Position/Width',  Width);
    Height := Read('DlgSettings/Position/Height', Height);

    PairSplitter1.Position := Read('DlgSettings/Position/Splitter', PairSplitter1.Position);
  end;

  last := Nil;
  for i:=0 to TreeViewSettings.Items.Count-1 do
  begin
    frame := ISettingsInterface(TreeViewSettings.Items[i].Data);
    if last = frame then continue;
    frame.Settings := mAppCnf;
    last := frame;
  end;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonOkClick
 ------------------------------------------------------------------------------}
procedure TFormSettings.ButtonOkClick(Sender: TObject);
var
  last, frame: ISettingsInterface;
  i: Integer;
begin
  last := Nil;
  for i:=0 to TreeViewSettings.Items.Count-1 do
  begin
    frame := ISettingsInterface(TreeViewSettings.Items[i].Data);
    if last = frame then continue;
    frame.GetSettings;
    last := frame;
  end;
  if Assigned(FSettingsNotify) then FSettingsNotify(Self);
  mAppCnf.Save();
  Close;
end;


{-------------------------------------------------------------------------------
  @NAME: ButtonCancelClick
 ------------------------------------------------------------------------------}
procedure TFormSettings.ButtonCancelClick(Sender: TObject);
var
  last, frame: ISettingsInterface;
  i: Integer;
begin
  last := Nil;
  for i:=0 to TreeViewSettings.Items.Count-1 do
  begin
    frame := ISettingsInterface(TreeViewSettings.Items[i].Data);
    if last = frame then continue;
    frame.SetCancel;
    last := frame;
  end;
  Close;
end;


{-------------------------------------------------------------------------------
  @NAME: FormDestroy
 ------------------------------------------------------------------------------}
procedure TFormSettings.FormDestroy(Sender: TObject);
begin
  mNFont.Free;
  mBFont.Free;
end;


{-------------------------------------------------------------------------------
  @NAME: TreeViewSettingsCustomDrawItem
 ------------------------------------------------------------------------------}
procedure TFormSettings.TreeViewSettingsCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if Node.Level = 0 then
    TreeViewSettings.Font.Assign(mBFont)
  else
    TreeViewSettings.Font.Assign(mNFont)
end;


{-------------------------------------------------------------------------------
  @NAME: TreeViewSettingsSelectionChanged
 ------------------------------------------------------------------------------}
procedure TFormSettings.TreeViewSettingsSelectionChanged(Sender: TObject);
var
  tn: TTreeNode;
begin
  tn := TreeViewSettings.Selected;
  if tn.Data = Nil then exit;
  if Not (mFrame = Nil) then mFrame.Visible := False;
  mFrame := ISettingsInterface(tn.Data);
  mFrame.Visible := True;
end;


{-------------------------------------------------------------------------------
  @NAME: InitTreeView
 ------------------------------------------------------------------------------}
procedure TFormSettings.InitTreeView;
var
  fontStyle: TFontStyles;
begin
  FontStyle:= [fsBold];
  mNFont := TFont.Create;
  mNFont.Assign(TreeViewSettings.Font);
  mBFont := TFont.Create;
  mBFont.Assign(TreeViewSettings.Font);
  mBFont.Style:=FontStyle;
end;


{-------------------------------------------------------------------------------
  Event OnReplaceLng
 ------------------------------------------------------------------------------}
procedure TFormSettings.OnReplaceLng();
var
  frame : ISettingsInterface;
  i, e  : Integer;
  ary   : TStringArray;
begin
  LCap('T_FormSettings_FormSettings', Self);
  LCap('T_General_BtnOk',             ButtonOk);
  LCap('T_General_BtnCancel',         ButtonCancel);

  for i := 0 to TreeViewSettings.Items.Count-1 do
  begin
    frame := ISettingsInterface(TreeViewSettings.Items[i].Data);
    ary   := frame.GetTreePath;
    e     := TreeViewSettings.Items[i].Level;
    TreeViewSettings.Items[i].Text := ary[e];
  end;
end;


{-------------------------------------------------------------------------------
  SetActiveFrame
 ------------------------------------------------------------------------------}
procedure TFormSettings.SetActiveFrame(index: Integer);
begin
  if index < TreeViewSettings.Items.Count then
  begin
    TreeViewSettings.Items[index].Selected := True;
  end;
end;



end.

