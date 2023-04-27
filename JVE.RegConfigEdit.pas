(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(*                                                                        *)
(* Based on the copyright(c) 1995-2015 Embarcadero Technologies, Inc code *)
(**************************************************************************)

unit JVE.RegConfigEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Actions,
  System.Types, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Menus,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ImgList, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnPopup,
  Vcl.PlatformDefaultStyleActnCtrls, DesignIntf, DesignWindows, ToolWnds,
  DesignEditors, JVE.Configuration;

const
  AM_DeferUpdate = WM_USER + 100;  // avoids break-before-make listview ugliness

type
  TJVEConfigValueClass = class of TJVEConfigurationValue;

type
  TJVERegConfigEdit = class(TToolbarDesignWindow)
    Panel3: TPanel;
    ImageList1: TImageList;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    DeleteCmd: TAction;
    SelectAllCmd: TAction;
    N2: TMenuItem;
    PopupMenu3: TPopupMenu;
    Integer1: TMenuItem;
    Boolean1: TMenuItem;
    N1: TMenuItem;
    Date1: TMenuItem;
    ime1: TMenuItem;
    DateandTime1: TMenuItem;
    Float1: TMenuItem;
    String1: TMenuItem;
    N3: TMenuItem;
    ListBox1: TListBox;
    IntegerwithRange1: TMenuItem;
    DoublewithRange1: TMenuItem;
    procedure DeleteClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure SelectAllCommandUpdate(Sender: TObject);
    procedure SelectionUpdate(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: Char);
    procedure ListView1KeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Integer1Click(Sender: TObject);
    procedure Float1Click(Sender: TObject);
    procedure String1Click(Sender: TObject);
    procedure Boolean1Click(Sender: TObject);
    procedure Date1Click(Sender: TObject);
    procedure ime1Click(Sender: TObject);
    procedure DateandTime1Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure IntegerwithRange1Click(Sender: TObject);
    procedure DoublewithRange1Click(Sender: TObject);
  private
    FClosing: Boolean;
    FStateLock: Integer;
    FSelectionError: Boolean;
    procedure AddValue(ValueClass: TJVEConfigValueClass);
    procedure AMDeferUpdate(var Msg); message AM_DeferUpdate;
    procedure CloseEditor;
    procedure ScanValues(Child: TComponent);
  protected
    procedure Activated; override;
    procedure LockState;
    procedure UnlockState;
    property StateLock: Integer read FStateLock;
    procedure SelectAll(DoUpdate: Boolean = True);
    procedure SelectNone(DoUpdate: Boolean = True);
  public
    Component: TComponent;
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent); override;
    procedure DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemsModified(const ADesigner: IDesigner); override;
    procedure GetSelection;
    procedure SelectionChanged(const ADesigner: IDesigner;
      const ASelection: IDesignerSelections); override;
    procedure SetSelection;
    procedure UpdateListbox;
  end;

function ShowConfigEditor(ADesigner: IDesigner; AComponent: TComponent): TJVERegConfigEdit;

implementation

{$R *.dfm}

uses System.TypInfo, DesignConst, ComponentDesigner;

var
  ConfigEditorsList: TList = nil;

function ShowConfigEditor(ADesigner: IDesigner; AComponent: TComponent): TJVERegConfigEdit;
var
  Idx: Integer;
begin
  if ConfigEditorsList = nil then
    ConfigEditorsList := TList.Create;

  for Idx := 0 to ConfigEditorsList.Count-1 do
  begin
    Result := TJVERegConfigEdit(ConfigEditorsList[Idx]);
    with Result do
      if (Designer = ADesigner) and (Component = AComponent) then
      begin
        Caption := AComponent.Name + ' Values';
        Show;
        BringToFront;
        Exit;
      end;
  end;

  Result := TJVERegConfigEdit.Create(Application);
  with Result do
  try
    Caption := AComponent.Name + ' Values';
    Designer := ADesigner;
    Component := AComponent;
    UpdateListbox;
    Show;
  except
    Free;
  end;
end;

{ TCollectionEditor }

procedure TJVERegConfigEdit.Activated;
begin
  Designer.Activate;
  SetSelection;
end;

procedure TJVERegConfigEdit.Integer1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationInteger);
end;

procedure TJVERegConfigEdit.IntegerwithRange1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationIntegerRange);
end;

procedure TJVERegConfigEdit.ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
begin
  if (AItem = nil) or FClosing then Exit;
  if (Component = nil) or (csDestroying in Component.ComponentState) or (AItem = Component) then
    CloseEditor;
end;

procedure TJVERegConfigEdit.DesignerClosed(const ADesigner: IDesigner; AGoingDormant: Boolean);
begin
  if Designer = ADesigner then
    CloseEditor;
end;

procedure TJVERegConfigEdit.DoublewithRange1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationFloatRange);
end;

procedure TJVERegConfigEdit.CloseEditor;
begin
  FClosing := True;
  Component := nil;
  Close;
end;

procedure TJVERegConfigEdit.ItemsModified(const ADesigner: IDesigner);
var
  Root: IRoot;
begin
  if FClosing then exit;
  if Component <> nil then
  begin
    Caption := Component.Name + ' Values';
    UpdateListbox;
    Root := ActiveRoot;
    if (Root = nil) or (Root.GetDesigner <> Designer) then
      Exit;
    GetSelection;
  end;
end;

procedure TJVERegConfigEdit.GetSelection;
var
  Idx, Item: Integer;
  List: IDesignerSelections;
begin
  LockState;
  try
    for Idx := 0 to ListBox1.Count - 1 do
      ListBox1.Selected[Idx] := False;
  finally
    UnlockState;
  end;

  List := CreateSelectionList;
  Designer.GetSelections(List);
  if List.Count = 0 then Exit;
  if not (List[0] is TJVEConfigurationValue) or
    (TJVEConfigurationValue(List[0]).GetParentComponent <> Component) then Exit;
  UpdateListbox;

  LockState;
  try
    for Idx := 0 to ListBox1.Count - 1 do
      for Item := 0 to List.Count - 1 do
        if ListBox1.Items.Objects[Idx] = List[Item] then
        begin
          ListBox1.Selected[Idx] := True;
          Break;
        end;
  finally
    UnlockState;
  end;
end;

procedure TJVERegConfigEdit.ime1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationTime);
end;

procedure TJVERegConfigEdit.LockState;
begin
  Inc(FStateLock);
end;

procedure TJVERegConfigEdit.SelectionChanged(const ADesigner: IDesigner;
  const ASelection: IDesignerSelections);
begin
end;

procedure TJVERegConfigEdit.SetSelection;
var
  Idx: Integer;
  List: IDesignerSelections;
begin
  if FSelectionError then
    Exit;

  try
    if ListBox1.SelCount > 0 then
    begin
      List := CreateSelectionList;
      for Idx := 0 to ListBox1.Count - 1 do
        if ListBox1.Selected[Idx] then
          List.Add(TComponent(ListBox1.Items.Objects[Idx]));
      Designer.SetSelections(List);
    end else
      Designer.SelectComponent(Component);
  except
    FSelectionError := True;
    Application.HandleException(ExceptObject);
    Close;
  end;
end;

procedure TJVERegConfigEdit.String1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationString);
end;

procedure TJVERegConfigEdit.ToolButton1Click(Sender: TObject);
begin
  ToolButton1.CheckMenuDropdown;
end;

procedure TJVERegConfigEdit.UnlockState;
begin
  Dec(FStateLock);
end;

procedure TJVERegConfigEdit.UpdateListbox;
begin
  if Component = nil then Exit;

  LockState;
  try
    ListBox1.Clear;
    TJVEConfiguration(Component).GetChildren(ScanValues, nil);
  finally
    UnlockState;
  end;
end;

procedure TJVERegConfigEdit.ScanValues(Child: TComponent);
begin
  ListBox1.AddItem(TJVEConfigurationValue(Child).Identifier + ' - ' + Child.Name, Child);
end;

procedure TJVERegConfigEdit.AddValue(ValueClass: TJVEConfigValueClass);
begin
  LockState;
  try
    TJVEConfigurationValue(Designer.CreateChild(ValueClass, Component)).
      SetParentComponent(Component);
  finally
    UnlockState;
    UpdateListbox;

    ListBox1.Selected[ListBox1.Count - 1] := True;
    SetSelection;
  end;
end;

procedure TJVERegConfigEdit.Date1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationDate);
end;

procedure TJVERegConfigEdit.DateandTime1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationDateTime);
end;

procedure TJVERegConfigEdit.DeleteClick(Sender: TObject);
var
  Idx: Integer;
begin
  LockState;
  try
    for Idx := 0 to ListBox1.Count - 1 do
      if ListBox1.Selected[Idx] then
        ListBox1.Items.Objects[Idx].Free;
  finally
    UnlockState;
    UpdateListBox;
    Designer.Modified;
  end;
end;

procedure TJVERegConfigEdit.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    ComponentDesigner.Environment.ModalEdit(#0, Self);
end;

procedure TJVERegConfigEdit.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    ComponentDesigner.Environment.ModalEdit(#0, Self);
end;

procedure TJVERegConfigEdit.Float1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationFloat);
end;

procedure TJVERegConfigEdit.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Component <> nil then
    Designer.SelectComponent(Component);

  Action := caFree;
  LockState;
end;

procedure TJVERegConfigEdit.FormCreate(Sender: TObject);
begin
  ConfigEditorsList.Add(Self);
end;

procedure TJVERegConfigEdit.FormDestroy(Sender: TObject);
begin
  if ConfigEditorsList <> nil then
    ConfigEditorsList.Remove(Self);
end;

procedure TJVERegConfigEdit.ListBox1Click(Sender: TObject);
var
  Msg: TMsg;
begin
  if (FStateLock = 0) and not PeekMessage(Msg, Handle, AM_DeferUpdate, AM_DeferUpdate, PM_NOREMOVE) then
    PostMessage(Handle, AM_DeferUpdate, 0, 0);
end;

procedure TJVERegConfigEdit.AMDeferUpdate(var Msg);
begin
  if FStateLock <> 0 then
    PostMessage(Handle, AM_DeferUpdate, TMessage(Msg).WParam, TMessage(Msg).LParam)
  else if TMessage(Msg).WParam = 0 then
    SetSelection
  else
    ItemsModified(nil);
end;

procedure TJVERegConfigEdit.Boolean1Click(Sender: TObject);
begin
  AddValue(TJVEConfigurationBoolean);
end;

procedure TJVERegConfigEdit.SelectAll1Click(Sender: TObject);
begin
  SelectAll;
end;

procedure TJVERegConfigEdit.SelectionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBox1.SelCount <> 0;
end;

procedure TJVERegConfigEdit.SelectAllCommandUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := ListBox1.Count > 0;
end;

procedure TJVERegConfigEdit.SelectAll(DoUpdate: Boolean);
begin
  LockState;
  try
    ListBox1.SelectAll;
  finally
    UnlockState;
    if DoUpdate then
      SetSelection;
  end;
end;

procedure TJVERegConfigEdit.SelectNone(DoUpdate: Boolean);
var
  Idx: Integer;
begin
  LockState;
  try
    for Idx := 0 to ListBox1.Count - 1 do
      ListBox1.Selected[Idx] := False;
  finally
    UnlockState;
    if DoUpdate then
      SetSelection;
  end;
end;

procedure TJVERegConfigEdit.ListView1KeyPress(Sender: TObject; var Key: Char);
begin
  if CharInSet(Key, ['!'..'~']) then
  begin
    ComponentDesigner.Environment.ModalEdit(Key, Self);
    Key := #0;
  end;
end;

procedure TJVERegConfigEdit.ListView1KeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = VK_PROCESSKEY then
    ComponentDesigner.Environment.ModalEdit(#0, Self);
end;

procedure TJVERegConfigEdit.FormShow(Sender: TObject);
begin
  inherited;
  MakeFullyVisible;
end;

initialization
finalization
  FreeAndNil(ConfigEditorsList);
end.
