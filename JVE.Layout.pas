(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Layout;

interface

uses
  System.SysUtils, System.UITypes, System.Types, System.Classes, FMX.Types,
  System.Math, FMX.Objects, FMX.Controls, FMX.Layouts, JVE.Utils
  {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

type
  // This layout allows you to span several cells (horizontally or vertically)
  // within the TJVELayout grid layout control (below).
  // This is its whole function, it should not be used anywhere else.
  [ComponentPlatformsAttribute($000B945F)]
  TJVESpan = class(TLayout)
  private
    FColSpan: Integer;
    FRowSpan: Integer;
    procedure SetColSpan(const Value: Integer);
    procedure SetRowSpan(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  published
    // Number of columns this layout will span (horizontally).
    property ColSpan: Integer read FColSpan write SetColSpan default 1;
    // Number of rows this layout will span (vertically).
    property RowSpan: Integer read FRowSpan write SetRowSpan default 1;
  end;

  // This is the definition of a single column or row. It specifies the number
  // of pixels this column will occupy plus the its share of the remaining pixels.
  // For example:
  // |                     Total width: 670px                        |
  // |--+------------------------+--+--------------+--------------+--+
  // |10|          50%           |10|     25%      |     25%      |10|
  // |px|                        |px|    +20px     |    +20px     |px|
  // Here each percent is (670-10-10-20-20-10)/100 = 6px and the result:
  // |10|         300px          |10|    170px     |    170px     |10|
  // You can use percents (50,25,25) or shares (2,1,1), i.e. the total number
  // of shares can be anything, not just 100.
  TJVELayoutItem = class(TCollectionItem)
  private
    FShares: Integer;
    FPixels: Integer;
    FDivider: Boolean;
    procedure SetDivider(const Value: Boolean);
    procedure SetPixels(const Value: Integer);
    procedure SetShares(const Value: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pixels: Integer read FPixels write SetPixels default 0;
    property Shares: Integer read FShares write SetShares default 1;
    // Setting Divider to True will indicate a visual divider column or row;
    // no controls will be placed in this column (row; except spanning).
    property Divider: Boolean read FDivider write SetDivider default False;
  end;

  TJVELayoutItems = class(TOwnedCollection)
  protected
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    function GetItem(Index: Integer): TJVELayoutItem;
  public
    property Items[Index: Integer]: TJVELayoutItem read GetItem; default;
  end;

  [ComponentPlatformsAttribute($000B945F)]
  TJVELayout = class(TJVESpan)
  private
    FColumns: TJVELayoutItems;
    FRows: TJVELayoutItems;
    procedure SetColumns(const Value: TJVELayoutItems);
    procedure SetRows(const Value: TJVELayoutItems);
    function CalcOffsets(const List: TJVELayoutItems;
      Offset, Width: Single): TArray<Integer>;
  protected
    procedure DoRealign; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Columns: TJVELayoutItems read FColumns write SetColumns;
    property Rows: TJVELayoutItems read FRows write SetRows;
  end;

implementation

{ TJVELayout }

procedure TJVELayout.DoAddObject(const AObject: TFmxObject);
begin
  inherited DoAddObject(AObject);
  Realign;
end;

procedure TJVELayout.DoRemoveObject(const AObject: TFmxObject);
begin
  inherited DoRemoveObject(AObject);
  Realign;
end;

procedure TJVELayout.SetColumns(const Value: TJVELayoutItems);
begin
  FColumns.Assign(Value);
  Realign;
end;

procedure TJVELayout.SetRows(const Value: TJVELayoutItems);
begin
  FRows.Assign(Value);
  Realign;
end;

constructor TJVELayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := TJVELayoutItems.Create(Self, TJVELayoutItem);
  FRows := TJVELayoutItems.Create(Self, TJVELayoutItem);
end;

destructor TJVELayout.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TJVELayout.Paint;
var
  Rect: TRectF;
  Brush: TBrush;
  X, Y: Integer;
  Xo, Yo: TArray<Integer>;
begin
  inherited Paint;
  if not (csDesigning in ComponentState) or Locked or FInPaintTo then
    Exit;

  if (FColumns = nil) or (FRows = nil) or (FColumns.Count = 0) or (FRows.Count = 0) then
  begin
    Rect := LocalRect;
    InflateRect(Rect, -0.5, -0.5);
    Canvas.DrawDashRect(Rect, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
    Canvas.Fill.Color := $FF909090;
    Canvas.FillText(Rect, 'Create at least one'#13#10'column and one row', False,
      AbsoluteOpacity, [], {$IF CompilerVersion < 27}TTextAlign.taCenter{$ELSE}TTextAlign.Center{$ENDIF});
  end else
  begin
    Brush := TBrush.Create({$IF CompilerVersion < 27}TBrushKind.bkSolid{$ELSE}TBrushKind.Solid{$ENDIF}, $2040FF40);
    Xo := CalcOffsets(FColumns, Margins.Left, Width - Margins.Left - Margins.Right);
    Yo := CalcOffsets(FRows, Margins.Top, Height - Margins.Top - Margins.Bottom);

    for X := 0 to FColumns.Count - 1 do
      for Y := 0 to FRows.Count - 1 do
      begin
        Rect := RectF(Xo[X], Yo[Y], Xo[X + 1], Yo[Y + 1]);
        if FColumns[X].FDivider or FRows[Y].FDivider then
        begin
          InflateRect(Rect, -0.5, -0.5);
          Canvas.DrawDashRect(Rect, 0, 0, AllCorners, AbsoluteOpacity, $30FF0000);
        end else
          Canvas.FillRect(Rect, 0, 0, AllCorners, AbsoluteOpacity, Brush);
      end;
  end;
end;

function TJVELayout.CalcOffsets(const List: TJVELayoutItems;
  Offset, Width: Single): TArray<Integer>;
var
  Shares, Index, Minimum, Size, Processed, Current: Integer;
begin
  SetLength(Result, List.Count + 1);

  Shares := 0;
  for Index := 0 to List.Count - 1 do
  begin
    Width := Width - List[Index].FPixels;
    Inc(Shares, List[Index].FShares);

    if List[Index].FPixels + List[Index].FShares = 0 then
      Inc(Shares);
  end;

  Processed := -1;
  repeat
    if Width < 0 then
      Width := 0;

    Minimum := MaxInt;
    for Index := 0 to List.Count - 1 do
    begin
      Current := List[Index].FShares;
      if List[Index].FPixels + Current = 0 then
        Inc(Current);

      if (Current > Processed) and (Current < Minimum) then
        Minimum := Current;
    end;

    if Minimum < MaxInt then
    begin
      Processed := Minimum;
      if (Minimum > 0) and (Shares > 0) then
        Size := Round(Width * Minimum / Shares)
      else
        Size := 0;

      for Index := 0 to List.Count - 1 do
      begin
        Current := List[Index].FShares;
        if List[Index].FPixels + Current = 0 then
          Inc(Current);

        if Current = Minimum then
        begin
          Result[Index + 1] := Max(Size + List[Index].FPixels, 1);
          Shares := Shares - Minimum;
          Width := Width - Size;
        end;
      end;
    end;
  until Minimum = MaxInt;

  Result[0] := Round(Offset);
  for Index := 0 to List.Count - 1 do
    Inc(Result[Index + 1], Result[Index]);
end;

procedure TJVELayout.DoRealign;
var
  Cnt, X, Y, Inner: Integer;
  Current: Single;
  Child: TControl;
  Size: TSizeF;
  Rect: TRectF;
  ColSpan, RowSpan: Integer;
  Xo, Yo: TArray<Integer>;
  Avails: TArray<TArray<Boolean>>;
begin
  if FDisableAlign or (FColumns = nil) or (FRows = nil) or
    (FColumns.Count = 0) or (FRows.Count = 0) then Exit;
  FDisableAlign := True;

  Xo := CalcOffsets(FColumns, Margins.Left, Width - Margins.Left - Margins.Right);
  Yo := CalcOffsets(FRows, Margins.Top, Height - Margins.Top - Margins.Bottom);

  SetLength(Avails, FColumns.Count, FRows.Count);
  for X := 0 to FColumns.Count - 1 do
    for Y := 0 to FRows.Count - 1 do
      Avails[X, Y] := not FColumns[X].FDivider and not FRows[Y].FDivider;

  X := -1;
  Y := 0;
  Cnt := 0;
  while Cnt < ControlsCount do
  begin
    Child := Controls[Cnt];
    if Child.ClassName = 'TGrabHandle.TGrabHandleRectangle' then
    begin
      Inc(Cnt);
      Continue;
    end;

    Inc(X);
    if X >= FColumns.Count then
    begin
      X := 0;
      Inc(Y);
      if Y >= FRows.Count then
        Break;
    end;

    if not Avails[X, Y] then
      Continue;

    if not (Child is TEllipse) then
    begin
      ColSpan := 1;
      RowSpan := 1;
      if Child is TJVESpan then
      begin
        ColSpan := Min(TJVESpan(Child).FColSpan, FColumns.Count - X);
        RowSpan := Min(TJVESpan(Child).FRowSpan, FRows.Count - Y);
      end;

      Rect := RectF(Xo[X] + Child.Padding.Left, Yo[Y] + Child.Padding.Top,
        Xo[X + ColSpan] - Child.Padding.Right, Yo[Y + RowSpan] - Child.Padding.Bottom);

      while RowSpan > 0 do
      begin
        Dec(RowSpan);
        for Inner := 0 to ColSpan - 1 do
          Avails[X + Inner, Y + RowSpan] := False;
      end;

      if Child.Align in [
        {$IF CompilerVersion < 27}TAlignLayout.alFit{$ELSE}TAlignLayout.Fit{$ENDIF},
        {$IF CompilerVersion < 27}TAlignLayout.alFitLeft{$ELSE}TAlignLayout.FitLeft{$ENDIF},
        {$IF CompilerVersion < 27}TAlignLayout.alFitRight{$ELSE}TAlignLayout.FitRight{$ENDIF}]
         then
      begin
        Size := Rect.size;
        Current := Child.Height / Child.Width;
        if Size.height / Size.width > Current then
          Rect.height := Rect.width * Current
        else
          Rect.width := Rect.height / Current;

        case Child.Align of
          {$IF CompilerVersion < 27}TAlignLayout.alFit{$ELSE}TAlignLayout.Fit{$ENDIF}: Rect.Offset((Size.Width - Rect.Width) / 2,
            (Size.Height - Rect.Height) / 2);
          {$IF CompilerVersion < 27}TAlignLayout.alFitRight{$ELSE}TAlignLayout.FitRight{$ENDIF}: Rect.Offset(Size.Width - Rect.Width,
            Size.Height - Rect.Height);
        end;
      end;

      Child.SetBounds(Rect.Left, Rect.Top, Rect.Width, Rect.Height);
    end;
    Inc(Cnt);
  end;

  FDisableAlign := False;
end;

{ TJVESpan }

constructor TJVESpan.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColSpan := 1;
  FRowSpan := 1;
end;

procedure TJVESpan.SetColSpan(const Value: Integer);
begin
  if FColSpan <> Value then
  begin
    FColSpan := Value;
    if (Parent <> nil) and (Parent is TJVELayout) then
      TJVELayout(Parent).DoRealign;
  end;
end;

procedure TJVESpan.SetRowSpan(const Value: Integer);
begin
  if FRowSpan <> Value then
  begin
    FRowSpan := Value;
    if (Parent <> nil) and (Parent is TJVELayout) then
      TJVELayout(Parent).DoRealign;
  end;
end;

{ TJVELayoutItem }

procedure TJVELayoutItem.Assign(Source: TPersistent);
var
  Item: TJVELayoutItem absolute Source;
begin
  if Source is TJVELayoutItem then
  begin
    FShares := Item.FShares;
    FPixels := Item.FPixels;
    FDivider := Item.FDivider;
  end else
    inherited Assign(Source);
end;

constructor TJVELayoutItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FShares := 1;
  FPixels := 0;
  FDivider := False;
end;

function TJVELayoutItem.GetDisplayName: string;
begin
  if FPixels = 0 then
    Result := IntToStr(FShares)
  else if FShares = 0 then
    Result := IntToStr(FPixels) + 'px'
  else
    Result := Format('%d + %dpx', [FShares, FPixels]);

  if FDivider then
    Result := Result + ' - (divider)';
end;

procedure TJVELayoutItem.SetDivider(const Value: Boolean);
begin
  if FDivider <> Value then
  begin
    FDivider := Value;
    TJVELayout(Collection.Owner).DoRealign;
  end;
end;

procedure TJVELayoutItem.SetPixels(const Value: Integer);
begin
  if FPixels <> Value then
  begin
    FPixels := Max(Value, 0);
    TJVELayout(Collection.Owner).DoRealign;
  end;
end;

procedure TJVELayoutItem.SetShares(const Value: Integer);
begin
  if FShares <> Value then
  begin
    FShares := Max(Value, 0);
    TJVELayout(Collection.Owner).DoRealign;
  end;
end;

{ TJVELayoutItems }

function TJVELayoutItems.GetItem(Index: Integer): TJVELayoutItem;
begin
  Result := TJVELayoutItem(inherited GetItem(Index));
end;

procedure TJVELayoutItems.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited Notify(Item, Action);
  TJVELayout(Owner).DoRealign;
end;

end.

