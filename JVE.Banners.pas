(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Banners;

interface

uses System.Classes, System.SysUtils, System.UITypes, FMX.Controls, FMX.Types,
  FMX.Objects, FMX.Ani, {$IF CompilerVersion >= 26} FMX.Graphics, {$ENDIF}
  JVE.Actions, JVE.Utils;

// This control presents an ad-banner like control, which cycles through the
// defined Banners.
// This control can be used as a fallback for actual ads service. You can use
// a ready TJVEAdMob control for that or, if you prefer another ad network,
// see TJVEAdMob for an example implementation of this sort.
//
// This control can present banners, out of the Banners collection, based on
// the weight provided for each.
// Any component, implementing IJVEExecutable (like TJVESocial, TJVEAppStore,
// TJVEBrowser, TJVEPaymentItem, TJVEOpenURL and TJVEEmail) can be referenced,
// but you need to provide the banner image (TBitmap) for each.
//
// Banners are capable of checking whether they need to be cycled:
// * TJVEPaymentItem might not be presented if it was already bought.
// * TJVEAppStore might not be presented if the app is already installed.

type
  TJVEBanners = class;
  TJVEBannersHasAdsChanged = procedure(Sender: TJVEBanners; HasAds: Boolean) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEBanners = class(TControl, IJVEActionsUpdated)
  private
    FBanners: TJVEActions;
    FBannerTimeout: Integer;
    FTimerHandle: TFmxHandle;
    FInitialized: Boolean;
    FOnHasAdsChanged: TJVEBannersHasAdsChanged;
    FBanner: TJVEAction;
    FOther: TImage;
    FOpen, FClose: TFloatAnimation;
    FReportedHasAds: Boolean;
    procedure SetBanners(const Value: TJVEActions);
    procedure SetBannerTimeout(const Value: Integer);
    procedure DoneAnimation(Sender: TObject);
  protected
    FStopped: Boolean;
    FCurrent: TImage;
    procedure Paint; override;
    procedure Click; override;
    procedure ResetTimer(Stop: Boolean = False);
    procedure NextBanner; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure UpdateHasAds;
    procedure Update(Item: TJVEAction); virtual;
    function IsActionValid(Item: TComponent): Boolean;
    function GetHasAds: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    property HasAds: Boolean read GetHasAds;
  published
    // This is the time between banner switches (if there are several).
    property BannerTimeout: Integer read FBannerTimeout write SetBannerTimeout default 45;

    // This is the list of banners to show. For AdMob (below), these are fallback
    // banners, used on platforms, which don't support AdMob or when there are no ads.
    property Banners: TJVEActions read FBanners write SetBanners;

    // This event is called when the status of the control is changed: it is either
    // now showing ads or it is no longer showing ads.
    // You can use this event to hide this control and reclaim screen space,
    // if there are no valid ads.
    property OnHasAdsChanged: TJVEBannersHasAdsChanged
      read FOnHasAdsChanged write FOnHasAdsChanged;

    // These are the standard TControl properties, normally exposed.
    property Align default {$IF CompilerVersion < 27}TAlignLayout.alBottom{$ELSE}TAlignLayout.Bottom{$ENDIF};
    property Anchors;
    property Cursor default crDefault;
    property Locked default False;
    property Height;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
  end;

implementation

uses System.Types, System.UIConsts, System.Generics.Collections,
  System.Math, FMX.Platform;

{ TJVEBanners }

constructor TJVEBanners.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := {$IF CompilerVersion < 27}TAlignLayout.alBottom{$ELSE}TAlignLayout.Bottom{$ENDIF};
  FBannerTimeout := 45;
  FBanners := TJVEActions.Create(Self);

  FOpen := TFloatAnimation.Create(Self);
  FOpen.Duration := 0.25;
  FOpen.StopValue := 1;
  FOpen.StartFromCurrent := True;
  FOpen.PropertyName := 'Opacity';

  FClose := TFloatAnimation.Create(Self);
  FClose.Duration := 0.25;
  FClose.StartValue := 1;
  FClose.StartFromCurrent := True;
  FClose.PropertyName := 'Opacity';
  FClose.OnFinish := DoneAnimation;
end;

destructor TJVEBanners.Destroy;
begin
  FreeAndNil(FBanners);
  ResetTimer(True);
  inherited Destroy;
end;

procedure TJVEBanners.DoneAnimation(Sender: TObject);
begin
  FClose.Parent := nil;
  FOther.Parent := nil;
  FOther.DisposeOf;
  FOther := nil;
end;

function TJVEBanners.GetHasAds: Boolean;
begin
  Result := FBanner <> nil;
end;

procedure TJVEBanners.Paint;
var
  R: TRectF;
begin
  inherited Paint;
  Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity,
    TBrush.Create({$IF CompilerVersion < 27}TBrushKind.bkSolid{$ELSE}TBrushKind.Solid{$ENDIF}, claBlack));

  if (csDesigning in ComponentState) and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
    Canvas.Fill.Color := claWhite;
    Canvas.FillText(R, Name + ': ' + ClassName, False,
      AbsoluteOpacity, [], {$IF CompilerVersion < 27}TTextAlign.taCenter{$ELSE}TTextAlign.Center{$ENDIF});
  end;
end;

procedure TJVEBanners.ResetTimer(Stop: Boolean = False);
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  FStopped := Stop;
  if ((not Stop and (FBannerTimeout <> 0)) or not FInitialized)
    and not (csDesigning in ComponentState) then
      FTimerHandle := (TPlatformServices.Current.GetPlatformService
        (IFMXTimerService) as IFMXTimerService).CreateTimer(
        IfThen(FInitialized, FBannerTimeout * 1000, 10), NextBanner)
    else
      FTimerHandle := 0;
end;

procedure TJVEBanners.SetBannerTimeout(const Value: Integer);
begin
  if FBannerTimeout <> Value then
  begin
    FBannerTimeout := Value;
    if FInitialized and not FStopped then
      ResetTimer;
  end;
end;

procedure TJVEBanners.UpdateHasAds;
begin
  if FReportedHasAds <> HasAds then
  begin
    FReportedHasAds := not FReportedHasAds;
    if Assigned(FOnHasAdsChanged) then
      FOnHasAdsChanged(Self, FReportedHasAds);
  end;
end;

procedure TJVEBanners.Update(Item: TJVEAction);
begin
  if FInitialized and ((Item = FBanner) or (Item = nil)) then
    NextBanner;
end;

procedure TJVEBanners.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (FBanners <> nil) and (Operation = opRemove) then
    FBanners.ComponentRemoved(AComponent);
end;

procedure TJVEBanners.SetBanners(const Value: TJVEActions);
begin
  FBanners.Assign(Value);
end;

procedure TJVEBanners.AfterConstruction;
begin
  inherited AfterConstruction;
  ResetTimer;
end;

function TJVEBanners.IsActionValid(Item: TComponent): Boolean;
var
  Index: Integer;
begin
  if not FInitialized or (Item = nil) then
    Exit(False);

  for Index := 0 to FBanners.Count - 1 do
    if FBanners[Index].Action = Item then
      Exit(True);

  Result := False;
end;

procedure TJVEBanners.Click;
var
  Exec: IJVEExecutable;
  Item: TComponent;
begin
  inherited Click;
  if (FBanner = nil) or not IsActionValid(FBanner.Action) then
  begin
    if FInitialized then
      NextBanner;
    Exit;
  end;

  Item := FBanner.Action;
  if Item.GetInterface(IJVEExecutable, Exec) and Exec.CanOpen(FBanner) then
    Exec.Open(FBanner)
  else if FInitialized then
    NextBanner;
end;

procedure TJVEBanners.NextBanner;
var
  MinValue, Value: Single;
  Index: Integer;
  Items: TList<TJVEAction>;
  Item: TJVEAction;
  Exec: IJVEExecutable;
begin
  if not FInitialized and not (csDesigning in ComponentState) then
  begin
    FInitialized := True;
    ResetTimer;
  end;

  if not FInitialized then
    Exit;

  for Index := 0 to FBanners.Count - 1 do
  begin
    Item := FBanners[Index];
    Item.Checked := (Item.Action = nil) or (Item = FBanner) or
      not Item.Action.GetInterface(IJVEExecutable, Exec);
  end;

  Items := TList<TJVEAction>.Create;
  try
    while True do
    begin
      MinValue := MaxInt;
      Items.Clear;
      for Index := 0 to FBanners.Count - 1 do
      begin
        Item := FBanners[Index];
        if Item.Checked then
          Continue;

        Value := Item.Impressions / Item.Weight;
        if MinValue > Value then
        begin
          MinValue := Value;
          Items.Clear;
          Items.Add(Item);
        end else if MinValue = Value then
          Items.Add(Item);
      end;

      if Items.Count = 0 then
      begin
        Item := nil;
        Break;
      end;

      // Selecting one at random is particularly important at the very first
      // calls, when several interstitials have the same relative weight.
      Item := Items[Random(Items.Count)];
      Item.Checked := True;
      Item.Impressions := Item.Impressions + 1;

      if Item.Action.GetInterface(IJVEExecutable, Exec) and Exec.CanOpen(Item) then
      begin
        if Assigned(Item.OnLoad) then
          Item.OnLoad(Item);

        if not Item.Bitmap.IsEmpty then
          Break;
      end;
    end;
  finally
    FreeAndNil(Items);
  end;

  if (Item = nil) and (FBanner <> nil) then
    for Index := 0 to FBanners.Count - 1 do
      if (FBanners[Index] = FBanner) and (FBanner.Action <> nil) then
      begin
        if Assigned(FBanner.OnLoad) then
          FBanner.OnLoad(FBanner);

        if not FBanner.Bitmap.IsEmpty then
          Item := FBanner;
        Break;
      end;

  FOpen.Stop;
  FOpen.Parent := nil;
  FClose.Stop;
  FClose.Parent := nil;

  if FOther <> nil then
  begin
    FOther.Parent := nil;
    FOther.DisposeOf;
    FOther := nil;
  end;

  if Item <> nil then
  begin
    FBanner := Item;
    if FCurrent <> nil then
    begin
      FOther := FCurrent;
      FClose.Parent := FOther;
      FClose.Start;
    end else
      UpdateHasAds;

    FCurrent := TImage.Create(Self);
    FCurrent.Align := {$IF CompilerVersion < 27}TAlignLayout.alFit{$ELSE}TAlignLayout.Fit{$ENDIF};
    FCurrent.HitTest := False;
    FCurrent.Opacity := 0;
    FCurrent.WrapMode := {$IF CompilerVersion < 27}TImageWrapMode.iwStretch{$ELSE}TImageWrapMode.Stretch{$ENDIF};
    FCurrent.Bitmap.Assign(Item.Bitmap);
    FCurrent.Width := Item.Bitmap.Width;
    FCurrent.Height := Item.Bitmap.Height;
    FCurrent.Parent := Self;
    Item.WasPresented := True;

    if Assigned(Item.OnUnload) then
      Item.OnUnload(Item);

    FOpen.Parent := FCurrent;
    FOpen.Start;
  end else
  if FBanner <> nil then
  begin
    if (FBanner.Action = nil) or
      not FBanner.Action.GetInterface(IJVEExecutable, Exec) or
      not Exec.CanOpen(FBanner) then
    begin
      FOther := FCurrent;
      FCurrent := nil;
      FBanner := nil;
      FClose.Parent := FOther;
      FClose.Start;

      UpdateHasAds;
    end else
      FCurrent.Bitmap.Assign(FBanner.Bitmap);
  end;
end;

end.


