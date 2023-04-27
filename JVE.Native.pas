(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Native;

interface

uses System.Classes, System.SysUtils, System.UITypes, FMX.Controls, FMX.Types,
  FMX.Objects, FMX.Ani, {$IF CompilerVersion >= 26} FMX.Graphics, {$ENDIF}
  {$IF CompilerVersion >= 27} System.Messaging, {$ELSE} FMX.Messages, {$ENDIF}
  JVE.Actions, FMX.Forms, FMX.StdCtrls, FMX.ActnList, System.StrUtils, JVE.Utils;

// This control presents is a container presenting a native ad. A native ad
// provides bitmaps and texts, but it is up to the application to decide how
// the ad will look (naturally matching the layout of the app).
// The native ad to show is selected at random (using the specified weight)
// from the Ads collection.
// This could be based on an internet service (like PubNative or MobFox) or
// this could be a fallback ad, internal to your app (using the OnShouldShow
// event of the TJVECustomAd component).
//
// Alternatively, any component, implementing IJVEExecutable (like TJVESocial,
// TJVEAppStore, TJVEBrowser, TJVEPaymentItem, TJVEOpenURL and TJVEEmail)
// can be referenced, but you need to provide the banner image (TBitmap) for each.
//
// Several native ad containers can be locked to present the same native ad.
// To achieve this select the first TJVENativeAd component itself as the only Ad
// within the second one.
//
// Component-based ads are capable of checking whether they need to be cycled:
// * TJVEPaymentItem might not be presented if it was already bought.
// * TJVEAppStore might not be presented if the app is already installed.

type
  TJVENativeAdState = (nasStartup, nasFirst, nasNext);
  TJVENativeAd = class;
  TJVENativeAdHasAdsChanged = procedure(Sender: TJVENativeAd; HasAds: Boolean) of object;
  TJVENativeAdAnimate = procedure(Sender: TJVENativeAd; Source: TBitmap) of object;
  TJVENativeAdPresentation = procedure(Sender: TJVENativeAd; App: TJVEAppDetails) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVENativeAd = class(TControl, IJVEActionsUpdated)
  private
    FAdTimeout: Integer;
    FAds: TJVEActions;
    FOnAnimation: TJVENativeAdAnimate;
    FOnHasAdsChanged: TJVENativeAdHasAdsChanged;
    FOnPresentation: TJVENativeAdPresentation;
    FTimerHandle: TFmxHandle;
    FAdState: TJVENativeAdState;
    FCurrentAd: TJVEAction;
    FReportedHasAds: Boolean;
    FInitialTimeout: Integer;
    FNativeAdChangedId: Integer;
    function GetTimeout: Integer;
    procedure SetAds(const Value: TJVEActions);
    procedure SetAdTimeout(const Value: Integer);
    procedure SetInitialTimeout(const Value: Integer);
    procedure Update(Item: TJVEAction);
  protected
    function GetHasAds: Boolean; virtual;
    procedure Paint; override;
    procedure Click; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NativeAdChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure ResetTimer(Stop: Boolean = False);
    procedure UpdateHasAds;
    procedure UpdateViewHierarchy(Control: TFmxObject); virtual;
    procedure UpdatePresentation(Animate: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    // This procedure simulates an Ad Click. If you are implementing a parental
    // gate, on the OnClick event do the following:
    // - Disable the ad timer (by setting AdTimeout := 0)
    // - Show the parental gate
    // - Upon confirmation call this method
    // - In any case reenable the ad timer (by setting AdTimeout := 45)
    procedure AdClicked;

    // Requests the next ad immediately, without waiting for the timeout.
    procedure NextAd; virtual;

    // Reapplies the ad to the control specified (will also reapply the data
    // to more Control's children). Does not use OnAnimation event.
    class procedure UpdateVisualControl(Control: TFmxObject);

    // This property indicates that the control is presenting an ad.
    property HasAds: Boolean read GetHasAds;
    // This property provides an access to the current ad (if any). Notice,
    // unless the timer is stopped, this can change at any moment (the change
    // is done in the main thread).
    property CurrentAd: TJVEAction read FCurrentAd;
  published
    // This is the time for the initial ad switch. In case there are real and
    // substitute ads, this will usually be the time the substitute is visible,
    // before the real ad is received.
    property InitialTimeout: Integer read FInitialTimeout write SetInitialTimeout default 15;

    // This is the time between ad switches (if there are several).
    property AdTimeout: Integer read FAdTimeout write SetAdTimeout default 45;

    // This is the list of ads to show. This includes both ad networks and
    // custom ads, which are automatically used as substitutes on platforms,
    // which don't support native ads.
    property Ads: TJVEActions read FAds write SetAds;

    // This event is called when the status of the control is changed: it is either
    // now showing ads or it is no longer showing ads.
    // You can use this event to hide this control and reclaim screen space,
    // if there are no valid ads.
    property OnHasAdsChanged: TJVENativeAdHasAdsChanged
      read FOnHasAdsChanged write FOnHasAdsChanged;

    // This event allows animated transition from one ad to another.
    // To create a transition, add a transition effect to this control with a
    // Float Animation on the Progress property running from 100 to 0 (backwards!).
    // Within the event write the following:
    //   TransitionEffect.Target.Assign(Source);
    //   TransitionAnimation.Start;
    property OnAnimation: TJVENativeAdAnimate read FOnAnimation write FOnAnimation;

    // Called just before the user interface is updated for the new app presentation.
    // This is the last chance to update the app definition or to realign the
    // ad GUI (for example if picture's aspect ratio might change).
    property OnPresentation: TJVENativeAdPresentation
      read FOnPresentation write FOnPresentation;

    // These are the standard TControl properties, normally exposed.
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest;
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
    property Visible;
    property Width;
    property TabOrder;
    property TabStop;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  IJVENativeAdDetail = interface
    ['{A5C39D29-BD8A-4238-A8E8-8A6A1F71349C}']
    // Notifies the visual control that it should update itself with the new
    // app details.
    procedure Update(App: TJVEAppDetails);
  end;

  TJVENativeLabelDetail = (Title, Description, CallToAction, Advertiser, Rating);

  // This control should be placed within the TJVENativeAd to present one of the
  // texts, provided by an ad.
  // This double as a simplest implementation of the native ad capable control,
  // if you want to provide your own implementation for some reason.
  [ComponentPlatformsAttribute($000B945F)]
  TJVENativeLabel = class(TLabel, IJVENativeAdDetail)
  private
    FDetail: TJVENativeLabelDetail;
    procedure SetDetail(const Value: TJVENativeLabelDetail);
  protected
    procedure Update(App: TJVEAppDetails); virtual;
  published
    property Detail: TJVENativeLabelDetail read FDetail write SetDetail;
    property Text stored False;
  end;

  TJVENativeImageDetail = (Icon, Bitmap);

  // This control should be placed within the TJVENativeAd to present one of the
  // bitmaps, provided by an ad.
  // Notice, Icons are square (80x80 or 256x256, for example), while Bitmaps
  // are 1.91:1 (usually 1200x627, i.e. the Facebook standard).
  [ComponentPlatformsAttribute($000B945F)]
  TJVENativeImage = class(TImage, IJVENativeAdDetail)
  private
    FDetail: TJVENativeImageDetail;
    procedure SetDetail(const Value: TJVENativeImageDetail);
  protected
    procedure Paint; override;
    procedure Update(App: TJVEAppDetails); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Detail: TJVENativeImageDetail read FDetail
      write SetDetail default TJVENativeImageDetail.Icon;
    property HitTest default False;
    property Bitmap stored False;
    property MultiResBitmap stored False;
  end;

implementation

uses System.Types, System.UIConsts, System.Generics.Collections, System.Math,
  FMX.Platform;

type
  TNativeAdChangedMessage = class(TMessage<Boolean>);

{ TJVENativeAd }

constructor TJVENativeAd.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialTimeout := 15;
  FAdTimeout := 45;
  FAds := TJVEActions.Create(Self);
  FNativeAdChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TNativeAdChangedMessage, NativeAdChangedHandler);
end;

destructor TJVENativeAd.Destroy;
begin
  // Clearing ads will update the ad, calling the Unload event.
  Ads.Clear;
  FreeAndNil(FAds);
  ResetTimer(True);
  TMessageManager.DefaultManager.Unsubscribe(TNativeAdChangedMessage, FNativeAdChangedId);
  inherited Destroy;
end;

procedure TJVENativeAd.AfterConstruction;
begin
  inherited AfterConstruction;
  ResetTimer;
end;

function TJVENativeAd.GetHasAds: Boolean;
begin
  Result := FCurrentAd <> nil;
end;

procedure TJVENativeAd.Paint;
var
  R: TRectF;
begin
  inherited Paint;
  if (csDesigning in ComponentState) and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
    Canvas.Fill.Color := $FF909090;
    Canvas.FillText(R, Name + ': ' + ClassName + #13#10'Use TJVENativeLabel ' +
      'and TJVENativeImage to present details.', False, AbsoluteOpacity, [],
      {$IF CompilerVersion < 27}TTextAlign.taCenter{$ELSE}TTextAlign.Center{$ENDIF});
  end;
end;

function TJVENativeAd.GetTimeout: Integer;
begin
  case FAdState of
    nasFirst: Result := FInitialTimeout * 1000;
    nasNext: Result := FAdTimeout * 1000;
  else
    Result := 10;
  end;
end;

procedure TJVENativeAd.ResetTimer(Stop: Boolean);
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  if Stop then
    Exit;

  if (GetTimeout = 0) or (csDesigning in ComponentState) then
    FTimerHandle := 0
  else
    FTimerHandle := (TPlatformServices.Current.GetPlatformService
      (IFMXTimerService) as IFMXTimerService).CreateTimer(GetTimeout, NextAd);
end;

procedure TJVENativeAd.SetAdTimeout(const Value: Integer);
begin
  if FAdTimeout <> Value then
  begin
    FAdTimeout := Value;
    if FAdState = nasNext then
      ResetTimer;
  end;
end;

procedure TJVENativeAd.SetInitialTimeout(const Value: Integer);
begin
  if FInitialTimeout <> Value then
  begin
    FInitialTimeout := Value;
    if FAdState = nasFirst then
      ResetTimer;
  end;
end;

procedure TJVENativeAd.UpdateHasAds;
begin
  if FReportedHasAds <> HasAds then
  begin
    FReportedHasAds := not FReportedHasAds;
    if Assigned(FOnHasAdsChanged) then
      FOnHasAdsChanged(Self, FReportedHasAds);
  end;
end;

procedure TJVENativeAd.Update(Item: TJVEAction);
begin
  if (FAdState = nasStartup) or (FCurrentAd = nil) then
    Exit
  else if Item = FCurrentAd then
    UpdatePresentation(not Item.WasPresented)
  else
    NextAd;
end;

procedure TJVENativeAd.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (FAds <> nil) and (Operation = opRemove) then
    FAds.ComponentRemoved(AComponent);
end;

procedure TJVENativeAd.SetAds(const Value: TJVEActions);
begin
  FAds.Assign(Value);
end;

procedure TJVENativeAd.Click;
begin
  if not GetHasAds then
    Exit
  else if Assigned(OnClick) then
    OnClick(Self)
  else
    AdClicked;
end;

procedure TJVENativeAd.AdClicked;
var
  Exec: IJVEExecutable;
begin
  if (FCurrentAd = nil) or (FCurrentAd.Action = nil) then
    Exit;

  if FCurrentAd.Action is TJVENativeAd then
    TJVENativeAd(FCurrentAd.Action).AdClicked
  else if FCurrentAd.Action.GetInterface(IJVEExecutable, Exec) then
    Exec.Open(FCurrentAd)
end;

procedure TJVENativeAd.NextAd;
var
  MinValue, Value: Single;
  Animate: Boolean;
  Index: Integer;
  Items: TList<TJVEAction>;
  Item: TJVEAction;
  Exec: IJVEExecutable;
begin
  if (csDesigning in ComponentState)
    {$IF Defined(ANDROID)} or not IsRunningInForeground {$ENDIF} then
      Exit;

  if FAdState = nasStartup then
    FAdState := nasFirst
  else if FAdState = nasFirst then
    FAdState := nasNext;
  ResetTimer;

  for Index := 0 to FAds.Count - 1 do
  begin
    Item := FAds[Index];
    Item.Checked := (Item.Action = nil) or
      ((Item = FCurrentAd) and not Item.Action.GetInterface(IJVEManagedExecutable, Exec)) or
      (not Item.Action.GetInterface(IJVEExecutable, Exec) and not (Item.Action is TJVENativeAd));
  end;

  Items := TList<TJVEAction>.Create;
  try
    while True do
    begin
      MinValue := MaxInt;
      Items.Clear;
      for Index := 0 to FAds.Count - 1 do
      begin
        Item := FAds[Index];
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
      // calls, when several ads have the same relative weight.
      Item := Items[Random(Items.Count)];
      Item.Checked := True;
      Item.Impressions := Item.Impressions + 1;

      if (Item.Action is TJVENativeAd) or
        (Item.Action.GetInterface(IJVEExecutable, Exec) and Exec.CanOpen(Item)) then
          Break;
    end;
  finally
    FreeAndNil(Items);
  end;

  if (Item = nil) and (FCurrentAd <> nil) then
    for Index := 0 to FAds.Count - 1 do
      if (FAds[Index] = FCurrentAd) and (FCurrentAd.Action <> nil) then
      begin
        Item := FCurrentAd;
        Break;
      end;

  if (Item <> FCurrentAd) and (FCurrentAd <> nil) and (FCurrentAd.Collection = FAds) and
    (FCurrentAd.AppDetails <> nil) and Assigned(FCurrentAd.OnUnload) then
      FCurrentAd.OnUnload(FCurrentAd);

  if (Item <> FCurrentAd) and (Item <> nil) and Assigned(Item.OnLoad) then
    Item.OnLoad(Item);

  if Item <> nil then
  begin
    Animate := (FCurrentAd <> Item) or not Item.WasPresented;
    Item.WasPresented := True;

    if FCurrentAd = nil then
    begin
      FCurrentAd := Item;
      UpdateHasAds;
    end else
      FCurrentAd := Item;

    UpdatePresentation(Animate);
  end else if FCurrentAd <> nil then
  begin
    FCurrentAd := nil;
    UpdatePresentation(True);
    UpdateHasAds;
  end;
end;

procedure TJVENativeAd.NativeAdChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if (FCurrentAd <> nil) and (FCurrentAd.Action = Sender) then
    UpdatePresentation(TNativeAdChangedMessage(Msg).Value);
end;

procedure TJVENativeAd.UpdateViewHierarchy(Control: TFmxObject);
var
  Visual: IJVENativeAdDetail;
  Index: Integer;
begin
  if (FCurrentAd <> nil) and (FCurrentAd.Action <> nil) and
    (FCurrentAd.Action is TJVENativeAd) then
  begin
    TJVENativeAd(FCurrentAd.Action).UpdateViewHierarchy(Control);
    Exit;
  end;

  if Control.GetInterface(IJVENativeAdDetail, Visual) then
    if FCurrentAd <> nil then
      Visual.Update(FCurrentAd.AppDetails)
    else
      Visual.Update(nil);

  for Index := 0 to Control.ChildrenCount - 1 do
    UpdateViewHierarchy(Control.Children[Index]);
end;

procedure TJVENativeAd.UpdatePresentation(Animate: Boolean);
var
  Screen: TBitmap;
  Root: TComponent;
begin
  Root := (Self as IActionClient).GetRoot;
  if (Scene = nil) or (Root = nil) or not (Root is TCommonCustomForm) or
    (TCommonCustomForm(Root).Handle = nil) then
      Exit;

  // Update native ads, pointing to this one.
  TMessageManager.DefaultManager.SendMessage(Self, TNativeAdChangedMessage.Create(Animate));

  if Assigned(FOnAnimation) and Animate then
    Screen := MakeScreenshot
  else
    Screen := nil;

  if Assigned(FOnPresentation) then
    FOnPresentation(Self, FCurrentAd.AppDetails);
  UpdateViewHierarchy(Self);
  // The documentation specifies that we must notify the server once the ad was
  // actually presented on the screen. We don't know that, for example because
  // parent's opacity is low, or the form is covered by another form, so this
  // is as good a guess as any.
  if FCurrentAd <> nil then
    FCurrentAd.AppDetails.Presented(True);

  if Assigned(FOnAnimation) and (Screen <> nil) then
    FOnAnimation(Self, Screen);
  FreeAndNil(Screen);
end;

class procedure TJVENativeAd.UpdateVisualControl(Control: TFmxObject);
var
  NativeAd: TFmxObject;
begin
  NativeAd := Control;
  while (NativeAd <> nil) and not (NativeAd is TJVENativeAd) do
    NativeAd := NativeAd.Parent;
  if NativeAd <> nil then
    TJVENativeAd(NativeAd).UpdateViewHierarchy(Control);
end;

{ TJVENativeImage }

constructor TJVENativeImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := False;
end;

procedure TJVENativeImage.Paint;
begin
  inherited Paint;
  if (csDesigning in ComponentState) and not FInPaintTo then
  begin
    Canvas.Fill.Color := $FF909090;
    Canvas.FillText(LocalRect, IfThen(FDetail = TJVENativeImageDetail.Icon,
      'Icon', 'Bitmap'), False, AbsoluteOpacity, [], {$IF CompilerVersion < 27}
      TTextAlign.taCenter{$ELSE}TTextAlign.Center{$ENDIF});
  end;
end;

procedure TJVENativeImage.SetDetail(const Value: TJVENativeImageDetail);
begin
  if FDetail <> Value then
  begin
    FDetail := Value;
    Bitmap.SetSize(0, 0);
    TJVENativeAd.UpdateVisualControl(Self);
  end;
end;

procedure TJVENativeImage.Update(App: TJVEAppDetails);
begin
  if App = nil then
    Bitmap.SetSize(0, 0)
  else case FDetail of
    TJVENativeImageDetail.Icon: Bitmap.Assign(App.Icon);
    TJVENativeImageDetail.Bitmap: Bitmap.Assign(App.Bitmap);
  end;
end;

{ TJVENativeLabel }

procedure TJVENativeLabel.SetDetail(const Value: TJVENativeLabelDetail);
begin
  if FDetail <> Value then
  begin
    Text := '';
    FDetail := Value;
    if not (csDesigning in ComponentState) then
      TJVENativeAd.UpdateVisualControl(Self);
  end;

  if csDesigning in ComponentState then
    case FDetail of
      TJVENativeLabelDetail.Title: Text := 'Title';
      TJVENativeLabelDetail.Description: Text := 'Description';
      TJVENativeLabelDetail.CallToAction: Text := 'Call To Action';
      TJVENativeLabelDetail.Advertiser: Text := 'Advertiser';
      // Example rating illustrates the value better than the textual description
      TJVENativeLabelDetail.Rating: Text := #$2605#$2605#$2605#$2605#$2606;
    end;
end;

procedure TJVENativeLabel.Update(App: TJVEAppDetails);
begin
  if App <> nil then
    case FDetail of
      TJVENativeLabelDetail.Title: Text := App.Title;
      TJVENativeLabelDetail.Description: Text := App.Description;
      TJVENativeLabelDetail.CallToAction: Text := App.CallToAction;
      TJVENativeLabelDetail.Advertiser: Text := App.Advertiser;
      TJVENativeLabelDetail.Rating:
        if (App.Rating > 0) and (App.Rating <= 5) then
          Text := StringOfChar(#$2605, Round(App.Rating)).PadRight(5, #$2606)
        else
          Text := '';
    end
  else
    Text := '';
end;

end.

