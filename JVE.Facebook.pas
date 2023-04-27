(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Facebook;

interface

uses System.SysUtils, System.Types, System.Classes, System.DateUtils, System.Math,
  System.Sensors, System.StrUtils, FMX.Types, FMX.Platform, FMX.Controls, JVE.Utils,
  FMX.Forms, JVE.Banners, JVE.Actions, {$IFDEF ANDROID}AndroidApi.JNI.Embarcadero,{$ENDIF}
  {$IF CompilerVersion >= 27} System.Messaging{$ELSE} FMX.Messages{$ENDIF};

type
  TJVEFacebookBanner = class;
  TJVEFacebookInterstitial = class;
  TJVEFacebookHasPaidAd = procedure(Sender: TJVEFacebookBanner) of object;
  TJVEFacebookNoPaidAd = procedure(Sender: TJVEFacebookBanner; Error: String) of object;
  TJVEFacebookInterstitialNoAd = procedure(Sender: TJVEFacebookInterstitial; Error: String) of object;
  TJVEFacebookInterstitialEvent = procedure(Sender: TJVEFacebookInterstitial) of object;

  TJVEFacebookAutoSize = (asNone, asAutoSize, asWithInset);
  TJVEFacebookHeight = (fh50Points, as90Points, as250Points);

  // This class provides Facebook Audience Network banners support.
  // Since Facebook does not support desktop applications, it falls back
  // to TJVEBanners on desktop or when there are no ads to be presented.
  // If you need to support another advertisements network, which has an
  // appropriate SDK, you can use this class as a basis for such functionality.
  //
  // Audience Network supports banners of height 50, 90 or 250 points.
  // It does not support changing from one height variant to another.
  // You should setup the control with one of these heights and not change it
  // (you can add bottom safe area inset: the control will use the top
  // 50, 90 or 250 points; see JVEFacebookSafeAreaBottomInset)
  //
  // This class does NOT collect analytics data, as all the analysis is available
  // through the Facebook services.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEFacebookBanner = class(TJVEBanners)
  private
    FFormActivateMessageId: Integer;
    FAdDelay: Integer;
    FiPadPlacementId: String;
    FiPhonePlacementId: String;
    FAndroidPlacementId: String;
    FOnHasPaidAd: TJVEFacebookHasPaidAd;
    FOnNoPaidAd: TJVEFacebookNoPaidAd;
    procedure FormActivateHandler(const Sender: TObject; const M: TMessage);
{$IF Defined(IOS) and not Defined(CPUX86)}
  private
    FAdView: IInterface;
    FAdDelegate: TObject;
    FHasPaidAds: Boolean;
    FInitialized: Boolean;
    FTimerHandle: TFmxHandle;
    procedure CreateBanner;
  protected
    procedure Move; override;
    procedure SetParent(const Value: TFmxObject); override;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure Show; override;
    procedure Hide; override;
    procedure NextBanner; override;
    function GetHasAds: Boolean; override;
  public
    procedure UpdateBanner;
{$ELSEIF Defined(ANDROID)}
  private
    FAdView: IInterface;
    FAdListener: TObject;
    FHasPaidAds: Boolean;
    FInitialized: Boolean;
    FOrientationChangedId: Integer;
    FTimerHandle: TFmxHandle;
    procedure CreateBanner;
    procedure OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
  protected
    procedure Move; override;
    procedure SetParent(const Value: TFmxObject); override;
    procedure AncestorVisibleChanged(const Visible: Boolean); override;
    procedure ParentChanged; override;
    procedure DoAbsoluteChanged; override;
    procedure Show; override;
    procedure Hide; override;
    procedure NextBanner; override;
    function GetHasAds: Boolean; override;
  public
    procedure UpdateBanner;
{$ENDIF}
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    // This is the time before actual ads are requested from Facebook (if fallback
    // banners are available).
    // It usually takes a couple of seconds for Facebook to bring the very first Ad,
    // so if this is 0 and there is a fallback banner, that fallback might be
    // presented for just 2 seconds: too short to react and too distracting.
    property AdDelay: Integer read FAdDelay write FAdDelay default 15;

    // The Audience Network PlacementId. Facebook dictates that Android and iOS
    // use different placement ids. You can use the same or different ids
    // for iPad and iPhone.
    // Audience Network does not support a change of PlacementId on the fly,
    // thus never change this value once if was already set!
    // If you want banners manager, but don't want Facebook, leave these blank.
    property iPadPlacementId: String read FiPadPlacementId write FiPadPlacementId;
    property iPhonePlacementId: String read FiPhonePlacementId write FiPhonePlacementId;
    property AndroidPlacementId: String read FAndroidPlacementId write FAndroidPlacementId;

    // If you don't want to use fallback banners, rather hiding the controls,
    // when real Facebook ads are not available, use these events.
    // Notice, the control starts with NO real ads (so you should create it hidden):
    // * When an ad is loaded and presented, OnPaidAdShown is called and you
    //   should show the control
    // * When Facebook is out of ads again OnPaidAdHidden event is called and you
    //   should hide the control, waiting for OnPaidAdShown again.
    property OnHasPaidAd: TJVEFacebookHasPaidAd read FOnHasPaidAd write FOnHasPaidAd;
    property OnNoPaidAd: TJVEFacebookNoPaidAd read FOnNoPaidAd write FOnNoPaidAd;
  end;

  // This component wraps the Facebook Audience Network interstitials API.
  // Similarly to banners API, this Ad serving API is only available on mobile.
  //
  // To use this component you need the Facebook Audience Network Placement ID.
  // This class does NOT collect analytics data, as all the analysis is available
  // through the Facebook services.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEFacebookInterstitial = class(TComponent, IJVEInterstitial)
  private
    FReward: Boolean;
    FOnAdClosed: TJVEFacebookInterstitialEvent;
    FOnAdShown: TJVEFacebookInterstitialEvent;
    FOnNoAd: TJVEFacebookInterstitialNoAd;
    FOnReward: TJVEFacebookInterstitialEvent;
    FiPadPlacementId: String;
    FiPhonePlacementId: String;
    FAndroidPlacementId: String;
{$IF Defined(IOS) and not Defined(CPUX86)}
    FAdInterstitial: IInterface;
    FAdDelegate: TObject;
{$ELSEIF Defined(ANDROID)}
    FAdInterstitial: IInterface;
    FIntListener: TObject;
    FRewardListener: TObject;
    FShowOnLoad: Boolean;
{$ENDIF}
  public
    destructor Destroy; override;

    // In three functions below, the Sender parameter is for the IJVEInterstitial
    // compatibility and should not be used.

    // Caches the ad, so that the Show procedure could present the ad immediately.
    procedure Cache(Sender: TJVEAction = nil);
    // Shows the ad (if any). If an ad is cached, it is presented immediately,
    // otherwise it is downloaded.
    procedure Show(Sender: TJVEAction = nil);
    // Checks whether an ad is cached. You can use this, if you don't want Show
    // procedure to download ads, i.e. "if FB.IsCached then FB.Show;".
    function IsCached(Sender: TJVEAction = nil): Boolean;
  published
    // Indicates whether a regular interstitial should be presented or a reward
    // video. Never change this value after calling Cache or Show functions!
    property Reward: Boolean read FReward write FReward default False;

    // The Audience Network PlacementId. Facebook dictates that Android and iOS
    // use different placement ids. You can use the same or different ids
    // for iPad and iPhone.
    // Audience Network does not support a change of PlacementId on the fly,
    // thus never change this value once if was already set!
    property iPadPlacementId: String read FiPadPlacementId write FiPadPlacementId;
    property iPhonePlacementId: String read FiPhonePlacementId write FiPhonePlacementId;
    property AndroidPlacementId: String read FAndroidPlacementId write FAndroidPlacementId;

    // These events are fired to notify when the ad was actually presented or hidden.
    property OnAdShown: TJVEFacebookInterstitialEvent read FOnAdShown write FOnAdShown;
    property OnAdClosed: TJVEFacebookInterstitialEvent read FOnAdClosed write FOnAdClosed;
    property OnNoAd: TJVEFacebookInterstitialNoAd read FOnNoAd write FOnNoAd;
    property OnReward: TJVEFacebookInterstitialEvent read FOnReward write FOnReward;
  end;

// Might be usefull to add to the banner height if it is to be placed
// at the bottom of the screen.
function JVEFacebookSafeAreaBottomInset: Single;
procedure JVEFacebookSetAdvertiserTracking(Enabled: Boolean);

implementation

{$IF Defined(IOS)}

uses System.TypInfo, FMX.Platform.iOS, Macapi.ObjectiveC, Posix.Dlfcn,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.UIKit;

function JVEFacebookSafeAreaBottomInset: Single;
var
  Size: NSSize;
begin
  Result := 0;

  if TOSVersion.Check(11) and (TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPhone) then
  begin
    Size := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen).bounds.size;
    if Max(Size.width, Size.height) >= 800 then
      Result := IfThen(SharedUIApplication.statusBarOrientation in
        [UIDeviceOrientationLandscapeLeft, UIDeviceOrientationLandscapeRight], 21, 34);
      // We are hard-coding these sizes as this code might be executed before
      // the safe insets are available from iOS (this will happen if the construction
      // is done during -loadView).
  end;
end;

{$ELSEIF Defined(ANDROID)}

uses System.TypInfo, FMX.Platform.Android, Androidapi.JNI.App, FMX.ZOrder.Android
  {$IF CompilerVersion > 26}, AndroidApi.Helpers, AndroidApi.JNI.Widget{$ENDIF},
  FMX.Helpers.Android, AndroidApi.JNI.Location, AndroidApi.JNI.JavaTypes,
  AndroidApi.JNIBridge, AndroidApi.JNI.Util, AndroidApi.JNI.GraphicsContentViewText;

function JVEFacebookSafeAreaBottomInset: Single;
begin
  Result := 0;
end;

{$ELSE DESKTOP}

function JVEFacebookSafeAreaBottomInset: Single;
begin
  Result := 0;
end;

{$ENDIF}

{$IF Defined(IOS) and not Defined(CPUX86)}

type
  FBAdSize = record
    size: CGSize;
  end;

  FBAdView = interface(UIView)
    function initWithPlacementID(placementID: NSString;
      adSize: FBAdSize; rootViewController: UIViewController): FBAdView; cdecl;
    procedure loadAd; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  FBAdViewClass = interface(UIViewClass)
  end;
  TFBAdView = class(TOCGenericImport<FBAdViewClass, FBAdView>) end;

  FBAdViewDelegate = interface(IObjectiveC)
    procedure adViewDidLoad(adView: FBAdView); cdecl;
    procedure adView(adView: FBAdView; didFailWithError: NSError); cdecl;
  end;

  TFBAdViewDelegate = class(TOCLocal, FBAdViewDelegate)
  private
    [Weak]FControl: TJVEFacebookBanner;
  public
    procedure adViewDidLoad(adView: FBAdView); cdecl;
    procedure adView(adView: FBAdView; didFailWithError: NSError); cdecl;
  end;

procedure TFBAdViewDelegate.adViewDidLoad(adView: FBAdView);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  FControl.ResetTimer(True);
  TUIView.OCClass.beginAnimations(nil, nil);
  TUIView.OCClass.setAnimationDuration(0.25);
  FBAdView(FControl.FAdView).setAlpha(1);
  TUIView.OCClass.commitAnimations;

  FControl.FHasPaidAds := True;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnHasPaidAd) then
    FControl.FOnHasPaidAd(FControl);

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := False;
end;

procedure TFBAdViewDelegate.adView(adView: FBAdView; didFailWithError: NSError);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  if FControl.FStopped then
    FControl.ResetTimer;
  TUIView.OCClass.beginAnimations(nil, nil);
  TUIView.OCClass.setAnimationDuration(0.25);
  FBAdView(FControl.FAdView).setAlpha(0);
  TUIView.OCClass.commitAnimations;

  FControl.FHasPaidAds := False;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnNoPaidAd) then
    if didFailWithError = nil then
      FControl.FOnNoPaidAd(FControl, '')
    else
      FControl.FOnNoPaidAd(FControl, FromNSSTR(didFailWithError.localizedDescription));

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := True;
end;

{ TJVEFacebook }

constructor TJVEFacebookBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;
  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

procedure TJVEFacebookBanner.NextBanner;
begin
  inherited NextBanner;

  if not FInitialized and not (csDesigning in ComponentState) then
  begin
    FInitialized := True;
    if (Banners.Count = 0) or (FAdDelay <= 0) then
      CreateBanner
    else
      FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
        as IFMXTimerService).CreateTimer(FAdDelay * 1000, CreateBanner);
  end;

  if Assigned(FCurrent) then
    FCurrent.Visible := not FHasPaidAds;
end;

function TJVEFacebookBanner.GetHasAds: Boolean;
begin
  Result := FHasPaidAds or inherited GetHasAds;
end;

procedure TJVEFacebookBanner.Move;
begin
  inherited Move;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.Show;
begin
  inherited Show;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.Hide;
begin
  inherited Hide;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.SetParent(const Value: TFmxObject);
begin
  if Parent <> Value then
  begin
    inherited;
    UpdateBanner;
  end
end;

procedure TJVEFacebookBanner.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.DoAbsoluteChanged;
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.ParentChanged;
begin
  inherited;
  if ([csLoading, csDesigning] * ComponentState) = [] then
    UpdateBanner;
end;

procedure TJVEFacebookBanner.UpdateBanner;
var
  View: FBAdView;
  Frame: TRectF;
begin
  if FAdView = nil then
    Exit;

  Frame := AbsoluteRect;
  if Frame.Height >= 250 then Frame.Height := 250 else
  if Frame.Height >= 90 then Frame.Height := 90 else Frame.Height := 50;

  View := FBAdView(FAdView);
  View.setBounds(CGRectMake(0, 0, Frame.Width, Frame.Height));
  View.setCenter(CGPointMake(Frame.Left + Frame.Width / 2, Frame.Top + Frame.Height / 2));
  View.setHidden(not ParentedVisible or not (Root.GetObject as TCommonCustomForm).Active);
  WindowHandleToPlatform(TCommonCustomForm(Root.GetObject).Handle).View.addSubview(View);
end;

procedure TJVEFacebookBanner.CreateBanner;
var
  View: FBAdView;
  AdSize: FBAdSize;
  PlacementId: String;
  Delegate: TFBAdViewDelegate;
  Controller: UIViewController;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  PlacementId := IfThen(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPad, FiPadPlacementId, FiPhonePlacementId);

  Controller := GetRootViewController;
  if (Controller = nil) or (PlacementId = '') then
  begin
    // We cannot create a Facebook view if we don't have a Root View Controller
    // or placement id is not set.
    // If we indeed do not have either, we delay initialization by 1 second and
    // try again.
    FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).CreateTimer(1000, CreateBanner);
    Exit;
  end;

  Delegate := TFBAdViewDelegate.Create;
  Delegate.FControl := Self;
  FAdDelegate := Delegate;

  AdSize.size.width := -1;
  if Height >= 250 then AdSize.size.height := 250 else
  if Height >= 90 then AdSize.size.height := 90 else AdSize.size.height := 50;

  View := TFBAdView.Alloc.initWithPlacementID(
    ToNSSTR(PlacementId), AdSize, Controller);
  View.setDelegate(Delegate.GetObjectID);
  View.setAlpha(0);
  View.loadAd;
  FAdView := View;

  UpdateBanner;
end;

type
  FBInterstitialAdDelegate = interface;
  FBRewardedVideoAdDelegate = interface;

  FBAdSettings = interface(NSObject)
  end;
  FBAdSettingsClass = interface(NSObjectClass)
    procedure setAdvertiserTrackingEnabled(advertiserTrackingEnabled: Boolean); cdecl;
    procedure setMixedAudience(mixedAudience: Boolean); cdecl;
  end;
  TFBAdSettings = class(TOCGenericImport<FBAdSettingsClass, FBAdSettings>) end;

  FBInterstitialAd = interface(NSObject)
    procedure setDelegate(delegate: FBInterstitialAdDelegate); cdecl;
    procedure loadAd; cdecl;
    function initWithPlacementID(placementID: NSString): FBInterstitialAd; cdecl;
    function isAdValid: Boolean; cdecl;
    function showAdFromRootViewController(
      rootViewController: UIViewController): Boolean; cdecl;
  end;
  FBInterstitialAdClass = interface(NSObjectClass)
  end;
  TFBInterstitialAd = class(TOCGenericImport<FBInterstitialAdClass, FBInterstitialAd>) end;

  FBRewardedVideoAd = interface(NSObject)
    procedure setDelegate(delegate: FBRewardedVideoAdDelegate); cdecl;
    procedure loadAd; cdecl;
    function isAdValid: Boolean; cdecl;
    function initWithPlacementID(placementID: NSString): FBRewardedVideoAd; cdecl;
    function showAdFromRootViewController(
      rootViewController: UIViewController): Boolean; cdecl;
  end;
  FBRewardedVideoAdClass = interface(NSObjectClass)
  end;
  TFBRewardedVideoAd = class(TOCGenericImport<FBRewardedVideoAdClass, FBRewardedVideoAd>) end;


  FBInterstitialAdDelegate = interface(IObjectiveC)
    procedure interstitialAdDidLoad(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdDidClose(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAd(interstitialAd: FBInterstitialAd;
      didFailWithError: NSError); cdecl;
  end;

  FBRewardedVideoAdDelegate = interface(IObjectiveC)
    procedure rewardedVideoAdDidLoad(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdDidClose(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdVideoComplete(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAd(rewardedVideoAd: FBRewardedVideoAd;
      didFailWithError: NSError); cdecl;
  end;

  TFBInterstitialDelegate = class(TOCLocal, FBInterstitialAdDelegate, FBRewardedVideoAdDelegate)
  private
    FShow: Boolean;
    [Weak]FControl: TJVEFacebookInterstitial;
  public
    procedure interstitialAdDidLoad(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAdDidClose(interstitialAd: FBInterstitialAd); cdecl;
    procedure interstitialAd(interstitialAd: FBInterstitialAd;
      didFailWithError: NSError); cdecl;
    procedure rewardedVideoAdDidLoad(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdDidClose(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAdVideoComplete(rewardedVideoAd: FBRewardedVideoAd); cdecl;
    procedure rewardedVideoAd(rewardedVideoAd: FBRewardedVideoAd;
      didFailWithError: NSError); cdecl;
  end;

{ TGADInterstitialDelegate }

procedure TFBInterstitialDelegate.interstitialAdDidLoad(
  interstitialAd: FBInterstitialAd);
begin
  rewardedVideoAdDidLoad(nil);
end;

procedure TFBInterstitialDelegate.interstitialAdDidClose(
  interstitialAd: FBInterstitialAd);
begin
  rewardedVideoAdDidClose(nil);
end;

procedure TFBInterstitialDelegate.interstitialAd(
  interstitialAd: FBInterstitialAd; didFailWithError: NSError);
begin
  rewardedVideoAd(nil, didFailWithError);
end;

procedure TFBInterstitialDelegate.rewardedVideoAdDidLoad(
  rewardedVideoAd: FBRewardedVideoAd);
begin
  if FShow then
    FControl.Show;
end;

procedure TFBInterstitialDelegate.rewardedVideoAdDidClose(
  rewardedVideoAd: FBRewardedVideoAd);
begin
  if Assigned(FControl.FOnAdClosed) then
    FControl.FOnAdClosed(FControl);
end;

procedure TFBInterstitialDelegate.rewardedVideoAdVideoComplete(
  rewardedVideoAd: FBRewardedVideoAd);
begin
  if Assigned(FControl.FOnReward) then
    FControl.FOnReward(FControl);
end;

procedure TFBInterstitialDelegate.rewardedVideoAd(
  rewardedVideoAd: FBRewardedVideoAd; didFailWithError: NSError);
begin
  FControl.FAdInterstitial := nil;
  if Assigned(FControl.FOnNoAd) then
    FControl.FOnNoAd(FControl, FromNSSTR(didFailWithError.localizedDescription));
end;

{ TJVEFacebookInterstitial }

procedure TJVEFacebookInterstitial.Cache(Sender: TJVEAction);

  function PlacementId: NSString;
  begin
    Result := ToNSSTR(IfThen(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
      userInterfaceIdiom = UIUserInterfaceIdiomPad, FiPadPlacementId, FiPhonePlacementId));
  end;

var
  Int: FBInterstitialAd;
  Reward: FBRewardedVideoAd;
begin
  if FAdInterstitial <> nil then
    Exit;
  if FAdDelegate = nil then
  begin
    FAdDelegate := TFBInterstitialDelegate.Create;
    TFBInterstitialDelegate(FAdDelegate).FControl := Self;
  end;

  if FReward then
  begin
    Reward := TFBRewardedVideoAd.Alloc.initWithPlacementID(PlacementId);
    FAdInterstitial := Reward;
    Reward.setDelegate(TFBInterstitialDelegate(FAdDelegate));
    Reward.loadAd;
  end else
  begin
    Int := TFBInterstitialAd.Alloc.initWithPlacementID(PlacementId);
    FAdInterstitial := Int;
    Int.setDelegate(TFBInterstitialDelegate(FAdDelegate));
    Int.loadAd;
  end;
end;

function TJVEFacebookInterstitial.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := (FAdInterstitial <> nil) and
    ((not FReward and FBInterstitialAd(FAdInterstitial).isAdValid) or
    (FReward and FBRewardedVideoAd(FAdInterstitial).isAdValid));
end;

procedure TJVEFacebookInterstitial.Show(Sender: TJVEAction);
begin
  Cache(Sender);
  if not IsCached(Sender) then
  begin
    TFBInterstitialDelegate(FAdDelegate).FShow := True;
    Exit;
  end;

  if Assigned(FOnAdShown) then
    FOnAdShown(Self);
  TFBInterstitialDelegate(FAdDelegate).FShow := False;

  if FReward then
    FBRewardedVideoAd(FAdInterstitial).showAdFromRootViewController(GetRootViewController)
  else
    FBInterstitialAd(FAdInterstitial).showAdFromRootViewController(GetRootViewController);
  FAdInterstitial := nil;
end;

{$ELSEIF Defined(ANDROID)}  // for XE5+

type
  [JavaSignature('com/facebook/ads/Ad')]
  Jfacebook_ads_Ad = interface(IJavaInstance)
  end;
  Jfacebook_ads_AdClass = interface(IJavaClass)
  end;
  TJfacebook_ads_Ad = class(TJavaGenericImport<Jfacebook_ads_AdClass, Jfacebook_ads_Ad>) end;

  [JavaSignature('com/facebook/ads/AdSize')]
  Jfacebook_ads_AdSize = interface(JSerializable)
  end;
  Jfacebook_ads_AdSizeClass = interface(JSerializableClass)
    function _GetBANNER_HEIGHT_50: Jfacebook_ads_AdSize; cdecl;
    function _GetBANNER_HEIGHT_90: Jfacebook_ads_AdSize; cdecl;
    function _GetRECTANGLE_HEIGHT_250: Jfacebook_ads_AdSize; cdecl;
    property BANNER_HEIGHT_50: Jfacebook_ads_AdSize read _GetBANNER_HEIGHT_50;
    property BANNER_HEIGHT_90: Jfacebook_ads_AdSize read _GetBANNER_HEIGHT_90;
    property RECTANGLE_HEIGHT_250: Jfacebook_ads_AdSize read _GetRECTANGLE_HEIGHT_250;
  end;
  TJfacebook_ads_AdSize = class(TJavaGenericImport<Jfacebook_ads_AdSizeClass, Jfacebook_ads_AdSize>) end;

  [JavaSignature('com/facebook/ads/Ad$LoadAdConfig')]
  JAd_LoadAdConfig = interface(IJavaInstance)
  end;
  JAd_LoadAdConfigClass = interface(IJavaClass)
  end;
  TJAd_LoadAdConfig = class(TJavaGenericImport<JAd_LoadAdConfigClass, JAd_LoadAdConfig>) end;

  [JavaSignature('com/facebook/ads/Ad$LoadConfigBuilder')]
  JAd_LoadConfigBuilder = interface(IJavaInstance)
    function build: JAd_LoadAdConfig; cdecl;
    function withBid(string_: JString): JAd_LoadConfigBuilder; cdecl;
  end;
  JAd_LoadConfigBuilderClass = interface(IJavaClass)
  end;
  TJAd_LoadConfigBuilder = class(TJavaGenericImport<JAd_LoadConfigBuilderClass, JAd_LoadConfigBuilder>) end;

  [JavaSignature('com/facebook/ads/AdView$AdViewLoadConfig')]
  JAdView_AdViewLoadConfig = interface(JAd_LoadAdConfig)
  end;
  JAdView_AdViewLoadConfigClass = interface(JAd_LoadAdConfigClass)
  end;
  TJAdView_AdViewLoadConfig = class(TJavaGenericImport<JAdView_AdViewLoadConfigClass, JAdView_AdViewLoadConfig>) end;

  [JavaSignature('com/facebook/ads/AdError')]
  Jads_AdError = interface(JObject)
    function getErrorCode: Integer; cdecl;
    function getErrorMessage: JString; cdecl;
  end;
  Jads_AdErrorClass = interface(JObjectClass)
  end;
  TJads_AdError = class(TJavaGenericImport<Jads_AdErrorClass, Jads_AdError>) end;

  [JavaSignature('com/facebook/ads/AdListener')]
  Jfacebook_ads_AdListener = interface(IJavaInstance)
    procedure onAdClicked(ad: Jfacebook_ads_Ad); cdecl;
    procedure onAdLoaded(ad: Jfacebook_ads_Ad); cdecl;
    procedure onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError); cdecl;
    procedure onLoggingImpression(ad: Jfacebook_ads_Ad); cdecl;
  end;
  Jfacebook_ads_AdListenerClass = interface(IJavaClass)
  end;
  TJfacebook_ads_AdListener = class(TJavaGenericImport<Jfacebook_ads_AdListenerClass, Jfacebook_ads_AdListener>) end;

  [JavaSignature('com/facebook/ads/AdView$AdViewLoadConfigBuilder')]
  JAdView_AdViewLoadConfigBuilder = interface(JAd_LoadConfigBuilder)
    function build: JAdView_AdViewLoadConfig; cdecl;
    function withAdListener(adListener: Jfacebook_ads_AdListener): JAdView_AdViewLoadConfigBuilder; cdecl;
    function withBid(string_: JString): JAdView_AdViewLoadConfigBuilder; cdecl;
  end;
  JAdView_AdViewLoadConfigBuilderClass = interface(JAd_LoadConfigBuilderClass)
  end;
  TJAdView_AdViewLoadConfigBuilder = class(TJavaGenericImport<JAdView_AdViewLoadConfigBuilderClass, JAdView_AdViewLoadConfigBuilder>) end;

  [JavaSignature('com/facebook/ads/AdView')]
  Jfacebook_ads_AdView = interface(JRelativeLayout)
    function buildLoadAdConfig: JAdView_AdViewLoadConfigBuilder; cdecl;
    procedure destroy; cdecl;
    procedure loadAd(adViewLoadConfig: JAdView_AdViewLoadConfig); cdecl; overload;
  end;
  Jfacebook_ads_AdViewClass = interface(JRelativeLayoutClass)
    function init(context: JContext; placement: JString; adSize: Jfacebook_ads_AdSize): Jfacebook_ads_AdView; cdecl;
  end;
  TJfacebook_ads_AdView = class(TJavaGenericImport<Jfacebook_ads_AdViewClass, Jfacebook_ads_AdView>) end;


  [JavaSignature('com/facebook/ads/FullScreenAd')]
  JFullScreenAd = interface(Jfacebook_ads_Ad)
    function buildLoadAdConfig: JAd_LoadConfigBuilder; cdecl;
    function show: Boolean; cdecl;
  end;
  JFullScreenAdClass = interface(Jfacebook_ads_AdClass)
  end;
  TJFullScreenAd = class(TJavaGenericImport<JFullScreenAdClass, JFullScreenAd>) end;

  [JavaSignature('com/facebook/ads/InterstitialAd$InterstitialLoadAdConfig')]
  JInterstitialAd_InterstitialLoadAdConfig = interface(JAd_LoadAdConfig)
  end;
  JInterstitialAd_InterstitialLoadAdConfigClass = interface(JAd_LoadAdConfigClass)
  end;
  TJInterstitialAd_InterstitialLoadAdConfig = class(TJavaGenericImport<JInterstitialAd_InterstitialLoadAdConfigClass, JInterstitialAd_InterstitialLoadAdConfig>) end;

  [JavaSignature('com/facebook/ads/InterstitialAdListener')]
  JInterstitialAdListener = interface(Jfacebook_ads_AdListener)
    procedure onInterstitialDismissed(ad: Jfacebook_ads_Ad); cdecl;
    procedure onInterstitialDisplayed(ad: Jfacebook_ads_Ad); cdecl;
  end;
  JInterstitialAdListenerClass = interface(Jfacebook_ads_AdListenerClass)
  end;
  TJInterstitialAdListener = class(TJavaGenericImport<JInterstitialAdListenerClass, JInterstitialAdListener>) end;

  [JavaSignature('com/facebook/ads/InterstitialAd$InterstitialAdLoadConfigBuilder')]
  JInterstitialAd_InterstitialAdLoadConfigBuilder = interface(JAd_LoadConfigBuilder)
    function build: JInterstitialAd_InterstitialLoadAdConfig; cdecl;
    function withAdListener(interstitialAdListener: JInterstitialAdListener): JInterstitialAd_InterstitialAdLoadConfigBuilder; cdecl;
  end;
  JInterstitialAd_InterstitialAdLoadConfigBuilderClass = interface(JAd_LoadConfigBuilderClass)
  end;
  TJInterstitialAd_InterstitialAdLoadConfigBuilder = class(TJavaGenericImport<JInterstitialAd_InterstitialAdLoadConfigBuilderClass, JInterstitialAd_InterstitialAdLoadConfigBuilder>) end;

  [JavaSignature('com/facebook/ads/InterstitialAd')]
  Jfacebook_ads_InterstitialAd = interface(JFullScreenAd)
    function buildLoadAdConfig: JInterstitialAd_InterstitialAdLoadConfigBuilder; cdecl;
    function isAdInvalidated: Boolean; cdecl;
    function isAdLoaded: Boolean; cdecl;
    procedure loadAd(interstitialLoadAdConfig: JInterstitialAd_InterstitialLoadAdConfig); cdecl; overload;
    function show: Boolean; cdecl; overload;
  end;
  Jfacebook_ads_InterstitialAdClass = interface(JFullScreenAdClass)
    function init(context: JContext; placement: JString): Jfacebook_ads_InterstitialAd; cdecl;
  end;
  TJfacebook_ads_InterstitialAd = class(TJavaGenericImport<Jfacebook_ads_InterstitialAdClass, Jfacebook_ads_InterstitialAd>) end;

  [JavaSignature('com/facebook/ads/RewardedVideoAd$RewardedVideoLoadAdConfig')]
  JRewardedVideoAd_RewardedVideoLoadAdConfig = interface(JAd_LoadAdConfig)
  end;
  JRewardedVideoAd_RewardedVideoLoadAdConfigClass = interface(JAd_LoadAdConfigClass)
  end;
  TJRewardedVideoAd_RewardedVideoLoadAdConfig = class(TJavaGenericImport<JRewardedVideoAd_RewardedVideoLoadAdConfigClass, JRewardedVideoAd_RewardedVideoLoadAdConfig>) end;

  [JavaSignature('com/facebook/ads/RewardedVideoAdListener')]
  Jads_RewardedVideoAdListener = interface(Jfacebook_ads_AdListener)
    procedure onRewardedVideoClosed; cdecl;
    procedure onRewardedVideoCompleted; cdecl;
  end;
  Jads_RewardedVideoAdListenerClass = interface(Jfacebook_ads_AdListenerClass)
  end;
  TJads_RewardedVideoAdListener = class(TJavaGenericImport<Jads_RewardedVideoAdListenerClass, Jads_RewardedVideoAdListener>) end;

  [JavaSignature('com/facebook/ads/RewardedVideoAd$RewardedVideoAdLoadConfigBuilder')]
  JRewardedVideoAd_RewardedVideoAdLoadConfigBuilder = interface(JAd_LoadConfigBuilder)
    function build: JRewardedVideoAd_RewardedVideoLoadAdConfig; cdecl;
    function withAdListener(rewardedVideoAdListener: Jads_RewardedVideoAdListener): JRewardedVideoAd_RewardedVideoAdLoadConfigBuilder; cdecl;
  end;
  JRewardedVideoAd_RewardedVideoAdLoadConfigBuilderClass = interface(JAd_LoadConfigBuilderClass)
  end;
  TJRewardedVideoAd_RewardedVideoAdLoadConfigBuilder = class(TJavaGenericImport<JRewardedVideoAd_RewardedVideoAdLoadConfigBuilderClass, JRewardedVideoAd_RewardedVideoAdLoadConfigBuilder>) end;

  [JavaSignature('com/facebook/ads/RewardedVideoAd')]
  Jads_RewardedVideoAd = interface(JFullScreenAd)
    function buildLoadAdConfig: JRewardedVideoAd_RewardedVideoAdLoadConfigBuilder; cdecl;
    function isAdInvalidated: Boolean; cdecl;
    function isAdLoaded: Boolean; cdecl;
    procedure loadAd(rewardedVideoLoadAdConfig: JRewardedVideoAd_RewardedVideoLoadAdConfig); cdecl; overload;
    function show: Boolean; cdecl; overload;
  end;
  Jads_RewardedVideoAdClass = interface(JFullScreenAdClass)
    function init(context: JContext; placement: JString): Jads_RewardedVideoAd; cdecl;
  end;
  TJads_RewardedVideoAd = class(TJavaGenericImport<Jads_RewardedVideoAdClass, Jads_RewardedVideoAd>) end;

  [JavaSignature('com/facebook/ads/AudienceNetworkAds')]
  JAudienceNetworkAds = interface(JObject)
  end;
  JAudienceNetworkAdsClass = interface(JObjectClass)
    procedure initialize(context: JContext); cdecl;
    function isInitialized(context: JContext): Boolean; cdecl;
  end;
  TJAudienceNetworkAds = class(TJavaGenericImport<JAudienceNetworkAdsClass, JAudienceNetworkAds>) end;


type
  TBannerViewListener = class(TJavaLocal, Jfacebook_ads_AdListener)
  private
    [Weak]FControl: TJVEFacebookBanner;
  public
    constructor Create(Ad: TJVEFacebookBanner);
    destructor Destroy; override;
    procedure onAdClicked(ad: Jfacebook_ads_Ad); cdecl;
    procedure onAdLoaded(ad: Jfacebook_ads_Ad); cdecl;
    procedure onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError); cdecl;
    procedure onLoggingImpression(ad: Jfacebook_ads_Ad); cdecl;
  end;

  TInterstitialAdListener = class(TJavaLocal, JInterstitialAdListener)
  private
    [Weak]FControl: TJVEFacebookInterstitial;
  public
    procedure onAdClicked(ad: Jfacebook_ads_Ad); cdecl;
    procedure onAdLoaded(ad: Jfacebook_ads_Ad); cdecl;
    procedure onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError); cdecl;
    procedure onLoggingImpression(ad: Jfacebook_ads_Ad); cdecl;
    procedure onInterstitialDismissed(ad: Jfacebook_ads_Ad); cdecl;
    procedure onInterstitialDisplayed(ad: Jfacebook_ads_Ad); cdecl;
  end;

  TRewardAdListener = class(TJavaLocal, Jads_RewardedVideoAdListener)
  private
    [Weak]FControl: TJVEFacebookInterstitial;
  public
    procedure onAdClicked(ad: Jfacebook_ads_Ad); cdecl;
    procedure onAdLoaded(ad: Jfacebook_ads_Ad); cdecl;
    procedure onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError); cdecl;
    procedure onLoggingImpression(ad: Jfacebook_ads_Ad); cdecl;
    procedure onRewardedVideoClosed; cdecl;
    procedure onRewardedVideoCompleted; cdecl;
  end;

var
  InitCalled: Boolean = False;

{ TBannerViewListener }

constructor TBannerViewListener.Create(Ad: TJVEFacebookBanner);
begin
  inherited Create;
  FControl := Ad;
end;

destructor TBannerViewListener.Destroy;
begin
  FControl := nil;
  inherited;
end;

procedure TBannerViewListener.onAdLoaded(ad: Jfacebook_ads_Ad);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  FControl.ResetTimer(True);
  Jfacebook_ads_AdView(FControl.FAdView).setAlpha(1);
  FControl.FHasPaidAds := True;
  FControl.UpdateHasAds;
  FControl.UpdateBanner;

  if Assigned(FControl.FOnHasPaidAd) then
    FControl.FOnHasPaidAd(FControl);

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := False;
end;

procedure TBannerViewListener.onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  if FControl.FStopped then
    FControl.ResetTimer;
  Jfacebook_ads_AdView(FControl.FAdView).setAlpha(0);
  FControl.FHasPaidAds := False;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnNoPaidAd) then
    FControl.FOnNoPaidAd(FControl, JStringToString(adError.getErrorMessage));

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := True;  // image cover
end;

procedure TBannerViewListener.onAdClicked(ad: Jfacebook_ads_Ad);
begin
end;

procedure TBannerViewListener.onLoggingImpression(ad: Jfacebook_ads_Ad);
begin
end;

{ TJVEFacebook }

constructor TJVEFacebookBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;

  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(
    TOrientationChangedMessage, OrientationChangedHandler);
  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

procedure TJVEFacebookBanner.NextBanner;
begin
  inherited NextBanner;

  if not FInitialized and not (csDesigning in ComponentState) then
  begin
    FInitialized := True;
    if (Banners.Count = 0) or (FAdDelay <= 0) then
      CreateBanner
    else
      FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
        as IFMXTimerService).CreateTimer(FAdDelay * 1000, CreateBanner);
  end;

  if Assigned(FCurrent) then
    FCurrent.Visible := not FHasPaidAds;
end;

function TJVEFacebookBanner.GetHasAds: Boolean;
begin
  Result := FHasPaidAds or inherited GetHasAds;
end;

procedure TJVEFacebookBanner.Move;
begin
  inherited Move;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.SetParent(const Value: TFmxObject);
begin
  if Parent <> Value then
  begin
    inherited;
    UpdateBanner;
  end
end;

procedure TJVEFacebookBanner.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.DoAbsoluteChanged;
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.ParentChanged;
begin
  inherited;
  if ([csLoading, csDesigning] * ComponentState) = [] then
    UpdateBanner;
end;

procedure TJVEFacebookBanner.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if Self.Visible then
  begin
    // Kill off existing ad..
    if FAdView <> nil then
    begin
      CallInUIThreadAndWaitFinishing(procedure
      begin
        // Special thanks to Chris Pimlott for finding and fixing (!) a bug here
        Jfacebook_ads_AdView(FAdView).destroy;
        FAdView := nil;

        if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
          WindowHandleToPlatform(TCommonCustomForm(Root).Handle).
            ZOrderManager.RemoveLink(Self);
      end);
    end;
    // ..and recreate it
    CreateBanner;
  end;
end;

procedure TJVEFacebookBanner.Show;
begin
  inherited Show;
  Visible := True;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.Hide;
begin
  inherited Hide;
  Visible := False;
  UpdateBanner;
end;

procedure TJVEFacebookBanner.UpdateBanner;
var
  ZOrder: TAndroidZOrderManager;
begin
  if (FAdView = nil) or (Root = nil) or not (Root.GetObject is TCommonCustomForm) then
    Exit;

  ZOrder := WindowHandleToPlatform(TCommonCustomForm(Root).Handle).ZOrderManager;
  ZOrder.AddOrSetLink(Self, JView(FAdView), nil);
  ZOrder.UpdateOrderAndBounds(Self);
end;

procedure TJVEFacebookBanner.CreateBanner;
var
  View: Jfacebook_ads_AdView;
  Listener: TBannerViewListener;
  Form: TCommonCustomForm;
  Frame: TRectF;
  Init: Boolean;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  Init := TJAudienceNetworkAds.JavaClass.isInitialized(GetSharedActivityContext);
  if (Root = nil) or not (Root.GetObject is TCommonCustomForm) or not Init then
  begin
    if not InitCalled then
    begin
      InitCalled := True;
      TJAudienceNetworkAds.JavaClass.initialize(GetSharedActivityContext);
    end;

    FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).CreateTimer(1000, CreateBanner);
    Exit;
  end;

  if Listener = nil then
    Listener := TBannerViewListener.Create(Self);
  FAdListener := Listener;

  CallInUIThreadAndWaitFinishing(procedure
  var
    Size: Jfacebook_ads_AdSize;
  begin
    if Height >= 250 then Size := TJfacebook_ads_AdSize.JavaClass.RECTANGLE_HEIGHT_250 else
    if Height >= 90 then Size := TJfacebook_ads_AdSize.JavaClass.BANNER_HEIGHT_90 else
      Size := TJfacebook_ads_AdSize.JavaClass.BANNER_HEIGHT_50;

    View := TJfacebook_ads_AdView.JavaClass.init(GetSharedActivityContext,
      StringToJString(FAndroidPlacementId), Size);
    View.setAlpha(0);
    View.loadAd(View.buildLoadAdConfig.withAdListener(Listener).build);
    FAdView := View;

    if Screen.ActiveForm <> nil then
      Form := Screen.ActiveForm
    else
      Form := Application.MainForm;
    if Form <> nil then
    begin
      Frame := WindowHandleToPlatform(Form.Handle).Bounds;
      Width := Frame.Width;
    end;

    if View.getVisibility <> TJView.JavaClass.VISIBLE then
      View.setVisibility(TJView.JavaClass.VISIBLE);
  end);

  UpdateBanner;
end;

{ TInterstitialAdListener }

procedure TInterstitialAdListener.onAdLoaded(ad: Jfacebook_ads_Ad);
begin
  if FControl.FShowOnLoad then
    FControl.Show;
end;

procedure TInterstitialAdListener.onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError);
begin
  FControl.FAdInterstitial := nil;
  if Assigned(FControl.FOnNoAd) then
    FControl.FOnNoAd(FControl, JStringToString(adError.getErrorMessage));
end;

procedure TInterstitialAdListener.onInterstitialDismissed(ad: Jfacebook_ads_Ad);
begin
  Screen.ActiveForm.BeginUpdate;
  Screen.ActiveForm.Invalidate;
  Screen.ActiveForm.EndUpdate;

  if Assigned(FControl.FOnAdClosed) then
    FControl.FOnAdClosed(FControl);
end;

procedure TInterstitialAdListener.onInterstitialDisplayed(ad: Jfacebook_ads_Ad);
begin
end;

procedure TInterstitialAdListener.onLoggingImpression(ad: Jfacebook_ads_Ad);
begin
end;

procedure TInterstitialAdListener.onAdClicked(ad: Jfacebook_ads_Ad);
begin
end;

{ TRewardAdListener }

procedure TRewardAdListener.onAdLoaded(ad: Jfacebook_ads_Ad);
begin
  if FControl.FShowOnLoad then
    FControl.Show;
end;

procedure TRewardAdListener.onError(ad: Jfacebook_ads_Ad; adError: Jads_AdError);
begin
  FControl.FAdInterstitial := nil;
  if Assigned(FControl.FOnNoAd) then
    FControl.FOnNoAd(FControl, JStringToString(adError.getErrorMessage));
end;

procedure TRewardAdListener.onRewardedVideoClosed;
begin
  Screen.ActiveForm.BeginUpdate;
  Screen.ActiveForm.Invalidate;
  Screen.ActiveForm.EndUpdate;

  if Assigned(FControl.FOnAdClosed) then
    FControl.FOnAdClosed(FControl);
end;

procedure TRewardAdListener.onRewardedVideoCompleted;
begin
  if Assigned(FControl.FOnReward) then
    FControl.FOnReward(FControl);
end;

procedure TRewardAdListener.onLoggingImpression(ad: Jfacebook_ads_Ad);
begin
end;

procedure TRewardAdListener.onAdClicked(ad: Jfacebook_ads_Ad);
begin
end;

{ TJVEFacebookInterstitial }

procedure TJVEFacebookInterstitial.Cache(Sender: TJVEAction);
begin
  if FAdInterstitial <> nil then
    Exit;
  if FIntListener = nil then
  begin
    FIntListener := TInterstitialAdListener.Create;
    TInterstitialAdListener(FIntListener).FControl := Self;
    FRewardListener := TRewardAdListener.Create;
    TRewardAdListener(FRewardListener).FControl := Self;
  end;

  if not InitCalled then
  begin
    InitCalled := True;
    TJAudienceNetworkAds.JavaClass.initialize(GetSharedActivityContext);
  end;

  CallInUIThreadAndWaitFinishing(procedure
  var
    Int: Jfacebook_ads_InterstitialAd;
    Reward: Jads_RewardedVideoAd;
  begin
    if FReward then
    begin
      Reward := TJads_RewardedVideoAd.JavaClass.init(GetSharedActivity, StringToJString(FAndroidPlacementId));
      FAdInterstitial := Reward;
      Reward.loadAd(Reward.buildLoadAdConfig.withAdListener(TRewardAdListener(FRewardListener)).build);
    end else
    begin
      Int := TJfacebook_ads_InterstitialAd.JavaClass.init(GetSharedActivity, StringToJString(FAndroidPlacementId));
      FAdInterstitial := Int;
      Int.loadAd(Int.buildLoadAdConfig.withAdListener(TInterstitialAdListener(FIntListener)).build);
    end;
  end);
end;

function TJVEFacebookInterstitial.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := (FAdInterstitial <> nil) and
    ((not FReward and Jfacebook_ads_InterstitialAd(FAdInterstitial).isAdLoaded
      and not Jfacebook_ads_InterstitialAd(FAdInterstitial).isAdInvalidated) or
    (FReward and Jads_RewardedVideoAd(FAdInterstitial).isAdLoaded
      and not Jads_RewardedVideoAd(FAdInterstitial).isAdInvalidated));
end;

procedure TJVEFacebookInterstitial.Show(Sender: TJVEAction);
begin
  Cache(Sender);
  if not IsCached(Sender) then
  begin
    FShowOnLoad := True;
    Exit;
  end;

  if Assigned(FOnAdShown) then
    FOnAdShown(Self);
  FShowOnLoad := False;

  if FReward then
    Jads_RewardedVideoAd(FAdInterstitial).show
  else
    Jfacebook_ads_InterstitialAd(FAdInterstitial).show;
  FAdInterstitial := nil;
end;

{$ELSE DESKTOP}

{ TJVEFacebook }

constructor TJVEFacebookBanner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;
  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

{ TJVEFacebookInterstitial }

procedure TJVEFacebookInterstitial.Cache(Sender: TJVEAction);
begin
end;

function TJVEFacebookInterstitial.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := False;
end;

procedure TJVEFacebookInterstitial.Show(Sender: TJVEAction);
begin
end;

{$ENDIF}

destructor TJVEFacebookInterstitial.Destroy;
begin
{$IF (Defined(IOS) and not Defined(CPUX86))}
  FreeAndNil(FAdDelegate);
{$ELSEIF Defined(ANDROID)}
  FreeAndNil(FIntListener);
  FreeAndNil(FRewardListener);
{$ENDIF}

  inherited Destroy;
end;

destructor TJVEFacebookBanner.Destroy;
begin
{$IF (Defined(IOS) and not Defined(CPUX86))}
  if FAdView <> nil then
  begin
    FBAdView(FAdView).removeFromSuperview;
    FAdView := nil;
    FreeAndNil(FAdDelegate);
  end;

  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);
{$ELSEIF Defined(ANDROID)}
  if FAdView <> nil then
  begin
    CallInUIThread(procedure
    begin
      Jfacebook_ads_AdView(FAdView).destroy;
      FAdView := nil;
      FreeAndNil(FAdListener);
    end);
  end;

  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);
{$ENDIF}

  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FFormActivateMessageId);
  inherited Destroy;
end;

procedure TJVEFacebookBanner.FormActivateHandler(const Sender: TObject; const M: TMessage);
begin
  {$IF (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
  UpdateBanner;
  {$ENDIF}
end;

procedure TJVEFacebookBanner.Resize;
begin
  inherited Resize;
  {$IF (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
  UpdateBanner;
  {$ENDIF}
end;

{$IF Defined(IOS) and not Defined(CPUX86)}

procedure JVEFacebookSetAdvertiserTracking(Enabled: Boolean);
begin
  TFBAdSettings.OCClass.setAdvertiserTrackingEnabled(Enabled);
end;

var
  FacebookBundle: NSBundle;
  ErrorPointer: Pointer = nil;
initialization
  FacebookBundle := TNSBundle.Wrap(TNSBundle.OCClass.bundleWithPath(
    TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).pathForResource(
    ToNSSTR('FBAudienceNetwork'), ToNSSTR('framework'), ToNSSTR('Frameworks'))));
  if not FacebookBundle.loadAndReturnError(@ErrorPointer) then
    Log.d('FAN error: %s', [FromNSSTR(TNSError.Wrap(ErrorPointer).localizedDescription)])
  else
    Log.d('FAN loaded');
finalization
  FacebookBundle.unload;
  FacebookBundle := nil;
{$ELSE}

procedure JVEFacebookSetAdvertiserTracking(Enabled: Boolean);
begin
end;

{$ENDIF}
end.

