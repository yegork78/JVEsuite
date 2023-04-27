(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.AdMob;

interface

uses System.SysUtils, System.Types, System.Classes, System.DateUtils, System.Math,
  System.Sensors, System.StrUtils, FMX.Types, FMX.Platform, FMX.Controls, JVE.Utils,
  FMX.Forms, JVE.Banners, JVE.Actions,
  {$IFDEF ANDROID}AndroidApi.JNI.Embarcadero, Androidapi.JNIBridge, {$ENDIF}
  {$IF CompilerVersion >= 27} System.Messaging{$ELSE} FMX.Messages{$ENDIF};

type
  TJVEAdMob = class;
  TJVEAdMobInterstitial = class;
  TJVEAdMobGender = (agUnknown, agMale, agFemale);
  TJVEAdMobCOPPA = (acUndefined, acChildDirected, acRegular);
  TJVEAdMobHasPaidAd = procedure(Sender: TJVEAdMob) of object;
  TJVEAdMobNoPaidAd = procedure(Sender: TJVEAdMob; Error: String) of object;
  TJVEAdMobInterstitialNoAd = procedure(Sender: TJVEAdMobInterstitial; Error: String) of object;
  TJVEAdMobInterstitialEvent = procedure of object;
  TJVEAdMobInterstitialReward = procedure(Sender: TJVEAdMobInterstitial;
    Kind: String; Amount: Double) of object;

  // This class provides AdMob banners support. Since AdMob does not support
  // desktop applications, it falls back to TJVEBanners on desktop or when
  // there are no ads to be presented.
  // If you need to support another advertisements network, which has an
  // appropriate SDK, you can use this class as a basis for such functionality.
  //
  // For best results, put this control directly on your form, Align = Bottom,
  // AutoSize = True. Then put a TLayout on the same form, Align = Client,
  // and create your user interface within that.
  //
  // This class does NOT collect analytics data, as all the analysis is available
  // through the AdMob services.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEAdMob = class(TJVEBanners)
  private
    FFormActivateMessageId: Integer;
    FAdDelay: Integer;
    FAutoSize: Boolean;
    FiPadUnitId: String;
    FiPhoneUnitId: String;
    FAndroidUnitId: String;
    FOnHasPaidAd: TJVEAdMobHasPaidAd;
    FOnNoPaidAd: TJVEAdMobNoPaidAd;
    FForcingSize: Boolean;
    FCOPPACompliance: TJVEAdMobCOPPA;
    FUserAgeYears: Integer;
    FUserGender: TJVEAdMobGender;
    FUserKeywords: TStrings;
    procedure SetiPadUnitId(const Value: String);
    procedure SetiPhoneUnitId(const Value: String);
    procedure SetAndroidUnitId(const Value: String);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
    procedure SetUserKeywords(const Value: TStrings);
    procedure SetUserAgeYears(const Value: Integer);
    procedure SetUserGender(const Value: TJVEAdMobGender);
    procedure SetUserLocation(const Value: TLocationCoord2D);
    procedure FormActivateHandler(const Sender: TObject; const M: TMessage);
{$IF Defined(IOS) and not Defined(CPUX86)}
  private
    FAdRequest: IInterface;
    FAdView: IInterface;
    FAdDelegate: TObject;
    FHasPaidAds: Boolean;
    FInitialized: Boolean;
    FTimerHandle: TFmxHandle;
    FUserLocation: TLocationCoord2D;
    procedure CreateBanner;
    procedure KeywordsChanged(Sender: TObject);
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
    FAdBuilder: IInterface;
    FAdView: IInterface;
    FAdListener: TObject;
    FHasPaidAds: Boolean;
    FInitialized: Boolean;
    FOrientationChangedId: Integer;
    FTimerHandle: TFmxHandle;
    FUserLocation: TLocationCoord2D;
    procedure CreateBanner;
    procedure KeywordsChanged(Sender: TObject);
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
    // This is the time before actual ads are requested from AdMob (if fallback
    // banners are available).
    // It usually takes a couple of seconds for AdMob to bring the very first Ad,
    // so if this is 0 and there is a fallback banner, that fallback might be
    // presented for just 2 seconds: too short to react and too distracting.
    property AdDelay: Integer read FAdDelay write FAdDelay default 15;

    // AdMob SDK prescribes a particular height for the control:
    // - 90 pixels on iPad
    // - 50 pixels on iPhone in portrait
    // - 32 pixels on iPhone in landscape
    // And we are using 90 pixels on desktop.
    // Use this property to allow the control to specify its own height based on
    // these rules and on the device in question (does not apply in design time).
    // See recommended placement rules in the component description above.
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;

    // The AdMob AdUnitID. AdMob dictates that each device type has its own
    // AdUnitID.
    // Set this value to the appropriate AdMob Publisher ID or Mediation ID.
    // If you want banners manager, but don't want AdMob, leave these blank.
    property iPadUnitId: String read FiPadUnitId write SetiPadUnitId;
    property iPhoneUnitId: String read FiPhoneUnitId write SetiPhoneUnitId;
    property AndroidUnitId: String read FAndroidUnitId write SetAndroidUnitId;


    // If you don't want to use fallback banners, rather hiding the controls,
    // when real AdMob ads are not available, use these events.
    // Notice, the control starts with NO real ads (so you should create it hidden):
    // * When an ad is loaded and presented, OnHasPaidAd is called and you
    //   should show the control
    // * When AdMob is out of ads again OnNoPaidAd event is called and you
    //   should hide the control, waiting for OnHasPaidAd again.
    property OnHasPaidAd: TJVEAdMobHasPaidAd read FOnHasPaidAd write FOnHasPaidAd;
    property OnNoPaidAd: TJVEAdMobNoPaidAd read FOnNoPaidAd write FOnNoPaidAd;

    // Use this flag to indicate whether AdMob should try to be compliant with
    // COPPA (disables behavioural targetting, etc).
    property COPPACompliance: TJVEAdMobCOPPA read FCOPPACompliance
      write SetCOPPACompliance default acUndefined;

    // All remaining properties are optional. They provide details about the user,
    // which will make the ad more appropriate (thus theoretically resulting in a
    // higher CPM).
    // Set these properties if you have some expectation of who the user will be
    // or if you know these from some questionnaire.
    property UserGender: TJVEAdMobGender read FUserGender
      write SetUserGender default agUnknown;
    property UserAgeYears: Integer read FUserAgeYears write SetUserAgeYears default 0;
    property UserKeywords: TStrings read FUserKeywords write SetUserKeywords;
  public
    // The user location should normally only be set, if you are using GPS for
    // something more usefull as well.
    property UserLocation: TLocationCoord2D write SetUserLocation;
  end;

  // This component wraps the AdMob interstitials API. Similarly to banners API,
  // this Ad serving API is only available on mobile.
  // It should be noted that AdMob only supports one interstitial at a time, thus
  // this component behaves as a singleton.
  //
  // To use this component you need the AdMob Publisher ID or Mediation ID.
  // Go to the AdMob site: https://www.google.com/admob, register an account there,
  // and create a new ad unit of Interstitial format (create an app if needed)
  // therein.
  //
  // This class does NOT collect analytics data, as all the analysis is available
  // through the AdMob services.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEAdMobInterstitial = class(TComponent, IJVEInterstitial)
  private
    FReward: Boolean;
    FOnAdClosed: TJVEAdMobInterstitialEvent;
    FOnAdShown: TJVEAdMobInterstitialEvent;
    FOnNoAd: TJVEAdMobInterstitialNoAd;
    FOnReward: TJVEAdMobInterstitialReward;
{$IF Defined(ANDROID)}
    FLoading: Boolean;
    FShowRequested: Boolean;
    FListener: IJavaInstance;
    FInterstitial: IJavaInstance;
    FRewardGranted: Boolean;
    FRewardAmount: Integer;
    FRewardKind: String;
    procedure MessageTrackingHandler(const Sender: TObject; const Msg: TMessage);
{$ELSEIF Defined(IOS) and not Defined(CPUX86)}
    FDelegate: TObject;
    FLoading: array[Boolean] of Boolean;
    FShowWhenReady: array[Boolean] of Boolean;
    FReadyAd: array[Boolean] of Pointer;
    class var FAdRequest: array[Boolean] of IInterface;
{$ENDIF}
    class var FiPadUnitId: array[Boolean] of String;
    class var FiPhoneUnitId: array[Boolean] of String;
    class var FAndroidUnitId: array[Boolean] of String;
    class var FCOPPACompliance: array[Boolean] of TJVEAdMobCOPPA;
    class var FUserKeywords: array[Boolean] of TStrings;
    class var FUserAgeYears: array[Boolean] of Integer;
    class var FUserGender: array[Boolean] of TJVEAdMobGender;
    class var FUserLocation: array[Boolean] of TLocationCoord2D;
    procedure SetiPadUnitId(const Value: String);
    procedure SetiPhoneUnitId(const Value: String);
    procedure SetAndroidUnitId(const Value: String);
    procedure SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
    procedure SetUserKeywords(const Value: TStrings);
    procedure SetUserAgeYears(const Value: Integer);
    procedure SetUserGender(const Value: TJVEAdMobGender);
    procedure SetUserLocation(const Value: TLocationCoord2D);
    function GetAndroidUnitId: String;
    function GetCOPPACompliance: TJVEAdMobCOPPA;
    function GetiPadUnitId: String;
    function GetiPhoneUnitId: String;
    function GetUserAgeYears: Integer;
    function GetUserGender: TJVEAdMobGender;
    function GetUserKeywords: TStrings;
    function GetUserLocation: TLocationCoord2D;
    class destructor Destroy;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // In three functions below, the Sender parameter is for the IJVEInterstitial
    // compatibility and should not be used.

    // Caches the ad, so that the Show procedure could present the ad immediately.
    procedure Cache(Sender: TJVEAction = nil);
    // Shows the ad (if any). If an ad is cached, it is presented immediately,
    // otherwise it is downloaded.
    procedure Show(Sender: TJVEAction = nil);
    // Checks whether an ad is cached. You can use this, if you don't want Show
    // procedure to download ads, i.e. "if AM.IsCached then AM.Show;".
    function IsCached(Sender: TJVEAction = nil): Boolean;
  published
    // Indicates whether a regular interstitial should be presented or a reward
    // video. Never change this value after calling Cache or Show functions!
    property Reward: Boolean read FReward write FReward default False;

    // The AdMob AdUnitID. AdMob dictates that each device type has its own
    // AdUnitID.
    // Set this value to the appropriate AdMob Publisher ID or Mediation ID.
    property iPadUnitId: String read GetiPadUnitId write SetiPadUnitId;
    property iPhoneUnitId: String read GetiPhoneUnitId write SetiPhoneUnitId;
    property AndroidUnitId: String read GetAndroidUnitId write SetAndroidUnitId;

    // Use this flag to indicate whether AdMob should try to be compliant with
    // COPPA (disables behavioural targetting, etc).
    property COPPACompliance: TJVEAdMobCOPPA read GetCOPPACompliance
      write SetCOPPACompliance default acUndefined;

    // All remaining properties are optional. They provide details about the user,
    // which will make the ad more appropriate (thus theoretically resulting in a
    // higher CPM).
    // Set these properties if you have some expectation of who the user will be
    // or if you know these from some questionnaire.
    property UserGender: TJVEAdMobGender read GetUserGender
      write SetUserGender default agUnknown;
    property UserAgeYears: Integer read GetUserAgeYears write SetUserAgeYears default 0;
    property UserKeywords: TStrings read GetUserKeywords write SetUserKeywords;

    // These events are fired to notify when the ad was actually presented or hidden.
    property OnAdShown: TJVEAdMobInterstitialEvent read FOnAdShown write FOnAdShown;
    property OnAdClosed: TJVEAdMobInterstitialEvent read FOnAdClosed write FOnAdClosed;
    property OnNoAd: TJVEAdMobInterstitialNoAd read FOnNoAd write FOnNoAd;
    property OnReward: TJVEAdMobInterstitialReward read FOnReward write FOnReward;
  public
    // The user location should normally only be set, if you are using GPS for
    // something more usefull as well.
    property UserLocation: TLocationCoord2D read GetUserLocation write SetUserLocation;
  end;

// Might be usefull, if you want to build your user interface around this.
function JVEAdMobBannerHeight(IncludeSafeArea: Boolean = False): Single;
function JVEAdMobSafeAreaBottomInset: Single;

procedure JVEAdMobInitAndShowConsent(Minors: Boolean);

implementation

{$IF Defined(IOS)}

uses System.TypInfo, FMX.Platform.iOS, Macapi.ObjectiveC, JVE.Mac.CodeBlocks,
  iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.CoreGraphics, iOSapi.UIKit;

function JVEAdMobBannerHeight(IncludeSafeArea: Boolean = False): Single;
begin
  Result := IfThen(IncludeSafeArea, JVEAdMobSafeAreaBottomInset, 0);

  if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPad then
      Result := Result + 90
  else
    Result := Result + IfThen(SharedUIApplication.statusBarOrientation in
      [UIDeviceOrientationLandscapeLeft, UIDeviceOrientationLandscapeRight], 32, 50);
end;

function JVEAdMobSafeAreaBottomInset: Single;
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
  FMX.Helpers.Android, AndroidApi.JNI, AndroidApi.JNI.Location, AndroidApi.JNI.JavaTypes,
  AndroidApi.JNI.Util, AndroidApi.JNI.GraphicsContentViewText
  {$IF CompilerVersion > 27}, AndroidApi.JNI.AdMob{$ENDIF};

function JVEAdMobBannerHeight(IncludeSafeArea: Boolean = False): Single;
begin
  Result := IfThen(IncludeSafeArea, JVEAdMobSafeAreaBottomInset, 0) +
    IfThen(Screen.Size.Height <= 400, 32, IfThen(Screen.Size.Height <= 720, 50, 90));
end;

function JVEAdMobSafeAreaBottomInset: Single;
begin
  Result := 0;
end;

{$ELSE DESKTOP}

function JVEAdMobBannerHeight(IncludeSafeArea: Boolean = False): Single;
begin
  Result := IfThen(IncludeSafeArea, JVEAdMobSafeAreaBottomInset, 0) + 90;
end;

function JVEAdMobSafeAreaBottomInset: Single;
begin
  Result := 0;
end;

{$ENDIF}

{$IF Defined(ANDROID)}

type
  TJVEAdMobMessage = class(TMessage)
  private
    FThis: JObject;
    FError: String;
    FInterstitial: JObject;
  public
    constructor Create(AThis: JObject; AError: String; AInterstitial: JObject);
    property This: JObject read FThis;
    property Error: String read FError;
    property Interstitial: JObject read FInterstitial;
  end;

{ TJVELocationMessage }

constructor TJVEAdMobMessage.Create(AThis: JObject; AError: String; AInterstitial: JObject);
begin
  FThis := AThis;
  FError := AError;
  FInterstitial := AInterstitial;
end;
{$ENDIF}

{ TJVEAdMobInterstitial }

constructor TJVEAdMobInterstitial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IF Defined(ANDROID)}
  TMessageManager.DefaultManager.SubscribeToMessage(TJVEAdMobMessage, MessageTrackingHandler);
{$ENDIF}

  if FUserKeywords[False] = nil then
  begin
    FUserKeywords[False] := TStringList.Create;
    FUserKeywords[True] := TStringList.Create;
  end;
end;

destructor TJVEAdMobInterstitial.Destroy;
begin
{$IF Defined(ANDROID)}
  TMessageManager.DefaultManager.Unsubscribe(TJVEAdMobMessage, MessageTrackingHandler);
{$ENDIF}
  inherited Destroy;
end;

class destructor TJVEAdMobInterstitial.Destroy;
begin
  FreeAndNil(FUserKeywords[False]);
  FreeAndNil(FUserKeywords[True]);
end;

{$IF Defined(ANDROID)}
procedure TJVEAdMobInterstitial.MessageTrackingHandler(const Sender: TObject; const Msg: TMessage);
var
  Data: String;
  Idx: Integer;
begin
  if not (Msg is TJVEAdMobMessage) or not TJNIResolver.IsSameObject(FListener, (TJVEAdMobMessage(Msg).This)) then
    Exit;

  Data := TJVEAdMobMessage(Msg).Error;
  if Data = '' then
  begin
    FInterstitial := TJVEAdMobMessage(Msg).FInterstitial;

    if FShowRequested then
    begin
      FShowRequested := False;
      Show;
    end;
  end else
  if Data = '#' then
  begin
    FInterstitial := nil;
    if Assigned(FOnAdShown) then
      FOnAdShown;
  end else
  if Data = '^' then
  begin
    FLoading := False;
    FShowRequested := False;
    FInterstitial := nil;

    if Screen.ActiveForm <> nil then
    begin
      Screen.ActiveForm.BeginUpdate;
      Screen.ActiveForm.Invalidate;
      Screen.ActiveForm.EndUpdate;
    end;

    if Assigned(FOnAdClosed) then
      FOnAdClosed;

    if FRewardGranted and Assigned(FOnReward) then
    begin
      FRewardGranted := False;
      FOnReward(Self, FRewardKind, FRewardAmount);
    end;
  end else
  if Data.Chars[0] = '$' then
  begin
    FRewardGranted := True;
    Data := Data.Substring(1);
    Idx := Data.LastIndexOf('/');
    FRewardKind := Data.Substring(0, Idx);
    FRewardAmount := StrToInt(Data.Substring(Idx + 1));
  end else
  begin
    FLoading := False;
    FShowRequested := False;
    FRewardGranted := False;
    FInterstitial := nil;

    if Assigned(FOnNoAd) then
      FOnNoAd(Self, Data);
  end;
end;
{$ENDIF}

function TJVEAdMobInterstitial.GetAndroidUnitId: String;
begin
  Result := FAndroidUnitId[FReward];
end;

function TJVEAdMobInterstitial.GetCOPPACompliance: TJVEAdMobCOPPA;
begin
  Result := FCOPPACompliance[FReward];
end;

function TJVEAdMobInterstitial.GetiPadUnitId: String;
begin
  Result := FiPadUnitId[FReward];
end;

function TJVEAdMobInterstitial.GetiPhoneUnitId: String;
begin
  Result := FiPhoneUnitId[FReward];
end;

function TJVEAdMobInterstitial.GetUserAgeYears: Integer;
begin
  Result := FUserAgeYears[FReward];
end;

function TJVEAdMobInterstitial.GetUserGender: TJVEAdMobGender;
begin
  Result := FUserGender[FReward];
end;

function TJVEAdMobInterstitial.GetUserKeywords: TStrings;
begin
  Result := FUserKeywords[FReward];
end;

function TJVEAdMobInterstitial.GetUserLocation: TLocationCoord2D;
begin
  Result := FUserLocation[FReward];
end;

procedure TJVEAdMobInterstitial.SetAndroidUnitId(const Value: String);
begin
  FAndroidUnitId[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetiPadUnitId(const Value: String);
begin
  FiPadUnitId[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetiPhoneUnitId(const Value: String);
begin
  FiPhoneUnitId[FReward] := Value;
end;

{$IF Defined(IOS) and not Defined(CPUX86)}

type
  GADRequest = interface(NSObject)
    procedure registerAdNetworkExtras(extras: Pointer); cdecl;
    function adNetworkExtrasFor(aClass: Pointer): Pointer; cdecl;
    procedure removeAdNetworkExtrasFor(aClass: Pointer); cdecl;
    procedure setKeywords(keywords: NSArray); cdecl;
    function keywords: NSArray; cdecl;
    procedure setContentURL(contentURL: NSString); cdecl;
    function contentURL: NSString; cdecl;
    procedure setNeighboringContentURLStrings(neighboringContentURLStrings
      : NSArray); cdecl;
    function neighboringContentURLStrings: NSArray; cdecl;
    procedure setRequestAgent(requestAgent: NSString); cdecl;
    function requestAgent: NSString; cdecl;
  end;
  GADRequestClass = interface(NSObjectClass)
    function request: GADRequest; cdecl;
  end;
  TGADRequest = class(TOCGenericImport<GADRequestClass, GADRequest>) end;

  GADAdSize = record
    size: CGSize;
    flags: NSUInteger;
  end;

  GADBannerView = interface(UIView)
    [MethodName('initWithAdSize:origin:')]
    function initWithAdSizeOrigin(adSize: GADAdSize; origin: CGPoint): GADBannerView; cdecl;
    [MethodName('initWithAdSize:')]
    function initWithAdSize(adSize: GADAdSize): GADBannerView; cdecl;
    procedure setAdUnitID(adUnitID: NSString); cdecl;
    function adUnitID: NSString; cdecl;
    procedure setRootViewController(rootViewController: UIViewController); cdecl;
    function rootViewController: UIViewController; cdecl;
    procedure setAdSize(adSize: GADAdSize); cdecl;
    function adSize: GADAdSize; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    function delegate: Pointer; cdecl;
    procedure setAdSizeDelegate(adSizeDelegate: Pointer); cdecl;
    function adSizeDelegate: Pointer; cdecl;
    procedure loadRequest(request: GADRequest); cdecl;
    procedure setAutoloadEnabled(autoloadEnabled: Boolean); cdecl;
    function isAutoloadEnabled: Boolean; cdecl;
  end;
  GADBannerViewClass = interface(UIViewClass)
  end;
  TGADBannerView = class(TOCGenericImport<GADBannerViewClass, GADBannerView>) end;

  GADBannerViewDelegate = interface(IObjectiveC)
    ['{1F1CF729-D448-40B4-8962-D9287590A997}']
    procedure bannerViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError); cdecl;
    //procedure bannerViewDidRecordImpression(bannerView: GADBannerView); cdecl;
    //procedure bannerViewDidRecordClick(bannerView: GADBannerView); cdecl;
    //procedure bannerViewWillPresentScreen(bannerView: GADBannerView); cdecl;
    //procedure bannerViewWillDismissScreen(bannerView: GADBannerView); cdecl;
    //procedure bannerViewDidDismissScreen(bannerView: GADBannerView); cdecl;
  end;

  GADFullScreenContentDelegate = interface(IObjectiveC)
    //procedure adDidRecordImpression(ad: Pointer); cdecl;
    //procedure adDidRecordClick(ad: Pointer); cdecl;
    //procedure ad(ad: Pointer; didFailToPresentFullScreenContentWithError: NSError); cdecl;
    procedure adWillPresentFullScreenContent(ad: Pointer); cdecl;
    procedure adWillDismissFullScreenContent(ad: Pointer); cdecl;
    //procedure adDidDismissFullScreenContent(ad: Pointer); cdecl;
  end;

  GADInterstitialAd = interface(NSObject)
    function adUnitID: NSString; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController;
      error: NSError): Boolean; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController); cdecl;
  end;
  GADInterstitialAdClass = interface(NSObjectClass)
    procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest;
      completionHandler: Pointer{GADInterstitialAdLoadCompletionHandler}); cdecl;
  end;
  TGADInterstitialAd = class(TOCGenericImport<GADInterstitialAdClass, GADInterstitialAd>) end;

  GADAdReward = interface(NSObject)
    function &type: NSString; cdecl;
    function amount: NSDecimalNumber; cdecl;
    function initWithRewardType(rewardType: NSString;
      rewardAmount: NSDecimalNumber): Pointer { instancetype }; cdecl;
  end;
  GADAdRewardClass = interface(NSObjectClass)
  end;
  TGADAdReward = class(TOCGenericImport<GADAdRewardClass, GADAdReward>) end;

  GADRewardedAd = interface(NSObject)
    function adUnitID: NSString; cdecl;
    function adReward: GADAdReward; cdecl;
    procedure setFullScreenContentDelegate(fullScreenContentDelegate: Pointer); cdecl;
    function fullScreenContentDelegate: Pointer; cdecl;
    function canPresentFromRootViewController(rootViewController: UIViewController; error: NSError): Boolean; cdecl;
    procedure presentFromRootViewController(rootViewController: UIViewController;
      userDidEarnRewardHandler: Pointer{GADUserDidEarnRewardHandler}); cdecl;
  end;
  GADRewardedAdClass = interface(NSObjectClass)
    procedure loadWithAdUnitID(adUnitID: NSString; request: GADRequest;
      completionHandler: Pointer{GADRewardedAdLoadCompletionHandler}); cdecl;
  end;
  TGADRewardedAd = class(TOCGenericImport<GADRewardedAdClass, GADRewardedAd>) end;

  GADMobileAds = interface(NSObject)
    function sdkVersion: NSString; cdecl;
    procedure setApplicationVolume(applicationVolume: Single); cdecl;
    function applicationVolume: Single; cdecl;
    procedure setApplicationMuted(applicationMuted: Boolean); cdecl;
    function applicationMuted: Boolean; cdecl;
    //function audioVideoManager: GADAudioVideoManager; cdecl;
    //function requestConfiguration: GADRequestConfiguration; cdecl;
    //function initializationStatus: GADInitializationStatus; cdecl;
    function isSDKVersionAtLeastMajor(major: NSInteger;
      minor: NSInteger; patch: NSInteger): Boolean; cdecl;
    procedure startWithCompletionHandler(completionHandler:
      Pointer{GADInitializationCompletionHandler}); cdecl;
    procedure disableSDKCrashReporting; cdecl;
    procedure disableMediationInitialization; cdecl;
  end;
  GADMobileAdsClass = interface(NSObjectClass)
    function sharedInstance: GADMobileAds; cdecl;
  end;
  TGADMobileAds = class(TOCGenericImport<GADMobileAdsClass, GADMobileAds>) end;

  TGADBannerViewDelegate = class(TOCLocal, GADBannerViewDelegate)
  private
    [Weak]FControl: TJVEAdMob;
  public
    procedure bannerViewDidReceiveAd(bannerView: GADBannerView); cdecl;
    procedure bannerView(bannerView: GADBannerView; didFailToReceiveAdWithError: NSError); cdecl;
  end;


procedure TGADBannerViewDelegate.bannerViewDidReceiveAd(bannerView: GADBannerView);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  FControl.ResetTimer(True);
  TUIView.OCClass.beginAnimations(nil, nil);
  TUIView.OCClass.setAnimationDuration(0.25);
  GADBannerView(FControl.FAdView).setAlpha(1);
  TUIView.OCClass.commitAnimations;

  FControl.FHasPaidAds := True;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnHasPaidAd) then
    FControl.FOnHasPaidAd(FControl);

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := False;
end;

procedure TGADBannerViewDelegate.bannerView(bannerView: GADBannerView;
  didFailToReceiveAdWithError: NSError);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  if FControl.FStopped then
    FControl.ResetTimer;
  TUIView.OCClass.beginAnimations(nil, nil);
  TUIView.OCClass.setAnimationDuration(0.25);
  GADBannerView(FControl.FAdView).setAlpha(0);
  TUIView.OCClass.commitAnimations;

  FControl.FHasPaidAds := False;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnNoPaidAd) then
    if didFailToReceiveAdWithError = nil then
      FControl.FOnNoPaidAd(FControl, '')
    else
      FControl.FOnNoPaidAd(FControl, FromNSSTR(
        didFailToReceiveAdWithError.localizedDescription));

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := True;
end;

{ TJVEAdMob }

constructor TJVEAdMob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;
  FAutoSize := True;
  FUserKeywords := TStringList.Create;
  TStringList(FUserKeywords).OnChange := KeywordsChanged;
  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

procedure TJVEAdMob.KeywordsChanged(Sender: TObject);
var
  List: NSMutableArray;
  Item: String;
begin
  if FAdRequest = nil then
    Exit;

  List := TNSMutableArray.Create;
  for Item in FUserKeywords do
    List.addObject(PointerNSSTR(Item));
  GADRequest(FAdRequest).setKeywords(List);
end;

procedure TJVEAdMob.SetAndroidUnitId(const Value: String);
begin
  FAndroidUnitId := Value;
end;

procedure TJVEAdMob.SetiPadUnitId(const Value: String);
begin
  FiPadUnitId := Value;

  if (FAdView <> nil) and (TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPad) then
      GADBannerView(FAdView).setAdUnitID(ToNSSTR(FiPadUnitId));
end;

procedure TJVEAdMob.SetiPhoneUnitId(const Value: String);
begin
  FiPhoneUnitId := Value;

  if (FAdView <> nil) and (TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPhone) then
      GADBannerView(FAdView).setAdUnitID(ToNSSTR(FiPhoneUnitId));
end;

procedure TJVEAdMob.NextBanner;
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

function TJVEAdMob.GetHasAds: Boolean;
begin
  Result := FHasPaidAds or inherited GetHasAds;
end;

procedure TJVEAdMob.Move;
begin
  inherited Move;
  UpdateBanner;
end;

procedure TJVEAdMob.Show;
begin
  inherited Show;
  UpdateBanner;
end;

procedure TJVEAdMob.Hide;
begin
  inherited Hide;
  UpdateBanner;
end;

procedure TJVEAdMob.SetParent(const Value: TFmxObject);
begin
  if Parent <> Value then
  begin
    inherited;
    UpdateBanner;
  end
end;

procedure TJVEAdMob.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEAdMob.DoAbsoluteChanged;
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEAdMob.ParentChanged;
begin
  inherited;
  if ([csLoading, csDesigning] * ComponentState) = [] then
    UpdateBanner;
end;

procedure TJVEAdMob.UpdateBanner;
var
  View: GADBannerView;
  Frame: TRectF;
  Parent: UIView;
  Inset: Single;
begin
  if (FAdView = nil) or FForcingSize then
    Exit;

  Inset := IfThen(FAutoSize and (Align in [{$IF CompilerVersion < 27}
    TAlignLayout.alBottom, TAlignLayout.alMostBottom{$ELSE}
    TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}]), JVEAdMobSafeAreaBottomInset, 0);

  Parent := WindowHandleToPlatform(TCommonCustomForm(Root.GetObject).Handle).View;
  Frame := AbsoluteRect;
  View := GADBannerView(FAdView);
  View.setBounds(CGRectMake(0, 0, Frame.Width, Frame.Height - Inset));
  View.setCenter(CGPointMake(Frame.Left + Frame.Width / 2, Frame.Top + (Frame.Height - Inset) / 2));
  View.setHidden(not ParentedVisible or not (Root.GetObject as TCommonCustomForm).Active);
  Parent.addSubview(View);
end;

procedure TJVEAdMob.CreateBanner;
var
  View: GADBannerView;
  Request: GADRequest;
  Delegate: TGADBannerViewDelegate;
  Controller: UIViewController;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  Controller := GetRootViewController;
  if Controller = nil then
  begin
    // We cannot create an AdMob view if we don't have a Root View Controller.
    // If we indeed do not have it, we delay initialization by 1 second and
    // try again.
    // This might happen, for example, if UIAlertView is open.
    FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).CreateTimer(1000, CreateBanner);
    Exit;
  end;

  Delegate := TGADBannerViewDelegate.Create;
  Delegate.FControl := Self;
  FAdDelegate := Delegate;

  Request := TGADRequest.OCClass.request;
  FAdRequest := Request;
  COPPACompliance := FCOPPACompliance;
  UserGender := FUserGender;
  UserAgeYears := FUserAgeYears;
  UserKeywords := FUserKeywords;
  UserLocation := FUserLocation;

  View := TGADBannerView.Create;
  View.setAdUnitID(ToNSSTR(IfThen(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPad, FiPadUnitId, FiPhoneUnitId)));
  View.setRootViewController(Controller);
  View.setDelegate(Delegate.GetObjectID);
  View.setAlpha(0);
  View.loadRequest(Request);
  FAdView := View;

  UpdateBanner;
end;

procedure TJVEAdMob.SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
begin
  FCOPPACompliance := Value;
end;

procedure TJVEAdMob.SetUserAgeYears(const Value: Integer);
begin
  FUserAgeYears := Value;
end;

procedure TJVEAdMob.SetUserGender(const Value: TJVEAdMobGender);
begin
  FUserGender := Value;
end;

procedure TJVEAdMob.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords.Assign(Value);
end;

procedure TJVEAdMob.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation := Value;
end;

type
  TGADFullScreenContentDelegate = class(TOCLocal, GADFullScreenContentDelegate)
  private
    FOwner: TJVEAdMobInterstitial;
  public
    procedure adWillPresentFullScreenContent(ad: Pointer); cdecl;
    procedure adWillDismissFullScreenContent(ad: Pointer); cdecl;
  end;

{ TGADFullScreenContentDelegate }

procedure TGADFullScreenContentDelegate.adWillPresentFullScreenContent(ad: Pointer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FOnAdShown) then
    FOwner.FOnAdShown;
end;

procedure TGADFullScreenContentDelegate.adWillDismissFullScreenContent(ad: Pointer);
begin
  if Assigned(FOwner) and Assigned(FOwner.FOnAdClosed) then
    FOwner.FOnAdClosed;

  if FOwner.FReadyAd[False] = ad then
  begin
    TGADInterstitialAd.Wrap(FOwner.FReadyAd[False]).release;
    FOwner.FReadyAd[False] := nil;
    FOwner.FLoading[False] := False;
  end;

  if FOwner.FReadyAd[True] = ad then
  begin
    TGADRewardedAd.Wrap(FOwner.FReadyAd[True]).release;
    FOwner.FReadyAd[True] := nil;
    FOwner.FLoading[True] := False;
  end;
end;

{ TJVEAdMobInterstitial }

procedure TJVEAdMobInterstitial.Cache(Sender: TJVEAction);
var
  Handler: Pointer;
  AdUnit: NSString;
  Request: GADRequest;
begin
  if FLoading[FReward] or (FReadyAd[FReward] <> nil) then
    Exit;

  FLoading[FReward] := True;
  if FAdRequest[FReward] = nil then
    FAdRequest[FReward] := TGADRequest.OCClass.request;
  if FDelegate = nil then
  begin
    FDelegate := TGADFullScreenContentDelegate.Create;
    TGADFullScreenContentDelegate(FDelegate).FOwner := Self;
  end;

  Handler := TObjCBlock.CreateBlockWithProcedure(procedure(ad: Pointer; error: Pointer)
    begin
      if error = nil then
      begin
        FReadyAd[FReward] := ad;
        TNSObject.Wrap(ad).retain;
        if FShowWhenReady[FReward] then
          Show(Sender);
      end else
      begin
        FLoading[FReward] := False;
        if Assigned(FOnNoAd) then
          FOnNoAd(Self, FromNSSTR(TNSError.Wrap(error).localizedDescription));
      end;
    end);

  Request := GADRequest(FAdRequest[FReward]);
  Request.retain;
  AdUnit := ToNSSTR(IfThen(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).
    userInterfaceIdiom = UIUserInterfaceIdiomPad, FiPadUnitId[FReward], FiPhoneUnitId[FReward]));

  if FReward then
    TGADRewardedAd.OCClass.loadWithAdUnitID(AdUnit, Request, Handler)
  else
    TGADInterstitialAd.OCClass.loadWithAdUnitID(AdUnit, Request, Handler);
end;

function TJVEAdMobInterstitial.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := FLoading[FReward] and (FReadyAd[FReward] <> nil);
end;

procedure TJVEAdMobInterstitial.Show(Sender: TJVEAction);
var
  Rewarded: GADRewardedAd;
  Interstitial: GADInterstitialAd;
begin
  if not IsCached(Sender) then
  begin
    FShowWhenReady[FReward] := True;
    Cache(Sender);
    Exit;
  end;

  FShowWhenReady[FReward] := False;
  if FReward then
  begin
    Rewarded := TGADRewardedAd.Wrap(FReadyAd[True]);
    Rewarded.setFullScreenContentDelegate(TGADFullScreenContentDelegate(FDelegate).GetObjectID);
    Rewarded.presentFromRootViewController(GetRootViewController,
      TObjCBlock.CreateBlockWithProcedure(procedure
      begin
        if Assigned(FOnReward) then
          FOnReward(Self, FromNSSTR(Rewarded.adReward.&type), Rewarded.adReward.amount.doubleValue);
      end))
  end else
  begin
    Interstitial := TGADInterstitialAd.Wrap(FReadyAd[False]);
    Interstitial.setFullScreenContentDelegate(TGADFullScreenContentDelegate(FDelegate).GetObjectID);
    Interstitial.presentFromRootViewController(GetRootViewController);
  end;
end;

procedure TJVEAdMobInterstitial.SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
begin
  FCOPPACompliance[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetUserAgeYears(const Value: Integer);
begin
  FUserAgeYears[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetUserGender(const Value: TJVEAdMobGender);
begin
  FUserGender[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetUserKeywords(const Value: TStrings);
var
  List: NSMutableArray;
  Item: String;
begin
  FUserKeywords[FReward].Assign(Value);
  if FAdRequest[FReward] = nil then
    FAdRequest[FReward] := TGADRequest.OCClass.request;

  List := TNSMutableArray.Create;
  for Item in FUserKeywords[FReward] do
    List.addObject(PointerNSSTR(Item));
  GADRequest(FAdRequest[FReward]).setKeywords(List);
end;

procedure TJVEAdMobInterstitial.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation[FReward] := Value;
end;

type
  UMPRequestParameters = interface(NSObject)
    procedure setTagForUnderAgeOfConsent(tagForUnderAgeOfConsent: Boolean); cdecl;
    function tagForUnderAgeOfConsent: Boolean; cdecl;
  end;
  UMPRequestParametersClass = interface(NSObjectClass)
  end;
  TUMPRequestParameters = class(TOCGenericImport<UMPRequestParametersClass, UMPRequestParameters>) end;

  UMPConsentInformation = interface(NSObject)
    function consentStatus: NSInteger; cdecl;
    function formStatus: NSInteger; cdecl;
    procedure requestConsentInfoUpdateWithParameters(parameters: UMPRequestParameters;
      completionHandler: Pointer{UMPConsentInformationUpdateCompletionHandler}); cdecl;
    procedure reset; cdecl;
  end;
  UMPConsentInformationClass = interface(NSObjectClass)
    procedure setSharedInstance(sharedInstance: UMPConsentInformation); cdecl;
    function sharedInstance: UMPConsentInformation; cdecl;
  end;
  TUMPConsentInformation = class(TOCGenericImport<UMPConsentInformationClass, UMPConsentInformation>) end;

  UMPConsentForm = interface(NSObject)
    procedure presentFromViewController(viewController: UIViewController;
      completionHandler: Pointer{UMPConsentFormPresentCompletionHandler}); cdecl;
  end;
  UMPConsentFormClass = interface(NSObjectClass)
    procedure loadWithCompletionHandler(
      completionHandler: Pointer{UMPConsentFormLoadCompletionHandler}); cdecl;
  end;
  TUMPConsentForm = class(TOCGenericImport<UMPConsentFormClass, UMPConsentForm>) end;

procedure JVEAdMobInitAndShowConsent(Minors: Boolean);
var
  Params: UMPRequestParameters;
begin
  TGADMobileAds.OCClass.sharedInstance.startWithCompletionHandler(nil);

  Params := TUMPRequestParameters.Wrap(TUMPRequestParameters.Alloc.init);
  Params.setTagForUnderAgeOfConsent(Minors);
  TUMPConsentInformation.OCClass.sharedInstance.requestConsentInfoUpdateWithParameters(
    Params, TObjCBlock.CreateBlockWithProcedure(procedure(Error: Pointer)
    begin
      if (Error = nil) and (TUMPConsentInformation.OCClass.sharedInstance.formStatus = 1) then
      begin
        TUMPConsentForm.OCClass.loadWithCompletionHandler(
          TObjCBlock.CreateBlockWithProcedure(procedure(Form: Pointer; Error: Pointer)
          begin
            if (Error = nil) and (TUMPConsentInformation.OCClass.sharedInstance.consentStatus = 1) then
              TUMPConsentForm.Wrap(Form).presentFromViewController(GetRootViewController,
                TObjCBlock.CreateBlockWithProcedure(procedure(Error: Pointer)
                begin
                end));
          end));
      end;
    end));
end;

{$ELSEIF Defined(ANDROID)}

{$IF CompilerVersion < 35}

type
  JAdRequest_Builder = interface; //com.google.android.gms.ads.AdRequest.Builder
  JAdView = interface; //com.google.android.gms.ads.AdView
  JAdSize = interface; //com.google.android.gms.ads.AdSize
  JAdListener = interface; //com.google.android.gms.ads.AdListener
  {$IF CompilerVersion < 28}
  JAdRequest = interface; //com.google.android.gms.ads.AdRequest
  {$ENDIF}
  JNetworkExtras = interface; //com.google.android.gms.ads.mediation.NetworkExtras

  JAdSizeClass = interface(JObjectClass)
  ['{5B0274F8-EDA4-4E6B-A48B-891B127E537B}']
    function _GetBANNER: JAdSize;
    function _GetFULL_BANNER: JAdSize;
    function _GetLARGE_BANNER: JAdSize;
    function _GetLEADERBOARD: JAdSize;
    function _GetMEDIUM_RECTANGLE: JAdSize;
    function _GetSMART_BANNER: JAdSize;
    function _GetWIDE_SKYSCRAPER: JAdSize;
    function init(width: Integer; height: Integer): JAdSize; cdecl;
    property BANNER: JAdSize read _GetBANNER;
    property FULL_BANNER: JAdSize read _GetFULL_BANNER;
    property LARGE_BANNER: JAdSize read _GetLARGE_BANNER;
    property LEADERBOARD: JAdSize read _GetLEADERBOARD;
    property MEDIUM_RECTANGLE: JAdSize read _GetMEDIUM_RECTANGLE;
    property SMART_BANNER: JAdSize read _GetSMART_BANNER;
    property WIDE_SKYSCRAPER: JAdSize read _GetWIDE_SKYSCRAPER;
  end;

  [JavaSignature('com/google/android/gms/ads/AdSize')]
  JAdSize = interface(JObject)
  ['{0ECA52BB-F8CF-4572-B952-BF5EE6BE4F1D}']
    {Property Methods}
    procedure _SetHEIGHT(Value: Integer);
    function _GetHEIGHT: Integer;
    procedure _SetWIDTH(Value: Integer);
    function _GetWIDTH: Integer;
    {Methods}
    function equals(other: JObject): Boolean; cdecl;
    function getHeight: Integer; cdecl;
    function getHeightInPixels(context: JContext): Integer; cdecl;
    function getWidth: Integer; cdecl;
    function getWidthInPixels(context: JContext): Integer; cdecl;
    function hashCode: Integer; cdecl;
    function isAutoHeight: Boolean; cdecl;
    function isFullWidth: Boolean; cdecl;
    function toString: JString; cdecl;
    {Properties}
    property HEIGHT: Integer read _GetHEIGHT write _SetHEIGHT;
    property WIDTH: Integer read _GetWIDTH write _SetWIDTH;
  end;
  TJAdSize = class(TJavaGenericImport<JAdSizeClass, JAdSize>) end;

  JRewardItemClass = interface(IJavaClass)
    ['{DF2C1BCF-EA24-4310-BDBA-672B0A94203F}']
  end;

  [JavaSignature('com/google/android/gms/ads/reward/RewardItem')]
  JRewardItem = interface(IJavaInstance)
    ['{5FB30884-DCA7-4FC4-BF52-C86B6FF9C59E}']
    function getAmount: Integer; cdecl;
    function getType: JString; cdecl;
  end;
  TJRewardItem = class(TJavaGenericImport<JRewardItemClass, JRewardItem>) end;

  JAdListenerClass = interface(JObjectClass)
  ['{2ED2925D-CD0F-4F77-806B-1B12F6179F79}']
    {Constructor}
    function init: JAdListener;
  end;

  [JavaSignature('com/google/android/gms/ads/AdListener')]
  JAdListener = interface(JObject)
  ['{D447E74F-ECC9-4E0A-936D-DF01161BA695}']
    {Methods}
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(errorCode: Integer); cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  end;
  TJAdListener = class(TJavaGenericImport<JAdListenerClass, JAdListener>) end;

  JNetworkExtrasClass = interface(IJavaClass)
  ['{B39A49EF-E69E-430D-8DD6-65BA23041EDE}']
  end;

  [JavaSignature('com/google/android/gms/ads/mediation/NetworkExtras')]
  JNetworkExtras = interface(IJavaInstance)
  ['{BB4A543F-11C9-4B4D-A217-8FCB41DE8C29}']
  end;
  TJNetworkExtras = class(TJavaGenericImport<JNetworkExtrasClass, JNetworkExtras>) end;

  JRewardedVideoAdListenerClass = interface(IJavaClass)
    ['{87C12A64-2D72-40EF-A368-7A1191C11701}']
  end;

  [JavaSignature('com/google/android/gms/ads/reward/RewardedVideoAdListener')]
  JRewardedVideoAdListener = interface(IJavaInstance)
    ['{B3B0208D-10D9-4F67-A6F6-CAFBC83BA5F6}']
    procedure onRewarded(reward: JRewardItem); cdecl;
    procedure onRewardedVideoAdClosed; cdecl;
    procedure onRewardedVideoAdFailedToLoad(error: Integer); cdecl;
    procedure onRewardedVideoAdLeftApplication; cdecl;
    procedure onRewardedVideoAdLoaded; cdecl;
    procedure onRewardedVideoAdOpened; cdecl;
    procedure onRewardedVideoCompleted; cdecl;
    procedure onRewardedVideoStarted; cdecl;
  end;
  TJRewardedVideoAdListener = class(TJavaGenericImport<JRewardedVideoAdListenerClass, JRewardedVideoAdListener>) end;

  JRewardedVideoAdClass = interface(IJavaClass)
    ['{25E427F1-F27B-4466-921A-A4AAC74331F7}']
  end;

  [JavaSignature('com/google/android/gms/ads/reward/RewardedVideoAd')]
  JRewardedVideoAd = interface(IJavaInstance)
    ['{FD0BA5B5-FD5F-4BF5-8072-9B87E94ABC12}']
    function isLoaded: Boolean; cdecl;
    procedure loadAd(adUnit: JString; request: JAdRequest); cdecl; overload;
    procedure setRewardedVideoAdListener(listener: JRewardedVideoAdListener); cdecl;
    procedure show; cdecl;
  end;
  TJRewardedVideoAd = class(TJavaGenericImport<JRewardedVideoAdClass, JRewardedVideoAd>) end;

  {$IF CompilerVersion < 28}
  JAdRequestClass = interface(JObjectClass)
  ['{7FD55571-7CB9-4C1D-B21A-FC47057BEDBE}']
    {Property methods}
    function _GetDEVICE_ID_EMULATOR: JString;
    function _GetCODE_INTERNAL_ERROR: Integer;
    function _GetCODE_INVALID_REQUEST: Integer;
    function _GetCODE_NETWORK_ERROR: Integer;
    function _GetCODE_NO_FILL: Integer;
    function _GetMALE: Integer;
    function _GetFEMALE: Integer;
    function _GetUNKNOWN: Integer;
    {Properties}
    property CODE_INTERNAL_ERROR: Integer read _GetCODE_INTERNAL_ERROR;
    property CODE_INVALID_REQUEST: Integer read _GetCODE_INVALID_REQUEST;
    property CODE_NETWORK_ERROR: Integer read _GetCODE_NETWORK_ERROR;
    property CODE_NO_FILL: Integer read _GetCODE_NO_FILL;
    property FEMALE: Integer read _GetFEMALE;
    property MALE: Integer read _GetMALE;
    property UNKNOWN: Integer read _GetUNKNOWN;
    property DEVICE_ID_EMULATOR: JString read _GetDEVICE_ID_EMULATOR;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest')]
  JAdRequest = interface(JObject)
  ['{538BCC72-BF70-46BF-8FE8-567047462865}']
    {Methods}
    function getBirthday: JDate; cdecl;
    function getGender: Integer; cdecl;
    function getKeywords: JSet; cdecl; // Set<String>
    function getLocation: JLocation; cdecl;
    function getNetworkExtras(networkExtrasClass: Jlang_Class): JNetworkExtras; // <T extends NetworkExtras> T getNetworkExtras(Class<T> networkExtrasClass);
    function isTestDevice(context: JContext): Boolean; cdecl;
  end;
  TJAdRequest = class(TJavaGenericImport<JAdRequestClass, JAdRequest>) end;
  {$ENDIF}

  JAdRequest_BuilderClass = interface(JObjectClass)
  ['{641028EC-942E-4A00-BDE4-CB47A1532F11}']
    {Constructor}
    function init: JAdRequest_Builder; cdecl;
  end;

  [JavaSignature('com/google/android/gms/ads/AdRequest$Builder')]
  JAdRequest_Builder = interface(JObject)
  ['{7DE5AF38-D19F-4740-9F94-FBC6A15F166E}']
    {Methods}
    function addKeyword(keyword: JString): JAdRequest_Builder; cdecl;
    function addNetworkExtras(networkExtras: JNetworkExtras): JAdRequest_Builder; cdecl;
    function addTestDevice(deviceId: JString): JAdRequest_Builder; cdecl;
    function build: JAdRequest; cdecl;
    function setBirthday(birthday: JDate): JAdRequest_Builder; cdecl;
    function setGender(gender: Integer): JAdRequest_Builder; cdecl;
    function setLocation(location: JLocation): JAdRequest_Builder; cdecl;
    function tagForChildDirectedTreatment(tagForChildDirectedTreatment: Boolean): JAdRequest_Builder; cdecl;
  end;
  TJAdRequest_Builder = class(TJavaGenericImport<JAdRequest_BuilderClass, JAdRequest_Builder>) end;

  JMobileAdsClass = interface(JObjectClass)
    ['{5965F027-66AC-4DB6-8447-BC90507743C6}']
    {class} function getRewardedVideoAdInstance(context: JContext): JRewardedVideoAd; cdecl;
    {class} procedure initialize(context: JContext); cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/MobileAds')]
  JMobileAds = interface(JObject)
    ['{9323CFE4-D8EF-400B-9CC3-413E907599B8}']
  end;
  TJMobileAds = class(TJavaGenericImport<JMobileAdsClass, JMobileAds>) end;

  JAdViewClass = interface(JViewGroupClass)
  ['{C716706C-E821-4B1C-941C-A6E7BBF5D53B}']
    {Constructors}
    function init(context: JContext): JAdView; cdecl; overload;
    function init(context: JContext; attrs: JAttributeSet): JAdView; cdecl; overload;
    function init(context: JContext; attrs: JAttributeSet; defStyle: Integer): JAdView; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/ads/AdView')]
  JAdView = interface(JViewGroup)
  ['{F3D5FFA0-4B80-4D46-B7F0-C3A0F5FD864C}']
    {Methods}
    procedure destroy; cdecl;
    function getAdListener: JAdListener; cdecl;
    function getAdSize: JAdSize; cdecl;
    function getAdUnitId: JString; cdecl;
    procedure loadAd(adRequest: JAdRequest); cdecl;
    procedure pause; cdecl;
    procedure resume; cdecl;
    procedure setAdListener(adListener: JAdListener); cdecl;
    procedure setAdSize(adSize: JAdSize); cdecl;
    procedure setAdUnitId(adUnitId: JString); cdecl;
  end;
  TJAdView = class(TJavaGenericImport<JAdViewClass, JAdView>) end;

  TBannerViewListener = class(TJavaLocal, JIAdListener)
  private
    [Weak]FControl: TJVEAdMob;
  public
    constructor Create(Ad: TJVEAdMob);
    destructor Destroy; override;

    procedure onAdFailedToLoad(errorCode: Integer); cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdOpened; cdecl;
  end;

  TInterstitialAdListener = class(TJavaLocal, JIAdListener)
  private
    FTimerHandle: TFmxHandle;
    procedure InterstitialClosed;
  public
    procedure onAdFailedToLoad(errorCode: Integer); cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdOpened; cdecl;
  end;

  TRewardAdListener = class(TJavaLocal, JRewardedVideoAdListener)
  private
    FTimerHandle: TFmxHandle;
    FGranted: Boolean;
    FKind: String;
    FValue: Integer;
    procedure RewardClosed;
  public
    procedure onRewarded(reward: JRewardItem); cdecl;
    procedure onRewardedVideoAdClosed; cdecl;
    procedure onRewardedVideoAdFailedToLoad(error: Integer); cdecl;
    procedure onRewardedVideoAdLeftApplication; cdecl;
    procedure onRewardedVideoAdLoaded; cdecl;
    procedure onRewardedVideoAdOpened; cdecl;
    procedure onRewardedVideoCompleted; cdecl;
    procedure onRewardedVideoStarted; cdecl;
  end;

var
  InterstitialShow: Boolean;  // Show ad immediately after complete load
  InterstitialAd: JInterstitialAd;
  InterstitialComponent: TJVEAdMobInterstitial;
  InterstitialListener: TInterstitialAdListener;
  RewardShow: Boolean;  // Show ad immediately after complete load
  RewardAd: JRewardedVideoAd;
  RewardComponent: TJVEAdMobInterstitial;
  RewardListener: TRewardAdListener;

// Called when a presented ad was just click.
procedure TBannerViewListener.onAdLeftApplication;
begin
end;

{ TInterstitialAdListener }

procedure TInterstitialAdListener.onAdLoaded;
begin
  if (InterstitialAd <> nil) and InterstitialShow then
    CallInUIThread(procedure
    begin
      InterstitialAd.show;
      InterstitialAd := nil;
      InterstitialShow := False;
    end);
end;

procedure TInterstitialAdListener.onAdFailedToLoad(errorCode: Integer);
begin
  InterstitialShow := False;
  InterstitialAd := nil;

  if Assigned(InterstitialComponent) and Assigned(InterstitialComponent.FOnNoAd) then
    InterstitialComponent.FOnNoAd(InterstitialComponent,
      'Failed to receive ad (' + IntToStr(errorCode) + ')');
end;

// Called when the user is about to return to the application after closing an ad.
procedure TInterstitialAdListener.onAdClosed;
begin
  FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
    as IFMXTimerService).CreateTimer(1000, InterstitialClosed);
end;

procedure TInterstitialAdListener.InterstitialClosed;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  if Screen.ActiveForm <> nil then
  begin
    Screen.ActiveForm.BeginUpdate;
    Screen.ActiveForm.Invalidate;
    Screen.ActiveForm.EndUpdate;
  end;

  if Assigned(InterstitialComponent) and Assigned(InterstitialComponent.FOnAdClosed) then
    InterstitialComponent.FOnAdClosed;
end;

// Called when an ad leaves the application (e.g., to go to the browser).
procedure TInterstitialAdListener.onAdLeftApplication;
begin
end;

// Called when an ad opens an overlay that covers the screen.
procedure TInterstitialAdListener.onAdOpened;
begin
  if Assigned(InterstitialComponent) and Assigned(InterstitialComponent.FOnAdShown) then
    InterstitialComponent.FOnAdShown;
end;

procedure TRewardAdListener.onRewarded(reward: JRewardItem);
begin
  FGranted := True;
  FKind := JStringToString(reward.getType);
  FValue := reward.getAmount;
end;

procedure TRewardAdListener.onRewardedVideoAdClosed;
begin
  FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
    as IFMXTimerService).CreateTimer(1000, RewardClosed);
end;

procedure TRewardAdListener.RewardClosed;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  if Screen.ActiveForm <> nil then
  begin
    Screen.ActiveForm.BeginUpdate;
    Screen.ActiveForm.Invalidate;
    Screen.ActiveForm.EndUpdate;
  end;

  if Assigned(RewardComponent) and Assigned(RewardComponent.FOnAdClosed) then
    RewardComponent.FOnAdClosed;

  FGranted := False;
  if Assigned(RewardComponent) and Assigned(RewardComponent.FOnReward) then
    RewardComponent.FOnReward(RewardComponent, FKind, FValue);
end;

procedure TRewardAdListener.onRewardedVideoAdFailedToLoad(error: Integer);
begin
  RewardShow := False;
  if Assigned(RewardComponent) and Assigned(RewardComponent.FOnNoAd) then
    RewardComponent.FOnNoAd(RewardComponent,
      'Failed to receive ad (' + IntToStr(error) + ')');
end;

procedure TRewardAdListener.onRewardedVideoAdLeftApplication;
begin
end;

procedure TRewardAdListener.onRewardedVideoAdLoaded;
begin
  if (RewardAd <> nil) and RewardShow then
    CallInUIThread(procedure
    begin
      RewardAd.show;
      RewardShow := False;
    end);
end;

procedure TRewardAdListener.onRewardedVideoAdOpened;
begin
  if Assigned(RewardComponent) and Assigned(RewardComponent.FOnAdShown) then
    RewardComponent.FOnAdShown;
end;

procedure TRewardAdListener.onRewardedVideoCompleted;
begin
end;

procedure TRewardAdListener.onRewardedVideoStarted;
begin
end;

procedure TBannerViewListener.onAdFailedToLoad(errorCode: Integer);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  if FControl.FStopped then
    FControl.ResetTimer;
  JAdView(FControl.FAdView).setAlpha(0);
  FControl.FHasPaidAds := False;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnNoPaidAd) then
    FControl.FOnNoPaidAd(FControl, 'Failed to receive ad (' + IntToStr(errorCode) + ')');

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := True;  // image cover
end;

{$ELSE}

type
  Jrewarded_RewardItemClass = interface(IJavaClass)
  end;
  [JavaSignature('com/google/android/gms/ads/rewarded/RewardItem')]
  Jrewarded_RewardItem = interface(IJavaInstance)
    function getAmount: Integer; cdecl;
    function getType: JString; cdecl;
  end;
  TJrewarded_RewardItem = class(TJavaGenericImport<Jrewarded_RewardItemClass, Jrewarded_RewardItem>) end;

  JOnUserEarnedRewardListenerClass = interface(IJavaClass)
  end;
  [JavaSignature('com/google/android/gms/ads/OnUserEarnedRewardListener')]
  JOnUserEarnedRewardListener = interface(IJavaInstance)
    procedure onUserEarnedReward(rewardItem: Jrewarded_RewardItem); cdecl;
  end;
  TJOnUserEarnedRewardListener = class(TJavaGenericImport<JOnUserEarnedRewardListenerClass, JOnUserEarnedRewardListener>) end;

  JRewardedInterstitialAdLoadCallbackClass = interface(JAdLoadCallbackClass)
  end;
  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAdLoadCallback')]
  JRewardedInterstitialAdLoadCallback = interface(JAdLoadCallback)
  end;
  TJRewardedInterstitialAdLoadCallback = class(TJavaGenericImport<JRewardedInterstitialAdLoadCallbackClass, JRewardedInterstitialAdLoadCallback>) end;

  JRewardedInterstitialAdClass = interface(JObjectClass)
    procedure load(context: JContext; placement: JString; adRequest: JAdRequest; rewardedInterstitialAdLoadCallback: JRewardedInterstitialAdLoadCallback); cdecl; overload;
  end;
  [JavaSignature('com/google/android/gms/ads/rewardedinterstitial/RewardedInterstitialAd')]
  JRewardedInterstitialAd = interface(JObject)
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedInterstitialAd = class(TJavaGenericImport<JRewardedInterstitialAdClass, JRewardedInterstitialAd>) end;

  JRewardedAdLoadCallbackClass = interface(JAdLoadCallbackClass)
  end;
  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAdLoadCallback')]
  JRewardedAdLoadCallback = interface(JAdLoadCallback)
  end;
  TJRewardedAdLoadCallback = class(TJavaGenericImport<JRewardedAdLoadCallbackClass, JRewardedAdLoadCallback>) end;

  JRewardedAdClass = interface(JObjectClass)
    procedure load(context: JContext; placement: JString; adRequest: JAdRequest; rewardedAdLoadCallback: JRewardedAdLoadCallback); cdecl; overload;
  end;
  [JavaSignature('com/google/android/gms/ads/rewarded/RewardedAd')]
  JRewardedAd = interface(JObject)
    procedure setFullScreenContentCallback(fullScreenContentCallback: JFullScreenContentCallback); cdecl;
    procedure show(activity: JActivity; onUserEarnedRewardListener: JOnUserEarnedRewardListener); cdecl;
  end;
  TJRewardedAd = class(TJavaGenericImport<JRewardedAdClass, JRewardedAd>) end;

  JFullScreenContentListenerClass = interface(JFullScreenContentCallbackClass)
    procedure initAdMob(activity: JActivity; minors: Boolean); cdecl;
  end;
  [JavaSignature('com/jvesoft/admob/FullScreenContentListener')]
  JFullScreenContentListener = interface(JFullScreenContentCallback)
    function getRewardListener: JOnUserEarnedRewardListener; cdecl;
  end;
  TJFullScreenContentListener = class(TJavaGenericImport<JFullScreenContentListenerClass, JFullScreenContentListener>) end;

  JInterstitialAdLoadListenerClass = interface(JInterstitialAdLoadCallbackClass)
  end;
  [JavaSignature('com/jvesoft/admob/InterstitialAdLoadListener')]
  JInterstitialAdLoadListener = interface(JInterstitialAdLoadCallback)
  end;
  TJInterstitialAdLoadListener = class(TJavaGenericImport<JInterstitialAdLoadListenerClass, JInterstitialAdLoadListener>) end;

  JRewardedInterstitialAdLoadListenerClass = interface(JRewardedInterstitialAdLoadCallbackClass)
  end;
  [JavaSignature('com/jvesoft/admob/RewardedInterstitialAdLoadListener')]
  JRewardedInterstitialAdLoadListener = interface(JRewardedInterstitialAdLoadCallback)
  end;
  TJRewardedInterstitialAdLoadListener = class(TJavaGenericImport<JRewardedInterstitialAdLoadListenerClass, JRewardedInterstitialAdLoadListener>) end;

  JRewardedAdLoadListenerClass = interface(JRewardedAdLoadCallbackClass)
  end;
  [JavaSignature('com/jvesoft/admob/RewardedAdLoadListener')]
  JRewardedAdLoadListener = interface(JRewardedAdLoadCallback)
  end;
  TJRewardedAdLoadListener = class(TJavaGenericImport<JRewardedAdLoadListenerClass, JRewardedAdLoadListener>) end;

  TBannerViewListener = class(TJavaLocal, JIAdListener)
  private
    [Weak]FControl: TJVEAdMob;
  public
    constructor Create(Ad: TJVEAdMob);
    destructor Destroy; override;

    procedure onAdClicked; cdecl;
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(adError: JLoadAdError); cdecl;
    procedure onAdImpression; cdecl;
    procedure onAdLoaded; cdecl;
    procedure onAdOpened; cdecl;
  end;

// Called when a presented ad was just click.
procedure TBannerViewListener.onAdClicked;
begin
end;

// Called when ad presentation was recorded.
procedure TBannerViewListener.onAdImpression;
begin
end;

procedure TBannerViewListener.onAdFailedToLoad(adError: JLoadAdError);
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  if FControl.FStopped then
    FControl.ResetTimer;
  JAdView(FControl.FAdView).setAlpha(0);
  FControl.FHasPaidAds := False;
  FControl.UpdateHasAds;

  if Assigned(FControl.FOnNoPaidAd) then
    FControl.FOnNoPaidAd(FControl, 'Failed to receive ad (' + JStringToString(adError.getMessage) + ')');

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := True;  // image cover
end;

{$ENDIF}


{ Module Global Functions }

function ConvertToJavaLocation(const Value: TLocationCoord2D): JLocation;
begin
  Result := TJLocation.JavaClass.init(StringToJString('whatever'));
  Result.setLatitude(Value.Latitude);
  Result.setLongitude(Value.Longitude);
  Result.setAccuracy(0);
end;

{ TBannerViewListener }

constructor TBannerViewListener.Create(Ad: TJVEAdMob);
begin
  inherited Create;
  FControl := Ad;
end;

destructor TBannerViewListener.Destroy;
begin
  FControl := nil;
  inherited;
end;

procedure TBannerViewListener.onAdLoaded;
begin
  if (FControl = nil) or (FControl.FAdView = nil) then
    Exit;

  FControl.ResetTimer(True);
  JAdView(FControl.FAdView).setAlpha(1);
  FControl.FHasPaidAds := True;
  FControl.UpdateHasAds;
  FControl.UpdateBanner;

  if Assigned(FControl.FOnHasPaidAd) then
    FControl.FOnHasPaidAd(FControl);

  if FControl.FCurrent <> nil then
    FControl.FCurrent.Visible := False;
end;

// Called when the user is about to return to the application after clicking on an ad.
procedure TBannerViewListener.onAdClosed;
begin
  if (FControl <> nil) and (FControl.Scene <> nil) and (FControl.Scene.Canvas <> nil) then
    FControl.Scene.AddUpdateRect(RectF(0, 0,
      FControl.Scene.Canvas.Width, FControl.Scene.Canvas.Height));
end;

// Called when an ad opens an overlay that covers the screen.
procedure TBannerViewListener.onAdOpened;
begin
end;

{ TJVEAdMob }

constructor TJVEAdMob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;
  FAutoSize := True;

  FOrientationChangedId := TMessageManager.DefaultManager.SubscribeToMessage(
    TOrientationChangedMessage, OrientationChangedHandler);

  if FUserKeywords = nil then
  begin
    FUserKeywords := TStringList.Create;
    TStringList(FUserKeywords).OnChange := KeywordsChanged;
  end;

  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
end;

procedure TJVEAdMob.KeywordsChanged(Sender: TObject);
var
  Item: String;
begin
  if FAdBuilder = nil then
    Exit;

  for Item in FUserKeywords do
    JAdRequest_Builder(FAdBuilder).addKeyword(StringToJString(Item));
end;

procedure TJVEAdMob.SetiPadUnitId(const Value: String);
begin
  FiPadUnitId := Value;
end;

procedure TJVEAdMob.SetiPhoneUnitId(const Value: String);
begin
  FiPhoneUnitId := Value;
end;

procedure TJVEAdMob.SetAndroidUnitId(const Value: String);
begin
  FAndroidUnitId := Value;

  if (FAdView <> nil) and (JAdView(FAdView).getAdUnitId = nil) then
    CallInUIThreadAndWaitFinishing(procedure
    begin
      JAdView(FAdView).setAdUnitID(StringToJString(FAndroidUnitId));
    end);
end;

procedure TJVEAdMob.NextBanner;
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

function TJVEAdMob.GetHasAds: Boolean;
begin
  Result := FHasPaidAds or inherited GetHasAds;
end;

procedure TJVEAdMob.Move;
begin
  inherited Move;
  UpdateBanner;
end;

procedure TJVEAdMob.SetParent(const Value: TFmxObject);
begin
  if Parent <> Value then
  begin
    inherited;
    UpdateBanner;
  end
end;

procedure TJVEAdMob.AncestorVisibleChanged(const Visible: Boolean);
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEAdMob.DoAbsoluteChanged;
begin
  inherited;
  UpdateBanner;
end;

procedure TJVEAdMob.ParentChanged;
begin
  inherited;
  if ([csLoading, csDesigning] * ComponentState) = [] then
    UpdateBanner;
end;

procedure TJVEAdMob.OrientationChangedHandler(const Sender: TObject; const Msg: TMessage);
begin
  if FAdView <> nil then
  begin
    // Kill off existing ad..
    CallInUIThreadAndWaitFinishing(procedure
    begin
      // Special thanks to Chris Pimlott for finding and fixing (!) a bug here
      JAdView(FAdView).setAdListener(nil);
      FreeAndNil(FAdListener);
      JAdView(FAdView).destroy;
      FAdView := nil;

      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
        WindowHandleToPlatform(TCommonCustomForm(Root).Handle).
          ZOrderManager.RemoveLink(Self);
    end);

    // ..and recreate it
    CreateBanner;
  end;
end;

procedure TJVEAdMob.Show;
begin
  inherited Show;
  Visible := True;
  UpdateBanner;
end;

procedure TJVEAdMob.Hide;
begin
  inherited Hide;
  Visible := False;
  UpdateBanner;
end;

procedure TJVEAdMob.UpdateBanner;
var
  ZOrder: TAndroidZOrderManager;
begin
  if (FAdView = nil) or FForcingSize or (Root = nil) or
    not (Root.GetObject is TCommonCustomForm) then
      Exit;

  ZOrder := WindowHandleToPlatform(TCommonCustomForm(Root).Handle).ZOrderManager;
  ZOrder.AddOrSetLink(Self, JView(FAdView), nil);
  ZOrder.UpdateOrderAndBounds(Self);
end;

procedure TJVEAdMob.CreateBanner;
var
  View: JAdView;
  Listener: TBannerViewListener;
  Form: TCommonCustomForm;
  Frame: TRectF;
begin
  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);

  if (Root = nil) or (Height < 32) or (Width < 100) or
    (FAutoSize and not SameValue(Height, JVEAdMobBannerHeight(
      Align in [{$IF CompilerVersion < 27}TAlignLayout.alBottom, TAlignLayout.alMostBottom
      {$ELSE}TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}]))) or
    (csLoading in ComponentState) or
    not ParentedVisible or not (Root.GetObject is TCommonCustomForm) then
  begin
    FTimerHandle := (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).CreateTimer(1000, CreateBanner);
    Exit;
  end;

  Listener := TBannerViewListener.Create(Self);
  FAdListener := Listener;

  CallInUIThreadAndWaitFinishing(procedure
  begin
    FAdBuilder := TJAdRequest_Builder.Create;
    COPPACompliance := FCOPPACompliance;
    UserGender := FUserGender;
    UserAgeYears := FUserAgeYears;
    UserKeywords := FUserKeywords;
    UserLocation := FUserLocation;

    View := TJAdView.JavaClass.init(GetSharedActivityContext);

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

    View.setAdSize(TJAdSize.JavaClass.SMART_BANNER);
    {$IF CompilerVersion < 35}
      View.setAdListener(TJAdListener.Wrap((Listener as ILocalObject).GetObjectID));
    {$ELSE}
      View.setAdListener(TJAdListenerAdapter.JavaClass.init(Listener));
    {$ENDIF}

    FAdView := View;
    SetAndroidUnitID(FAndroidUnitId);
  end);

  UpdateBanner;

  ExecuteInIdle(procedure
  begin
    View.loadAd(JAdRequest_Builder(FAdBuilder).build);
  end);
end;

procedure TJVEAdMob.SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
{$IF CompilerVersion < 35}
begin
  FCOPPACompliance := Value;
  if (FAdBuilder <> nil) and (FUserAgeYears <> 0) and (Value <> acUndefined) then
    JAdRequest_Builder(FAdBuilder).tagForChildDirectedTreatment(Value = acChildDirected);
{$ELSE}
var
  Config: JRequestConfiguration_Builder;
begin
  FCOPPACompliance := Value;
  if Value <> acUndefined then
  begin
    Config := TJMobileAds.JavaClass.getRequestConfiguration().toBuilder();
    if Value = acChildDirected then
      Config.setMaxAdContentRating(TJRequestConfiguration.JavaClass.MAX_AD_CONTENT_RATING_G).
        setTagForUnderAgeOfConsent(TJRequestConfiguration.JavaClass.TAG_FOR_UNDER_AGE_OF_CONSENT_TRUE)
    else
      Config.setMaxAdContentRating(TJRequestConfiguration.JavaClass.MAX_AD_CONTENT_RATING_MA).
        setTagForUnderAgeOfConsent(TJRequestConfiguration.JavaClass.TAG_FOR_UNDER_AGE_OF_CONSENT_FALSE);
    TJMobileAds.JavaClass.setRequestConfiguration(Config.build());
  end;
{$ENDIF}
end;

procedure TJVEAdMob.SetUserAgeYears(const Value: Integer);
begin
  // As of Delphi 11 - no longer used in Android
  FUserAgeYears := Value;
end;

procedure TJVEAdMob.SetUserGender(const Value: TJVEAdMobGender);
begin
  // As of Delphi 11 - no longer used in Android
  FUserGender := Value;
end;

procedure TJVEAdMob.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords.Assign(Value);
end;

procedure TJVEAdMob.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation := Value;
  if (FAdBuilder <> nil) and ((Value.Latitude <> 0) or (Value.Longitude <> 0)) then
    JAdRequest_Builder(FAdBuilder).setLocation(ConvertToJavaLocation(Value));
end;

{ TJVEAdMobInterstitial }

procedure TJVEAdMobInterstitial.Cache(Sender: TJVEAction);
begin
{$IF CompilerVersion < 35}
  if IsCached or (InterstitialAd <> nil) then
    Exit;

  CallInUIThreadAndWaitFinishing(procedure
  var
    Item: String;
    AdBuilder: JAdRequest_Builder;
  begin
    AdBuilder := TJAdRequest_Builder.Create;

    // For ads testing add your device UUID below
    //AdBuilder.addTestDevice(StringToJString('here goes UUID'));
    AdBuilder.addTestDevice(TJAdRequest.JavaClass.DEVICE_ID_EMULATOR);
    if (FUserAgeYears[FReward] <> 0) then
      AdBuilder.tagForChildDirectedTreatment(FCOPPACompliance[FReward] = acChildDirected);
    if ((FUserLocation[FReward].Latitude <> 0) or (FUserLocation[FReward].Longitude <> 0)) then
      AdBuilder.setLocation(ConvertToJavaLocation(FUserLocation[FReward]));
    for Item in FUserKeywords[FReward] do
      AdBuilder.addKeyword(StringToJString(Item));

    if InterstitialListener = nil then
      InterstitialListener := TInterstitialAdListener.Create;
    if RewardListener = nil then
      RewardListener := TRewardAdListener.Create;

    if FReward then
    begin
      RewardComponent := Self;
      if RewardAd = nil then
      begin
        RewardAd := TJMobileAds.JavaClass.getRewardedVideoAdInstance(MainActivity);
        RewardAd.setRewardedVideoAdListener(RewardListener);
      end;
      RewardAd.loadAd(StringToJString(FAndroidUnitId[FReward]), AdBuilder.build);
    end else
    begin
      InterstitialComponent := Self;
      InterstitialAd := TJInterstitialAd.JavaClass.init(MainActivity);
      {$IF CompilerVersion < 28}
        InterstitialAd.setAdListener(TJAdListener.Wrap((InterstitialListener as ILocalObject).GetObjectID));
      {$ELSE}
        InterstitialAd.setAdListener(TJAdListenerAdapter.JavaClass.init(InterstitialListener));
      {$ENDIF}

      if InterstitialAd.getAdUnitId = nil then
        InterstitialAd.setAdUnitID(StringToJString(FAndroidUnitId[FReward]));
      InterstitialAd.loadAd(AdBuilder.build);
    end;
  end);
{$ELSE}
  if FLoading then
    Exit;
  FLoading := True;

  CallInUIThreadAndWaitFinishing(procedure
  var
    Item: String;
    AdBuilder: JAdRequest_Builder;
  begin
    AdBuilder := TJAdRequest_Builder.Create;

    // Location
    if ((FUserLocation[FReward].Latitude <> 0) or (FUserLocation[FReward].Longitude <> 0)) then
      AdBuilder.setLocation(ConvertToJavaLocation(FUserLocation[FReward]));

    // Keywords
    for Item in FUserKeywords[FReward] do
      AdBuilder.addKeyword(StringToJString(Item));

    if FReward then
    begin
      FListener := TJRewardedAdLoadListener.Create;
      TJRewardedAd.JavaClass.load(MainActivity, StringToJString(
        FAndroidUnitId[FReward]), AdBuilder.build, JRewardedAdLoadListener(FListener));
    end else
    begin
      FListener := TJInterstitialAdLoadListener.Create;
      TJinterstitial_InterstitialAd.JavaClass.load(MainActivity, StringToJString(
        FAndroidUnitId[FReward]), AdBuilder.build, JInterstitialAdLoadListener(FListener));
    end;
  end);
{$ENDIF}
end;

function TJVEAdMobInterstitial.IsCached(Sender: TJVEAction): Boolean;
{$IF CompilerVersion < 35}
var
  Loaded: Boolean;
begin
  CallInUIThreadAndWaitFinishing(procedure
    begin
      Loaded := (FReward and (RewardAd <> nil) and RewardAd.isLoaded) or
        (not FReward and (InterstitialAd <> nil) and InterstitialAd.isLoaded);
    end);
  Result := Loaded;
{$ELSE}
begin
  Result := FInterstitial <> nil;
{$ENDIF}
end;

procedure TJVEAdMobInterstitial.Show(Sender: TJVEAction);
{$IF CompilerVersion < 35}
begin
  if FReward then
    if not IsCached(Sender) then
    begin
      // Cache, then display
      RewardShow := True;
      Cache(Sender);
    end else
      CallInUIThread(procedure
      begin
        RewardShow := False;
        RewardAd.show;
      end)
  else
    if not IsCached(Sender) then
    begin
      // Cache, then display
      InterstitialShow := True;
      Cache(Sender);
    end else
      CallInUIThread(procedure
      begin
        InterstitialShow := False;
        InterstitialAd.show;
        InterstitialAd := nil;
      end);
{$ELSE}
var
  Rew: JRewardedAd;
  Int: Jinterstitial_InterstitialAd;
begin
  if not IsCached then
  begin
    FShowRequested := True;
    Cache(Sender);
  end else
  begin
    FListener := TJFullScreenContentListener.Create;
    if FReward then
    begin
      Rew := TJRewardedAd.Wrap(FInterstitial);
      Rew.setFullScreenContentCallback(JFullScreenContentListener(FListener));
      Rew.show(MainActivity, JFullScreenContentListener(FListener).getRewardListener);
    end else
    begin
      Int := TJinterstitial_InterstitialAd.Wrap(FInterstitial);
      Int.setFullScreenContentCallback(JFullScreenContentListener(FListener));
      Int.show(MainActivity);
    end;
  end;
{$ENDIF}
end;

procedure JVEAdMobInitAndShowConsent(Minors: Boolean);
begin
{$IF CompilerVersion >= 35}
  // Sadly this still does not work, due to compiler issue.
  //TJFullScreenContentListener.JavaClass.initAdMob(MainActivity, Minors);
{$ENDIF}
end;

{$ELSE DESKTOP}

{ TJVEAdMob }

procedure TJVEAdMob.SetiPadUnitId(const Value: String);
begin
  FiPadUnitId := Value;
end;

procedure TJVEAdMob.SetiPhoneUnitId(const Value: String);
begin
  FiPhoneUnitId := Value;
end;

procedure TJVEAdMob.SetAndroidUnitId(const Value: String);
begin
  FAndroidUnitId := Value;
end;

procedure TJVEAdMob.SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
begin
  FCOPPACompliance := Value;
end;

procedure TJVEAdMob.SetUserAgeYears(const Value: Integer);
begin
  FUserAgeYears := Value;
end;

procedure TJVEAdMob.SetUserGender(const Value: TJVEAdMobGender);
begin
  FUserGender := Value;
end;

procedure TJVEAdMob.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords.Assign(Value);
end;

procedure TJVEAdMob.SetUserLocation(const Value: TLocationCoord2D);
begin
end;

constructor TJVEAdMob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAdDelay := 15;
  FAutoSize := True;

  FFormActivateMessageId := TMessageManager.DefaultManager.
    SubscribeToMessage(TFormActivateMessage, FormActivateHandler);
  if FUserKeywords = nil then
    FUserKeywords := TStringList.Create;
end;

{ TJVEAdMobInterstitial }

procedure TJVEAdMobInterstitial.Cache(Sender: TJVEAction);
begin
end;

function TJVEAdMobInterstitial.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := False;
end;

procedure TJVEAdMobInterstitial.Show(Sender: TJVEAction);
begin
end;

procedure JVEAdMobInitAndShowConsent(Minors: Boolean);
begin
end;

{$ENDIF}

destructor TJVEAdMob.Destroy;
begin
{$IF (Defined(IOS) and not Defined(CPUX86))}
  if FAdView <> nil then
  begin
    GADBannerView(FAdView).removeFromSuperview;
    GADBannerView(FAdView).setDelegate(nil);
    GADBannerView(FAdView).release;
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
      JAdView(FAdView).setAdListener(nil);
      FreeAndNil(FAdListener);
      JAdView(FAdView).destroy;
      FAdView := nil;

      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
        WindowHandleToPlatform(TCommonCustomForm(Root).Handle).
          ZOrderManager.RemoveLink(Self);
    end);
  end;

  if FTimerHandle <> 0 then
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);
{$ENDIF}

  TMessageManager.DefaultManager.Unsubscribe(TFormActivateMessage, FFormActivateMessageId);
  inherited Destroy;
end;

procedure TJVEAdMob.FormActivateHandler(const Sender: TObject; const M: TMessage);
begin
  {$IF (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
  UpdateBanner;
  {$ENDIF}
end;

procedure TJVEAdMob.SetAutoSize(const Value: Boolean);
var
  Banner: Single;
begin
  FAutoSize := Value;
  if FAutoSize then
  begin
    Banner := JVEAdMobBannerHeight(Align in [{$IF CompilerVersion < 27}TAlignLayout.alBottom,
      TAlignLayout.alMostBottom{$ELSE}TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}]);
    if not SameValue(Height, Banner) then
      Height := Banner;
  end;
end;

procedure TJVEAdMob.Resize;
begin
  inherited Resize;

  if FForcingSize or not FAutoSize or SameValue(Height, JVEAdMobBannerHeight(
    Align in [{$IF CompilerVersion < 27}TAlignLayout.alBottom, TAlignLayout.alMostBottom
    {$ELSE}TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}])) then
  begin
    {$IF (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
    UpdateBanner;
    {$ENDIF}
    Exit;
  end;

  if Align <> {$IF CompilerVersion < 27}TAlignLayout.alNone{$ELSE}TAlignLayout.None{$ENDIF} then
  begin
    FForcingSize := True;
    ExecuteInIdle(procedure
    begin
      Height := JVEAdMobBannerHeight(Align in [{$IF CompilerVersion < 27}TAlignLayout.alBottom,
        TAlignLayout.alMostBottom{$ELSE}TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}]);
      FForcingSize := False;

      {$IF (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
      UpdateBanner;
      {$ENDIF}
    end);
  end else
    Height := JVEAdMobBannerHeight(Align in [{$IF CompilerVersion < 27}TAlignLayout.alBottom,
      TAlignLayout.alMostBottom{$ELSE}TAlignLayout.Bottom, TAlignLayout.MostBottom{$ENDIF}]);
end;

{$IF Defined(IOS) and not Defined(CPUX86)}

function LibAdMob1: Pointer; cdecl;
  external 'libGoogleMobileAds.a' name 'OBJC_CLASS_$_GADBannerView';
function LibAdMob2: Pointer; cdecl;
  external 'libGoogleAppMeasurement.a' name 'OBJC_CLASS_$_APMDatabase';
function LibAdMob3: Pointer; cdecl;
  external 'libGoogleUtilities.a' name 'OBJC_CLASS_$_GULNetwork';
function LibAdMob4: Pointer; cdecl;
  external 'libnanopb.a' name 'pb_read';
function LibAdMob5: Pointer; cdecl;
  external 'libUserMessagingPlatform.a' name 'OBJC_CLASS_$_UMPView';
function LibAdMob6: Pointer; cdecl;
  external 'libGoogleAppMeasurementIdentitySupport.a'
  name 'OBJC_CLASS_$_APMPlatformIdentitySupport';
function LibAdMob7: Pointer; cdecl;
  external 'libPromisesObjC.a' name 'OBJC_CLASS_$_FBLPromise';
procedure CLangRTLoader; cdecl;
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';

// AdMob supports other networks' mediation. Here is an example of how
// to define a mediation layer (based on iAd, which is deprecated).
// If you need to define your own additional mediation layer, you can define
// them the same way (you can use any OBJ_CLASS_$_ you find in binary).
// Don't forget to add a call to the function you define below.
// function LibAdapterIAd: Pointer; cdecl;
//   external 'libAdapterIAd.a' name 'OBJC_CLASS_$_GADMAdapterIAd';

initialization
finalization
  if TOSVersion.Major < 0 then // We need any False, which compiler cannot preevaluate
  begin
    LibAdMob1;
    LibAdMob2;
    LibAdMob3;
    LibAdMob4;
    LibAdMob5;
    // Don't forget, while adding your own networks: the call should be compiled,
    // but should not be called. A check above is good enough for your code as well.
    // LibAdapterIAd;
  end;
{$ELSE}

procedure TJVEAdMobInterstitial.SetCOPPACompliance(const Value: TJVEAdMobCOPPA);
{$IF Defined(ANDROID) and (CompilerVersion >= 35)}
var
  Config: JRequestConfiguration_Builder;
begin
  FCOPPACompliance[True] := Value;
  FCOPPACompliance[False] := Value;

  if Value <> acUndefined then
  begin
    Config := TJMobileAds.JavaClass.getRequestConfiguration().toBuilder();
    if Value = acChildDirected then
      Config.setMaxAdContentRating(TJRequestConfiguration.JavaClass.MAX_AD_CONTENT_RATING_G).
        setTagForUnderAgeOfConsent(TJRequestConfiguration.JavaClass.TAG_FOR_UNDER_AGE_OF_CONSENT_TRUE)
    else
      Config.setMaxAdContentRating(TJRequestConfiguration.JavaClass.MAX_AD_CONTENT_RATING_MA).
        setTagForUnderAgeOfConsent(TJRequestConfiguration.JavaClass.TAG_FOR_UNDER_AGE_OF_CONSENT_FALSE);
    TJMobileAds.JavaClass.setRequestConfiguration(Config.build());
  end;
end;
{$ELSE}
begin
  FCOPPACompliance[FReward] := Value;
end;
{$ENDIF}

procedure TJVEAdMobInterstitial.SetUserAgeYears(const Value: Integer);
begin
  FUserAgeYears[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetUserGender(const Value: TJVEAdMobGender);
begin
  FUserGender[FReward] := Value;
end;

procedure TJVEAdMobInterstitial.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords[FReward].Assign(Value);
end;

procedure TJVEAdMobInterstitial.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation[FReward] := Value;
end;

{$ENDIF}

{$IF Defined(ANDROID)}

procedure AdMobDone(PEnv: PJNIEnv; Self: JNIObject; Error: JNIString); cdecl;
var
  Msg: TJVEAdMobMessage;
begin
  Msg := TJVEAdMobMessage.Create(TJObject.Wrap(Self), JNIStringToString(PEnv, Error), nil);
  ExecuteInIdle(procedure
  begin
    TMessageManager.DefaultManager.SendMessage(nil, Msg);
  end);
end;

procedure AdMobLoaded(PEnv: PJNIEnv; Self, Interstitial: JNIObject); cdecl;
var
  Msg: TJVEAdMobMessage;
begin
  Msg := TJVEAdMobMessage.Create(TJObject.Wrap(Self), '', TJObject.Wrap(Interstitial));
  ExecuteInIdle(procedure
  begin
    TMessageManager.DefaultManager.SendMessage(nil, Msg);
  end);
end;

procedure RegisterBaseNativeMethods;
var
  PEnv: PJNIEnv;
  AdMobClass: JNIClass;
  NativeMethod: JNINativeMethod;

  procedure RegisterLoader(Name: String);
  begin
    AdMobClass := TJNIResolver.GetJavaClassID('com/jvesoft/admob/' + Name);
    if AdMobClass <> nil then
    begin
      PEnv := TJNIResolver.GetJNIEnv;
      NativeMethod.Name := 'error';
      NativeMethod.Signature := '(Ljava/lang/String;)V';
      NativeMethod.FnPtr := @AdMobDone;
      PEnv^.RegisterNatives(PEnv, AdMobClass, @NativeMethod, 1);

      NativeMethod.Name := 'loaded';
      NativeMethod.Signature := '(Ljava/lang/Object;)V';
      NativeMethod.FnPtr := @AdMobLoaded;
      PEnv^.RegisterNatives(PEnv, AdMobClass, @NativeMethod, 1);
      PEnv^.DeleteLocalRef(PEnv, AdMobClass);
    end;
  end;

begin
  AdMobClass := TJNIResolver.GetJavaClassID('com/jvesoft/admob/FullScreenContentListener');
  if AdMobClass <> nil then
  begin
    PEnv := TJNIResolver.GetJNIEnv;
    NativeMethod.Name := 'done';
    NativeMethod.Signature := '(Ljava/lang/String;)V';
    NativeMethod.FnPtr := @AdMobDone;
    PEnv^.RegisterNatives(PEnv, AdMobClass, @NativeMethod, 1);
    PEnv^.DeleteLocalRef(PEnv, AdMobClass);
  end;

  RegisterLoader('InterstitialAdLoadListener');
  RegisterLoader('RewardedInterstitialAdLoadListener');
  RegisterLoader('RewardedAdLoadListener');
end;

initialization
  RegisterBaseNativeMethods;
{$ENDIF}
end.

