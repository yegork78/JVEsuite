(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.ChartBoost;

interface

uses System.Classes, System.SysUtils, JVE.Actions, JVE.Utils, System.Messaging
  {$IF Defined(IOS) and not Defined(CPUX86)}, iOSapi.Foundation{$ENDIF};

// This component wraps the Chartboost API. This Ad serving API is only available
// for mobile, so right now this works only on real devices (iOS simulator is not
// supported due to compiler issues).
// The actual API is provided within the "libChartboost.a" and "chartboost.jar" files.
//
// To use this component you need App ID and App Signature. Go to the chartboost
// site: www.chartboost.com, register an account there and create an app therein.
// That app will have App ID and App Signature, which should be provided here.
// Notice, to see ads and earn money you will need to create a campaign as well;
// see information therein.
//
// This class does NOT collect analytics data, as all the analysis is available
// through the Chartboost services.
type
  TJVEChartBoost = class;
  TJVEChartBoostClosed = procedure(Sender: TJVEChartBoost) of object;
  TJVEChartBoostNoAd = procedure(Sender: TJVEChartBoost; Error: String) of object;
  TJVEChartBoostReward = procedure(Sender: TJVEChartBoost; Reward: Integer) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEChartBoost = class(TComponent, IJVEInterstitial)
  private
    class var FiOSAppId: String;
    class var FiOSAppSignature: String;
    class var FAndroidAppId: String;
    class var FAndroidAppSignature: String;
  private
    FOnNoAd: TJVEChartBoostNoAd;
    FOnClosed: TJVEChartBoostClosed;
    FOnReward: TJVEChartBoostReward;
    FReward: Boolean;
    FShouldShow: Boolean;
{$IF Defined(IOS) and not Defined(CPUX86)}
    //FAdObject: NSObject;
{$ENDIF}
    function GetiOSAppId: String;
    function GetiOSAppSignature: String;
    function GetAndroidAppId: String;
    function GetAndroidAppSignature: String;
    procedure SetiOSAppId(const Value: String);
    procedure SetiOSAppSignature(const Value: String);
    procedure SetAndroidAppId(const Value: String);
    procedure SetAndroidAppSignature(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MessageTrackingHandler(const Sender: TObject; const Msg: TMessage);

    // In three functions below, the Sender parameter is for the IJVEInterstitial
    // compatibility and should not be used.

    // Caches the ad, so that the Show procedure could present the ad immediately.
    procedure Cache(Sender: TJVEAction = nil);
    // Shows the ad (if any). If an ad is cached, it is presented immediately,
    // otherwise it is downloaded.
    procedure Show(Sender: TJVEAction = nil);
    // Checks whether an ad is cached. You can use this, if you don't want Show
    // procedure to download ads, i.e. "if CB.IsCached then CB.Show;".
    function IsCached(Sender: TJVEAction = nil): Boolean;
  published
    // Indicates whether a regular interstitial should be presented or a reward
    // video. Never change this value after calling Cache or Show functions!
    property Reward: Boolean read FReward write FReward default False;

    // These are the application identifiers, provided by the ChartBoost site.
    property iOSAppId: String read GetiOSAppId write SetiOSAppId;
    property iOSAppSignature: String read GetiOSAppSignature write SetiOSAppSignature;
    property AndroidAppId: String read GetAndroidAppId write SetAndroidAppId;
    property AndroidAppSignature: String read GetAndroidAppSignature write SetAndroidAppSignature;

    property OnClosed: TJVEChartBoostClosed read FOnClosed write FOnClosed;
    property OnNoAd: TJVEChartBoostNoAd read FOnNoAd write FOnNoAd;
    property OnReward: TJVEChartBoostReward read FOnReward write FOnReward;
  end;

procedure JVEChartBoostSetGDPRConsent(Behavioral: Boolean);

implementation

{$IF Defined(IOS) and not Defined(CPUX86)}
uses Macapi.ObjectiveC, iOSapi.CocoaTypes, iOSapi.UIKit, System.Math;
{$ELSEIF Defined(ANDROID)}
uses AndroidApi.JNIBridge, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.App,
  AndroidApi.JNI.Widget, AndroidApi.JNI.GraphicsContentViewText, FMX.Types
  {$IF CompilerVersion >= 27}, AndroidApi.Helpers{$ENDIF}, FMX.Helpers.Android,
  AndroidApi.JNI;
{$ENDIF}

type
  TJVELocationMessage = class(TMessage)
  private
    FLocation: String;
  public
    constructor Create(ALocation: String);
    property Location: String read FLocation;
  end;

  TJVELocationErrorMessage = class(TJVELocationMessage)
  private
    FError: String;
  public
    constructor Create(ALocation, AError: String);
    property Error: String read FError;
  end;

  TJVELocationRewardMessage = class(TJVELocationMessage)
  private
    FReward: Integer;
  public
    constructor Create(ALocation: String; AReward: Integer);
    property Reward: Integer read FReward;
  end;

{ TJVELocationMessage }

constructor TJVELocationMessage.Create(ALocation: String);
begin
  inherited Create;
  FLocation := ALocation;
end;

{ TJVELocationErrorMessage }

constructor TJVELocationErrorMessage.Create(ALocation, AError: String);
begin
  inherited Create(ALocation);
  FError := AError;
end;

{ TJVELocationRewardMessage }

constructor TJVELocationRewardMessage.Create(ALocation: String; AReward: Integer);
begin
  inherited Create(ALocation);
  FReward := AReward;
end;

{ TJVEChartBoost }

constructor TJVEChartBoost.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TMessageManager.DefaultManager.SubscribeToMessage(TJVELocationMessage, MessageTrackingHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TJVELocationErrorMessage, MessageTrackingHandler);
  TMessageManager.DefaultManager.SubscribeToMessage(TJVELocationRewardMessage, MessageTrackingHandler);
end;

destructor TJVEChartBoost.Destroy;
begin
{$IF Defined(IOS) and not Defined(CPUX86)}
  //FAdObject.release;
  //FAdObject := nil;
{$ENDIF}
  TMessageManager.DefaultManager.Unsubscribe(TJVELocationRewardMessage, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVELocationErrorMessage, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVELocationMessage, MessageTrackingHandler);
  inherited Destroy;
end;

procedure TJVEChartBoost.MessageTrackingHandler(const Sender: TObject; const Msg: TMessage);
begin
  if not (Msg is TJVELocationMessage) or (TJVELocationMessage(Msg).Location <> Name) then
    Exit;

  if Msg is TJVELocationErrorMessage then
  begin
    if TJVELocationErrorMessage(Msg).Error = '' then
    begin
      if FShouldShow then
      begin
        FShouldShow := False;
        Show;
      end;
    end else
    if Assigned(FOnNoAd) then
      FOnNoAd(Self, TJVELocationErrorMessage(Msg).Error);
  end else
  if Msg is TJVELocationRewardMessage then
  begin
    if Assigned(FOnReward) then
      FOnReward(Self, TJVELocationRewardMessage(Msg).Reward);
  end else if Assigned(FOnClosed) then
    FOnClosed(Self);
end;

{$IF Defined(IOS) and not Defined(CPUX86)}

(* This component is currently published with a legacy version
   of Chartboost API. The new API is commented out.
   The reason is that the new API's delegates don't work as consistently
   as the old one's do.
   This could be due to Delphi's lack of support for inherited protocols
   or incorrect minimum iOS version.
   Anyway, currently the old API works perfectly and the new one sometimes
   skips caching events and always skips reward events.
   As long as the old API is present, this shouldn't be an issue.


type
  CBLocation = NSString;
  CHBCacheErrorCode = NSUInteger;
  TChartboostConfirmationHandler = procedure(param1: Boolean) of object;
  CHBInterstitial = interface;

  ChartboostClass = interface(NSObjectClass)
    ['{996857D7-0254-446C-AEE1-29869F89EF08}']
    [MethodName('startWithAppId:appSignature:completion:')]
    procedure startWithAppIdAppSignatureCompletion(appId: NSString;
      appSignature: NSString; completion: TChartboostConfirmationHandler); cdecl;
  end;
  Chartboost = interface(NSObject)
    ['{899A0653-127B-4CCF-A461-DD094B52AC2C}']
  end;
  TChartboost = class(TOCGenericImport<ChartboostClass, Chartboost>) end;

  CHBAd = interface(IObjectiveC)
    ['{AE3DBB5D-D434-460F-914F-D60FBDA60DDA}']
    function location: CBLocation; cdecl;
  end;

  CHBAdEvent = interface(NSObject)
    ['{66F80DCA-49CE-4E28-A5EF-08710D3F9FEB}']
    function ad: CHBAd; cdecl;
  end;
  CHBAdEventClass = interface(NSObjectClass)
    ['{7B05A62D-E321-4468-B969-02C125545A5C}']
  end;
  TCHBAdEvent = class(TOCGenericImport<CHBAdEventClass, CHBAdEvent>) end;

  CHBCacheEvent = interface(CHBAdEvent)
    ['{63B33818-9955-4043-AF5E-65B03551E6C4}']
  end;
  CHBCacheEventClass = interface(CHBAdEventClass)
    ['{E6F539B9-1F37-4B65-9904-265370B3314B}']
  end;
  TCHBCacheEvent = class(TOCGenericImport<CHBCacheEventClass, CHBCacheEvent>) end;

  CHBDismissEvent = interface(CHBAdEvent)
    ['{08986966-EB59-497D-BE5C-1C9F2D80188F}']
  end;
  CHBDismissEventClass = interface(CHBAdEventClass)
    ['{A2E6267B-FF23-47D0-8986-B8BD5D9FD5BB}']
  end;
  TCHBDismissEvent = class(TOCGenericImport<CHBDismissEventClass, CHBDismissEvent>) end;

  CHBCacheError = interface(NSObject)
    ['{C014903E-4FBE-448A-A1BE-2F278F82C824}']
    function code: CHBCacheErrorCode; cdecl;
  end;
  CHBCacheErrorClass = interface(NSObjectClass)
    ['{5A974D02-07DC-4A74-AC11-2A5EAAD40F41}']
  end;
  TCHBCacheError = class(TOCGenericImport<CHBCacheErrorClass, CHBCacheError>) end;

  CHBInterstitialDelegate = interface(IObjectiveC)
    ['{0DC7A268-DFD6-4F89-A207-00D8C9C6E9AD}']
    procedure didCacheAd(event: CHBCacheEvent; error: CHBCacheError); cdecl;
    procedure didDismissAd(event: CHBDismissEvent); cdecl;
  end;

  CHBInterstitialClass = interface(NSObjectClass)
    ['{079894D4-470A-428B-8204-83FDCF1594F7}']
  end;
  CHBInterstitial = interface(NSObject)
    ['{77339129-7B3A-4E28-B3A0-393BADED501A}']
    function isCached: Boolean; cdecl;
    [MethodName('initWithLocation:delegate:')]
    function initWithLocationDelegate(location: CBLocation; delegate: Pointer): CHBInterstitial; cdecl;
    procedure cache; cdecl;
    procedure showFromViewController(viewController: UIViewController); cdecl;
  end;
  TCHBInterstitial = class(TOCGenericImport<CHBInterstitialClass, CHBInterstitial>) end;


  CHBRewardEvent = interface(CHBAdEvent)
    ['{41204DB1-56B2-4AE9-BE31-E3B31105CCE0}']
    function reward: NSInteger; cdecl;
  end;
  CHBRewardEventClass = interface(CHBAdEventClass)
    ['{8B55841E-8924-4DEA-A3AE-216DA0E150FD}']
  end;
  TCHBRewardEvent = class(TOCGenericImport<CHBRewardEventClass, CHBRewardEvent>) end;

  CHBRewardedDelegate = interface(IObjectiveC)
    ['{4E853D51-9204-4F03-A9FC-C35DA2E76A2B}']
    procedure didCacheAd(event: CHBCacheEvent; error: CHBCacheError); cdecl;
    procedure didDismissAd(event: CHBDismissEvent); cdecl;
    // The below callback does not work for some reason, so using a legacy one...
    procedure didEarnReward(event: CHBRewardEvent); cdecl;
  end;

  CHBRewardedClass = interface(NSObjectClass)
    ['{12CA9CC9-84D5-4E3C-B2BA-03F5931AD3FB}']
  end;
  CHBRewarded = interface(NSObject)
    ['{39BD23A3-D22F-4567-A8B4-A6CFA9619764}']
    function isCached: Boolean; cdecl;
    [MethodName('initWithLocation:delegate:')]
    function initWithLocationDelegate(location: CBLocation; delegate: Pointer): CHBRewarded; cdecl;
    procedure cache; cdecl;
    procedure showFromViewController(viewController: UIViewController); cdecl;
  end;
  TCHBRewarded = class(TOCGenericImport<CHBRewardedClass, CHBRewarded>) end;


  TChartboostDelegate = class(TOCLocal, CHBRewardedDelegate, CHBInterstitialDelegate)
  public
    procedure didCacheAd(event: CHBCacheEvent; error: CHBCacheError); cdecl;
    procedure didDismissAd(event: CHBDismissEvent); cdecl;
    procedure didEarnReward(event: CHBRewardEvent); cdecl;
  end;

var
  GlobalChartboostDelegate: TChartboostDelegate;

const
  ChartBoostLoadingErrors: array[0..35] of String = ('INTERNAL',
    'INTERNET_UNAVAILABLE', 'TOO_MANY_CONNECTIONS', 'WRONG_ORIENTATION',
    'FIRST_SESSION_INTERSTITIALS_DISABLED', 'NETWORK_FAILURE',
    'NO_AD_FOUND', 'SESSION_NOT_STARTED', 'IMPRESSION_ALREADY_VISIBLE',
    '', 'USER_CANCELLATION', 'INVALID_LOCATION', '', '', '', '',
    'ASSETS_DOWNLOAD_FAILURE', '', '', '', '', 'ASSET_PREFETCH_IN_PROGRESS',
    'WEB_VIEW_CLIENT_RECEIVED_ERROR', '', '', 'INTERNET_UNAVAILABLE_AT_SHOW', '', '',
    '', '', '', '', '', 'PRESENTATION_FAILURE', 'NO_CACHED_AD', 'PUBLISHER_DISABLED');

{ TGADBannerViewDelegate }

procedure TChartboostDelegate.didCacheAd(event: CHBCacheEvent; error: CHBCacheError);
var
  Text: String;
begin
  if (error = nil) or (error.code = 0) then
    Text := ''
  else if (error.code <= High(ChartBoostLoadingErrors)) and (ChartBoostLoadingErrors[error.code] <> '') then
    Text := ChartBoostLoadingErrors[error.code]
  else
    Text := IntToStr(error.code);
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationErrorMessage.Create(FromNSSTR(event.ad.location), Text));
end;

procedure TChartboostDelegate.didDismissAd(event: CHBDismissEvent);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationMessage.Create(FromNSSTR(event.ad.location)));
end;

procedure TChartboostDelegate.didEarnReward(event: CHBRewardEvent);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationRewardMessage.Create(FromNSSTR(event.ad.location), event.reward));
end;

{ TJVEChartBoost }

procedure TJVEChartBoost.Cache(Sender: TJVEAction = nil);
begin
  if FReward then
  begin
    if FAdObject = nil then
      FAdObject := TCHBRewarded.Alloc.initWithLocationDelegate(ToNSSTR(Name), GlobalChartboostDelegate.GetObjectID);
    CHBRewarded(FAdObject).cache;
  end else
  begin
    if FAdObject = nil then
      FAdObject := TCHBInterstitial.Alloc.initWithLocationDelegate(ToNSSTR(Name), GlobalChartboostDelegate.GetObjectID);
    CHBInterstitial(FAdObject).cache;
  end;
end;

function TJVEChartBoost.IsCached(Sender: TJVEAction = nil): Boolean;
begin
  if FAdObject = nil then
    Result := False
  else if FReward then
    Result := CHBRewarded(FAdObject).isCached
  else
    Result := CHBInterstitial(FAdObject).isCached;
end;

procedure TJVEChartBoost.Show(Sender: TJVEAction = nil);
begin
  if not IsCached then
  begin
    Cache;
    FShouldShow := True;
  end else if FReward then
    CHBRewarded(FAdObject).showFromViewController(GetRootViewController)
  else
    CHBInterstitial(FAdObject).showFromViewController(GetRootViewController);
end;*)

type
  CHBGDPRDataUseConsent = interface(NSObject)
  end;
  CHBGDPRDataUseConsentClass = interface(NSObjectClass)
    function gdprConsent(consent: Integer): CHBGDPRDataUseConsent; cdecl;
  end;
  TCHBGDPRDataUseConsent = class(TOCGenericImport<CHBGDPRDataUseConsentClass, CHBGDPRDataUseConsent>) end;

  ChartboostDelegate = interface;
  Chartboost = interface(NSObject)
  end;
  ChartboostClass = interface(NSObjectClass)
    procedure startWithAppId(appId: NSString; appSignature: NSString;
      delegate: ChartboostDelegate); cdecl;
    procedure showInterstitial(location: NSString); cdecl;
    procedure cacheInterstitial(location: NSString); cdecl;
    function hasInterstitial(location: NSString): Boolean; cdecl;
    procedure showRewardedVideo(location: NSString); cdecl;
    procedure cacheRewardedVideo(location: NSString); cdecl;
    function hasRewardedVideo(location: NSString): Boolean; cdecl;
    procedure addDataUseConsent(consent: CHBGDPRDataUseConsent); cdecl;
  end;
  TChartboost = class(TOCGenericImport<ChartboostClass, Chartboost>) end;

  ChartboostDelegate = interface(IObjectiveC)
    procedure didFailToLoadInterstitial(location: NSString; withError: Integer); cdecl;
    procedure didDismissInterstitial(location: NSString); cdecl;
    procedure didFailToLoadRewardedVideo(location: NSString; withError: Integer); cdecl;
    procedure didDismissRewardedVideo(location: NSString); cdecl;
    procedure didCompleteRewardedVideo(location: NSString; withReward: Integer); cdecl;
  end;

  TChartboostDelegate = class(TOCLocal, ChartboostDelegate)
  public
    procedure didFailToLoadInterstitial(location: NSString; withError: Integer); cdecl;
    procedure didDismissInterstitial(location: NSString); cdecl;
    procedure didFailToLoadRewardedVideo(location: NSString; withError: Integer); cdecl;
    procedure didDismissRewardedVideo(location: NSString); cdecl;
    procedure didCompleteRewardedVideo(location: NSString; withReward: Integer); cdecl;
  end;

var
  GlobalChartboostDelegate: TChartboostDelegate;

const
  ChartBoostLoadingErrors: array[0..25] of String = ('INTERNAL',
    'INTERNET_UNAVAILABLE', 'TOO_MANY_CONNECTIONS', 'WRONG_ORIENTATION',
    'FIRST_SESSION_INTERSTITIALS_DISABLED', 'NETWORK_FAILURE',
    'NO_AD_FOUND', 'SESSION_NOT_STARTED', 'IMPRESSION_ALREADY_VISIBLE',
    '9', 'USER_CANCELLATION', 'INVALID_LOCATION', '12', '13', '14', '15',
    'ASSETS_DOWNLOAD_FAILURE', '17', '18', '19', '20', 'ASSET_PREFETCH_IN_PROGRESS',
    'WEB_VIEW_CLIENT_RECEIVED_ERROR', '23', '24', 'INTERNET_UNAVAILABLE_AT_SHOW');

{ TGADBannerViewDelegate }

procedure TChartboostDelegate.didFailToLoadInterstitial(location: NSString; withError: Integer);
var
  Error: String;
begin
  if (withError >= 0) and (withError <= High(ChartBoostLoadingErrors)) then
    Error := ChartBoostLoadingErrors[withError]
  else
    Error := IntToStr(withError);
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationErrorMessage.Create(FromNSSTR(location), Error));
end;

procedure TChartboostDelegate.didDismissInterstitial(location: NSString);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationMessage.Create(FromNSSTR(location)));
end;

procedure TChartboostDelegate.didFailToLoadRewardedVideo(location: NSString; withError: Integer);
begin
  didFailToLoadInterstitial(location, withError);
end;

procedure TChartboostDelegate.didDismissRewardedVideo(location: NSString);
begin
  didDismissInterstitial(location);
end;

procedure TChartboostDelegate.didCompleteRewardedVideo(location: NSString; withReward: Integer);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVELocationRewardMessage.Create(FromNSSTR(location), withReward));
end;

{ TJVEChartBoost }

procedure TJVEChartBoost.Cache(Sender: TJVEAction = nil);
begin
  if FReward then
    TChartboost.OCClass.cacheRewardedVideo(ToNSSTR(Name))
  else
    TChartboost.OCClass.cacheInterstitial(ToNSSTR(Name));
end;

function TJVEChartBoost.IsCached(Sender: TJVEAction = nil): Boolean;
begin
  if FReward then
    Result := TChartboost.OCClass.hasRewardedVideo(ToNSSTR(Name))
  else
    Result := TChartboost.OCClass.hasInterstitial(ToNSSTR(Name));
end;

procedure TJVEChartBoost.Show(Sender: TJVEAction = nil);
begin
  if FReward then
    TChartboost.OCClass.showRewardedVideo(ToNSSTR(Name))
  else
    TChartboost.OCClass.showInterstitial(ToNSSTR(Name));
end;

function TJVEChartBoost.GetiOSAppId: String;
begin
  Result := FiOSAppId;
end;

function TJVEChartBoost.GetiOSAppSignature: String;
begin
  Result := FiOSAppSignature;
end;

procedure TJVEChartBoost.SetiOSAppId(const Value: String);
begin
  if FiOSAppId <> Value then
  begin
    FiOSAppId := Value;
    if (FiOSAppId <> '') and (FiOSAppSignature <> '') then
    begin
      if GlobalChartboostDelegate = nil then
        GlobalChartboostDelegate := TChartboostDelegate.Create;
      TChartboost.OCClass.startWithAppId(ToNSSTR(FiOSAppId),
        ToNSSTR(FiOSAppSignature), GlobalChartboostDelegate);
      {TChartboost.OCClass.startWithAppIdAppSignatureCompletion(
        ToNSSTR(FiOSAppId), ToNSSTR(FiOSAppSignature), nil);}
    end;
  end;
end;

procedure TJVEChartBoost.SetiOSAppSignature(const Value: String);
begin
  if FiOSAppSignature <> Value then
  begin
    FiOSAppSignature := Value;
    if (FiOSAppId <> '') and (FiOSAppSignature <> '') then
    begin
      if GlobalChartboostDelegate = nil then
        GlobalChartboostDelegate := TChartboostDelegate.Create;
      TChartboost.OCClass.startWithAppId(ToNSSTR(FiOSAppId),
        ToNSSTR(FiOSAppSignature), GlobalChartboostDelegate);
      {TChartboost.OCClass.startWithAppIdAppSignatureCompletion(
        ToNSSTR(FiOSAppId), ToNSSTR(FiOSAppSignature), nil);}
    end;
  end;
end;

function TJVEChartBoost.GetAndroidAppId: String;
begin
  Result := FAndroidAppId;
end;

function TJVEChartBoost.GetAndroidAppSignature: String;
begin
  Result := FAndroidAppSignature;
end;

procedure TJVEChartBoost.SetAndroidAppId(const Value: String);
begin
  FAndroidAppId := Value;
end;

procedure TJVEChartBoost.SetAndroidAppSignature(const Value: String);
begin
  FAndroidAppSignature := Value;
end;

procedure JVEChartBoostSetGDPRConsent(Behavioral: Boolean);
begin
  TChartboost.OCClass.addDataUseConsent(TCHBGDPRDataUseConsent.OCClass.gdprConsent(IfThen(Behavioral, 1, 0)));
end;

function LibChartBoost: Pointer; cdecl;
  external 'libChartboost.a' name 'OBJC_CLASS_$_Chartboost';
procedure CLangRTLoader; cdecl;
  external '/usr/lib/clang/lib/darwin/libclang_rt.ios.a';
initialization
finalization
  if TOSVersion.Major < 0 then // We need any False, which compiler cannot preevaluate
    LibChartBoost;
{$ELSEIF Defined(ANDROID)}

type
  JChartboost_CBPIDataUseConsent = interface;
  JChartboost_CBPIDataUseConsentClass = interface(JEnumClass)
    ['{9A26F0FE-99DA-4C0F-B437-34C817304C8E}']
    function _GetNO_BEHAVIORAL: JChartboost_CBPIDataUseConsent; cdecl;
    function _GetUNKNOWN: JChartboost_CBPIDataUseConsent; cdecl;
    function _GetYES_BEHAVIORAL: JChartboost_CBPIDataUseConsent; cdecl;
    property NO_BEHAVIORAL: JChartboost_CBPIDataUseConsent read _GetNO_BEHAVIORAL;
    property UNKNOWN: JChartboost_CBPIDataUseConsent read _GetUNKNOWN;
    property YES_BEHAVIORAL: JChartboost_CBPIDataUseConsent read _GetYES_BEHAVIORAL;
  end;
  [JavaSignature('com/chartboost/sdk/Chartboost$CBPIDataUseConsent')]
  JChartboost_CBPIDataUseConsent = interface(JEnum)
    ['{EA9F87AB-9838-432C-98DA-8B1B8C6CE20A}']
  end;
  TJChartboost_CBPIDataUseConsent = class(TJavaGenericImport<JChartboost_CBPIDataUseConsentClass, JChartboost_CBPIDataUseConsent>) end;

  JChartboostDelegateClass = interface(IJavaClass)
  ['{4CB336C8-F064-414F-BCF9-5F9C7E8C99C5}']
  end;
  [JavaSignature('com/chartboost/sdk/ChartboostDelegate')]
  JChartboostDelegate = interface(IJavaInstance)
  ['{86FC9960-ABA2-4F83-99A1-F231C0FE5B33}']
  end;
  TJChartboostDelegate = class(TJavaGenericImport<JChartboostDelegateClass, JChartboostDelegate>) end;

  JChartboostClass = interface(JObjectClass)
    ['{3CE9FE80-1B96-4BCA-AC92-AFD69FF3813B}']
    {class} procedure cacheInterstitial(string_: JString); cdecl; overload;
    {class} procedure cacheInterstitial(string_: JString; string_1: JString); cdecl; overload;
    {class} procedure cacheMoreApps(string_: JString); cdecl;
    {class} procedure cacheRewardedVideo(string_: JString); cdecl; overload;
    {class} procedure cacheRewardedVideo(string_: JString; string_1: JString); cdecl; overload;
    {class} procedure closeImpression; cdecl;
    {class} function getAutoCacheAds: Boolean; cdecl;
    {class} function getCustomId: JString; cdecl;
    {class} function getPIDataUseConsent: JChartboost_CBPIDataUseConsent; cdecl;
    {class} function getSDKVersion: JString; cdecl;
    {class} function hasInterstitial(string_: JString): Boolean; cdecl;
    {class} function hasMoreApps(string_: JString): Boolean; cdecl;
    {class} function hasRewardedVideo(string_: JString): Boolean; cdecl;
    {class} function isAnyViewVisible: Boolean; cdecl;
    {class} function isSdkStarted: Boolean; cdecl;
    {class} function isWebViewEnabled: Boolean; cdecl;
    {class} function onBackPressed: Boolean; cdecl;
    {class} procedure setActivityAttrs(activity: JActivity); cdecl;
    {class} procedure setAutoCacheAds(b: Boolean); cdecl;
    {class} procedure setChartboostWrapperVersion(string_: JString); cdecl;
    {class} procedure setCustomId(string_: JString); cdecl;
    {class} procedure setDelegate(chartboostDelegate: JChartboostDelegate); cdecl;
    {class} procedure setPIDataUseConsent(context: JContext; cBPIDataUseConsent: JChartboost_CBPIDataUseConsent); cdecl;
    {class} procedure setShouldDisplayLoadingViewForMoreApps(b: Boolean); cdecl;
    {class} procedure setShouldHideSystemUI(boolean: JBoolean); cdecl;
    {class} procedure setShouldPrefetchVideoContent(b: Boolean); cdecl;
    {class} procedure setShouldRequestInterstitialsInFirstSession(b: Boolean); cdecl;
    {class} procedure showInterstitial(string_: JString); cdecl;
    {class} procedure showMoreApps(string_: JString); cdecl;
    {class} procedure showRewardedVideo(string_: JString); cdecl;
    {class} procedure startWithAppId(context: JContext; appId: JString; Signature: JString); cdecl; overload;
    {class} procedure startWithAppId(activity: JActivity; appId: JString; signature: JString); cdecl; overload;
  end;

  [JavaSignature('com/chartboost/sdk/Chartboost')]
  JChartboost = interface(JObject)
    ['{2594BBD5-68A8-4803-87B4-03A259ACDC4E}']
  end;
  TJChartboost = class(TJavaGenericImport<JChartboostClass, JChartboost>) end;

  JChartBoostBaseClass = interface(JChartboostDelegateClass)
  ['{CF705EE3-1ACF-4817-9FC5-2A50163DE1F6}']
  end;
  [JavaSignature('com/jvesoft/chartboost/ChartBoostBase')]
  JChartBoostBase = interface(JChartboostDelegate)
  ['{7189F2F6-A746-4748-9B6C-D8B443193965}']
  end;
  TJChartBoostBase = class(TJavaGenericImport<JChartBoostBaseClass, JChartBoostBase>) end;

{ TJVEChartBoost }

procedure TJVEChartBoost.Cache(Sender: TJVEAction = nil);
begin
  CallInUIThread(procedure
  begin
    if FReward then
      TJChartboost.JavaClass.cacheRewardedVideo(StringToJString(Name))
    else
      TJChartboost.JavaClass.cacheInterstitial(StringToJString(Name));
  end);
end;

function TJVEChartBoost.IsCached(Sender: TJVEAction = nil): Boolean;
var
  Cached: Boolean;
begin
  CallInUIThreadAndWaitFinishing(procedure
  begin
    if FReward then
      Cached := TJChartboost.JavaClass.hasRewardedVideo(StringToJString(Name))
    else
      Cached := TJChartboost.JavaClass.hasInterstitial(StringToJString(Name));
  end);
  Result := Cached;
end;

procedure TJVEChartBoost.Show(Sender: TJVEAction = nil);
begin
  CallInUIThread(procedure
  begin
    if FReward then
      TJChartboost.JavaClass.showRewardedVideo(StringToJString(Name))
    else
      TJChartboost.JavaClass.showInterstitial(StringToJString(Name));
  end);
end;

function TJVEChartBoost.GetiOSAppId: String;
begin
  Result := FiOSAppId;
end;

function TJVEChartBoost.GetiOSAppSignature: String;
begin
  Result := FiOSAppSignature;
end;

procedure TJVEChartBoost.SetiOSAppId(const Value: String);
begin
  FiOSAppId := Value;
end;

procedure TJVEChartBoost.SetiOSAppSignature(const Value: String);
begin
  FiOSAppSignature := Value;
end;

function TJVEChartBoost.GetAndroidAppId: String;
begin
  Result := FAndroidAppId;
end;

function TJVEChartBoost.GetAndroidAppSignature: String;
begin
  Result := FAndroidAppSignature;
end;

procedure TJVEChartBoost.SetAndroidAppId(const Value: String);
begin
  if FAndroidAppId <> Value then
  begin
    FAndroidAppId := Value;
    if (FAndroidAppId <> '') and (FAndroidAppSignature <> '') then
      CallInUIThread(procedure
      begin
        TJChartboost.JavaClass.startWithAppId(GetSharedActivity,
          StringToJString(FAndroidAppId), StringToJString(FAndroidAppSignature));
        TJChartboost.JavaClass.setDelegate(TJChartBoostBase.Create);
      end);
  end;
end;

procedure TJVEChartBoost.SetAndroidAppSignature(const Value: String);
begin
  if FAndroidAppSignature <> Value then
  begin
    FAndroidAppSignature := Value;
    if (FAndroidAppId <> '') and (FAndroidAppSignature <> '') then
      CallInUIThread(procedure
      begin
        TJChartboost.JavaClass.startWithAppId(GetSharedActivity,
          StringToJString(FAndroidAppId), StringToJString(FAndroidAppSignature));
        TJChartboost.JavaClass.setDelegate(TJChartBoostBase.Create);
      end);
  end;
end;

var
  JNICopy: Boolean = False;

procedure ChartBoostNotReceived(PEnv: PJNIEnv; Self, JNILocation, JNIMessage: JNIObject); cdecl;
begin
  TMessageManager.DefaultManager.SendMessage(nil, TJVELocationErrorMessage.Create(
    UTF8ToString(PEnv^.GetStringUTFChars(PEnv, JNILocation, @JNICopy)),
    UTF8ToString(PEnv^.GetStringUTFChars(PEnv, JNIMessage, @JNICopy))));
end;

procedure ChartBoostClosed(PEnv: PJNIEnv; Self, JNILocation: JNIObject); cdecl;
begin
  TMessageManager.DefaultManager.SendMessage(nil, TJVELocationMessage.Create(
    UTF8ToString(PEnv^.GetStringUTFChars(PEnv, JNILocation, @JNICopy))));
end;

procedure ChartBoostReward(PEnv: PJNIEnv; Self, JNILocation: JNIObject; Reward: JNIInt); cdecl;
begin
  TMessageManager.DefaultManager.SendMessage(nil, TJVELocationRewardMessage.Create(
    UTF8ToString(PEnv^.GetStringUTFChars(PEnv, JNILocation, @JNICopy)), Reward));
end;

procedure RegisterBaseNativeMethods;
var
  PEnv: PJNIEnv;
  ChartBoostBaseClass: JNIClass;
  NativeMethod: JNINativeMethod;
begin
  ChartBoostBaseClass := TJNIResolver.GetJavaClassID('com/jvesoft/chartboost/ChartBoostBase');
  if ChartBoostBaseClass <> nil then
  begin
    PEnv := TJNIResolver.GetJNIEnv;
    NativeMethod.Name := 'ChartBoostNotReceived';
    NativeMethod.Signature := '(Ljava/lang/String;Ljava/lang/String;)V';
    NativeMethod.FnPtr := @ChartBoostNotReceived;
    PEnv^.RegisterNatives(PEnv, ChartBoostBaseClass, @NativeMethod, 1);

    NativeMethod.Name := 'ChartBoostClosed';
    NativeMethod.Signature := '(Ljava/lang/String;)V';
    NativeMethod.FnPtr := @ChartBoostClosed;
    PEnv^.RegisterNatives(PEnv, ChartBoostBaseClass, @NativeMethod, 1);

    NativeMethod.Name := 'ChartBoostReward';
    NativeMethod.Signature := '(Ljava/lang/String;I)V';
    NativeMethod.FnPtr := @ChartBoostReward;
    PEnv^.RegisterNatives(PEnv, ChartBoostBaseClass, @NativeMethod, 1);
    PEnv^.DeleteLocalRef(PEnv, ChartBoostBaseClass);
  end;
end;

procedure JVEChartBoostSetGDPRConsent(Behavioral: Boolean);
begin
  if Behavioral then
    TJChartboost.JavaClass.setPIDataUseConsent(GetSharedActivityContext,
      TJChartboost_CBPIDataUseConsent.JavaClass.YES_BEHAVIORAL)
  else
    TJChartboost.JavaClass.setPIDataUseConsent(GetSharedActivityContext,
      TJChartboost_CBPIDataUseConsent.JavaClass.NO_BEHAVIORAL);
end;

initialization
  RegisterBaseNativeMethods;
{$ELSE}

{ TJVEChartBoost }

function TJVEChartBoost.IsCached(Sender: TJVEAction = nil): Boolean;
begin
  Result := False;
end;

function TJVEChartBoost.GetiOSAppId: String;
begin
  Result := FiOSAppId;
end;

function TJVEChartBoost.GetiOSAppSignature: String;
begin
  Result := FiOSAppSignature;
end;

procedure TJVEChartBoost.SetiOSAppId(const Value: String);
begin
  FiOSAppId := Value;
end;

procedure TJVEChartBoost.SetiOSAppSignature(const Value: String);
begin
  FiOSAppSignature := Value;
end;

function TJVEChartBoost.GetAndroidAppId: String;
begin
  Result := FAndroidAppId;
end;

function TJVEChartBoost.GetAndroidAppSignature: String;
begin
  Result := FAndroidAppSignature;
end;

procedure TJVEChartBoost.SetAndroidAppId(const Value: String);
begin
  FAndroidAppId := Value;
end;

procedure TJVEChartBoost.SetAndroidAppSignature(const Value: String);
begin
  FAndroidAppSignature := Value;
end;

procedure TJVEChartBoost.Cache(Sender: TJVEAction = nil);
begin
end;

procedure TJVEChartBoost.Show(Sender: TJVEAction = nil);
begin
end;

procedure JVEChartBoostSetGDPRConsent(Behavioral: Boolean);
begin
end;

{$ENDIF}

end.

