(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.PubNative;

interface

uses System.Classes, System.SysUtils, System.Sensors, System.Generics.Collections, JVE.Utils,
  System.JSON, JVE.Actions, JVE.OpenURL {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

// This component integrates native ads interface with PubNative services.
// This Ad serving API is only available for mobile, so right now this works
// only on mobile devices and iOS Simulator.
//
// To use this component you need an App Token. Go to the pubnative site:
// www.pubnative.com, register an account there and create an app therein. That app
// will have an App Token, which should be provided here. See therein for more data.
//
// This class does NOT collect analytics data, as all the analysis is available
// through the PubNative services.
// Using this component with the TJVENativeAd is suggested.
type
  TJVEPubNative = class;
  TJVEPubNativeGender = (pgUnknown, pgMale, pgFemale);
  TJVEPubNativeAdEvent = procedure(Sender: TJVEPubNative; Item: TJVEAppDetails) of object;
  TJVEPubNativeErrorEvent = procedure(Sender: TJVEPubNative; Error: String) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEPubNative = class(TComponent, IJVEManagedExecutable, IJVEExecutable)
  private
    FCurrentRequests: TList<TJVEAction>;
    FRequestsCache: TDictionary<TJVEAction,TJVEAppDetails>;
    FiOSAppToken: String;
    FAndroidAppToken: String;
    FUserAge: Integer;
    FUserGender: TJVEPubNativeGender;
    FUserKeywords: TStrings;
    FUserLocation: TLocationCoord2D;
    FOnAdAvailable: TJVEPubNativeAdEvent;
    FOnError: TJVEPubNativeErrorEvent;
    procedure SetiOSAppToken(const Value: String);
    procedure SetAndroidAppToken(const Value: String);
    procedure SetUserAge(const Value: Integer);
    procedure SetUserGender(const Value: TJVEPubNativeGender);
    procedure SetUserKeywords(const Value: TStrings);
    procedure SetUserLocation(const Value: TLocationCoord2D);
  protected
    // Performs the action specified by the action's ad (i.e. opens the URL).
    procedure Open(Sender: TJVEAction);
    // Starts a request for an ad. If a requested ad is available, updates
    // the action, returns True and starts a request for the next ad.
    // Only updates the action, if the current action's ad was already presented.
    function CanOpen(Sender: TJVEAction): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // This function requests an ad from the server. Once the ad is ready,
    // the Done callback is called, passing the ad as the parameter (or nil,
    // if the ad was not available).
    // If the parameter Done is not provided, then the OnAdAvailable event
    // is called with an ad (it is not called if an ad is not available;
    // OnError is called instead.
    procedure RequestAd(Done: TProc<TJVEAppDetails> = nil);
  published
    // These are the application tokens, provided by the PubNative site.
    property iOSAppToken: String read FiOSAppToken write SetiOSAppToken;
    property AndroidAppToken: String read FAndroidAppToken write SetAndroidAppToken;

    // All remaining properties are optional. They provide details about the user,
    // which will make the ad more appropriate (thus theoretically resulting in a
    // higher CPM).
    // Set these properties if you have some expectation of who the user will be
    // or if you know these from some questionnaire.
    property UserGender: TJVEPubNativeGender read FUserGender
      write SetUserGender default pgUnknown;
    property UserAge: Integer read FUserAge write SetUserAge default 0;
    property UserKeywords: TStrings read FUserKeywords write SetUserKeywords;

    // Called when an ad is available (unless a Done callback is provided in a
    // RequestAd method
    property OnAdAvailable: TJVEPubNativeAdEvent read FOnAdAvailable write FOnAdAvailable;
    property OnError: TJVEPubNativeErrorEvent read FOnError write FOnError;
  public
    // The user location should normally only be set, if you are using GPS for
    // something more usefull as well.
    property UserLocation: TLocationCoord2D read FUserLocation write SetUserLocation;
  end;

implementation

uses System.Math, System.StrUtils
  {$IF Defined(IOS)}, Posix.Dlfcn, iOSapi.Foundation, iOSapi.UIKit, Macapi.ObjectiveC
  {$ELSEIF Defined(ANDROID)}, Androidapi.Helpers, Androidapi.JNI.AdMob,
    Androidapi.JNI.Os, Androidapi.JNI.Util, Androidapi.JNI.JavaTypes,
    Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge
  {$ENDIF};

{$IF Defined(ANDROID)}

{$IF CompilerVersion >= 33}
type
  [JavaSignature('com/google/android/gms/ads/identifier/AdvertisingIdClient$Info')]
  JAdvertisingIdClient_Info = interface(JObject)
    ['{A374133F-B45F-4FBE-959D-ABF8C9265F50}']
    function getId: JString; cdecl;
    function isLimitAdTrackingEnabled: Boolean; cdecl;
  end;

  JAdvertisingIdClient_InfoClass = interface(JObjectClass)
    ['{AEB70444-348C-4CA8-B655-97E55204C286}']
    {class} function init(advertisingId: JString; limitAdTrackingEnabled: Boolean): JAdvertisingIdClient_Info; cdecl;
  end;
  TJAdvertisingIdClient_Info = class(TJavaGenericImport<JAdvertisingIdClient_InfoClass, JAdvertisingIdClient_Info>) end;

  [JavaSignature('com/google/android/gms/ads/identifier/AdvertisingIdClient')]
  JAdvertisingIdClient = interface(JObject)
    ['{BE695BA2-4967-46DA-AAF8-E16C09394769}']
  end;

  JAdvertisingIdClientClass = interface(JObjectClass)
    ['{EBCEADC5-14A2-4E4C-A048-993746356F8E}']
    {class} function getAdvertisingIdInfo(context: JContext): JAdvertisingIdClient_Info; cdecl;
    {class} function init: JAdvertisingIdClient; cdecl;
  end;
  TJAdvertisingIdClient = class(TJavaGenericImport<JAdvertisingIdClientClass, JAdvertisingIdClient>) end;
{$ENDIF}

var
  AdIdAccess: JAdvertisingIdClient_Info;

{$ELSEIF Defined(IOS)}

type
  ASIdentifierManager = interface(NSObject)
    function advertisingIdentifier: NSUUID; cdecl;
    function isAdvertisingTrackingEnabled: Boolean; cdecl;
  end;
  ASIdentifierManagerClass = interface(NSObjectClass)
    function sharedManager: ASIdentifierManager; cdecl;
  end;
  TASIdentifierManager = class(TOCGenericImport<ASIdentifierManagerClass, ASIdentifierManager>) end;

{$ENDIF}

{ TJVERevMob }

constructor TJVEPubNative.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentRequests := TList<TJVEAction>.Create;
  FRequestsCache := TDictionary<TJVEAction,TJVEAppDetails>.Create;
  FUserKeywords := TStringList.Create;
end;

destructor TJVEPubNative.Destroy;
begin
  FreeAndNil(FCurrentRequests);
  FreeAndNil(FRequestsCache);
  FreeAndNil(FUserKeywords);
  inherited Destroy;
end;

procedure TJVEPubNative.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords.Assign(Value);
end;

procedure TJVEPubNative.SetiOSAppToken(const Value: String);
begin
  FiOSAppToken := Value;
end;

procedure TJVEPubNative.SetAndroidAppToken(const Value: String);
begin
  FAndroidAppToken := Value;
end;

procedure TJVEPubNative.SetUserAge(const Value: Integer);
begin
  FUserAge := Value;
end;

procedure TJVEPubNative.SetUserGender(const Value: TJVEPubNativeGender);
begin
  FUserGender := Value;
end;

procedure TJVEPubNative.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation := Value;
end;

procedure TJVEPubNative.RequestAd(Done: TProc<TJVEAppDetails>);
begin
{$IF Defined(ANDROID)}
  if AdIdAccess = nil then
    AdIdAccess := TJAdvertisingIdClient.JavaClass.getAdvertisingIdInfo(GetSharedActivityContext);
{$ENDIF}

{$IF Defined(IOS) or Defined(ANDROID)}
  DetachThread(procedure(Synchronize: TProc<TThreadProcedure>)
    function GetURLSuffix: String;
{$IFDEF IOS}
    begin
      if TASIdentifierManager.OCClass.sharedManager.isAdvertisingTrackingEnabled then
        Result := 'apple_idfa=' + FromNSSTR(TASIdentifierManager.OCClass.
          sharedManager.advertisingIdentifier.UUIDString)
{$ELSE}
    begin
      if not AdIdAccess.isLimitAdTrackingEnabled then
        Result := 'android_advertiser_id=' + JStringToString(AdIdAccess.getId)
{$ENDIF}
      else
        Result := 'no_user_id=1';

      if (UserLocation.Latitude <> 0) or (UserLocation.Longitude <> 0) then
        Result := Result + Format('&lat=%.6f&long=%.6f', [UserLocation.Latitude, UserLocation.Longitude]);
      if UserGender <> pgUnknown then
        Result := Result + IfThen(UserGender = pgMale, '&gender=male', '&gender=female');
      if UserAge <> 0 then
        Result := Result + '&age=' + IntToStr(UserAge);
      if UserKeywords.Count <> 0 then
        Result := Result + '&keywords=' + TJVEOpenURL.URLEncode(UserKeywords.CommaText);
    end;
    function GetJSONString(JSON: TJSONObject; Key: String): String;
    var
      Value: TJSONValue;
    begin
      Value := JSON.GetValue(Key);
      if Value = nil then
        Result := ''
      else
        Result := TJSONString(Value).Value;
    end;
  var
    App, ResultApp: TJVEAppDetails;
    Error, URLScheme: String;
    JSON, JSONApp: TJSONObject;
    JSONBeacon: TJSONValue;
    Reply: TMemoryStream;
    StrStream: TStringStream;
  {$IFDEF IOS}
    Device: UIDevice;
    Screen: UIScreen;
  {$ELSE}
    Metrics: JDisplayMetrics;
  {$ENDIF}
  begin
    Error := '';
    ResultApp := nil;
    App := nil;
    Reply := nil;
    StrStream := nil;
    try
      if {$IFDEF IOS} FiOSAppToken {$ELSE} FAndroidAppToken {$ENDIF} = '' then
      begin
        Error := 'Missing App Token';
        Exit;
      end;

      {$IFDEF IOS}
      Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
      Screen := TUIScreen.Wrap(TUIScreen.OCClass.mainScreen);
      {$ELSE}
      Metrics := GetSharedActivityContext.getResources.getDisplayMetrics;
      {$ENDIF}

      Reply := TJVEOpenURL.DownloadURL(Format(
        'https://api.pubnative.net/api/partner/v2/promotions/native' +
        '?app_token=%s' +
        '&bundle_id=%s' +
        '&os_version=%s' +
        '&device_model=%s' +
        '&device_type=%s' +
        '&device_resolution=%dx%d' +
        '&locale=%s' +
        '&icon_size=80x80' +
        '&banner_size=1200x627' +
        '&ad_count=1&%s' +
      {$IFDEF IOS}
        '&os=ios',
        [FiOSAppToken,
        TJVEOpenURL.URLEncode(FromNSSTR(TNSBundle.Wrap(TNSBundle.OCClass.MainBundle).bundleIdentifier)),
        TJVEOpenURL.URLEncode(FromNSSTR(Device.systemVersion)),
        TJVEOpenURL.URLEncode(FromNSSTR(Device.model)),
        IfThen(Device.userInterfaceIdiom = UIUserInterfaceIdiomPhone, 'phone', 'tablet'),
        Round(Screen.bounds.size.width * Screen.scale), Round(Screen.bounds.size.width * Screen.scale),
        TJVEOpenURL.URLEncode(FromNSSTR(TNSLocale.Wrap(TNSLocale.OCClass.currentLocale).objectForKey(PointerNSObject(NSLocaleLanguageCode)))),
      {$ELSE}
        '&os=android',
        [FAndroidAppToken,
        TJVEOpenURL.URLEncode(JStringToString(GetSharedActivityContext.getPackageName)),
        IntToStr(TOSVersion.Major) + '.' + IntToStr(TOSVersion.Minor),
        JStringToString(TJBuild.JavaClass.MODEL),
        IfThen(Min(Metrics.widthPixels, Metrics.heightPixels) / Metrics.density < 600, 'phone', 'tablet'),
        Metrics.widthPixels, Metrics.heightPixels,
        TJVEOpenURL.URLEncode(JStringToString(TJLocale.JavaClass.getDefault.getLanguage)),
      {$ENDIF}
        GetURLSuffix]), Error);

      try
        if Error <> '' then
          Exit;

        StrStream := TStringStream.Create('', TEncoding.UTF8);
        StrStream.LoadFromStream(Reply);
        FreeAndNil(Reply);
        JSON := TJSONObject(TJSONObject.ParseJSONValue(StrStream.DataString));
        FreeAndNil(StrStream);

        Error := GetJSONString(JSON, 'status');
        if Error = 'ok' then
          Error := ''
        else
          Exit;

        if JSON.GetValue<TJSONArray>('ads').Count = 0 then
        begin
          Error := 'No ad to show';
          Exit;
        end;
        JSON := TJSONObject(JSON.GetValue<TJSONArray>('ads').Items[0]);

        App := TJVEAppDetails.Create(nil);
        App.URL := GetJSONString(JSON, 'click_url');
        App.Title := GetJSONString(JSON, 'title');
        App.Description := GetJSONString(JSON, 'description');
        App.CallToAction := GetJSONString(JSON, 'cta_text');

        JSONApp := TJSONObject(JSON.GetValue('app_details'));
        if JSONApp <> nil then
        begin
          App.Advertiser := GetJSONString(JSONApp, 'publisher');
          App.Rating := JSONApp.GetValue<TJSONNumber>('store_rating').AsDouble;

          {$IFDEF IOS}
          URLScheme := GetJSONString(JSONApp, 'url_scheme');
          if URLScheme <> '' then
            URLScheme := IfThen(TJVEOpenURL.CanOpenURL(URLScheme), '&installed=1', '');
          {$ELSE}
          URLScheme := GetJSONString(JSONApp, 'store_id');
          if URLScheme <> '' then
            try
              GetSharedActivityContext.getPackageManager.getPackageInfo(
                StringToJString(URLScheme), TJPackageManager.JavaClass.GET_ACTIVITIES);
              URLScheme := '&installed=1';
            except
              URLScheme := '';
            end;
          {$ENDIF}
        end else
        begin
          URLScheme := '';
          if JSON.GetValue('store_rating') <> nil then
            App.Rating := JSON.GetValue<TJSONNumber>('store_rating').AsDouble
          else
            App.Rating := 5;
        end;

        App.Beacons.Clear;
        for JSONBeacon in JSON.GetValue<TJSONArray>('beacons') do
          App.Beacons.Add(GetJSONString(TJSONObject(JSONBeacon), 'url') + URLScheme);

        if URLScheme <> '' then
        begin
          // The app is already installed. Notify the server and retry the call.
          App.Presented(False);
          Error := '###';
        end;

        App.Icon.Download(GetJSONString(JSON, 'icon_url'), Error);
        if Error <> '' then
          Exit;

        App.Bitmap.Download(GetJSONString(JSON, 'banner_url'), Error);
        if Error <> '' then
          Exit;
      except
        on E: Exception do
          Error := E.Message;
      end;

      ResultApp := App;
      App := nil;
      Synchronize(procedure
      begin
        if Assigned(Done) then
          Done(ResultApp)
        else if Assigned(FOnAdAvailable) then
          FOnAdAvailable(Self, ResultApp)
        else
          ResultApp := nil;
      end);
    finally
      Synchronize(procedure
      begin
        if Error <> '###' then
        begin
          if (ResultApp = nil) and Assigned(Done) then
            Done(nil);
          if (Error <> '') and Assigned(FOnError) then
            FOnError(Self, Error);
        end else
          RequestAd(Done); // The app is already installed: silently retry the call.

        FreeAndNil(StrStream);
        FreeAndNil(Reply);
        FreeAndNil(App);
        FreeAndNil(ResultApp);
      end);
    end;
  end);
{$ELSE}
  if Assigned(Done) then
    Done(nil)
  else if Assigned(FOnError) then
    FOnError(Self, 'Not supported on this platform');
{$ENDIF}
end;

procedure TJVEPubNative.Open(Sender: TJVEAction);
begin
  if Sender.AppDetails.URL <> '' then
    TJVEOpenURL.OpenURL(Sender.AppDetails.URL);
end;

function TJVEPubNative.CanOpen(Sender: TJVEAction): Boolean;
  procedure CheckRequest;
  begin
    if FCurrentRequests.Contains(Sender) then
      Exit;

    FCurrentRequests.Add(Sender);
    RequestAd(procedure(App: TJVEAppDetails)
      begin
        FCurrentRequests.Remove(Sender);
        if App <> nil then
          FRequestsCache.AddOrSetValue(Sender, App);
      end);
  end;
begin
  if (Sender.AppDetails.URL <> '') and not Sender.WasPresented then
  begin
    CheckRequest;
    Exit(True);
  end;

  if FRequestsCache.ContainsKey(Sender) then
  begin
    Sender.AppDetails.Assign(FRequestsCache[Sender]);
    Sender.WasPresented := False;
    FRequestsCache.Remove(Sender);
    CheckRequest;
    Exit(True);
  end;

  CheckRequest;
  Result := False;
end;

{$IF Defined(IOS) and Defined(CPUX86)}
var
  Module: THandle;
initialization
  Module := dlopen(MarshaledAString(
    '/System/Library/Frameworks/AdSupport.framework/AdSupport'), RTLD_LAZY);
finalization
  dlclose(Module);
{$ENDIF}
end.

