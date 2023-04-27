(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.MobFox;

interface

uses System.Classes, System.SysUtils, System.Sensors, System.Generics.Collections, FMX.Dialogs,
  System.JSON, JVE.Actions, JVE.OpenURL {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF}
  {$IF (CompilerVersion >= 27) and Defined(ANDROID)}, AndroidApi.Helpers{$ENDIF}, JVE.Utils;

// This component integrates native ads interface with MobFox services.
// This Ad serving API is only available for mobile, so right now this works
// only on mobile devices and iOS Simulator.
//
// To use this component you need an Inventory Hash. Go to the MobFox site:
// www.mobfox.com, register an account there and create an app therein. That app
// will have an Inventory Hash, which should be provided here. See therein for more data.
//
// This class does NOT collect analytics data, as all the analysis is available
// through the MobFox services.
// Using this component with the TJVENativeAd is suggested.
// The default inventory hashes provided by this component are the MobFox demo values.
type
  TJVEMobFox = class;
  TJVEMobFoxGender = (mgUnknown, mgMale, mgFemale);
  TJVEMobFoxAdEvent = procedure(Sender: TJVEMobFox; Item: TJVEAppDetails) of object;
  TJVEMobFoxErrorEvent = procedure(Sender: TJVEMobFox; Error: String) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEMobFox = class(TComponent, IJVEManagedExecutable, IJVEExecutable)
  private
    FCurrentRequests: TList<TJVEAction>;
    FRequestsCache: TDictionary<TJVEAction,TJVEAppDetails>;
    FiOSInventoryHash: String;
    FAndroidInventoryHash: String;
    FUserAge: Integer;
    FUserGender: TJVEMobFoxGender;
    FUserKeywords: TStrings;
    FUserLocation: TLocationCoord2D;
    FOnAdAvailable: TJVEMobFoxAdEvent;
    FOnError: TJVEMobFoxErrorEvent;
    function IsiOSInventoryHashStored: Boolean;
    function IsAndroidInventoryHashStored: Boolean;
    procedure SetiOSInventoryHash(const Value: String);
    procedure SetAndroidInventoryHash(const Value: String);
    procedure SetUserAge(const Value: Integer);
    procedure SetUserGender(const Value: TJVEMobFoxGender);
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
    // OnError is called instead).
    procedure RequestAd(Done: TProc<TJVEAppDetails> = nil);
  published
    // These are the application inventory hashes, provided by the MobFox site.
    property iOSInventoryHash: String read FiOSInventoryHash
      write SetiOSInventoryHash stored IsiOSInventoryHashStored;
    property AndroidInventoryHash: String read FAndroidInventoryHash
      write SetAndroidInventoryHash stored IsAndroidInventoryHashStored;

    // All remaining properties are optional. They provide details about the user,
    // which will make the ad more appropriate (thus theoretically resulting in a
    // higher CPM).
    // Set these properties if you have some expectation of who the user will be
    // or if you know these from some questionnaire.
    property UserGender: TJVEMobFoxGender read FUserGender
      write SetUserGender default mgUnknown;
    property UserAge: Integer read FUserAge write SetUserAge default 0;
    property UserKeywords: TStrings read FUserKeywords write SetUserKeywords;

    // Called when an ad is available (unless a Done callback is provided in a
    // RequestAd method
    property OnAdAvailable: TJVEMobFoxAdEvent read FOnAdAvailable write FOnAdAvailable;
    property OnError: TJVEMobFoxErrorEvent read FOnError write FOnError;
  public
    // The user location should normally only be set, if you are using GPS for
    // something more usefull as well.
    property UserLocation: TLocationCoord2D read FUserLocation write SetUserLocation;
  end;

implementation

uses System.Math, System.StrUtils, FMX.Types
  {$IF Defined(ANDROID)}, Androidapi.JNI.AdMob, Androidapi.JNI.Os, Androidapi.JNI.Util,
  Androidapi.JNI.JavaTypes, Androidapi.JNI.GraphicsContentViewText, AndroidApi.JNIBridge,
  FMX.Helpers.Android
  {$ELSEIF Defined(IOS)}, Posix.Dlfcn, iOSapi.Foundation, iOSapi.UIKit,
  Macapi.ObjectiveC, iOSapi.CoreGraphics {$ENDIF};

const
  SandboxInventoryHash = 'a764347547748896b84e0b8ccd90fd62';

{$IF Defined(IOS)}

type
  ASIdentifierManager = interface(NSObject)
    function advertisingIdentifier: NSUUID; cdecl;
    function isAdvertisingTrackingEnabled: Boolean; cdecl;
  end;
  ASIdentifierManagerClass = interface(NSObjectClass)
    function sharedManager: ASIdentifierManager; cdecl;
  end;
  TASIdentifierManager = class(TOCGenericImport<ASIdentifierManagerClass, ASIdentifierManager>) end;

var
  UserAgentRegular, UserAgentEncoded: String;

procedure CreateUserAgent; inline;
begin
  if UserAgentRegular = '' then
  begin
    UserAgentRegular := 'Mozilla/5.0 (iPhone; CPU iPhone OS ' +
      IntToStr(TOSVersion.Major) + '_' + IntToStr(TOSVersion.Minor) +
      ' like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/' +
      IntToStr(TOSVersion.Major) + '.' + IntToStr(TOSVersion.Minor);
    UserAgentEncoded := TJVEOpenURL.URLEncode(UserAgentRegular);
  end;
end;

{$ELSEIF Defined(ANDROID)}

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

type
  [JavaSignature('java/lang/System')]
  JSystem = interface(JObject)
  ['{A1046CA2-6B5C-446A-878F-5CB0B91E29A1}']
  end;

  JSystemClass = interface(JObjectClass)
  ['{2236D0E6-0544-444C-8707-E4E91AF278BA}']
    {Property Methods}
    function _Geterr: JPrintStream;
    {Methods}
    function getProperty(const name: JString): JString; overload;
    function getProperty(const name, defaultValue: JString): JString; overload;
    {Properties}
    property err: JPrintStream read _Geterr;
  end;

  TJSystem = class(TJavaGenericImport<JSystemClass, JSystem>) end;

var
  UserAgentRegular, UserAgentEncoded: String;
  AlternativeAgentString: String;
  AdIdAccess: JAdvertisingIdClient_Info;

procedure CreateUserAgent; inline;
begin
  if UserAgentRegular = '' then
  begin
    UserAgentRegular := JStringToString(TJSystem.JavaClass.
      getProperty(StringToJString('http.agent'), StringToJString(Format(
      'Mozilla/5.0 (Linux; Android %d.%d)', [TOSVersion.Major, TOSVersion.Minor]))));
    UserAgentEncoded := TJVEOpenURL.URLEncode(UserAgentRegular);

    AlternativeAgentString := JStringToString(TJLocale.JavaClass.getDefault.getLanguage) +
      '-' + JStringToString(TJLocale.JavaClass.getDefault.getCountry);
    if (Pos('-', AlternativeAgentString) = 1) or
      (Pos('-', AlternativeAgentString) = Length(AlternativeAgentString)) then
        AlternativeAgentString := 'en-us';

    AlternativeAgentString := TJVEOpenURL.URLEncode(Format(
      'Mozilla/5.0 (Linux; U; Android %d.%d; %s; %s Build/%s) ' +
      'AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.1',
      [TOSVersion.Major, TOSVersion.Minor,
      AlternativeAgentString,
      JStringToString(TJBuild.JavaClass.MODEL),
      JStringToString(TJBuild.JavaClass.ID)]));
  end;
end;

{$ENDIF}

{ TJVERevMob }

constructor TJVEMobFox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAndroidInventoryHash := SandboxInventoryHash;
  FiOSInventoryHash := SandboxInventoryHash;
  FCurrentRequests := TList<TJVEAction>.Create;
  FRequestsCache := TDictionary<TJVEAction,TJVEAppDetails>.Create;
  FUserKeywords := TStringList.Create;
end;

destructor TJVEMobFox.Destroy;
begin
  FreeAndNil(FCurrentRequests);
  FreeAndNil(FRequestsCache);
  FreeAndNil(FUserKeywords);
  inherited Destroy;
end;

function TJVEMobFox.IsAndroidInventoryHashStored: Boolean;
begin
  Result := FAndroidInventoryHash <> SandboxInventoryHash;
end;

function TJVEMobFox.IsiOSInventoryHashStored: Boolean;
begin
  Result := FiOSInventoryHash <> SandboxInventoryHash;
end;

procedure TJVEMobFox.SetUserKeywords(const Value: TStrings);
begin
  FUserKeywords.Assign(Value);
end;

procedure TJVEMobFox.SetiOSInventoryHash(const Value: String);
begin
  FiOSInventoryHash := Value;
end;

procedure TJVEMobFox.SetAndroidInventoryHash(const Value: String);
begin
  FAndroidInventoryHash := Value;
end;

procedure TJVEMobFox.SetUserAge(const Value: Integer);
begin
  FUserAge := Value;
end;

procedure TJVEMobFox.SetUserGender(const Value: TJVEMobFoxGender);
begin
  FUserGender := Value;
end;

procedure TJVEMobFox.SetUserLocation(const Value: TLocationCoord2D);
begin
  FUserLocation := Value;
end;

procedure TJVEMobFox.RequestAd(Done: TProc<TJVEAppDetails>);
begin
{$IF Defined(IOS) or Defined(ANDROID)}
  CreateUserAgent;
  DetachThread(procedure(Synchronize: TProc<TThreadProcedure>)
    function GetURLSuffix: String;
{$IFDEF IOS}
    begin
      if TASIdentifierManager.OCClass.sharedManager.isAdvertisingTrackingEnabled then
        Result := '&o_iosadvid=' + FromNSSTR(TASIdentifierManager.OCClass.
          sharedManager.advertisingIdentifier.UUIDString);
{$ELSE}
    begin
      if AdIdAccess = nil then
        AdIdAccess := TJAdvertisingIdClient.JavaClass.getAdvertisingIdInfo(GetSharedActivityContext);
      Result := Format('&o_andadvid=%s&o_andadvdnt=%d',
        [JStringToString(AdIdAccess.getId), IfThen(AdIdAccess.isLimitAdTrackingEnabled, 1, 0)]);
{$ENDIF}

      if (UserLocation.Latitude <> 0) or (UserLocation.Longitude <> 0) then
        Result := Result + Format('&latitude=%.6f&longitude=%.6f',
          [UserLocation.Latitude, UserLocation.Longitude]);
      if UserGender <> mgUnknown then
        Result := Result + IfThen(UserGender = mgMale, '&demo_gender=m', '&demo_gender=f');
      if UserAge <> 0 then
        Result := Result + '&demo_age=' + IntToStr(UserAge);
      if UserKeywords.Count <> 0 then
        Result := Result + '&demo_keywords=' + TJVEOpenURL.URLEncode(UserKeywords.CommaText);
    end;
    function GetJSONString(JSON: TJSONValue; Key: String): String;
    var
      Value: TJSONValue;
    begin
      if JSON.TryGetValue(Key, Value) then
        Result := TJSONString(Value).Value
      else
        Result := '';
    end;
  var
    App, ResultApp: TJVEAppDetails;
    Error, Asset: String;
    JSON: TJSONObject;
    JSONList: TJSONValue;
    Reply: TMemoryStream;
    StrStream: TStringStream;
    Headers: TStringList;
  {$IFDEF IOS}
    Device: UIDevice;
  {$ENDIF}
  begin
    Error := '';
    ResultApp := nil;
    App := nil;
    Reply := nil;
    StrStream := nil;
    try
      if {$IFDEF IOS} FiOSInventoryHash {$ELSE} FAndroidInventoryHash {$ENDIF} = '' then
      begin
        Error := 'Missing Inventory Hash';
        Exit;
      end;

      Headers := TStringList.Create;
      Headers.Values['User-Agent'] := UserAgentRegular;
      {$IFDEF IOS}
      Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
      {$ENDIF}

      Reply := TJVEOpenURL.DownloadURL(Format(
        'https://my.mobfox.com/request.php' +
        '?r_type=native' +
        '&r_resp=json' +
        '&n_img=icon,main' +
        '&n_txt=headline,description,cta,advertiser,rating' +
        '&n_ver=1.1' +
        '&u=%s' +
        '&u_wv=%s' +
        '&u_br=%s' +
        '&v=6.2.2' +
        '&s=%s' +
        '&r_random=%d%s' +
      {$IFDEF IOS}
        '&rt=%s' +
        '&iphone_osversion=%s',
        [UserAgentEncoded, UserAgentEncoded,
        UserAgentEncoded + '%20Version%2Funknown%20Safari%2Funknown',
        FiOSInventoryHash,
        Random(50000),
        GetURLSuffix,
        IfThen(Device.userInterfaceIdiom = UIUserInterfaceIdiomPhone, 'iphone_app', 'ipad_app'),
        TJVEOpenURL.URLEncode(FromNSSTR(Device.systemVersion))]), Headers, Error);
      {$ELSE}
        '&rt=android_app' +
        '&u2=%s',
        [UserAgentEncoded, UserAgentEncoded, UserAgentEncoded,
        FAndroidInventoryHash,
        Random(50000),
        GetURLSuffix,
        AlternativeAgentString]), Headers, Error);
      {$ENDIF}

      try
        if Error <> '' then
          Exit;

        if Reply.Size < 5 then
        begin
          Error := 'No ad to show';
          Exit;
        end;

        StrStream := TStringStream.Create('', TEncoding.UTF8);
        StrStream.LoadFromStream(Reply);
        FreeAndNil(Reply);

        if StrStream.DataString.StartsWith('{"error"') then
        begin
          Error := StrStream.DataString.Substring(9);
          Exit;
        end;
        JSON := TJSONObject(TJSONObject.ParseJSONValue(StrStream.DataString));
        FreeAndNil(StrStream);

        App := TJVEAppDetails.Create(nil);
        App.BeaconHeaders.Assign(Headers);
        Headers := nil;

        JSON := JSON.GetValue<TJSONObject>('native');
        App.URL := GetJSONString(JSON, 'link.url');

        App.Beacons.Clear;
        for JSONList in JSON.GetValue<TJSONArray>('imptrackers') do
          App.Beacons.Add(TJSONString(JSONList).Value);

        for JSONList in JSON.GetValue<TJSONArray>('assets') do
        begin
          Asset := GetJSONString(JSONList, 'type');
          if Asset = 'icon' then
            App.Icon.Download(GetJSONString(JSONList, 'img.url'))
          else if Asset = 'main' then
            App.Bitmap.Download(GetJSONString(JSONList, 'img.url'))
          else if Asset = 'title' then
            App.Title := GetJSONString(JSONList, 'title.text')
          else if Asset = 'desc' then
            App.Description := GetJSONString(JSONList, 'data.value')
          else if Asset = 'ctatext' then
            App.CallToAction := GetJSONString(JSONList, 'data.value')
          else if Asset = 'sponsored' then
            App.Advertiser := GetJSONString(JSONList, 'data.value');
        end;
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
        if (ResultApp = nil) and Assigned(Done) then
          Done(nil);
        if (Error <> '') and Assigned(FOnError) then
          FOnError(Self, Error);

        FreeAndNil(StrStream);
        FreeAndNil(Reply);
        FreeAndNil(App);
        FreeAndNil(ResultApp);
        FreeAndNil(Headers);
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

procedure TJVEMobFox.Open(Sender: TJVEAction);
begin
  if Sender.AppDetails.URL <> '' then
    TJVEOpenURL.OpenURL(Sender.AppDetails.URL);
end;

function TJVEMobFox.CanOpen(Sender: TJVEAction): Boolean;
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

