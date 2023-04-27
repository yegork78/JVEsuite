(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Pushes;

interface

uses System.SysUtils, System.Classes, System.UITypes, FMX.Types, FMX.Platform,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  FMX.Dialogs, JVE.Messaging, JVE.Utils, System.StrUtils;

// Push notifications support, provided below, does not work in Windows or in
// Simulator (i.e. only Android and iOS devices and Mac OS X 10.7 or later).
// Push notifications are singleton by design. Care should be taken to only
// ever have one instance of TJVEPushNotification (or descendant thereof)
// ever created in the app (otherwise it shouldn't fail, but might not work
// exactly as expected).
//
// Making Push Notifications work, in those platforms, is actually much more
// complicated than it would seem (or, probably, more than it should have been).
// This class provides operating system integration, but not the communication
// with your server-side.
// A complete implementation (which could also be used as an example of server
// integration) is provided in class TJVEPushWoosh for the PushWoosh service.
//
// The following steps are generally required to make Push Notifications work
// (notice, it is not a complete description):
// * Create an App ID for the app (you cannot use wildcard App ID).
// * Configure Push Notifications for the App ID, download the certificate
//   and update your server. You can see explicit details, for example, here:
//   http://www.pushwoosh.com/programming-push-notification/iphone-configuration-guide/
// * Create a provisioning profile for the app (wildcard might not work).
// * Build your app with the Push Notifications entitlement (iOS and Android).
type
  TJVEPushNotificationType = (ntBadge, ntSound, ntAlert, ntNewsstand);
  TJVEPushNotificationTypes = set of TJVEPushNotificationType;
  TJVEPushNotificationPlatform = (npiOS, npMac, npAndroid);
  TJVEPushNotificationPlatforms = set of TJVEPushNotificationPlatform;
  TJVEPushNotification = class;

  // This is the push notification data. Use these functions to get the standard
  // payload; specific implementations (like PushWoosh) might add a class helper
  // with further getters.
  TJVEPushData = class(TStringList)
    function GetMessage: String;
    function GetBadge: Integer;
    function GetSound: String;
    function GetIsNewsstand: Boolean;
  end;

  // This event is called when Operating System accepted the request and push
  // notifications can be sent to this device (the Token is provided).
  // You can use this event to send the Token to your server.
  TJVEPushToken = procedure(Sender: TJVEPushNotification; Token: String) of object;
  // Operating System refused tokens; the Error parameter is the message;
  // set DefaultProc to False if you don't want this error presented on screen.
  TJVEPushError = procedure(Sender: TJVEPushNotification; Error: String;
    var DefaultProc: Boolean) of object;
  // Push notification was just received; set DefaultProc to False, if you
  // don't want the component to display this message.
  // OnAppStart is True, if the app was started as a result of the user clicking
  // the push notification; otherwise the notification was received while the
  // app was working.
  TJVEPushReceived = procedure(Sender: TJVEPushNotification; Push: TJVEPushData;
    OnAppStart: Boolean; var DefaultProc: Boolean) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEPushNotification = class(TComponent)
  private
    FTypes: TJVEPushNotificationTypes;
    FPlatforms: TJVEPushNotificationPlatforms;
    FClearBadgeOnStart: Boolean;
    FOnToken: TJVEPushToken;
    FOnError: TJVEPushError;
    FOnReceived: TJVEPushReceived;
    FAndroidSenderId: String;
    FFCMConversionApiKey: String;
    FFCMConversionIsSandbox: Boolean;
    class var [Weak]FGlobalInstance: TJVEPushNotification;
    procedure SetTypes(const Value: TJVEPushNotificationTypes);
    class function GetGlobalInstance: TJVEPushNotification; static;
  protected
    // The first method here is the main method to override; use it to send
    // the Token to your server.
    // These methods duplicate the events and provide the basic functionality.
    procedure DoToken(Token: String); virtual;
    procedure DoError(Error: String; Report: Boolean); virtual;
    procedure DoReceived(Push: TJVEPushData; OnAppStart: Boolean); virtual;
    procedure PresentPush(Push: TJVEPushData); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // This is the global singleton instance of this class (or its descendants).
    class property GlobalInstance: TJVEPushNotification read GetGlobalInstance;
  published
    // Leave on True to have the component Clear the budges on the app icon on start.
    // Set to False to prevent the component from doing that.
    property ClearBadgeOnStart: Boolean read FClearBadgeOnStart
      write FClearBadgeOnStart default True;

    // The types of notifications, permissions for which are to be requested
    // from the Operating System.
    property Types: TJVEPushNotificationTypes read FTypes write SetTypes
      default [ntBadge, ntSound, ntAlert];

    // The types of notifications, permissions for which are to be requested
    // from the Operating System.
    property Platforms: TJVEPushNotificationPlatforms read FPlatforms
      write FPlatforms default [npiOS, npMac, npAndroid];

    // See event types definitions above for details on these events.
    property OnToken: TJVEPushToken read FOnToken write FOnToken;
    property OnError: TJVEPushError read FOnError write FOnError;
    property OnReceived: TJVEPushReceived read FOnReceived write FOnReceived;

    // Your own project number, the one you got from the Google API Console
    // No longer used - use Project Options instead.
    property AndroidSenderId: String read FAndroidSenderId write FAndroidSenderId;

    // If you want to convert Apple tokens to FCM tokens, provide your API key here.
    // IsSandbox indicates whether the app is running in sandbox environment
    // or in production. Sadly, even though this information is compiled into
    // the app, the SecTaskCopyValueForEntitlement function is considered to be
    // private API in iOS and thus cannot be used.
    property FCMConversionApiKey: String read FFCMConversionApiKey
      write FFCMConversionApiKey;
    property FCMConversionIsSandbox: Boolean read FFCMConversionIsSandbox
      write FFCMConversionIsSandbox default False;
  end;

implementation

{$IF (Defined(MACOS) and not Defined(IOS)) or (Defined(IOS) and not Defined(CPUX86))}

uses System.TypInfo, Macapi.ObjectiveC, Macapi.ObjCRuntime, JVE.OpenURL, System.JSON,
  {$IFDEF IOS} iOSapi.CocoaTypes, iOSapi.Foundation, iOSapi.UIKit
  {$ELSE} Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit {$ENDIF};

type
  INotificationHandler = interface(NSObject)
    procedure FinishLaunchingNotification(notification: Pointer); cdecl;
  end;

  TNotificationHandler = class(TOCLocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure FinishLaunchingNotification(notification: Pointer); cdecl;
  end;

var
  PostInitialization: Boolean;
  StartUpNotification: TJVEPushData;
  NotificationHandler: TNotificationHandler;

const
  APSToken = 'application:didRegisterForRemoteNotificationsWithDeviceToken:';
  APSError = 'application:didFailToRegisterForRemoteNotificationsWithError:';
  APSReceived = 'application:didReceiveRemoteNotification:';

function TNotificationHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(INotificationHandler);
end;

procedure TNotificationHandler.FinishLaunchingNotification(Notification: Pointer); cdecl;
var
  Dict: NSDictionary;
  Ptr: Pointer;
  List: TStrings;
begin
  Dict := TNSNotification.Wrap(Notification).userInfo;
  if Dict <> nil then
  begin
    Ptr := Dict.objectForKey(PointerNSSTR(
      {$IFDEF IOS}'UIApplicationLaunchOptionsRemoteNotificationKey'
      {$ELSE}'NSApplicationLaunchRemoteNotificationKey'{$ENDIF}));
    if Ptr <> nil then
      try
        List := NSDictionaryToStrings(TNSDictionary.Wrap(Ptr));
        StartUpNotification := TJVEPushData.Create;
        StartUpNotification.AddStrings(List);
      finally
        FreeAndNil(List);
      end;
  end;
end;

{$IFDEF IOS}
procedure APSConvertToFCM(Str: String);
begin
  DetachThread(procedure(Sync: TProc<TThreadProcedure>)
  var
    Data, Error, App: String;
    Headers: TStrings;
    JSON: TJSONValue;
  begin
    Headers := TStringList.Create;
    Headers.Values['Content-Type'] := 'application/json';
    Headers.Values['Authorization'] := 'key=' +
      TJVEPushNotification.FGlobalInstance.FFCMConversionApiKey;

    App := FromNSSTR(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).
      infoDictionary.objectForKey(PointerNSSTR('CFBundleIdentifier')));
    Error := '';
    try
      Data := TJVEOpenURL.DownloadString(
        'https://iid.googleapis.com/iid/v1:batchImport', Headers,
        '{"application": "' + App + '", "sandbox":' +
        IfThen(TJVEPushNotification.FGlobalInstance.FFCMConversionIsSandbox,
        'true', 'false') + ', "apns_tokens":["' + Str + '"]}', Error);
      FreeAndNil(Headers);

      if Error = '' then
      begin
        JSON := TJSONObject.ParseJSONValue(Data);
        Error := JSON.GetValue<String>('results[0].status');
        if Error = 'OK' then
        begin
          Error := '';
          App := JSON.GetValue<String>('results[0].registration_token');
        end;
        FreeAndNil(JSON);
      end;

      Sync(procedure
      begin
        if Error = '' then
          TJVEPushNotification.FGlobalInstance.DoToken(App)
        else
          TJVEPushNotification.FGlobalInstance.DoError(Error, True);
      end);
    except
      on E: Exception do
        Sync(procedure
        begin
          TJVEPushNotification.FGlobalInstance.DoError(E.Message, True);
        end);
    end;
  end);
end;
{$ENDIF}

procedure APSGotToken(Self: Pointer; _cmd: SEL;
  Application: Pointer; Token: Pointer); cdecl;
var
  Data: NSData;
  Bytes: array[0..127] of Byte;
  Idx: Integer;
  Str: String;
begin
  if (TJVEPushNotification.FGlobalInstance <> nil) and (Token <> nil) then
  begin
    Data := TNSData.Wrap(Token);
    if (Data.length > 0) and (Data.length <= 128) then
    begin
      Data.getBytes(@Bytes, Data.length);
      Str := '';
      for Idx := 0 to Data.length - 1 do
        Str := Str + IntToHex(Bytes[Idx], 2);

{$IFDEF IOS}
      if TJVEPushNotification.FGlobalInstance.FFCMConversionApiKey <> '' then
        APSConvertToFCM(Str)
      else
{$ENDIF}
        TJVEPushNotification.FGlobalInstance.DoToken(Str);
    end;
  end;
end;

procedure APSGotError(Self: Pointer; _cmd: SEL;
  Application: Pointer; Error: Pointer); cdecl;
begin
  if TJVEPushNotification.FGlobalInstance <> nil then
    TJVEPushNotification.FGlobalInstance.DoError(
      FromNSSTR(TNSError.Wrap(Error).localizedDescription), True);
end;

procedure APSReceivedMessage(Self: Pointer; _cmd: SEL;
  Application: Pointer; Notification: Pointer); cdecl;
var
  Push: TJVEPushData;
  List: TStrings;
begin
  if (TJVEPushNotification.FGlobalInstance <> nil) and (Notification <> nil) then
    try
      Push := nil; // If next line fails, this ensures there will be no exception in finally
      List := NSDictionaryToStrings(TNSDictionary.Wrap(Notification));
      Push := TJVEPushData.Create;
      Push.AddStrings(List);
      TJVEPushNotification.FGlobalInstance.DoReceived(Push, False);
    finally
      FreeAndNil(Push);
      FreeAndNil(List);
    end;
end;

{$IFDEF IOS}
type
  // Redefined as iOS 8 functions are not defined as part of Delphi Runtime.
  UIUserNotificationSettings = interface(NSObject)
  end;
  UIUserNotificationSettingsClass = interface(NSObjectClass)
    function settingsForTypes(types: NSUInteger; categories: NSSet): UIUserNotificationSettings; cdecl;
  end;
  TUIUserNotificationSettings = class(TOCGenericImport
    <UIUserNotificationSettingsClass, UIUserNotificationSettings>) end;

  UIApplication = interface(UIResponder)
    ['{BD1002BE-B8C1-45BE-9AB8-9720A9EB1102}']
    procedure registerForRemoteNotifications; cdecl;
    procedure registerUserNotificationSettings(notificationSettings: UIUserNotificationSettings); cdecl;
    // This pre-iOS 8 function is defined incorrectly in iOSapi.UIKit.pas.
    // The parameter must be an integer, not a pointer.
    procedure registerForRemoteNotificationTypes(types: NSUInteger); cdecl;
  end;
  UIApplicationClass = interface(UIResponderClass)
    ['{EB8D5CD5-75CA-469F-B1B9-3A32D640B450}']
    {class} function sharedApplication: UIApplication; cdecl;
  end;
  TUIApplication = class(TOCGenericImport<UIApplicationClass, UIApplication>) end;
{$ELSE}
type
  // Redefined as these two functions (only available in Mac OS X 10.7) are
  // not defined as part of Delphi Runtime.
  NSApplication = interface(NSResponder)
    function dockTile: NSDockTile; cdecl;
    procedure registerForRemoteNotificationTypes(types: NSUInteger); cdecl;
    procedure unregisterForRemoteNotifications; cdecl;
  end;
  NSApplicationClass = interface(NSResponderClass)
    function sharedApplication: NSApplication; cdecl;
  end;
  TNSApplication = class(TOCGenericImport<NSApplicationClass, NSApplication>) end;
{$ENDIF}

procedure PlatformRequest;
var
  Cls: Pointer;
  Send: Integer;
begin
  if not PostInitialization then
  begin
    PostInitialization := True;

    // For iOS Delphi creates the AppDelegate manually (for some reason),
    // for Mac OS X it is a simple delegate, in any case it does NOT register
    // (or expose) any of the following delegate functions.
    // To tell the truth, I have no idea how to add these the "Delphi style"
    // (without changing the RTL), but patching Objective-C works quite well!
    Cls := objc_getClass({$IFDEF IOS}'DelphiAppDelegate');{$ELSE}'TFMXApplicationDelegate');
    if Cls = nil then
      Cls := objc_getClass('TApplicationDelegate');
    {$ENDIF}

    if class_getInstanceMethod(Cls, sel_getUid(APSToken)) <> nil then
    begin
      class_addMethod(Cls, sel_registerName('jve_' + APSToken), @APSGotToken, 'v@:@@');
      method_exchangeImplementations(
        class_getInstanceMethod(Cls, sel_getUid(APSToken)),
        class_getInstanceMethod(Cls, sel_getUid('jve_' + APSToken)));
    end else
      class_addMethod(Cls, sel_getUid(APSToken), @APSGotToken, 'v@:@@');

    if class_getInstanceMethod(Cls, sel_getUid(APSError)) <> nil then
    begin
      class_addMethod(Cls, sel_registerName('jve_' + APSError), @APSGotError, 'v@:@@');
      method_exchangeImplementations(
        class_getInstanceMethod(Cls, sel_getUid(APSError)),
        class_getInstanceMethod(Cls, sel_getUid('jve_' + APSError)));
    end else
      class_addMethod(Cls, sel_getUid(APSError), @APSGotError, 'v@:@@');

    if class_getInstanceMethod(Cls, sel_getUid(APSReceived)) <> nil then
    begin
      class_addMethod(Cls, sel_registerName('jve_' + APSReceived),
        @APSReceivedMessage, 'v@:@@');
      method_exchangeImplementations(
        class_getInstanceMethod(Cls, sel_getUid(APSReceived)),
        class_getInstanceMethod(Cls, sel_getUid('jve_' + APSReceived)));
    end else
      class_addMethod(Cls, sel_getUid(APSReceived), @APSReceivedMessage, 'v@:@@');
  end;

  if TJVEPushNotification.GlobalInstance = nil then
    Exit;

  if TJVEPushNotification.GlobalInstance.FClearBadgeOnStart then
  begin
    {$IFDEF IOS}
    SharedUIApplication.setApplicationIconBadgeNumber(0);
    {$ELSE}
    TNSApplication.OCClass.sharedApplication.dockTile.setBadgeLabel(ToNSSTR(''));
    {$ENDIF}
  end;

  {$IFDEF IOS}
  if not (npiOS in TJVEPushNotification.GlobalInstance.FPlatforms) then
    Exit;
  {$ELSE}
  // Push Notifications are only available starting with Mac OS X 10.7
  if not (npMac in TJVEPushNotification.GlobalInstance.FPlatforms)
    or not TOSVersion.Check(10, 7) then
      Exit;
  {$ENDIF}

  if TJVEPushNotification.GlobalInstance.FTypes <> [] then
  begin
    Send := 0;
    if ntBadge in TJVEPushNotification.GlobalInstance.FTypes then
      Inc(Send, 1); // UI/NS RemoteNotificationTypeBadge
    if ntSound in TJVEPushNotification.GlobalInstance.FTypes then
      Inc(Send, 2); // UI/NS RemoteNotificationTypeSound
    if ntAlert in TJVEPushNotification.GlobalInstance.FTypes then
      Inc(Send, 4); // UI/NS RemoteNotificationTypeAlert
    {$IFDEF IOS}
    // This type of notifications is not available in Mac OS X.
    if ntNewsstand in TJVEPushNotification.GlobalInstance.FTypes then
      Inc(Send, 8); // UIRemoteNotificationTypeNewsstandContentAvailability

    if TOSVersion.Major >= 8 then
    begin
      TUIApplication.OCClass.sharedApplication.registerUserNotificationSettings(
        TUIUserNotificationSettings.OCClass.settingsForTypes(Send, nil));
      TUIApplication.OCClass.sharedApplication.registerForRemoteNotifications;
    end else
      TUIApplication.OCClass.sharedApplication.registerForRemoteNotificationTypes(Send);
    {$ELSE}
    TNSApplication.OCClass.sharedApplication.registerForRemoteNotificationTypes(Send);
    {$ENDIF}
  end else
  begin
    {$IFDEF IOS}
      SharedUIApplication.unregisterForRemoteNotifications;
    {$ELSE}
      TNSApplication.OCClass.sharedApplication.unregisterForRemoteNotifications;
    {$ENDIF}
  end;
end;

{$ELSEIF Defined(ANDROID)}

uses {$IF CompilerVersion >= 30} System.Notification,{$ELSE} FMX.Notification,{$ENDIF}
  System.PushNotification, FMX.PushNotification.Android, System.JSON;

type
  TPushListener = class
  public
    procedure OnReceived(Sender: TObject; const ANotification: TPushServiceNotification);
    procedure OnToken(Sender: TObject; AChange: TPushService.TChanges);
  end;

var
  PushConnection: TPushServiceConnection;
  PushListener: TPushListener;
  NotificationCenter: TNotificationCenter;

procedure ShowNotification(Text, Sound: String);
var
  Notification: TNotification;
begin
  // Delphi 10 does not have this function, instead CreateNotification might return nil
  {$IF CompilerVersion < 30}
  if NotificationCenter.Supported then
  {$ENDIF}
  begin
    Notification := NotificationCenter.CreateNotification;
    if Assigned(Notification) then
    try
      Notification.Name := 'Push Notification';
      Notification.AlertBody := Text;
      Notification.EnableSound := True;
      {$IF CompilerVersion >= 30}
      Notification.SoundName := Sound;
      {$ENDIF}
      Notification.FireDate := Now;

      NotificationCenter.PresentNotification(Notification);
    finally
      Notification.DisposeOf;
    end;
  end;
end;

procedure TPushListener.OnToken(Sender: TObject; AChange: TPushService.TChanges);
begin
{$IF CompilerVersion >= 30}
  if TJVEPushNotification.FGlobalInstance = nil then
    Exit;

  if TPushService.TChange.DeviceToken in AChange then
    TJVEPushNotification.FGlobalInstance.DoToken(PushConnection.Service.DeviceToken[0].Value)
  else
    TJVEPushNotification.FGlobalInstance.DoError(PushConnection.Service.StartupError, True);
{$ENDIF}
end;

procedure TPushListener.OnReceived(Sender: TObject; const ANotification: TPushServiceNotification);
var
  Push: TJVEPushData;
  List: TStrings;
begin
  if TJVEPushNotification.FGlobalInstance <> nil then
  try
    Push := TJVEPushData.Create;
    List := JSONObjectToStrings(ANotification.DataObject);
    if List.Count > 1 then
    begin
      Push.AddStrings(List);
      TJVEPushNotification.FGlobalInstance.DoReceived(Push, Sender = nil);
    end;
  finally
    FreeAndNil(Push);
    FreeAndNil(List);
  end;
end;

procedure PlatformRequest;
begin
{$IF CompilerVersion >= 30}
  try
    if NotificationCenter = nil then
    begin
      NotificationCenter := TNotificationCenter.Create(nil);
      if TJVEPushNotification.GlobalInstance.FClearBadgeOnStart then
        NotificationCenter.CancelAll;
    end;

    PushListener := TPushListener.Create;
    PushConnection := TPushServiceManager.Instance.GetServiceByName(
      TPushService.TServiceNames.FCM).CreateConnection;
    PushConnection.OnReceiveNotification := PushListener.OnReceived;
    PushConnection.OnChange := PushListener.OnToken;
    PushConnection.Active := True;

    if (PushConnection.Service.StartupNotifications <> nil) and
      (Length(PushConnection.Service.StartupNotifications) > 0) then
        PushListener.OnReceived(nil, PushConnection.Service.StartupNotifications[0]);
  except
    on E: Exception do
      if TJVEPushNotification.FGlobalInstance <> nil then
        TJVEPushNotification.FGlobalInstance.DoError(E.Message, True);
  end;
{$ELSE}
  TJVEPushNotification.FGlobalInstance.DoError('Unsupported version', True);
{$ENDIF}
end;

{$ENDIF}

{ TJVEPushNotification }

constructor TJVEPushNotification.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPlatforms := [npiOS, npMac, npAndroid];
  FClearBadgeOnStart := True;
  FGlobalInstance := Self;
  Types := [ntBadge, ntSound, ntAlert];
end;

destructor TJVEPushNotification.Destroy;
begin
  if FGlobalInstance = Self then
    FGlobalInstance := nil;
  inherited Destroy;
end;

procedure TJVEPushNotification.DoToken(Token: String);
begin
  TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create(
    'Push Notification', 'Has Token', '', 0, acJVEPushes));

  if Assigned(FOnToken) then
    FOnToken(Self, Token);
end;

procedure TJVEPushNotification.DoError(Error: String; Report: Boolean);
var
  DefaultProc: Boolean;
begin
  TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create(
    'Push Notification', 'Has Error', Error, 0, acJVEPushes));

  DefaultProc := Report;
  if Assigned(FOnError) then
    FOnError(Self, Error, DefaultProc);

  if DefaultProc then
    ShowDialog(Error, TMsgDlgType.mtError);
end;

procedure TJVEPushNotification.DoReceived(Push: TJVEPushData; OnAppStart: Boolean);
var
  DefaultProc: Boolean;
begin
  TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create(
    'Push Notification', 'Message', Push.GetMessage, 0, acJVEPushes));

  DefaultProc := True;
  if Assigned(FOnReceived) then
    FOnReceived(Self, Push, OnAppStart, DefaultProc);

  if DefaultProc then
    PresentPush(Push);
end;

procedure TJVEPushNotification.PresentPush(Push: TJVEPushData);
begin
{$IFDEF ANDROID}
  if IsRunningInForeground then
    ShowDialog(Push.GetMessage, TMsgDlgType.mtInformation)
  else
    ShowNotification(Push.GetMessage, Push.GetSound);
{$ELSE}
  ShowDialog(Push.GetMessage, TMsgDlgType.mtInformation);
{$ENDIF}
end;

class function TJVEPushNotification.GetGlobalInstance: TJVEPushNotification;
begin
  Result := FGlobalInstance;
end;

procedure TJVEPushNotification.SetTypes(const Value: TJVEPushNotificationTypes);
begin
  FTypes := Value;
{$IF (Defined(MACOS) and not Defined(IOS)) or (Defined(IOS) and not Defined(CPUX86)) or Defined(ANDROID)}
  ExecuteInIdle(
    procedure
    begin
      PlatformRequest;
  {$IF not Defined(ANDROID)}
      if StartUpNotification <> nil then
        try
          DoReceived(StartUpNotification, True);
        finally
          FreeAndNil(StartUpNotification);
        end;
  {$ENDIF}
    end);
{$ENDIF}
end;

{ TJVEPushData }

function TJVEPushData.GetBadge: Integer;
begin
  Result := StrToIntDef(Values['aps.badge'], 0);
  if Result = 0 then
    Result := StrToIntDef(Values['data.badge'], 0);
end;

function TJVEPushData.GetIsNewsstand: Boolean;
begin
  Result := (StrToIntDef(Values['aps.content-available'], 0) <> 0) or
    (StrToIntDef(Values['data.content-available'], 0) <> 0);
end;

function TJVEPushData.GetMessage: String;
begin
  Result := Values['alert'];
  if Result = '' then
    Result := Values['aps.alert'];
  if Result = '' then
    Result := Values['aps.alert.body'];

  if Result = '' then
    Result := Values['msg'];
  if Result = '' then
    Result := Values['message'];
  if Result = '' then
    Result := Values['data.body'];
  if Result = '' then
    Result := Values['data.message'];
  if Result = '' then
    Result := Values['data.message.body'];

  if Result = '' then
    Result := Values['title'];  // PushWoosh
end;

function TJVEPushData.GetSound: String;
begin
  Result := Values['aps.sound'];
  if Result = '' then
    Result := Values['data.sound'];
  if Result = '' then
    Result := Values['s'];  // PushWoosh
end;

{$IF (Defined(MACOS) and not Defined(IOS)) or (Defined(IOS) and not Defined(CPUX86))}

procedure JVEPushTokenToFCM(ServerKey, Token: String; NewToken: TProc<String>);
begin
end;

initialization
  NotificationHandler := TNotificationHandler.Create;
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).
    addObserver(NotificationHandler.GetObjectID,
    sel_getUid('FinishLaunchingNotification:'), PointerNSSTR(
    {$IFDEF IOS}'UIApplicationDidFinishLaunchingNotification'
    {$ELSE}'NSApplicationDidFinishLaunchingNotification'{$ENDIF}), nil);
finalization
{$ELSE}

procedure JVEPushTokenToFCM(ServerKey, Token: String; NewToken: TProc<String>);
begin
  NewToken(Token);
end;

{$ENDIF}
end.
