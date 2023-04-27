(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.AppStore;

interface

uses System.Classes, System.SysUtils, System.StrUtils, System.UITypes, System.Math,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  FMX.Dialogs, FMX.Controls, FMX.Types, JVE.OpenURL, JVE.Browser, JVE.Messaging,
  JVE.Configuration, JVE.Actions, JVE.Utils;

type
  TJVEAppStore = class;
  TJVEAppStorePlatform = (
    spWindows,   // Windows app URL
    spAppStore,  // App Store (i.e. iOS devices)
    spMacStore,  // Mac Store (i.e. desktop apps)
    spPlayStore, // Play Store (i.e. Android devices)
    spNone);     // Return this value to indicate that there are no alternatives

  TJVEAppStoreAppExists = (
    saOpen,    // Open the app, instead of opening the store, if app already installed
    saStore,   // Open the store, even if the app is already installed
    saIgnore); // If the app is installed, take no action (ignore Open call)

  TJVEAppRaterNativeiOS = (
    srUseAppStore,  // Use App Store app (the current application will go into background)
    sriOS6Dialog,   // If available, use iOS 6.0 native API (since iOS 8.0
      // the native interface doesn't allow the user to write a review)
    sriOS10Dialog); // If available, use iOS 10.3 native API

  TJVEAppStoreFallback = procedure(Sender: TJVEAppStore;
    var FallbackTo: TJVEAppStorePlatform) of object;

  TJVEAppBasicData = class(TComponent)
  private
    FEmbed: Boolean;
    FWindowsURL: String;
    FMacIdentifier: String;
    FiOSIdentifier: String;
    FAndroidIdentifier: String;
    FMacURL: String;
    FiOSURL: String;
    FAndroidURL: String;
  protected
    procedure OpenURL(URL: String; AppPlatform: TJVEAppStorePlatform;
      CanEmbed: Boolean = True); virtual;
    function ValidPlatform(Check: TJVEAppStorePlatform): Boolean;
  public
    // The platform, for which the store (rating) will be presented.
    function ActivePlatform: TJVEAppStorePlatform; virtual; abstract;
  published
    // iTunes Product ID (i.e. "123456789"). If specified, the app is available
    // for iOS and native interface is attempted.
    property iOSIdentifier: String read FiOSIdentifier write FiOSIdentifier;
    // Normally blank (the component can create this URL automatically).
    // If specified, overrides the default store URL, when native interface is
    // not available and on other platforms.
    // If specified, iOSIdentifier can be left blank, to ensure no native interface.
    property iOSURL: String read FiOSURL write FiOSURL;

    // iTunes Product ID (i.e. "987654321"). If specified, the app is available for Mac.
    property MacIdentifier: String read FMacIdentifier write FMacIdentifier;
    // Normally blank (the component can create this URL automatically).
    // If specified, overrides the default store URL for the app.
    property MacURL: String read FMacURL write FMacURL;

    // Play Market Product ID (i.e. "com.example.android").
    // If specified, the app is available for Android.
    property AndroidIdentifier: String read FAndroidIdentifier write FAndroidIdentifier;
    // Normally blank (the component can create this URL automatically).
    // If specified, overrides the default store URL for the app.
    property AndroidURL: String read FAndroidURL write FAndroidURL;

    // If specified, the app is available for Windows. Provides the URL to open.
    property WindowsURL: String read FWindowsURL write FWindowsURL;

    // In case a web browser is opened, indicates whether to open it embedded.
    // In the app rater also indicates that the native app view should be opened.
    property Embed: Boolean read FEmbed write FEmbed default False;
  end;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEAppStore = class(TJVEAppBasicData, IJVEExecutable)
  private
    FAppScheme: String;
    FExistsAction: TJVEAppStoreAppExists;
    FOnFallback: TJVEAppStoreFallback;
  protected
    procedure OpenURL(URL: String; AppPlatform: TJVEAppStorePlatform;
      CanEmbed: Boolean = True); override;
  protected
    // IJVEExecutable support
    function CanOpen(Sender: TJVEAction): Boolean;
  public
    // Opens the store, native if available or in browser, for the platform
    // returned by ActivePlatform.
    // The Sender parameter is for IJVEExecutable support and should not be used.
    procedure Open(Sender: TJVEAction = nil);
    // The platform, for which the store will be presented, based on app
    // availability and fallback event calls.
    function ActivePlatform: TJVEAppStorePlatform; override;
  published
    // The URL scheme, associated with this app. If specified, the component
    // uses this scheme to identify whether the app is already installed
    // on the local system.
    // If installed, ExistsAction is used to select the action to be performed
    // on Open (i.e. open the app or still open the store, or even do nothing).
    // Leave AppScheme blank to disable this functionality.
    property AppScheme: String read FAppScheme write FAppScheme;
    property ExistsAction: TJVEAppStoreAppExists
      read FExistsAction write FExistsAction default saOpen;

    // Used to choose alternative platform, if the current one is not available.
    // Default order: Windows, then iOS, then Mac, then Android.
    property OnFallback: TJVEAppStoreFallback read FOnFallback write FOnFallback;
  end;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEAppRater = class(TJVEAppBasicData, IJVEInterstitial)
  private
    FInitialEventsCount: Integer;
    FInitialDaysDelay: Integer;
    FInitialUsesCount: Integer;
    FReminderEventsCount: Integer;
    FReminderDaysDelay: Integer;
    FReminderUsesCount: Integer;
    FQuestion: String;
    FSessionActive: Boolean;
    FAutoTranslate: Boolean;
    FStyleChangedId: Integer;
    FNativeiOS: TJVEAppRaterNativeiOS;
    function IsQuestionStored: Boolean;
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
    procedure NativeIniOSReader(Reader: TReader);
    function GetNativeIniOS: Boolean;
    procedure SetNativeIniOS(const Value: Boolean);
  protected
    procedure OpenURL(URL: String; AppPlatform: TJVEAppStorePlatform;
      CanEmbed: Boolean = True); override;
    // IJVEInterstitial support
    procedure Cache(Sender: TJVEAction);
    procedure Show(Sender: TJVEAction);
    function IsCached(Sender: TJVEAction): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DefineProperties(Filer: TFiler); override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;

    // Opens the rating entry immediately, overriding the usual delay and without
    // a confirmation dialog. For the normal functionality, use ProcessMajorEvent.
    procedure Open;
    // The current platform, if the query can be presented or spNone, if the
    // current platform is not defined.
    function ActivePlatform: TJVEAppStorePlatform; override;
    // If user has not requested not to be bugged, presents a dialog asking him,
    // whether he would like to rate the app. A callback is called after a reply,
    // passing True if the user accepted (and you can call the Open function).
    procedure QueryForFeedback(Result: TProc<Boolean>);

    // If the user chose not to ask him again, this would reset this selection.
    // You should only use this in outstanding circumstances. For example, you
    // are out with a new app version and want the user to reconsider.
    // If DontDisturb is set, will not ask the user again.
    procedure ResetDontAsk(DontDisturb: Boolean = False);
    // Use this function to notify the App Rater that it is an appropriate time
    // to show the Rate request. Only opens the popup if appropriate time has
    // passed and other requires are met.
    // This is the main and ONLY function you should normally call in this class.
    // Pass False in OpenQuery to check applicability without opening a query.
    // Returns True, if the interstitial query was indeed shown (or should now
    // be shown, if requested not to open; counters are reset anyway in this case).
    function ProcessMajorEvent(OpenQuery: Boolean = True): Boolean;
    // Legacy support property, use NativeiOS instead
    property NativeIniOS: Boolean read GetNativeIniOS write SetNativeIniOS;
  published
    // Should not be set to too low values: since you would only want positive
    // rating, you should wait to make sure the user likes your app.

    // Number of days, uses and major events before the app first asks to rate itself.
    property InitialDaysDelay: Integer read FInitialDaysDelay
      write FInitialDaysDelay default 5;
    property InitialUsesCount: Integer read FInitialUsesCount
      write FInitialUsesCount default 10;
    property InitialEventsCount: Integer read FInitialEventsCount
      write FInitialEventsCount default 5;

    // Number of days, uses and major events before the app reminds the user
    // to rate itself, after he first refused to.
    property ReminderDaysDelay: Integer read FReminderDaysDelay
      write FReminderDaysDelay default 2;
    property ReminderUsesCount: Integer read FReminderUsesCount
      write FReminderUsesCount default 5;
    property ReminderEventsCount: Integer read FReminderEventsCount
      write FReminderEventsCount default 3;

    // This is the default query, to be presented within the dialog.
    property Question: String read FQuestion write FQuestion stored IsQuestionStored;
    // Use embedded browser or native interface to rate (where available).
    property Embed default True;
    // This property defines which embedded interface will be used in iOS.
    property NativeiOS: TJVEAppRaterNativeiOS read FNativeiOS
      write FNativeiOS default sriOS10Dialog;
  end;

implementation

{$IF Defined(IOS) and not Defined(CPUX86)}

uses Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation, Posix.Dlfcn;

var
  SKStoreProductParameterITunesItemIdentifier: PPointer;

type
  SKStoreReviewControllerClass = interface(NSObjectClass)
    procedure requestReview; cdecl;
  end;

  SKStoreReviewController = interface(NSObject)
  end;

  TSKStoreReviewController = class(TOCGenericImport
    <SKStoreReviewControllerClass, SKStoreReviewController>) end;


  TSKStoreProductViewControllerBlock = procedure(result: Pointer; error: Pointer) of object;

  SKStoreProductViewControllerClass = interface(UIViewControllerClass)
  end;

  SKStoreProductViewController = interface(UIViewController)
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
    procedure loadProductWithParameters(parameters: NSDictionary;
      completionBlock: TSKStoreProductViewControllerBlock); cdecl;
  end;

  TSKStoreProductViewController = class(TOCGenericImport
    <SKStoreProductViewControllerClass, SKStoreProductViewController>) end;


  SKStoreProductViewControllerDelegate = interface(IObjectiveC)
    procedure productViewControllerDidFinish(
      viewController: SKStoreProductViewController); cdecl;
  end;

  TSKStoreProductDelegate = class(TOCLocal, SKStoreProductViewControllerDelegate)
  public
    Store: SKStoreProductViewController;
    procedure ViewControllerBlock(result: Pointer; error: Pointer);
  public
    class procedure OpenProduct(ProductId: String);
    procedure productViewControllerDidFinish(
      viewController: SKStoreProductViewController); cdecl;
  end;

var
  StoreProductDelegate: TSKStoreProductDelegate;

{ TSKStoreProductDelegate }

// Actually the first parameter should be a boolean, but an exception is reported
// if it is. Probably a Delphi bug in Objective C blocks interface.
// But then, blocks' interface is implemented incorrectly anyway: they should have been
// "references to procedures", not "procedures of objects".
procedure TSKStoreProductDelegate.ViewControllerBlock(result: Pointer; error: Pointer);
begin
  if result = nil then
    productViewControllerDidFinish(nil);
end;

procedure TSKStoreProductDelegate.productViewControllerDidFinish(
  viewController: SKStoreProductViewController);
begin
  if Store <> nil then
    Store.dismissModalViewControllerAnimated(True);
  Store := nil;
end;

class procedure TSKStoreProductDelegate.OpenProduct(ProductId: String);
begin
  if StoreProductDelegate = nil then
    StoreProductDelegate := TSKStoreProductDelegate.Create;

  StoreProductDelegate.Store := TSKStoreProductViewController.Create;
  StoreProductDelegate.Store.setDelegate(StoreProductDelegate.GetObjectID);
  StoreProductDelegate.Store.loadProductWithParameters(TNSDictionary.Wrap(
    TNSDictionary.OCClass.dictionaryWithObject(PointerNSSTR(ProductId),
    SKStoreProductParameterITunesItemIdentifier^)),
    StoreProductDelegate.ViewControllerBlock);
  GetRootViewController.presentModalViewController(StoreProductDelegate.Store, True);
end;

{$ENDIF}

const
  AppRaterStatusInitial = 0;
  AppRaterStatusReminder = 1;
  AppRaterStatusDone = 2;

  AppRaterDate = 'TJVEAppRaterDate-';
  AppRaterUses = 'TJVEAppRaterUses-';
  AppRaterEvents = 'TJVEAppRaterEvents-';
  AppRaterStatus = 'TJVEAppRaterStatus-';

  TJVEAppRaterQuestion = 'If you enjoy using this app, would you mind ' +
    'taking a moment to rate it? If you choose No, we won''t bug you again.';
  TJVEAppStorePlatformNames: array[TJVEAppStorePlatform] of String =
    ('Windows', 'iOS', 'Mac', 'Android', 'None');

{ TJVEAppBasicData }

procedure TJVEAppBasicData.OpenURL(URL: String;
  AppPlatform: TJVEAppStorePlatform; CanEmbed: Boolean);
begin
  if CanEmbed and FEmbed then
    TJVEBrowser.OpenURL(URL, ' ')
  else
    TJVEOpenURL.OpenURL(URL);
end;

function TJVEAppBasicData.ValidPlatform(Check: TJVEAppStorePlatform): Boolean;
begin
  case Check of
    spWindows: Result := FWindowsURL <> '';
    spPlayStore: Result := (FAndroidIdentifier <> '') or (FAndroidURL <> '');
    spAppStore: Result := (FiOSIdentifier <> '') or (FiOSURL <> '');
    spMacStore:
      {$IFDEF IOS}
        // iOS actually does not support showing Mac app anywhere, neither
        // in browser, nor in native interface!
        Result := False;
      {$ELSE}
        Result := (FMacIdentifier <> '') or (FMacURL <> '');
      {$ENDIF}
  else
    Result := True;
  end;
end;

{ TJVEStore }

function TJVEAppStore.ActivePlatform: TJVEAppStorePlatform;
begin
  Result := {$IF Defined(IOS)} spAppStore;
    {$ELSEIF Defined(MACOS)} spMacStore;
    {$ELSEIF Defined(Android)} spPlayStore;
    {$ELSEIF Defined(MSWINDOWS)} spWindows; {$ENDIF}
  if ValidPlatform(Result) then Exit;

  if Assigned(FOnFallback) then
  begin
    if Result = spWindows then
      Result := spAppStore
    else
      Result := spWindows;

    FOnFallback(Self, Result);
    if ValidPlatform(Result) then Exit;
  end else
    for Result := Low(Result) to High(Result) do
      if ValidPlatform(Result) then Exit;

  Result := spNone;
end;

function TJVEAppStore.CanOpen(Sender: TJVEAction): Boolean;
begin
  if (FAppScheme <> '') and TJVEOpenURL.SchemeSupported(FAppScheme) then
    Result := FExistsAction <> saIgnore
  else
    Result := ActivePlatform <> spNone;
end;

procedure TJVEAppStore.Open;
begin
  if (FAppScheme <> '') and (FExistsAction <> saStore) and
    TJVEOpenURL.SchemeSupported(FAppScheme) then
  begin
    if FExistsAction = saOpen then
      TJVEOpenURL.OpenURL(FAppScheme + '://' + ClassName);
    Exit;
  end;

  case ActivePlatform of
    spWindows: OpenURL(FWindowsURL, spWindows);

    spMacStore: if FMacURL <> '' then
        OpenURL(FMacURL, spMacStore)
      else
        // iOS is not supported (see ValidPlatform).
        {$IFDEF MACOS}
          OpenURL('macappstore://itunes.apple.com/app/id' +
            FMacIdentifier, spMacStore, False);
        {$ELSE}
          OpenURL('https://itunes.apple.com/app/id' + FMacIdentifier, spMacStore);
        {$ENDIF}

    spAppStore:
      // App Store is not (fully) implemented in the simulator. We cannot use
      // native interface, as blocks do not really work in simulator, and we
      // cannot use URL, as the store cannot be opened.
      // In other word: don't expect this to work in simulator, but you won't
      // get any exceptions either.
      {$IF Defined(IOS) and not Defined(CPUX86)}
        // This is only supported since iOS 6, but we are compatible with iOS 5.
        if TSKStoreProductViewController.Defined and (FiOSIdentifier <> '') then
        begin
          TSKStoreProductDelegate.OpenProduct(iOSIdentifier);
          TMessageManager.DefaultManager.SendMessage(Self,
            TJVETrackEvent.Create('Store', TJVEAppStorePlatformNames[spAppStore],
            'Native: ' + iOSIdentifier, 0, acJVEAppStore));
        end else
        if FiOSURL <> '' then
          OpenURL(FiOSURL, spAppStore)
        else
          OpenURL('itms-apps://itunes.apple.com/app/id' + FiOSIdentifier, spAppStore, False);
      {$ELSE}
        if FiOSURL <> '' then
          OpenURL(FiOSURL, spAppStore)
        else
          OpenURL('https://itunes.apple.com/app/id' + FiOSIdentifier, spAppStore);
      {$ENDIF}

    spPlayStore: if FAndroidURL <> '' then
        OpenURL(FAndroidURL, spPlayStore)
      else
        OpenURL('market://details?id=' + FAndroidIdentifier, spPlayStore, False);
  end;
end;

procedure TJVEAppStore.OpenURL(URL: String; AppPlatform: TJVEAppStorePlatform;
  CanEmbed: Boolean);
begin
  inherited OpenURL(URL, AppPlatform, CanEmbed);
  TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create(
    'Store', TJVEAppStorePlatformNames[AppPlatform], URL, 0, acJVEAppStore));
end;

{ TJVEAppRater }

constructor TJVEAppRater.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialDaysDelay := 5;
  FInitialUsesCount := 10;
  FInitialEventsCount := 5;
  FReminderDaysDelay := 2;
  FReminderUsesCount := 5;
  FReminderEventsCount := 3;
  FQuestion := TJVEAppRaterQuestion;
  FEmbed := True;
  FAutoTranslate := True;
  FNativeiOS := sriOS10Dialog;
  FStyleChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

procedure TJVEAppRater.NativeIniOSReader(Reader: TReader);
begin
  if Reader.ReadBoolean then
    FNativeiOS := sriOS10Dialog
  else
    FNativeiOS := srUseAppStore;
end;

procedure TJVEAppRater.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('NativeIniOS', NativeIniOSReader, nil, False);
end;

destructor TJVEAppRater.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  inherited Destroy;
end;

function TJVEAppRater.GetNativeIniOS: Boolean;
begin
  Result := FNativeiOS > srUseAppStore;
end;

procedure TJVEAppRater.Cache(Sender: TJVEAction);
begin
end;

procedure TJVEAppRater.SetNativeIniOS(const Value: Boolean);
begin
  if Value then
    FNativeiOS := sriOS10Dialog
  else
    FNativeiOS := srUseAppStore;
end;

procedure TJVEAppRater.Show(Sender: TJVEAction);
begin
  QueryForFeedback(procedure(Accepted: Boolean)
    begin
      if Accepted then
        Open;
    end);
end;

procedure TJVEAppRater.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  Translation: String;
begin
  if FAutoTranslate and (FQuestion <> '') then
  begin
    Translation := Translate(FQuestion); // Mandatory for texts collection
    if not (csDesigning in ComponentState) then
      Question := Translation;
  end;
end;

function TJVEAppRater.IsCached(Sender: TJVEAction): Boolean;
begin
  Result := ProcessMajorEvent(False);
end;

function TJVEAppRater.IsQuestionStored: Boolean;
begin
  Result := FQuestion <> TJVEAppRaterQuestion;
end;

function TJVEAppRater.ActivePlatform: TJVEAppStorePlatform;
begin
  {$IF Defined(IOS)}
  if ValidPlatform(spAppStore) then Result := spAppStore else
  {$ELSEIF Defined(MACOS)}
  if ValidPlatform(spMacStore) then Result := spMacStore else
  {$ELSEIF Defined(ANDROID)}
  if ValidPlatform(spPlayStore) then Result := spPlayStore else
  {$ELSEIF Defined(MSWINDOWS)}
  if ValidPlatform(spWindows) then Result := spWindows else
  {$IFEND}
    Result := spNone;
end;

procedure TJVEAppRater.OpenURL(URL: String; AppPlatform: TJVEAppStorePlatform;
  CanEmbed: Boolean);
begin
  inherited OpenURL(URL, AppPlatform, CanEmbed);

  TJVEConfiguration.WriteInteger(AppRaterStatus + Name, AppRaterStatusDone);
  TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create(
    'Rater', TJVEAppStorePlatformNames[AppPlatform], URL, 0, acJVEAppRater));
end;

function TJVEAppRater.ProcessMajorEvent(OpenQuery: Boolean = True): Boolean;
var
  Days, Use, Events, State: Integer;
begin
  State := TJVEConfiguration.ReadInteger(AppRaterStatus + Name, AppRaterStatusInitial);
  if State = AppRaterStatusDone then
    Exit(False);

  Days := TJVEConfiguration.ReadInteger(AppRaterDate + Name, 0);
  if Days = 0 then
  begin
    Days := Trunc(Date);
    TJVEConfiguration.WriteInteger(AppRaterDate + Name, Days);
  end;

  Use := TJVEConfiguration.ReadInteger(AppRaterUses + Name, 0);
  if not FSessionActive then
  begin
    FSessionActive := True;
    Inc(Use);
    TJVEConfiguration.WriteInteger(AppRaterUses + Name, Use);
  end;

  Events := TJVEConfiguration.ReadInteger(AppRaterEvents + Name, 0) + 1;
  Result := ((Trunc(Date) - Days) >= IfThen(State = AppRaterStatusInitial,
      FInitialDaysDelay, FReminderDaysDelay)) and
    (Use >= IfThen(State = AppRaterStatusInitial,
      FInitialUsesCount, FReminderUsesCount)) and
    (Events >= IfThen(State = AppRaterStatusInitial,
      FInitialEventsCount, FReminderEventsCount));

  if Result then
  begin
    TJVEConfiguration.WriteInteger(AppRaterDate + Name, Trunc(Date));
    TJVEConfiguration.WriteInteger(AppRaterUses + Name, 0);
    TJVEConfiguration.WriteInteger(AppRaterEvents + Name, 0);

    if State = AppRaterStatusInitial then
      TJVEConfiguration.WriteInteger(AppRaterStatus + Name, AppRaterStatusReminder);

    if OpenQuery then
      Show(nil);
  end else
    TJVEConfiguration.WriteInteger(AppRaterEvents + Name, Events);
end;

procedure TJVEAppRater.QueryForFeedback(Result: TProc<Boolean>);
begin
{$IF Defined(IOS) and not Defined(CPUX86)}
  // The new iOS API does not guarantee that the dialog will be presented,
  // thus asking the user upfront is not possible: we cannot guarantee that
  // choosing "Yes" will perform any action; we cannot even know that it did.
  if TSKStoreReviewController.Defined and FEmbed and (FNativeiOS >= sriOS10Dialog) then
  begin
    // We are executing this in Idle, just in case the calling code is not ready
    // for an immediate invocation (because it depended on the dialog being
    // not blocking).
    ExecuteInIdle(procedure
      begin
        Result(True);
      end);
    Exit;
  end;
{$ENDIF}

  if (ActivePlatform = spNone) or (TJVEConfiguration.ReadInteger(
    AppRaterStatus + Name, AppRaterStatusInitial) = AppRaterStatusDone) then
  begin
    // We are executing this in Idle, just in case the calling code is not ready
    // for an immediate invocation (because it depended on the dialog being
    // not blocking).
    ExecuteInIdle(procedure
      begin
        Result(False);
      end);
    Exit;
  end;

  ShowDialog(FQuestion, TMsgDlgType.mtConfirmation,
    mbYesNoCancel, procedure(Choice: TModalResult)
    begin
      if Choice = mrNo then
        TJVEConfiguration.WriteInteger(AppRaterStatus + Name, AppRaterStatusDone);
      Result(Choice = mrYes);
    end);
end;

procedure TJVEAppRater.ResetDontAsk(DontDisturb: Boolean = False);
begin
  case TJVEConfiguration.ReadInteger(AppRaterStatus + Name, AppRaterStatusInitial) of
    AppRaterStatusDone: if not DontDisturb then
      TJVEConfiguration.WriteInteger(AppRaterStatus + Name, AppRaterStatusReminder);
    AppRaterStatusInitial, AppRaterStatusReminder: if DontDisturb then
      TJVEConfiguration.WriteInteger(AppRaterStatus + Name, AppRaterStatusDone);
  end;
end;

procedure TJVEAppRater.Open;
begin
  if ActivePlatform = spNone then
    Exit;

{$IF Defined(IOS)}
  // App Store is not (fully) implemented in the simulator. We cannot use
  // native interface, as blocks do not really work in simulator, and we
  // cannot use URL, as the store cannot be opened.
  // In other word: don't expect this to work in simulator, but you won't
  // get any exceptions either.
  {$IFDEF CPUX86}
    ShowDialog('This feature is not supported within the simulator.', TMsgDlgType.mtError);
  {$ELSE}
    // This is only supported since iOS 10.3, but we are compatible with iOS 5.
    if TSKStoreReviewController.Defined and FEmbed and (FNativeiOS >= sriOS10Dialog) then
    begin
      TSKStoreReviewController.OCClass.requestReview;
      TMessageManager.DefaultManager.SendMessage(Self, TJVETrackEvent.Create('Rater',
        TJVEAppStorePlatformNames[spAppStore], 'Native 10.3', 0, acJVEAppStore));
    end else
    // This is only supported since iOS 6, but we are compatible with iOS 5.
    if TSKStoreProductViewController.Defined and
      (FiOSIdentifier <> '') and FEmbed and (FNativeiOS >= sriOS6Dialog) then
    begin
      TSKStoreProductDelegate.OpenProduct(iOSIdentifier);
      TMessageManager.DefaultManager.SendMessage(Self,
        TJVETrackEvent.Create('Rater', TJVEAppStorePlatformNames[spAppStore],
        'Native: ' + iOSIdentifier, 0, acJVEAppStore));
    end else
    if FiOSURL <> '' then
      OpenURL(FiOSURL, spAppStore)
    else
      OpenURL('itms-apps://ax.itunes.apple.com/WebObjects/MZStore.woa/wa/viewContents' +
        'UserReviews?type=Purple+Software&id=' + FiOSIdentifier, spAppStore, False);
  {$ENDIF}

{$ELSEIF Defined(MACOS)}
  if FMacURL <> '' then
    OpenURL(FMacURL, spMacStore)
  else
    OpenURL('macappstore://itunes.apple.com/app/id' + FMacIdentifier, spMacStore, False);

{$ELSEIF Defined(ANDROID)}
  if FAndroidURL <> '' then
    OpenURL(FAndroidURL, spPlayStore)
  else
    OpenURL('market://details?id=' + FAndroidIdentifier, spPlayStore, False);

{$ELSEIF Defined(MSWINDOWS)}
  OpenURL(FWindowsURL, spWindows);
{$ENDIF}
end;

{$IF Defined(IOS) and not Defined(CPUX86)}
initialization
  SKStoreProductParameterITunesItemIdentifier := CocoaPointerConst(
    '/System/Library/Frameworks/StoreKit.framework/StoreKit',
    'SKStoreProductParameterITunesItemIdentifier');
finalization
  FreeAndNil(StoreProductDelegate);
{$ENDIF}
end.

