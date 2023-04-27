(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Payment;

interface

uses System.SysUtils, System.Classes, System.Math, System.SyncObjs, FMX.Controls,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  {$IF Defined(MSWINDOWS) and (CompilerVersion >= 33)}WinAPI.WindowsStore,
  Winapi.CommonTypes, System.Win.WinRT, Winapi.Winrt, Winapi.ServicesRT.Store,{$ENDIF}
  FMX.Types, JVE.Browser, JVE.Messaging, JVE.Actions, System.TypInfo, JVE.Utils, FMX.Forms;

// To use PayPal you need PayPal credentials (the ones supplied as default in this
// unit are published sandbox credentials, which you cannot use in production).
//
// PPAPIApplicationId
// ~~~~~~~~~~~~~~~~~~
// Go to apps.paypal.com and login.
// Click New App under My Applications and fill your app data (important: tick
// Basic Payments under Adaptive Payments). If you don't choose to many extra
// items in that screen, your app should be automatically approved.
// On the main screen you will now have your Sandbox ID and Live App ID, set the
// PPAPIApplicationId property to one of them (set PPAPISendbox appropriately).
//
// PPAPIUserId, PPAPIPassword and PPAPISignature
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Now, go to www.paypal.com and login.
// To use PayPal API you must have a Premier or Business PayPal account (you can
// upgrade Personal to Premier for free in a minute by clicking Upgrade next
// to Account Type field on My Account => Overview, but I would suggest having
// separate personal and premier/business accounts).
// Go to My Account => Profile and choose API Access under Account Information.
// Choose Option 2 - Request API credentials.
// Choose the left option - Request API signature and click Agree and Submit.
// That's it. Copy the API Username, API Password and Signature to the matching
// properties and click Done.
//
// For Apple In-App Purchases to work, you must NOT use a wildcard bundle identifier
// and the CFBundleIdentifier should be set correctly in Project -> Options
type
  TJVEPaymentPlatform = (// Platforms, supported by the payment:
    ppMacStore, // Mac Store (i.e. desktop apps)
    ppAppStore, // App Store (i.e. iOS devices)
    ppPlay,     // Google Play (i.e. Android devices)
    ppPayPal,   // PayPal (supported on all devices; review app review guidelines!)
    ppWindows); // Windows Store (only available in some versions of Windows 10)
  TJVEPaymentPlatforms = set of TJVEPaymentPlatform;

  // The error is passed at the end of the process to the OnFinished event.
  // Only the last error is preserved, so if you start two purchases simultenously,
  // only one error will be reported (Ok for messaging).
  // Notice, in most cases a failure in one purchase will NOT abort the other:
  // OnPurchased event must always be listened!
  TJVEPaymentStatus = (
    psSuccess,         // All requests up to now were successfully processed
    psCancelled,       // User cancelled the purchase
    psInvalidPlatform, // Current platform is not included in the platforms list
    psInvalidId,       // Identifier (Product ID) is not recognized by the store
    psNotAllowed,      // The user is not allowed to purchase the item
    psNetworkError,    // Cannot connect to the store
    psUnexpectedError);// Should not normally occur, but Apple sometimes reports this

  TJVEPayment = class;
  TJVEPaymentItem = class;
  TJVEPaymentNotify = procedure(Payment: TJVEPayment; Item: TJVEPaymentItem) of object;
  TJVEPaymentPurchased = procedure(Payment: TJVEPayment;
    Item: TJVEPaymentItem; Receipt: String; PurchaseDate: TDateTime) of object;
  TJVEPaymentFinished = procedure(Payment: TJVEPayment;
    Status: TJVEPaymentStatus; const Error: String) of object;

  TJVEPPPaymentType = (ptDigitalGoods, ptGoods, ptService, ptGoodsExternal, ptServiceExternal);
  // ptDigitalGoods requires a special PayPal site, which works really bad in embedded
  // browsers in ALL platforms, but with different errors (amazing achievement!).
  // That's why I am opening an external browser for Digital Goods.
  // After the purchase is complete, retrying the buy (or restoring the purchase)
  // will recognize the completed payment.
  // It should be noted, though, that upon a completion of a payment the browser
  // will take the user to Google's site. You can fix it by providing your own URL,
  // in case Digital Goods are requested.
  // I would suggest, though, using Service, instead of Digital Goods - works much
  // more consistently; no need to provide extra explanations; stay within the app, etc.
  // Goods and Service External are same as regular, but open in external browser
  // similarly to Digital Goods.

  TJVEPPCurrencies = (// Currencies supported by PayPal, as of end of July, 2014
    pcAustralianDollar, pcCanadianDollar, pcEuro, pcBritishPound,
    pcJapaneseYen, pcUSDollar, pcNewZealandDollar, pcSwissFranc,
    pcHongKongDollar, pcSingaporeDollar, pcSwedishKrona, pcDanishKrone,
    pcPolishZloty, pcNorwegianKrone, pcHungarianForint, pcCzechKoruna,
    pcIsraeliNewShekel, pcMexicanPeso, pcBrazilianReal, pcMalaysianRinggit,
    pcPhilippinePeso, pcNewTaiwanDollar, pcThaiBaht, pcTurkishLira, pcRussianRuble);

  // This is an internal item state
  TJVEPaymentState = (pePassive, peRestoring, peBuying, peValidating);

  // Unlike other classes, due to stores' requirements (i.e.
  //   digital goods MUST be bought via an in-app purchase;
  //   non-digital goods must NOT be an in-app purchase),
  // this class will NOT fall back to PayPal automatically.
  // Use Platforms property to control supported platforms per item.
  TJVEPaymentItem = class(TCollectionItem)
  private
    FState: TJVEPaymentState;
    FPPPrice: Double;
    FPPCurrency: TJVEPPCurrencies;
    FPPTitle: String;
    FPPType: TJVEPPPaymentType;
    FiOSIdentifier: String;
    FMacIdentifier: String;
    FAndroidIdentifier: String;
    FWindowsToken: String;
    FPPIdentifier: String;
    FPersistent: Boolean;
    FPlatforms: TJVEPaymentPlatforms;
    FOnPurchased: TJVEPaymentPurchased;
    FOnCacheUpdated: TJVEPaymentNotify;
    FPurchased: Boolean;
    FPriceCache: String;
    FTitleCache: String;
    function GetTitle: String;
    function GetPurchased: Boolean;
    function IsPriceStored: Boolean;
    function GetIdentifier: String;
    procedure RepeatPurchase(Finished: Boolean);
    procedure SetIdentifier(const Value: String);
    procedure SetiOSIdentifier(const Value: String);
    procedure SetMacIdentifier(const Value: String);
    procedure SetAndroidIdentifier(const Value: String);
    procedure SetWindowsToken(const Value: String);
    procedure SetPPIdentifier(const Value: String);
    procedure SetPersistent(const Value: Boolean);
    procedure SetCurrency(const Value: TJVEPPCurrencies);
    procedure SetPrice(const Value: Double);
    procedure SetCache(const Price, Title: String);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function GetPayment: TJVEPayment;

    // Adds current item to the purchase queue. This is the prefered way
    // to start the purchase process.
    procedure Buy;

    // Persistent only: indicates whether the item is already bought.
    property Purchased: Boolean read GetPurchased;

    // Persistent only: flags an item as if it was already purchased.
    // This is useful, for example, when the user unlocked the item using
    // a coupon or some other means, outside the regular purchase process.
    procedure FlagAsPurchased;

    // If prices are cached, either user-friendly formatted price or,
    // if identifier is invalid, "-". Blank if the price is not cached or
    // if the persistent item is already purchased.
    // Call the TJVEPayment.CachePrices to asynchroneously update this.
    property PriceCache: String read FPriceCache;

    // This is public for the use in on-screen selection for actions.
    // Should not be normally used elsewhere.
    function GetDisplayName: string; override;
  published
    property Platforms: TJVEPaymentPlatforms read FPlatforms
      write FPlatforms default [ppMacStore, ppAppStore, ppPlay, ppPayPal, ppWindows];

    // Indicates that this item is not-consumable (subscriptions are not persistent!).
    // Notice, while Persistent in Apple stores actually refers to the user,
    // I implemented PayPal Persistency based on hardware (no access to user).
    // In Android all products are persistent, not Persistent indicates that
    // the component will automatically mark the item payment as consumed once
    // OnPurchased event returns.
    property Persistent: Boolean read FPersistent write SetPersistent default True;

    // This is a convinience and backward compatibility property, currently
    // hidden from the object inspector. Getter returns the currently applicable
    // identifier; setter sets all three identifiers below.
    property Identifier: String read GetIdentifier write SetIdentifier stored False;

    // The item identifier in external store.
    // Notice, PayPal purchase process is not "per publisher", so, for best
    // compatibility, try to use PayPal identifier, which other publishers and apps
    // are unlikely to use (for example, include your company and application names)
    property iOSIdentifier: String read FiOSIdentifier write SetiOSIdentifier;
    property MacIdentifier: String read FMacIdentifier write SetMacIdentifier;
    property AndroidIdentifier: String read FAndroidIdentifier write SetAndroidIdentifier;
    property WindowsToken: String read FWindowsToken write SetWindowsToken;
    property PPIdentifier: String read FPPIdentifier write SetPPIdentifier;

    // Below are PayPal only properties (since PayPal does not have products
    // repository, the products should be created within the app).
    property PPPrice: Double read FPPPrice write SetPrice stored IsPriceStored;
    property PPCurrency: TJVEPPCurrencies read FPPCurrency
      write SetCurrency default pcUSDollar;
    property PPType: TJVEPPPaymentType read FPPType write FPPType default ptService;

    // Set to the PayPal title of the item (continuing the above list of properties).
    // Read to get the title: if App/Mac Store was contacted (i.e. the PriceCache
    // is not empty), this will hold the localized title of the item.
    property Title: String read GetTitle write FPPTitle;

    // Called when a user-friendly item price is updated (might happen after a purchase
    // or a CachePrices call). Use to update user interface.
    property OnCacheUpdated: TJVEPaymentNotify read FOnCacheUpdated write FOnCacheUpdated;

    // Called when an item was purchased. You must always listen to this event,
    // even if you have not requested a purchase (if previous session did, then
    // failed, Apple delivers the product in the next session without any request).
    //
    // If the receipt is not available, an empty string will be passed;
    // if the date is not available, the current date will be passed
    // (this might happen if not supported or when the purchase is being restored).
    property OnPurchased: TJVEPaymentPurchased read FOnPurchased write FOnPurchased;
  end;

  TJVEPaymentItems = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TJVEPaymentItem;
  public
    property Items[Index: Integer]: TJVEPaymentItem read GetItem; default;
  end;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEPayment = class(TComponent, IJVEExecutable)
  private
    FLock: TCriticalSection;
    FThread: TThread;
    FDeviceId: String;
    FBrowser: TJVEBrowser;
    FNativelyRestoring: Boolean;
    FStyleChangedId: Integer;
    {$IF Defined(MSWINDOWS)}
    FWindowsStore: {$IF CompilerVersion < 33}TObject;{$ELSE}TWindowsStoreCore;
    procedure ProcessMessages;
    {$ENDIF}
    {$ENDIF}
    procedure Locked(Proc: TProc);
    procedure PayPalClosed(Browser: TJVEBrowser);
    procedure PayPalError(Browser: TJVEBrowser;
      const Error: String; var DefaultProc: Boolean);
    procedure PayPalStarting(Browser: TJVEBrowser;
      const URL: string; var Action: TJVEBrowserAction);
  private
    FItems: TJVEPaymentItems;
    FOnFinished: TJVEPaymentFinished;
    FAPIUserId: String;
    FAPIApplicationId: String;
    FAPISignature: String;
    FAPIPassword: String;
    FRecipientEmail: String;
    FAPISandbox: Boolean;
    FAutoTranslate: Boolean;
    FShowShade: Boolean;
    FShadeIdentifier: Integer;
    FAndroidLicense: String;
    function GetIsBusy: Boolean;
    function IsAPIApplicationIdStored: Boolean;
    function IsAPIPasswordStored: Boolean;
    function IsAPISignatureStored: Boolean;
    function IsAPIUserIdStored: Boolean;
    procedure SetAndroidLicense(const Value: String);
    procedure SetItems(const Value: TJVEPaymentItems);
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
  protected
    FLastError: String;
    FLastStatus: TJVEPaymentStatus;
    procedure DoFinished;
    // IJVEExecutable support
    procedure Open(Sender: TJVEAction);
    function CanOpen(Sender: TJVEAction): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;

    // Restore previously made purchases. Only restores persistent items
    // with PayPal it also restores the very last successful non-persisten item,
    // thus supporting Digital Goods purchase in external browser.
    procedure RestorePurchases;

    // Caches the prices of the items; see PriceCache property, above.
    procedure CachePrices;

    // Convenience method for buying by Identifier, rather than via TJVEPaymentItem.
    // For safer processing use the TJVEPaymentItem function.
    // Returns False if the identifier was not found; expect no events in this case;
    // if this function returns True, OnFinished event will be called.
    function Buy(Id: String): Boolean;

    // Indicates whether OS native in-app purchases are enabled (PayPal is always enabled).
    class function CanBuyNatively: Boolean;

    // Indicates whether currently buying or restoring. Do NOT ever update
    // items while this is True, particularly from secondary threads.
    property IsBusy: Boolean read GetIsBusy;
  published
    property Items: TJVEPaymentItems read FItems write SetItems;

    // Specifies whether the app is shaded during the purchase process.
    property ShowShade: Boolean read FShowShade write FShowShade default True;

    // These are the API credentials. You must replace them (the defaults belong
    // to a published sandbox account and could not be used in production).
    // Go to x.com, register there are request AdaptivePayments credentials.
    property PPAPIApplicationId: String read FAPIApplicationId
      write FAPIApplicationId stored IsAPIApplicationIdStored;
    property PPAPIUserId: String read FAPIUserId
      write FAPIUserId stored IsAPIUserIdStored;
    property PPAPIPassword: String read FAPIPassword
      write FAPIPassword stored IsAPIPasswordStored;
    property PPAPISignature: String read FAPISignature
      write FAPISignature stored IsAPISignatureStored;
    property PPAPISandbox: Boolean read FAPISandbox write FAPISandbox default True;

    // This is the email address of the payments recipient. Normally - App publisher.
    property PPRecipientEmail: String read FRecipientEmail write FRecipientEmail;

    // This is the Android's application license key.
    property AndroidLicense: String read FAndroidLicense write SetAndroidLicense;

    // This method is guaranteed to be called after every Busy period.
    // For error code - see above. You should present an error message,
    // unless the status is psSuccess or psCancelled.
    property OnFinished: TJVEPaymentFinished read FOnFinished write FOnFinished;
  end;

// These might be of some use outside this unit, so they are left public
const
  JVEPPCurrencyCodes: array[TJVEPPCurrencies] of String = (
    'AUD', 'CAD', 'EUR', 'GBP', 'JPY', 'USD', 'NZD', 'CHF', 'HKD', 'SGD', 'SEK', 'DKK',
    'PLN', 'NOK', 'HUF', 'CZK', 'ILS', 'MXN', 'BRL', 'MYR', 'PHP', 'TWD', 'THB', 'TRY',
    'RUB');

  JVEPPCurrencyLocales: array[TJVEPPCurrencies] of String = (
    'en-AU', 'en-CA', 'de-DE', 'en-GB', 'ja-JP', 'en-US', 'en-NZ', 'de-CH',
    'zh-HK', 'zh-SG', 'sv-SE', 'da-DK', 'pl-PL', 'nb-NO', 'hu-HU', 'cs-CZ',
    'he-IL', 'es-MX', 'pt-BR', 'ms-MY', 'en-PH', 'zh-TW', 'th-TH', 'tr-TR',
    'ru-RU');

implementation

uses JVE.OpenURL, JVE.Configuration, JVE.Shade, System.StrUtils
  {$IF Defined(IOS)}, Posix.Dlfcn, iOSapi.CocoaTypes, iOSapi.Foundation,
    Macapi.ObjectiveC, System.Generics.Collections, Macapi.Helpers
  {$ELSEIF Defined(MACOS)}, Posix.Dlfcn, Macapi.CocoaTypes, Macapi.Foundation,
    Macapi.ObjectiveC, System.Generics.Collections, Macapi.Helpers, Posix.Unistd
  {$ELSEIF Defined(ANDROID)}, FMX.Helpers.Android, FMX.InAppPurchase, AndroidApi.Consts,
    AndroidApi.Helpers, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.GraphicsContentViewText,
    System.Generics.Collections
  {$ELSE}, FMX.Platform
  {$ENDIF};

const
  SandboxAPIApplicationId = 'APP-80W284485P519543T';
  SandboxAPIUserId = 'jb-us-seller_api1.paypal.com';
  SandboxAPIPassword = 'WX4WTU3S8MY44S7F';
  SandboxAPISignature = 'AFcWxV21C7fd0v3bYYYRCpSSRl31A7yDhhsPUU2XhtMoZXsWHFxu-RWy';

type
  TPayPalData = (pdStatus, pdRedirect, pdKey);
  TPayPalThread = class(TThread)
  private
    FPayment: TJVEPayment;
    function Call(const Request: String): String;
    function FindId(const Id: String; Data: TPayPalData): String;
    function GetNextItem: TJVEPaymentItem;
    function ValidateDeviceId: String;
    function TypeString(Item: TJVEPaymentItem): String;
    function StatusBought(const Status: String): Boolean;
    procedure BuyItem(Item: TJVEPaymentItem);
  protected
    procedure Execute; override;
  public
    constructor Create(Payment: TJVEPayment);
    class function Hash(const Input: String): String;
  end;

{$IF Defined(MACOS)}

const
  SKErrorUnknown = 0;
  SKErrorClientInvalid = 1;
  SKErrorPaymentCancelled = 2;
  SKErrorPaymentInvalid = 3;
  SKErrorPaymentNotAllowed = 4;
  SKPaymentTransactionStatePurchasing = 0;
  SKPaymentTransactionStatePurchased = 1;
  SKPaymentTransactionStateFailed = 2;
  SKPaymentTransactionStateRestored = 3;

type
  SKPaymentClass = interface;
  SKPayment = interface;
  SKPaymentQueueClass = interface;
  SKPaymentQueue = interface;
  SKPaymentTransactionClass = interface;
  SKPaymentTransaction = interface;
  SKProductClass = interface;
  SKProduct = interface;
  SKRequestClass = interface;
  SKRequest = interface;
  SKProductsRequestClass = interface;
  SKProductsRequest = interface;
  SKProductsResponseClass = interface;
  SKProductsResponse = interface;
  SKPaymentTransactionObserver = interface;
  SKRequestDelegate = interface;
  SKProductsRequestDelegate = interface;

  SKPaymentClass = interface(NSObjectClass)
    function paymentWithProduct(product: SKProduct): SKPayment; cdecl;
  end;
  SKPayment = interface(NSObject)
    function productIdentifier: NSString; cdecl;
  end;
  TSKPayment = class(TOCGenericImport<SKPaymentClass, SKPayment>) end;

  SKPaymentQueueClass = interface(NSObjectClass)
    function defaultQueue: SKPaymentQueue; cdecl;
    function canMakePayments: Boolean; cdecl;
  end;
  SKPaymentQueue = interface(NSObject)
    procedure addPayment(payment: SKPayment); cdecl;
    procedure restoreCompletedTransactions; cdecl;
    procedure finishTransaction(transaction: SKPaymentTransaction); cdecl;
    procedure addTransactionObserver(observer: Pointer); cdecl;
    procedure removeTransactionObserver(observer: Pointer); cdecl;
    function transactions: NSArray; cdecl;
  end;
  TSKPaymentQueue = class(TOCGenericImport<SKPaymentQueueClass, SKPaymentQueue>) end;

  SKPaymentTransactionClass = interface(NSObjectClass)
  end;
  SKPaymentTransaction = interface(NSObject)
    function error: NSError; cdecl;
    function originalTransaction: SKPaymentTransaction; cdecl;
    function payment: SKPayment; cdecl;
    function transactionDate: NSDate; cdecl;
    function transactionIdentifier: NSString; cdecl;
    function transactionState: NSInteger; cdecl;
    function transactionReceipt: NSData; cdecl;
  end;
  TSKPaymentTransaction = class(TOCGenericImport
    <SKPaymentTransactionClass, SKPaymentTransaction>) end;

  SKProductClass = interface(NSObjectClass)
  end;
  SKProduct = interface(NSObject)
    function localizedDescription: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    function price: NSDecimalNumber; cdecl;
    function priceLocale: NSLocale cdecl;
    function productIdentifier: NSString; cdecl;
  end;
  TSKProduct = class(TOCGenericImport<SKProductClass, SKProduct>) end;

  SKRequestClass = interface(NSObjectClass)
  end;
  SKRequest = interface(NSObject)
    function delegate: SKRequestDelegate; cdecl;
    procedure setDelegate(delegate: SKProductsRequestDelegate); cdecl;
    procedure cancel; cdecl;
    procedure start; cdecl;
  end;
  TSKRequest = class(TOCGenericImport<SKRequestClass, SKRequest>) end;

  SKProductsRequestClass = interface(SKRequestClass)
  end;
  SKProductsRequest = interface(SKRequest)
    function initWithProductIdentifiers(productIds: NSSet): SKProductsRequest; cdecl;
    function delegate: SKProductsRequestDelegate; cdecl;
    procedure setDelegate(delegate: Pointer); cdecl;
  end;
  TSKProductsRequest = class(TOCGenericImport
    <SKProductsRequestClass, SKProductsRequest>) end;

  SKProductsResponseClass = interface(NSObjectClass)
  end;
  SKProductsResponse = interface(NSObject)
    function products: NSArray; cdecl;
    function invalidProductIdentifiers: NSArray; cdecl;
  end;
  TSKProductsResponse = class(TOCGenericImport
    <SKProductsResponseClass, SKProductsResponse>) end;

  SKPaymentTransactionObserver = interface(IObjectiveC)
    procedure paymentQueue(queue: SKPaymentQueue;
      updatedTransactions: NSArray); overload; cdecl;
    procedure paymentQueue(queue: SKPaymentQueue;
      restoreCompletedTransactionsFailedWithError: NSError); overload; cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); cdecl;
  end;

  SKRequestDelegate = interface(IObjectiveC)
    procedure requestDidFinish(request: SKRequest); cdecl;
    procedure request(request: SKRequest; didFailWithError: NSError); cdecl;
  end;

  SKProductsRequestDelegate = interface(SKRequestDelegate)
    procedure productsRequest(request: SKProductsRequest;
      didReceiveResponse: SKProductsResponse); cdecl;
  end;

  TItemFunc = reference to function(Item: TJVEPaymentItem): Boolean;
  TPaymentFunc = reference to function(Item: TJVEPayment): Boolean;

  // This is a workaround for an inability of TList to store weak references.
  TWeakJVEPayment = class
    [Weak]FPayment: TJVEPayment;
  end;

  TJVEStoreKitDelegate = class(TOCLocal,
    SKPaymentTransactionObserver, SKProductsRequestDelegate)
  protected
    FGlobalRestoring: Boolean;
    FPayments: TList<TWeakJVEPayment>;
    FBuying: TList<String>;
    FRequesting: TList<String>;
    FProducts: NSMutableDictionary;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Buy(Identifier: String);
    procedure RequestIdentifiers(ForceId: String = '');
    procedure ScanItems(ItemProc: TItemFunc; PaymentProc: TPaymentFunc);
    procedure AddListener(Listener: TJVEPayment);
    procedure RemoveListener(Listener: TJVEPayment);
    function CanBuyNatively: Boolean;
  public
    procedure requestDidFinish(request: SKRequest); cdecl;
    procedure request(request: SKRequest; didFailWithError: NSError); cdecl;
    procedure productsRequest(request: SKProductsRequest;
      didReceiveResponse: SKProductsResponse); cdecl;
    procedure paymentQueue(queue: SKPaymentQueue;
      updatedTransactions: NSArray); overload; cdecl;
    procedure paymentQueue(queue: SKPaymentQueue;
      restoreCompletedTransactionsFailedWithError: NSError); overload; cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished(queue: SKPaymentQueue); cdecl;
    function paymentQueue(queue: SKPaymentQueue; shouldAddStorePayment: SKPayment;
      forProduct: SKProduct): Boolean; overload; cdecl;
  end;

var
  GlobalDelegate: TJVEStoreKitDelegate;

constructor TJVEStoreKitDelegate.Create;
begin
  inherited Create;
  FBuying := TList<String>.Create;
  FRequesting := TList<String>.Create;
  FPayments := TList<TWeakJVEPayment>.Create;

  // Using Cocoa because of some nasty reference counting bug.
  // The app will fail, if we replace this with TList or something.
  FProducts := TNSMutableDictionary.Create;
  if CanBuyNatively then
    TSKPaymentQueue.OCClass.defaultQueue.addTransactionObserver(Self.GetObjectID);
end;

destructor TJVEStoreKitDelegate.Destroy;
begin
  TSKPaymentQueue.OCClass.defaultQueue.removeTransactionObserver(Self.GetObjectID);
  FreeAndNil(FPayments);
  FreeAndNil(FRequesting);
  FreeAndNil(FBuying);
  FProducts := nil;
  inherited Destroy;
end;

procedure TJVEStoreKitDelegate.ScanItems(ItemProc: TItemFunc; PaymentProc: TPaymentFunc);
var
  WeakPayment: TWeakJVEPayment;
  Payment: TJVEPayment;
  CollectionItem: TCollectionItem;
  Item: TJVEPaymentItem absolute CollectionItem;
  Match: Boolean;
begin
  for WeakPayment in FPayments do
  begin
    Payment := WeakPayment.FPayment;
    if Payment = nil then Continue;

    Match := False;
    for CollectionItem in Payment.FItems do
      {$IFDEF IOS}
      if ppAppStore in Item.FPlatforms then
      {$ELSE}
      if ppMacStore in Item.FPlatforms then
      {$ENDIF}
        Match := Match or ItemProc(Item);

    if Match and (not Assigned(PaymentProc) or PaymentProc(Payment)) then
      Break;
  end;
end;

procedure TJVEStoreKitDelegate.RequestIdentifiers(ForceId: String = '');
var
  Ids: NSMutableSet;
  Request: SKProductsRequest;
begin
  Ids := TNSMutableSet.Create;
  if ForceId <> '' then
    Ids.addObject(PointerNSSTR(ForceId));

  ScanItems(function(Item: TJVEPaymentItem): Boolean
  var
    Id: Pointer;
    Ident: String;
  begin
    Ident := {$IFDEF IOS}Item.FiOSIdentifier{$ELSE}Item.FMacIdentifier{$ENDIF};
    Id := PointerNSSTR(Ident);
    if not Item.GetPurchased and not FRequesting.Contains(Ident) and
      (FProducts.objectForKey(Id) = nil) then
    begin
      Ids.addObject(Id);
      FRequesting.Add(Ident);
    end;
    Result := False;
  end, nil);

  if Ids.count <> 0 then
  begin
    Request := TSKProductsRequest.Alloc.initWithProductIdentifiers(Ids);
    Request.setDelegate(Self.GetObjectID);
    Request.start;
  end;
end;

procedure TJVEStoreKitDelegate.Buy(Identifier: String);
var
  Data: Pointer;
begin
  if FBuying.Contains(Identifier) then
    Exit;

  Data := FProducts.objectForKey(PointerNSSTR(Identifier));
  if Data <> nil then
  begin
    FBuying.Add(Identifier);
    TSKPaymentQueue.OCClass.defaultQueue.addPayment(
      TSKPayment.OCClass.paymentWithProduct(TSKProduct.Wrap(Data)));
    Exit;
  end;

  if FRequesting.Contains(Identifier) then
    Exit;

  FRequesting.Add(Identifier);
  RequestIdentifiers(Identifier);
end;

function TJVEStoreKitDelegate.CanBuyNatively: Boolean;
begin
  Result := TSKPaymentQueue.OCClass.canMakePayments;
end;

procedure TJVEStoreKitDelegate.AddListener(Listener: TJVEPayment);
var
  Weak: TWeakJVEPayment;
begin
  Weak := TWeakJVEPayment.Create;
  Weak.FPayment := Listener;
  FPayments.Add(Weak);
end;

procedure TJVEStoreKitDelegate.RemoveListener(Listener: TJVEPayment);
var
  Index: Integer;
begin
  for Index := FPayments.Count - 1 downto 0 do
    if (FPayments[Index].FPayment = Listener) or (FPayments[Index].FPayment = nil) then
      FPayments.Delete(Index);
end;

procedure TJVEStoreKitDelegate.requestDidFinish(request: SKRequest);
begin
end;

procedure TJVEStoreKitDelegate.request(request: SKRequest; didFailWithError: NSError);
begin
  FBuying.Clear;
  FRequesting.Clear;

  ScanItems(function(Item: TJVEPaymentItem): Boolean
  begin
    Result := Item.FState <> pePassive;
    if Result then
      Item.FState := pePassive;
  end, function(Payment: TJVEPayment): Boolean
  begin
    Payment.FLastError := Format('%s (%s, %d)', [FromNSSTR(didFailWithError.
      localizedDescription), FromNSSTR(didFailWithError.domain), didFailWithError.code]);
    Payment.FLastStatus := psNetworkError;
    Payment.DoFinished;
    Result := False;
  end);
end;

function TJVEStoreKitDelegate.paymentQueue(queue: SKPaymentQueue;
  shouldAddStorePayment: SKPayment; forProduct: SKProduct): Boolean;
begin
  Result := True;
end;

procedure TJVEStoreKitDelegate.productsRequest(request: SKProductsRequest;
  didReceiveResponse: SKProductsResponse);
var
  Index: Integer;
  Product: SKProduct;
  Formatter: NSNumberFormatter;
  Id, Price, Title: String;
  Data: Pointer;
  Buy: Boolean;
begin
  if didReceiveResponse.products.count <> 0 then
  begin
    Formatter := TNSNumberFormatter.Create;
    Formatter.setFormatterBehavior(NSNumberFormatterBehavior10_4);
    Formatter.setNumberStyle(NSNumberFormatterCurrencyStyle);

    for Index := 0 to didReceiveResponse.products.count - 1 do
    begin
      Data := didReceiveResponse.products.objectAtIndex(Index);
      Product := TSKProduct.Wrap(Data);
      FProducts.setObject(Data, PointerNSObject(Product.productIdentifier));

      Id := FromNSSTR(Product.productIdentifier);
      FRequesting.Remove(Id);

      Formatter.setLocale(Product.priceLocale);
      Price := FromNSSTR(Formatter.stringFromNumber(Product.price));
      Title := FromNSSTR(Product.localizedTitle);

      Buy := False;
      ScanItems(function(Item: TJVEPaymentItem): Boolean
      begin
        Result := False;
        if Item.{$IFDEF IOS}FiOSIdentifier{$ELSE}FMacIdentifier{$ENDIF} = Id then
        begin
          Item.SetCache(Price, Title);
          Buy := Buy or (Item.FState <> pePassive);
        end;
      end, nil);

      if Buy then
      begin
        FBuying.Add(Id);
        TSKPaymentQueue.OCClass.defaultQueue.addPayment(
          TSKPayment.OCClass.paymentWithProduct(Product));
      end;
    end;
  end;

  for Index := 0 to didReceiveResponse.invalidProductIdentifiers.count - 1 do
  begin
    Id := FromNSSTR(didReceiveResponse.invalidProductIdentifiers.objectAtIndex(Index));
    FRequesting.Remove(Id);

    ScanItems(function(Item: TJVEPaymentItem): Boolean
    begin
      if Item.{$IFDEF IOS}FiOSIdentifier{$ELSE}FMacIdentifier{$ENDIF} = Id then
      begin
        Item.SetCache('-', '');
        Result := Item.FState <> pePassive;
        if Result then
          Item.FState := pePassive;
      end else
        Result := False;
    end, function(Payment: TJVEPayment): Boolean
    begin
      Payment.FLastError := 'Invalid Identifier: ' + Id;
      Payment.FLastStatus := psInvalidId;
      Payment.DoFinished;
      Result := False;
    end);
  end;
end;

procedure TJVEStoreKitDelegate.paymentQueue(queue: SKPaymentQueue;
  updatedTransactions: NSArray);
var
  Index: Integer;
  Transaction: SKPaymentTransaction;
  Id, Receipt: String;
  Done: Boolean;
  Data: NSData;
begin
  for Index := Integer(updatedTransactions.count) - 1 downto 0 do
  begin
    Transaction := TSKPaymentTransaction.Wrap(updatedTransactions.objectAtIndex(Index));
    if Transaction.transactionState = SKPaymentTransactionStatePurchasing then
      Continue;

    Id := FromNSSTR(Transaction.payment.productIdentifier);
    FBuying.Remove(Id);

    if Transaction.transactionState <> SKPaymentTransactionStateFailed then
    begin
      Done := False;
      ScanItems(function(Item: TJVEPaymentItem): Boolean
      var
        AnalyticsId: String;
        Idx: Integer;
      begin
        Result := Item.{$IFDEF IOS}FiOSIdentifier{$ELSE}FMacIdentifier{$ENDIF} = Id;
        if Result then
        begin
          Item.FState := pePassive;
          if not Item.GetPurchased or not Item.FPersistent then
          begin
            Item.FlagAsPurchased;
            if Assigned(Item.FOnPurchased) then
            begin
              Data := Transaction.transactionReceipt;
              Receipt := '';
              if Data <> nil then
                for Idx := 0 to Data.length - 1 do
                  Receipt := Receipt + Char(PByteArray(Data.bytes)[Idx]);
              Item.FOnPurchased(Item.GetPayment, Item, Receipt,
                NSDateToDateTime(Transaction.transactionDate));
            end;

            // We are not reporting restored purchases
            if Transaction.transactionState <> SKPaymentTransactionStatePurchased then
            begin
              AnalyticsId := IntToStr(Random(MaxInt));
              TMessageManager.DefaultManager.SendMessage(Self,
                TJVETrackTransaction.Create(AnalyticsId, '', Item.FPPPrice,
                0, 0, JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
              TMessageManager.DefaultManager.SendMessage(Self,
                TJVETrackItem.Create(AnalyticsId, Id, Item.FPPPrice,
                1, '', '', JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
            end;
          end;
        end;
      end, function(Payment: TJVEPayment): Boolean
      begin
        Payment.DoFinished;
        Result := False;
        Done := True;
      end);

      if Done then
        TSKPaymentQueue.OCClass.defaultQueue.finishTransaction(Transaction);
    end else
      ScanItems(function(Item: TJVEPaymentItem): Boolean
      begin
        Result := (Item.{$IFDEF IOS}FiOSIdentifier{$ELSE}
          FMacIdentifier{$ENDIF} = Id) and (Item.FState <> pePassive);
        if Result then
          Item.FState := pePassive;
      end, function(Payment: TJVEPayment): Boolean
      begin
        Payment.FLastError := Format('%s (%s, %d)', [FromNSSTR(Transaction.error.
          localizedDescription), FromNSSTR(Transaction.error.domain), Transaction.error.code]);
        case Transaction.error.code of
          SKErrorClientInvalid: Payment.FLastStatus := psNotAllowed;
          SKErrorPaymentCancelled: Payment.FLastStatus := psCancelled;
          SKErrorPaymentInvalid: Payment.FLastStatus := psInvalidId;
          SKErrorPaymentNotAllowed: Payment.FLastStatus := psNotAllowed;
        else
          Payment.FLastStatus := psUnexpectedError;
        end;
        Payment.DoFinished;
        Result := False;
      end);
  end;
end;

procedure TJVEStoreKitDelegate.paymentQueue(queue: SKPaymentQueue;
  restoreCompletedTransactionsFailedWithError: NSError);
var
  WeakPayment: TWeakJVEPayment;
  Payment: TJVEPayment;
  Error: String;
  Status: TJVEPaymentStatus;
begin
  Error := Format('%s (%s, %d)', [FromNSSTR(restoreCompletedTransactionsFailedWithError.
    localizedDescription), FromNSSTR(restoreCompletedTransactionsFailedWithError.domain),
    restoreCompletedTransactionsFailedWithError.code]);
  case restoreCompletedTransactionsFailedWithError.code of
    SKErrorClientInvalid: Status := psNotAllowed;
    SKErrorPaymentCancelled: Status := psCancelled;
    SKErrorPaymentInvalid: Status := psInvalidId;
    SKErrorPaymentNotAllowed: Status := psNotAllowed;
  else
    Status := psUnexpectedError;
  end;

  FGlobalRestoring := False;
  for WeakPayment in FPayments do
  begin
    Payment := WeakPayment.FPayment;
    if (Payment <> nil) and Payment.FNativelyRestoring then
    begin
      Payment.FLastError := Error;
      Payment.FLastStatus := Status;
      Payment.FNativelyRestoring := False;
      Payment.DoFinished;
    end;
  end;
end;

procedure TJVEStoreKitDelegate.paymentQueueRestoreCompletedTransactionsFinished(
  queue: SKPaymentQueue);
var
  WeakPayment: TWeakJVEPayment;
  Payment: TJVEPayment;
begin
  FGlobalRestoring := False;
  for WeakPayment in FPayments do
  begin
    Payment := WeakPayment.FPayment;
    if (Payment <> nil) and Payment.FNativelyRestoring then
    begin
      Payment.FNativelyRestoring := False;
      Payment.DoFinished;
    end;
  end;
end;

{$ELSEIF Defined(ANDROID) AND (CompilerVersion >= 27)}   // Delphi XE6+

  TItemFunc = reference to function(Item: TJVEPaymentItem): Boolean;
  TPaymentFunc = reference to function(Item: TJVEPayment): Boolean;

  // This is a workaround for an inability of TList to store weak references.
  TWeakJVEPayment = class
    [Weak]FPayment: TJVEPayment;
  end;

  TJVEInAppListener = class
  protected
    FPayments: TList<TWeakJVEPayment>;
    FBuying: TList<String>;
    FRequesting: TList<String>;
    FProducts: TDictionary<String, TProduct>;
    FPurchaseD: TInAppPurchase;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Buy(Identifier: String);
    procedure RequestIdentifiers(ForceId: String = '');
    procedure ScanItems(ItemProc: TItemFunc; PaymentProc: TPaymentFunc);
    procedure AddListener(Listener: TJVEPayment);
    procedure RemoveListener(Listener: TJVEPayment);
    function IsProductPurchased(Identifier: String): Boolean;
    function CanBuyNatively: Boolean;
  private
    procedure RecordTransaction(Sender: TObject; const ProductID,
      TransactionID: string; TransactionDate: TDateTime);
    procedure ProductsRequestResponse(Sender: TObject;
      const Products: TIAPProductList; const InvalidProductIDs: TStrings);
    procedure Error(Sender: TObject; ErrorKind: TFailureKind;
      const ErrorMessage: String);
  end;

var
  GlobalListener: TJVEInAppListener;

constructor TJVEInAppListener.Create;
begin
  inherited Create;
  FBuying := TList<String>.Create;
  FRequesting := TList<String>.Create;
  FPayments := TList<TWeakJVEPayment>.Create;
  FPurchaseD := TInAppPurchase.Create(nil);

  FProducts := TDictionary<String, TProduct>.Create;
  FPurchaseD.OnRecordTransaction := RecordTransaction;
  FPurchaseD.OnError := Error;
  FPurchaseD.OnProductsRequestResponse := ProductsRequestResponse;
  FPurchaseD.SetupInAppPurchase;
end;

destructor TJVEInAppListener.Destroy;
begin
  FreeAndNil(FProducts);
  FreeAndNil(FPurchaseD);
  FreeAndNil(FPayments);
  FreeAndNil(FRequesting);
  FreeAndNil(FBuying);
  inherited Destroy;
end;

function TJVEInAppListener.IsProductPurchased(Identifier: string): Boolean;
begin
  Result := FPurchased.IsSetupComplete and FPurchased.IsProductPurchased(Identifier);
end;

procedure TJVEInAppListener.ScanItems(ItemProc: TItemFunc; PaymentProc: TPaymentFunc);
var
  WeakPayment: TWeakJVEPayment;
  Payment: TJVEPayment;
  CollectionItem: TCollectionItem;
  Item: TJVEPaymentItem absolute CollectionItem;
  Match: Boolean;
begin
  for WeakPayment in FPayments do
  begin
    Payment := WeakPayment.FPayment;
    if Payment = nil then Continue;

    Match := False;
    for CollectionItem in Payment.FItems do
      if ppPlay in Item.FPlatforms then
        Match := Match or ItemProc(Item);

    if Match and (not Assigned(PaymentProc) or PaymentProc(Payment)) then
      Break;
  end;
end;

procedure TJVEInAppListener.RequestIdentifiers(ForceId: String = '');
var
  Size: Integer;
begin
  if not FPurchaseD.IsSetupComplete then
  begin
    ExecuteInIdle(procedure begin RequestIdentifiers(ForceId); end);
    Exit;
  end;

  Size := FPurchaseD.ProductIDs.Count;
  if ForceId <> '' then
    FPurchaseD.ProductIDs.Add(ForceId);

  ScanItems(function(Item: TJVEPaymentItem): Boolean
  var
    Ident: String;
  begin
    Ident := Item.GetIdentifier;
    if not Item.GetPurchased and not FRequesting.Contains(Ident) and
      not (FProducts.ContainsKey(Ident)) then
    begin
      FPurchaseD.ProductIDs.Add(Ident);
      FRequesting.Add(Ident);
    end;
    Result := False;
  end, nil);

  if Size < FPurchaseD.ProductIDs.Count then
    FPurchaseD.QueryProducts;
end;

procedure TJVEInAppListener.Buy(Identifier: String);
begin
  if FBuying.Contains(Identifier) then
    Exit;

  if FPurchaseD.IsSetupComplete then
  begin
    FBuying.Add(Identifier);
    FPurchaseD.PurchaseProduct(Identifier);
  end else
    ExecuteInIdle(procedure begin Buy(Identifier); end);
end;

function TJVEInAppListener.CanBuyNatively: Boolean;
begin
  Result := FPurchaseD.CanMakeInAppPurchases;
end;

procedure TJVEInAppListener.AddListener(Listener: TJVEPayment);
var
  Weak: TWeakJVEPayment;
begin
  Weak := TWeakJVEPayment.Create;
  Weak.FPayment := Listener;
  FPayments.Add(Weak);
end;

procedure TJVEInAppListener.RemoveListener(Listener: TJVEPayment);
var
  Index: Integer;
begin
  for Index := FPayments.Count - 1 downto 0 do
    if (FPayments[Index].FPayment = Listener) or (FPayments[Index].FPayment = nil) then
      FPayments.Delete(Index);
end;

procedure TJVEInAppListener.RecordTransaction(Sender: TObject;
  const ProductID, TransactionID: string; TransactionDate: TDateTime);
begin
  FBuying.Remove(ProductId);

  ScanItems(function(Item: TJVEPaymentItem): Boolean
  var
    AnalyticsId: String;
  begin
    Result := Item.GetIdentifier = ProductId;
    if Result then
    begin
      Item.FState := pePassive;
      if not Item.GetPurchased or not Item.FPersistent then
      begin
        Item.FlagAsPurchased;
        if Assigned(Item.FOnPurchased) then
          Item.FOnPurchased(Item.GetPayment, Item, TransactionID, TransactionDate);
        if not Item.FPersistent then
          FPurchaseD.ConsumeProduct(ProductID);

        // We are not reporting restored purchases
        if Now - TransactionDate < 1 then
        begin
          AnalyticsId := IntToStr(Random(MaxInt));
          TMessageManager.DefaultManager.SendMessage(Self,
            TJVETrackTransaction.Create(AnalyticsId, '', Item.FPPPrice,
            0, 0, JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
          TMessageManager.DefaultManager.SendMessage(Self,
            TJVETrackItem.Create(AnalyticsId, ProductId, Item.FPPPrice,
            1, '', '', JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
        end;
      end;
    end;
  end, function(Payment: TJVEPayment): Boolean
  begin
    Payment.DoFinished;
    Result := False;
  end);
end;

procedure TJVEInAppListener.ProductsRequestResponse(Sender: TObject;
  const Products: TIAPProductList; const InvalidProductIDs: TStrings);
var
  Index: Integer;
  Product: TProduct;
  Buy: Boolean;
  WeakPayment: TWeakJVEPayment;
  Payment: TJVEPayment;
begin
  if Products.Count <> 0 then
  begin
    for Index := 0 to Products.Count - 1 do
    begin
      Product := Products.Items[Index];
      FProducts.AddOrSetValue(Product.ProductID, Product);
      FRequesting.Remove(Product.ProductID);

      Buy := False;
      ScanItems(function(Item: TJVEPaymentItem): Boolean
      begin
        Result := False;
        if (ppPlay in Item.FPlatforms) and (Item.FAndroidIdentifier = Product.ProductID) then
        begin
          Item.SetCache(Product.LocalizedPrice, Product.LocalizedTitle);
          if Item.GetPayment.FNativelyRestoring and not Item.FPurchased and
            FPurchaseD.IsProductPurchased(Product.ProductID) then
          begin
            Item.FlagAsPurchased;
            if Assigned(Item.FOnPurchased) then
              Item.FOnPurchased(Item.GetPayment, Item, '', Now);
          end else
            Buy := Buy or (Item.FState <> pePassive);
        end;
      end, nil);

      if Buy then
      begin
        FBuying.Add(Product.ProductID);
        CallInUIThread(procedure
        begin
          try
            FPurchaseD.PurchaseProduct(Product.ProductID);
          except
            on E: Exception do
              Exit
          end;
        end);
      end;
    end;
  end;

  if InvalidProductIDs <> nil then
    for Index := 0 to InvalidProductIDs.Count - 1 do
    begin
      FRequesting.Remove(InvalidProductIDs[Index]);

      ScanItems(function(Item: TJVEPaymentItem): Boolean
      begin
        if Item.FAndroidIdentifier = InvalidProductIDs[Index] then
        begin
          Item.SetCache('-', '');
          Result := Item.FState <> pePassive;
          if Result then
            Item.FState := pePassive;
        end else
          Result := False;
      end, function(Payment: TJVEPayment): Boolean
      begin
        Payment.FLastError := 'Invalid Identifier: ' + InvalidProductIDs[Index];
        Payment.FLastStatus := psInvalidId;
        Payment.DoFinished;
        Result := False;
      end);
    end;

  for WeakPayment in FPayments do
  begin
    Payment := WeakPayment.FPayment;
    if (Payment <> nil) and Payment.FNativelyRestoring then
    begin
      Payment.FNativelyRestoring := False;
      Payment.DoFinished;
    end;
  end;
end;

procedure TJVEInAppListener.Error(Sender: TObject; ErrorKind: TFailureKind;
  const ErrorMessage: String);
var
  Status: TJVEPaymentStatus;
begin
  FBuying.Clear;
  FRequesting.Clear;

  if ErrorKind = TFailureKind.ProductsRequest then
  begin  // Request failure
    ScanItems(function(Item: TJVEPaymentItem): Boolean
    begin
      Result := Item.FState <> pePassive;
      if Result then
        Item.FState := pePassive;
    end, function(Payment: TJVEPayment): Boolean
    begin
      Payment.FLastError := ErrorMessage;
      Payment.FLastStatus := psNetworkError;
      Payment.DoFinished;
      Result := False;
    end);
  end
  else
  begin
    // Not a bright idea, but FMX.InAppPurchase does not provide error code
    if ErrorMessage.Contains(SBuyFailure) or ErrorMessage.Contains(SPurchaseProblem) or
      ErrorMessage.Contains(SItemAlreadyOwned) or ErrorMessage.Contains(SVerificationFailed) then
        Status := psNotAllowed
    else if ErrorMessage.Contains(SUserCanceled) then
      Status := psCancelled
    else if ErrorMessage.Contains(SItemUnavailable) then
      Status := psInvalidId
    else
      Status := psUnexpectedError;

    ScanItems(function(Item: TJVEPaymentItem): Boolean
    begin
      Result := Item.FState <> pePassive;
      if Result then
        Item.FState := pePassive;
    end, function(Payment: TJVEPayment): Boolean
    begin
      Payment.FLastError := ErrorMessage;
      Payment.FLastStatus := Status; // or just psNetworkError ?
      Payment.DoFinished;
      Result := False;
    end);
  end;
end;

{$ENDIF}

{ TJVEPaymentItem }

procedure TJVEPaymentItem.Assign(Source: TPersistent);
var
  Item: TJVEPaymentItem absolute Source;
begin
  if Source is TJVEPaymentItem then
  begin
    FPlatforms := Item.FPlatforms;
    FiOSIdentifier := Item.FiOSIdentifier;
    FMacIdentifier := Item.FMacIdentifier;
    FAndroidIdentifier := Item.FAndroidIdentifier;
    FPPIdentifier := Item.FPPIdentifier;
    FPersistent := Item.FPersistent;
    FOnPurchased := Item.FOnPurchased;
    FOnCacheUpdated := Item.FOnCacheUpdated;
    FPPPrice := Item.FPPPrice;
    FPPCurrency := Item.FPPCurrency;
    FPPTitle := Item.FPPTitle;
    FPPType := Item.FPPType;
  end else
    inherited Assign(Source);
end;

constructor TJVEPaymentItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPlatforms := [ppMacStore, ppAppStore, ppPlay, ppPayPal, ppWindows];
  FPersistent := True;
  FPPType := ptService;
  FPPPrice := 0.99;
  FPPCurrency := pcUSDollar;
end;

function TJVEPaymentItem.GetDisplayName: string;
begin
  Result := FPPTitle;
end;

function TJVEPaymentItem.GetPayment: TJVEPayment;
begin
  Result := TJVEPayment(Collection.Owner);
end;

function TJVEPaymentItem.GetPurchased: Boolean;
var
  Id: String;
  Flag: Boolean;
begin
  if not FPersistent then Exit(False);
  if FPurchased then Exit(True);

  Flag := False;
  for Id in GetDeviceUniqueIds do
    if TJVEConfiguration.ReadBool(TPayPalThread.Hash(Id + '.' + GetIdentifier), False) then
      FPurchased := True
    else
      Flag := True;

  // The list of Device Unique Ids changed.
  if FPurchased and Flag then
    FlagAsPurchased;

  Result := FPurchased;
end;

function TJVEPaymentItem.GetTitle: String;
begin
  if FTitleCache <> '' then
    Result := FTitleCache
  else
    Result := FPPTitle;
end;

procedure TJVEPaymentItem.FlagAsPurchased;
var
  Id: String;
begin
  FPurchased := True;
  for Id in GetDeviceUniqueIds do
    TJVEConfiguration.WriteBool(TPayPalThread.Hash(Id + '.' + GetIdentifier), True);
end;

function TJVEPaymentItem.IsPriceStored: Boolean;
begin
  Result := not SameValue(FPPPrice, 0.99);
end;

procedure TJVEPaymentItem.SetCurrency(const Value: TJVEPPCurrencies);
begin
  if FPPCurrency <> Value then
  begin
    FPPCurrency := Value;
    SetCache('', '');
  end;
end;

function TJVEPaymentItem.GetIdentifier: String;
begin
  {$IF Defined(MACOS)}
  if {$IFDEF IOS}(FiOSIdentifier <> '') and (ppAppStore{$ELSE}
    (FMacIdentifier <> '') and (ppMacStore{$ENDIF} in FPlatforms) then
      Result := {$IFDEF IOS}FiOSIdentifier{$ELSE}FMacIdentifier{$ENDIF}
  else
  {$ELSEIF Defined(ANDROID)}
  if (FAndroidIdentifier <> '') and (ppPlay in FPlatforms) then
    Result := FAndroidIdentifier
  else
  {$ELSE}
  if (FWindowsToken <> '') and Assigned(GetPayment.FWindowsStore) and (ppWindows in FPlatforms) then
    Result := FWindowsToken
  else
  {$ENDIF}
  if ppPayPal in FPlatforms then
    Result := FPPIdentifier
  else
    Result := '';
end;

procedure TJVEPaymentItem.SetIdentifier(const Value: String);
begin
  iOSIdentifier := Value;
  MacIdentifier := Value;
  AndroidIdentifier := Value;
  WindowsToken := Value;
  PPIdentifier := Value;
end;

procedure TJVEPaymentItem.SetiOSIdentifier(const Value: String);
begin
  if FiOSIdentifier <> Value then
  begin
    FiOSIdentifier := Value;
    FPurchased := False;
{$IF Defined(IOS)}
    if ppAppStore in FPlatforms then
      SetCache('', '');
{$ENDIF}
  end;
end;

procedure TJVEPaymentItem.SetMacIdentifier(const Value: String);
begin
  if FMacIdentifier <> Value then
  begin
    FMacIdentifier := Value;
    FPurchased := False;
{$IF Defined(MACOS) and not Defined(IOS)}
    if ppMacStore in FPlatforms then
      SetCache('', '');
{$ENDIF}
  end;
end;

procedure TJVEPaymentItem.SetAndroidIdentifier(const Value: String);
begin
  if FAndroidIdentifier <> Value then
  begin
    FAndroidIdentifier := Value;
    FPurchased := False;
{ $IF Defined(ANDROID)
    if ppPlay in FPlatforms then
      SetCache('', '');
{$ENDIF}
  end;
end;

procedure TJVEPaymentItem.SetWindowsToken(const Value: String);
begin
  if FWindowsToken <> Value then
  begin
    FWindowsToken := Value;
    FPurchased := False;
{$IF Defined(MSWINDOWS)}
    if ppWindows in FPlatforms then
      SetCache('', '');
{$ENDIF}
  end;
end;

procedure TJVEPaymentItem.SetPPIdentifier(const Value: String);
begin
  if FPPIdentifier <> Value then
  begin
    FPPIdentifier := Value;
    FPurchased := False;
    if ppPayPal in FPlatforms then
      SetCache('', '');
  end;
end;

procedure TJVEPaymentItem.SetPersistent(const Value: Boolean);
begin
  if FPersistent <> Value then
  begin
    FPersistent := Value;
    FPurchased := False;
  end;
end;

procedure TJVEPaymentItem.SetPrice(const Value: Double);
begin
  if not SameValue(FPPPrice, Value) then
  begin
    FPPPrice := Value;
    SetCache('', '');
  end;
end;

procedure TJVEPaymentItem.SetCache(const Price, Title: String);
begin
  if (FPriceCache <> Price) or (FTitleCache <> Title) then
  begin
    FPriceCache := Price;
    FTitleCache := Title;
    if Assigned(FOnCacheUpdated) then
      FOnCacheUpdated(GetPayment, Self);
  end;
end;

procedure TJVEPaymentItem.RepeatPurchase(Finished: Boolean);
begin
  FlagAsPurchased;
  if Assigned(FOnPurchased) then
    FOnPurchased(GetPayment, Self, '', Now);
  if Finished and Assigned(GetPayment.FOnFinished) and not GetPayment.IsBusy then
    GetPayment.FOnFinished(GetPayment, psSuccess, '');
end;

procedure TJVEPaymentItem.Buy;
{$IF Defined(MSWINDOWS) and (CompilerVersion >= 33)}
var
  Idx: Integer;
  AnalyticsId: String;
{$ENDIF}
begin
  if GetPurchased {$IF Defined(ANDROID)} or ((ppPlay in FPlatforms) and
    GlobalListener.IsProductPurchased(FAndroidIdentifier)) {$ENDIF} then
  begin
    // For whatever reason persistent buy was retried. Basically this is a bug.
    // Let's be good and pretend that the purchase was immediately approved.
    // We're also not reporting to analytics: we've already done that.
    RepeatPurchase(True);
    Exit;
  end;

  if {$IF Defined(IOS)}(FiOSIdentifier <> '') and (ppAppStore
    {$ELSEIF Defined(MACOS)}(FMacIdentifier <> '') and (ppMacStore
    {$ELSEIF Defined(ANDROID)}(FAndroidIdentifier <> '') and (ppPlay
    {$ELSE}(FWindowsToken <> '') and Assigned(GetPayment.FWindowsStore) and (ppWindows
    {$ENDIF} in FPlatforms) then
  begin
    if GetPayment.CanBuyNatively then
    begin
      // Special thanks to Philipp Hofmann for finding an incompatibility here.
      {$IF not Defined(WINDOWS)}
      if GetPayment.FShowShade and (GetPayment.FShadeIdentifier = 0) then
        GetPayment.FShadeIdentifier := TJVEShade.Show;
      {$ENDIF}

      {$IF Defined(MSWINDOWS) and (CompilerVersion >= 33)}
      GetPayment.FLastStatus := psSuccess;
      Idx := GetPayment.FWindowsStore.AppProducts.IndexOf(FWindowsToken);
      if Idx = -1 then
      begin
        if GetPayment.FWindowsStore.UserHasBought(FWindowsToken) then
          RepeatPurchase(False)
        else
          GetPayment.FLastStatus := psInvalidId;
      end else
      begin
        case GetPayment.FWindowsStore.PurchaseProduct(GetPayment.FWindowsStore.AppProducts[Idx]) of
          StorePurchaseStatus.NotPurchased: GetPayment.FLastStatus := psCancelled;
          StorePurchaseStatus.NetworkError: GetPayment.FLastStatus := psNetworkError;
          StorePurchaseStatus.ServerError: GetPayment.FLastStatus := psUnexpectedError;
          else
            if Assigned(FOnPurchased) then
              FOnPurchased(GetPayment, Self, '', Now);
            if not Persistent then
              GetPayment.FWindowsStore.
                ReportUnmanagedConsumableFulfillment(FWindowsToken, TGUID.NewGuid);

            AnalyticsId := IntToStr(Random(MaxInt));
            TMessageManager.DefaultManager.SendMessage(Self,
              TJVETrackTransaction.Create(AnalyticsId, '', FPPPrice,
              0, 0, JVEPPCurrencyCodes[FPPCurrency], acJVEPayment));
            TMessageManager.DefaultManager.SendMessage(Self,
              TJVETrackItem.Create(AnalyticsId, FWindowsToken, FPPPrice,
              1, '', '', JVEPPCurrencyCodes[FPPCurrency], acJVEPayment));
        end;
      end;

      case GetPayment.FLastStatus of
        psSuccess: GetPayment.FLastError := '';
        psCancelled: GetPayment.FLastError := 'Payment cancelled';
        psInvalidId: GetPayment.FLastError := 'Token not found in store';
        psNetworkError: GetPayment.FLastError := 'Network error occured';
        psUnexpectedError: GetPayment.FLastError := 'Server error occured';
      end;
      GetPayment.DoFinished;
      {$ELSEIF Defined(MACOS)}
      FState := peBuying;
      GlobalDelegate.Buy({$IFDEF IOS}FiOSIdentifier{$ELSE}FMacIdentifier{$ENDIF});
      {$ELSEIF Defined(ANDROID)}
      FState := peBuying;
      GlobalListener.Buy(FAndroidIdentifier);
      {$ENDIF}
    end else
    begin
      GetPayment.FLastError := 'In-app purchasing disabled';
      GetPayment.FLastStatus := psNotAllowed;
      GetPayment.DoFinished;
    end;
  end else
  if ppPayPal in FPlatforms then
    GetPayment.Locked(procedure
    begin
      if GetPayment.FShowShade and (GetPayment.FShadeIdentifier = 0) then
        GetPayment.FShadeIdentifier := TJVEShade.Show;

      if not (FState in [peBuying, peValidating]) then
        FState := peBuying;
      if (GetPayment.FThread = nil) and (GetPayment.FBrowser = nil) then
        GetPayment.FThread := TPayPalThread.Create(GetPayment);
    end)
  else
  begin
    GetPayment.FLastError := 'Unsupported on this platform';
    GetPayment.FLastStatus := psInvalidPlatform;
    GetPayment.DoFinished;
  end;
end;

{ TJVEPaymentItems }

function TJVEPaymentItems.GetItem(Index: Integer): TJVEPaymentItem;
begin
  Result := TJVEPaymentItem(inherited GetItem(Index));
end;

{ TJVEPayment }

constructor TJVEPayment.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowShade := True;
  FLock := TCriticalSection.Create;
  FItems := TJVEPaymentItems.Create(Self, TJVEPaymentItem);
  FAutoTranslate := True;
  FAPIApplicationId := SandboxAPIApplicationId;
  FAPIUserId := SandboxAPIUserId;
  FAPIPassword := SandboxAPIPassword;
  FAPISignature := SandboxAPISignature;
  FAPISandbox := True;
  FStyleChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TJVEPayment.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
{$IF Defined(MACOS)}
  GlobalDelegate.RemoveListener(Self);
{$ELSEIF Defined(ANDROID)}
  GlobalListener.RemoveListener(Self);
{$ELSE}
  FreeAndNil(FWindowsStore);
{$ENDIF}
  FreeAndNil(FItems);
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TJVEPayment.Open(Sender: TJVEAction);
var
  Identifier: String;
  Item: TCollectionItem;
  Payment: TJVEPaymentItem absolute Item;
begin
  for Identifier in Sender.Identifier.Split([',']) do
    for Item in FItems do
      if Payment.Identifier = Identifier then
      begin
        Payment.Buy;
        Exit;
      end;
end;

procedure TJVEPayment.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  Translation: String;
  CollectionItem: TCollectionItem;
  Item: TJVEPaymentItem absolute CollectionItem;
begin
  if not FAutoTranslate then
    Exit;

  for CollectionItem in FItems do
    if Item.FPPTitle <> '' then
    begin
      Translation := Translate(Item.FPPTitle); // Mandatory for texts collection
      if not (csDesigning in ComponentState) then
        Item.FPPTitle := Translation;
    end;
end;

procedure TJVEPayment.DoFinished;
var
  CollectionItem: TCollectionItem;
  Item: TJVEPaymentItem absolute CollectionItem;
begin
  if IsBusy then Exit;

  for CollectionItem in FItems do
    if ppPayPal in Item.FPlatforms then
      Item.FState := pePassive;

  if FShadeIdentifier <> 0 then
  begin
    TJVEShade.Hide(FShadeIdentifier);
    FShadeIdentifier := 0;
  end;

  if Assigned(FOnFinished) then
    FOnFinished(Self, FLastStatus, FLastError);
  FLastError := '';
  FLastStatus := psSuccess;
end;

procedure TJVEPayment.Locked(Proc: TProc);
begin
  FLock.Acquire;
  try
    Proc;
  finally
    FLock.Release;
  end;
end;

function TJVEPayment.IsAPIApplicationIdStored: Boolean;
begin
  Result := FAPIApplicationId <> SandboxAPIApplicationId;
end;

function TJVEPayment.IsAPIPasswordStored: Boolean;
begin
  Result := FAPIPassword <> SandboxAPIPassword;
end;

function TJVEPayment.IsAPISignatureStored: Boolean;
begin
  Result := FAPISignature <> SandboxAPISignature;
end;

function TJVEPayment.IsAPIUserIdStored: Boolean;
begin
  Result := FAPIUserId <> SandboxAPIUserId;
end;

function TJVEPayment.CanOpen(Sender: TJVEAction): Boolean;
var
  Identifier: String;
  Item: TCollectionItem;
  Payment: TJVEPaymentItem absolute Item;
begin
  for Identifier in Sender.Identifier.Split([',']) do
    for Item in FItems do
      if Payment.Identifier = Sender.Identifier then
      begin
        if Payment.GetPurchased then
          Exit(False);

        if {$IF Defined(IOS)}(Payment.iOSIdentifier <> '') and (ppAppStore
          {$ELSEIF Defined(MACOS)}(Payment.MacIdentifier <> '') and (ppMacStore
          {$ELSEIF Defined(ANDROID)}(Payment.AndroidIdentifier <> '') and (ppPlay
          {$ELSE}(Payment.WindowsToken <> '') and Assigned(FWindowsStore) and (ppWindows
          {$ENDIF} in Payment.Platforms) then
            Exit(CanBuyNatively)
        else
          Exit(ppPayPal in Payment.Platforms);
      end;
    Result := False;
end;

function TJVEPayment.GetIsBusy: Boolean;
{$IF Defined(MACOS) or Defined(ANDROID)}
var
  Item: TCollectionItem;
{$ENDIF}
begin
  {$IF Defined(MACOS) or Defined(ANDROID)}
  for Item in FItems do
    if ({$IF Defined(IOS)}ppAppStore{$ELSEIF Defined(MACOS)}ppMacStore{$ELSE}ppPlay{$ENDIF} in TJVEPaymentItem(Item).Platforms)
      and (TJVEPaymentItem(Item).FState <> pePassive) then
        Exit(True);
  {$ENDIF}
  Result := (FThread <> nil) or (FBrowser <> nil) or FNativelyRestoring;
end;

procedure TJVEPayment.AfterConstruction;
begin
{$IF Defined(MACOS)}
  if GlobalDelegate = nil then
    GlobalDelegate := TJVEStoreKitDelegate.Create;

  GlobalDelegate.AddListener(Self);
  GlobalDelegate.paymentQueue(TSKPaymentQueue.OCClass.defaultQueue,
    TSKPaymentQueue.OCClass.defaultQueue.transactions);
{$ELSEIF Defined(ANDROID)}
  if GlobalListener = nil then
    GlobalListener := TJVEInAppListener.Create;

  GlobalListener.AddListener(Self);
{$ELSEIF CompilerVersion >= 33}
  if TOSVersion.Major >= 10 then
    FWindowsStore := TWindowsStoreCore.Create(ProcessMessages);
{$ENDIF}

  inherited AfterConstruction;
end;

{$IF Defined(MSWINDOWS) and (CompilerVersion >= 33)}
procedure TJVEPayment.ProcessMessages;
var
  AppService: IFMXApplicationService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, AppService) then
    AppService.HandleMessage;
end;
{$ENDIF}

function TJVEPayment.Buy(Id: String): Boolean;
var
  Item: TCollectionItem;
  Payment: TJVEPaymentItem absolute Item;
begin
  if Id = '' then
    Exit(False);

  for Item in FItems do
    if Payment.Identifier = Id then
    begin
      Payment.Buy;
      Exit(True);
    end;

  for Item in FItems do
    if (Payment.FiOSIdentifier = Id) or (Payment.FMacIdentifier = Id) or
      (Payment.FAndroidIdentifier = Id) or (Payment.FPPIdentifier = Id) or
      (Payment.FWindowsToken = Id) then
    begin
      Payment.Buy;
      Exit(True);
    end;

  Id := ' ' + Id + ' ';
  for Item in FItems do
    if Id.Contains(' ' + Payment.Identifier + ' ') then
    begin
      Payment.Buy;
      Exit(True);
    end;

  Result := False;
end;

procedure TJVEPayment.SetAndroidLicense(const Value: String);
begin
  FAndroidLicense := Value;
  {$IF Defined(ANDROID)}
  GlobalListener.FPurchaseD.ApplicationLicenseKey := Value;
  {$ENDIF}
end;

procedure TJVEPayment.SetItems(const Value: TJVEPaymentItems);
begin
  FItems.Assign(Value);
end;

procedure TJVEPayment.CachePrices;
var
  Item: TCollectionItem;
  Payment: TJVEPaymentItem absolute Item;
  Format: TFormatSettings;
  {$IF Defined(MACOS) or Defined(ANDROID)}
  Start: Boolean;
  {$ELSEIF CompilerVersion >= 33}
  Idx: Integer;
  {$ENDIF}
begin
  {$IF Defined(MACOS) or Defined(ANDROID)}
  Start := False;
  {$ELSEIF CompilerVersion >= 33}
  if Assigned(FWindowsStore) then
    FWindowsStore.RefreshInfo;
  {$ENDIF}

  for Item in FItems do
  begin
    if Payment.GetPurchased or (Payment.FPriceCache <> '') then Continue;

    {$IF Defined(IOS)}
    if (Payment.iOSIdentifier <> '') and (ppAppStore in Payment.FPlatforms) then Start := True else
    {$ELSEIF Defined(MACOS)}
    if (Payment.MacIdentifier <> '') and (ppMacStore in Payment.FPlatforms) then Start := True else
    {$ELSEIF Defined(ANDROID)}
    if (Payment.AndroidIdentifier <> '') and (ppPlay in Payment.FPlatforms) then Start := True else
    {$ELSEIF CompilerVersion >= 33}
    if (Payment.WindowsToken <> '') and Assigned(FWindowsStore) and
      (ppWindows in Payment.Platforms) then
    begin
      Idx := FWindowsStore.AppProducts.IndexOf(Payment.WindowsToken);
      if Idx = -1 then
      begin
        Payment.FPriceCache := '-';
        Payment.FTitleCache := '';
      end else
      begin
        Payment.SetCache(TWindowsString.
          HStringToString(FWindowsStore.AppProducts[Idx].Price.FormattedPrice),
          TWindowsString.HStringToString(FWindowsStore.AppProducts[Idx].Title));
      end;
    end else
    {$ENDIF}
    if ppPayPal in Payment.FPlatforms then
    begin
      Format := TFormatSettings.Create(JVEPPCurrencyLocales[Payment.FPPCurrency]);
      Payment.SetCache(FloatToStrF(Payment.FPPPrice,
        ffCurrency, 18, Format.CurrencyDecimals, Format), '');
    end else
    begin
      Payment.FPriceCache := '-';
      Payment.FTitleCache := '';
    end;
  end;

  {$IF Defined(MACOS)}
  if Start then
    GlobalDelegate.RequestIdentifiers;
  {$ELSEIF Defined(ANDROID)}
  if Start then
    GlobalListener.RequestIdentifiers;
  {$ENDIF}
end;

class function TJVEPayment.CanBuyNatively: Boolean;
begin
{$IF Defined(MACOS)}
  if GlobalDelegate = nil then
    GlobalDelegate := TJVEStoreKitDelegate.Create;

  Result := GlobalDelegate.CanBuyNatively;
{$ELSEIF Defined(ANDROID)}
  if GlobalListener = nil then
    GlobalListener := TJVEInAppListener.Create;

  Result := GlobalListener.CanBuyNatively;
{$ELSE}
  Result := True;
{$ENDIF}
end;

procedure TJVEPayment.RestorePurchases;
var
  Item: TCollectionItem;
begin
  // On Android the restore functionality is irrelevant thanks to the inventory
  // system automatically identifying which products have been purchased

  for Item in FItems do
    TJVEPaymentItem(Item).GetPurchased;

  Locked(procedure
  var
    Item: TCollectionItem;
    Payment: TJVEPaymentItem absolute Item;
    StartPP{$IF Defined(MACOS) or Defined(ANDROID)}, StartIA{$ENDIF}: Boolean;
  begin
    StartPP := False;
    {$IF Defined(MACOS) or Defined(ANDROID)}StartIA := False;{$ENDIF}
    for Item in FItems do
    begin
      if Payment.FPurchased or (Payment.FState <> pePassive) then Continue;

      {$IF Defined(IOS)}
      if (Payment.iOSIdentifier <> '') and (ppAppStore in Payment.FPlatforms) then StartIA := True else
      {$ELSEIF Defined(MACOS)}
      if (Payment.MacIdentifier <> '') and (ppMacStore in Payment.FPlatforms) then StartIA := True else
      {$ELSEIF Defined(ANDROID)}
      if (Payment.AndroidIdentifier <> '') and (ppPlay in Payment.FPlatforms) then StartIA := True else
      {$ELSEIF CompilerVersion >= 33}
      if (Payment.WindowsToken <> '') and Assigned(FWindowsStore) and
        (ppWindows in Payment.Platforms) then
      begin
        if FWindowsStore.UserHasBought(Payment.FWindowsToken) then
        begin
          Payment.FlagAsPurchased;
          if Assigned(Payment.FOnPurchased) then
            Payment.FOnPurchased(Self, Payment, '', Now);
        end;
      end else
      {$ENDIF}
      if ppPayPal in Payment.FPlatforms then
      begin
        Payment.FState := peRestoring;
        StartPP := True;
      end;
    end;

    {$IF Defined(MACOS)}
    if StartIA and not FNativelyRestoring then
    begin
      FNativelyRestoring := True;
      if not GlobalDelegate.FGlobalRestoring then
      begin
        GlobalDelegate.FGlobalRestoring := True;
        TSKPaymentQueue.OCClass.defaultQueue.restoreCompletedTransactions;
      end;
    end;
    {$ELSEIF Defined(ANDROID)}
    if StartIA and not FNativelyRestoring then
    begin
      FNativelyRestoring := True;
      CachePrices;
      if GlobalListener.FRequesting.Count = 0 then
      begin
        StartIA := False;
        FNativelyRestoring := False;
      end;

      for Item in FItems do
        if (ppPlay in Payment.FPlatforms) and not Payment.FPurchased and
          GlobalListener.IsProductPurchased(Payment.FAndroidIdentifier) then
        begin
          Payment.FlagAsPurchased;
          if Assigned(Payment.FOnPurchased) then
            Payment.FOnPurchased(Payment.GetPayment, Payment, '', Now);
        end;
    end;
    {$ENDIF}

    if StartPP and (FThread = nil) and (FBrowser = nil) then
      FThread := TPayPalThread.Create(Self);

    if StartPP {$IF Defined(MACOS) or Defined(ANDROID)} or StartIA {$ENDIF} then
    begin
      if FShowShade and (FShadeIdentifier = 0) then
        FShadeIdentifier := TJVEShade.Show;
    end else
    if Assigned(FOnFinished) and not IsBusy then
      FOnFinished(Self, psSuccess, '');
  end);
end;

procedure TJVEPayment.PayPalClosed(Browser: TJVEBrowser);
begin
  if (FThread = nil) and (FBrowser.Tag = 0) then
    FThread := TPayPalThread.Create(Self);
  FreeAndNil(FBrowser);
  DoFinished;
end;

procedure TJVEPayment.PayPalError(Browser: TJVEBrowser;
  const Error: String; var DefaultProc: Boolean);
var
  Item: TCollectionItem;
begin
  DefaultProc := False;
  for Item in FItems do
    TJVEPaymentItem(Item).FState := pePassive;

  FLastError := Error;
  FLastStatus := psNetworkError;
  FBrowser.Tag := 1;
  FBrowser.Close;
end;

procedure TJVEPayment.PayPalStarting(Browser: TJVEBrowser;
  const URL: string; var Action: TJVEBrowserAction);
begin
  // If you are replacing the target site (usefull for Digital Goods, see below),
  // then you should also update google. below. This will allow other payment
  // types to be be used in an embedded fashion.
  if URL.Contains('google.') or URL.Contains('?dispatch=') then
    Action := baClose;
end;

{ TPayPalThread }

constructor TPayPalThread.Create(Payment: TJVEPayment);
begin
  inherited Create;
  FPayment := Payment;
end;

procedure TPayPalThread.Execute;
var
  Item: TJVEPaymentItem;
  {$IFDEF MACOS}Pool: NSAutoreleasePool;{$ENDIF}
begin
  {$IFDEF MACOS}Pool := TNSAutoreleasePool.Create;{$ENDIF}
  try
    try
      FPayment.FLastStatus := psSuccess;
      ValidateDeviceId;

      repeat
        Item := GetNextItem;
        if Item <> nil then
          BuyItem(Item);
      until Item = nil;
    except
      on EAbort do;
      on E: Exception do
      begin
        FPayment.FLastError := E.Message;
        FPayment.FLastStatus := psUnexpectedError;
      end;
    end;
  finally
    if FPayment.FThread = Self then
      FPayment.FThread := nil;
    FreeOnTerminate := True;

    if not FPayment.IsBusy then
      Synchronize(procedure
      begin
        FPayment.DoFinished;
      end);
    {$IFDEF MACOS}Pool.drain;{$ENDIF}
  end;
end;

procedure TPayPalThread.BuyItem(Item: TJVEPaymentItem);
var
  Index: Integer;
  Perform, Analyze: Boolean;
  Status, PayPalId: String;
  Format: TFormatSettings;
begin
  Index := TJVEConfiguration.ReadInteger(Hash(FPayment.FDeviceId + '->' +
    Item.FPPIdentifier), IfThen(Item.FPersistent, 0, Random(1000000000)));
  while True do
  begin
    PayPalId := System.SysUtils.Format('%s.%s.%d', [FPayment.FDeviceId, Item.FPPIdentifier, Index]);
    Status := FindId(PayPalId, pdStatus);
    if (Item.FState = peValidating) or (Status = '') or
      (Status = 'CREATED') or StatusBought(Status) then Break;

    if not Item.FPersistent then
      Index := Random(1000000000)
    else
      Inc(Index);
  end;

  TJVEConfiguration.WriteInteger(
    Hash(FPayment.FDeviceId + '->' + Item.FPPIdentifier), Index);

  if StatusBought(Status) then
  begin
    Perform := False;
    Analyze := False;
    FPayment.Locked(procedure
    begin
      if not Item.FPurchased then
      begin
        // Replace with True to switch to the alternative processing (see below).
        Analyze := Item.FState = peValidating;
        Perform := Assigned(Item.FOnPurchased);
        Item.FState := pePassive;
        if Item.FPersistent then
          Item.FlagAsPurchased;
      end;
    end);

    if not Item.FPersistent then
      TJVEConfiguration.WriteInteger(Hash(
        FPayment.FDeviceId + '->' + Item.FPPIdentifier), Random(1000000000));

    if Perform or Analyze then
      Synchronize(procedure
      var
        AnalyticsId: String;
      begin
        if Perform and Assigned(Item.FOnPurchased) then
          Item.FOnPurchased(FPayment, Item, '', Now);

        if Analyze then
        begin
          // It should be noted that an application failure during payment
          // with a subsequent restore will not be reported.
          // An alternative is reporting restores as payments. To switch to this
          // alternative, replace condition with True (see above).
          AnalyticsId := IntToStr(Random(MaxInt));
          TMessageManager.DefaultManager.SendMessage(Self,
            TJVETrackTransaction.Create(AnalyticsId, '', Item.FPPPrice,
            0, 0, JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
          TMessageManager.DefaultManager.SendMessage(Self,
            TJVETrackItem.Create(AnalyticsId, Item.FPPIdentifier, Item.FPPPrice,
            1, '', '', JVEPPCurrencyCodes[Item.FPPCurrency], acJVEPayment));
        end;
      end);
  end else
  if Item.FState = peRestoring then
  begin
    Item.FState := pePassive;
  end else
  if Item.FState = peValidating then
  begin
    FPayment.FLastStatus := psCancelled;
    FPayment.FLastError := 'Payment cancelled';
    Abort;
  end else
  if Item.FState = peBuying then
  begin
    if Status = '' then
    begin
      Format := TFormatSettings.Create(JVEPPCurrencyLocales[Item.FPPCurrency]);
      Format.DecimalSeparator := '.';
      Call('Pay?actionType=PAY&currencyCode=' + JVEPPCurrencyCodes[Item.FPPCurrency] +
        // This is the place to put your own site reference, if you are using
        // Digital Goods. See above for the problem therein.
        '&cancelUrl=https://www.google.com/' +
        '&returnUrl=https://www.google.com/' +
        '&requestEnvelope.errorLanguage=en_US' +
        '&receiverList.receiver(0).paymentType=' + TypeString(Item) +
        '&receiverList.receiver(0).amount=' + FloatToStrF(Item.FPPPrice, ffFixed, 18, Format.CurrencyDecimals, Format) +
        '&receiverList.receiver(0).email=' + FPayment.FRecipientEmail +
        '&trackingId=' + Hash(PayPalId));
    end;

    PayPalId := FindId(PayPalId, pdKey);
    if PayPalId = '' then
    begin
      FPayment.FLastStatus := psNotAllowed;
      if FPayment.FLastError = '' then
        FPayment.FLastError := 'Payment creation refused';
      Abort;
    end;

    Synchronize(procedure
    begin
      case Item.FPPType of
        ptGoods, ptService:
          begin
            FPayment.FBrowser := TJVEBrowser.Create(nil);
            FPayment.FBrowser.Title := IfThen(Item.FPPTitle = '', 'PayPal', Item.FPPTitle);
            FPayment.FBrowser.URL := IfThen(FPayment.FAPISandbox,
              'https://www.sandbox.paypal.com/', 'https://www.paypal.com/') +
              'cgi-bin/webscr?cmd=_ap-payment&paykey=' + PayPalId;
            FPayment.FBrowser.OnClosed := FPayment.PayPalClosed;
            FPayment.FBrowser.OnError := FPayment.PayPalError;
            FPayment.FBrowser.OnStarting := FPayment.PayPalStarting;
            FPayment.FBrowser.Open;
          end;

        ptGoodsExternal, ptServiceExternal:
          TJVEOpenURL.OpenURL(IfThen(FPayment.FAPISandbox,
            'https://www.sandbox.paypal.com/', 'https://www.paypal.com/') +
            'cgi-bin/webscr?cmd=_ap-payment&paykey=' + PayPalId);
      else
        TJVEOpenURL.OpenURL(IfThen(FPayment.FAPISandbox,
          'https://www.sandbox.paypal.com/', 'https://www.paypal.com/') +
          'webapps/adaptivepayment/flow/pay?paykey=' + PayPalId);
      end;
    end);

    Item.FState := peValidating;
    Abort;
  end;
end;

function TPayPalThread.GetNextItem: TJVEPaymentItem;
var
  Output: TJVEPaymentItem;
begin
  Output := nil;
  FPayment.Locked(procedure
  var CollectionItem: TCollectionItem;
  begin
    for CollectionItem in FPayment.FItems do
    begin
      Output := TJVEPaymentItem(CollectionItem);
      if not (ppPayPal in Output.FPlatforms) or (Output.FState = pePassive) then
        Output := nil
      else
        Break;
    end;
  end);
  Result := Output;
end;

function TPayPalThread.ValidateDeviceId: String;
var
  AssignIds: TStringList;
  Current, FoundId: String;
begin
  if FPayment.FDeviceId = '' then
  begin
    AssignIds := TStringList.Create;
    try
      for Current in GetDeviceUniqueIds do
      begin
        FoundId := FindId('register.id.' + Current, pdRedirect);
        if FoundId = '' then
          AssignIds.Add(Current)
        else
          FPayment.FDeviceId := FoundId;
      end;

      if FPayment.FDeviceId = '' then
        FPayment.FDeviceId := GetDeviceUniqueIds[0];
      for Current in AssignIds do
        Call('Pay?actionType=PAY&currencyCode=USD' +
          '&cancelUrl=https://references.cancel.com' +
          '&returnUrl=https://references.' + FPayment.FDeviceId + '.com' +
          '&requestEnvelope.errorLanguage=en_US' +
          '&receiverList.receiver(0).amount=1' +
          '&receiverList.receiver(0).email=register.redirect@mail.com' +
          '&trackingId=' + Hash('register.id.' + Current));
    finally
      FreeAndNil(AssignIds);
    end;
  end;
end;

function TPayPalThread.StatusBought(const Status: String): Boolean;
begin
  Result := (Status = 'HOLD') or (Status = 'CLAIMED') or
    (Status = 'COMPLETED') or (Status = 'UNCLAIMED') or
    (Status = 'PENDING') or (Status = 'PROCESSING');
end;

function TPayPalThread.TypeString(Item: TJVEPaymentItem): String;
begin
  case Item.FPPType of
    ptGoods, ptGoodsExternal: Result := 'GOODS';
    ptService, ptServiceExternal: Result := 'SERVICE';
    ptDigitalGoods: Result := 'DIGITALGOODS';
  end;
end;

// https://developer.paypal.com/docs/classic/adaptive-accounts/integration-guide/ACGettingStarted/
function TPayPalThread.Call(const Request: String): String;
var
  CustomHeaders: TStrings;
  Error, URL: String;
  Response: TMemoryStream;
  Encoding: TEncoding;
  Bytes: TBytes;
  Offset: Integer;
begin
  Error := '';
  URL := IfThen(not FPayment.FAPISandbox, 'https://svcs.paypal.com/',
    'https://svcs.sandbox.paypal.com/') + 'AdaptivePayments/' + Request;
  CustomHeaders := TStringList.Create;
  CustomHeaders.Values['X-PAYPAL-REQUEST-DATA-FORMAT'] := 'NV';
  CustomHeaders.Values['X-PAYPAL-RESPONSE-DATA-FORMAT'] := 'NV';
  CustomHeaders.Values['X-PAYPAL-SECURITY-USERID'] := FPayment.FAPIUserId;
  CustomHeaders.Values['X-PAYPAL-SECURITY-PASSWORD'] := FPayment.FAPIPassword;
  CustomHeaders.Values['X-PAYPAL-SECURITY-SIGNATURE'] := FPayment.FAPISignature;
  CustomHeaders.Values['X-PAYPAL-APPLICATION-ID'] := FPayment.FAPIApplicationId;
  Response := TJVEOpenURL.DownloadURL(URL, CustomHeaders, Error);
  try
    if (Error <> '') or (Response = nil) or (Response.Size = 0) then
    begin
      FPayment.FLastError := Error;
      if FPayment.FLastError = '' then
        FPayment.FLastError := 'Unexpected error';
      FPayment.FLastStatus := psNetworkError;
      Abort;
    end else
    begin
      Encoding := nil;
      SetLength(Bytes, Response.Size);
      Response.Read(Bytes, 0, Response.Size);
      Offset := TEncoding.GetBufferEncoding(Bytes, Encoding, TEncoding.UTF8);
      Result := Encoding.GetString(Bytes, Offset, Response.Size - Offset);
    end;
  finally
    FreeAndNil(Response);
  end;
end;

function TPayPalThread.FindId(const Id: String; Data: TPayPalData): String;
var
  Response: TStringList;
begin
  Response := TStringList.Create;
  try
    Response.AddStrings(Call('PaymentDetails?requestEnvelope.' +
      'errorLanguage=en_US&trackingId=' + Hash(Id)).Split(['&']));
    Result := Response.Values['status'];

    if (Result <> '') and (Data <> pdStatus) then
    begin
      if Data = pdRedirect then
      begin
        Result := Response.Values['returnUrl'];
        if Result <> '' then
        begin
          Response.Clear;
          Response.AddStrings(Result.Split(['.']));

          if Response.Count = 3 then
            Result := Response[1]
          else
            Result := '';
        end;
      end else
      if Result = 'CREATED' then
        Result := Response.Values['payKey'];
    end;

    if (Result = '') and (FPayment.FLastError = '') then
      FPayment.FLastError := Response.Values['error(0).message'].Replace('+', ' ');
  finally
    FreeAndNil(Response);
  end;
end;

{$Q-}
class function TPayPalThread.Hash(const Input: String): String;
var
  Ch: Char;
  Counter: UInt64;
begin
  Counter := 0;
  for Ch in Input do
    Counter := Counter * 101 + Ord(Ch);
  Result := UIntToStr(Counter);
end;

{$IFDEF MACOS}
  {$IFNDEF IOS}

type
  NSBundleClass = interface(NSObjectClass)
    function mainBundle: Pointer; cdecl;
  end;
  NSBundle = interface(NSObject)
    function appStoreReceiptURL: NSURL; cdecl;
  end;
  TNSBundle = class(TOCGenericImport<NSBundleClass, NSBundle>)  end;

// Running away from direct calls to "dlopen" for the app review (just in case).
function LibStoreKit: Pointer; cdecl;
  external '/System/Library/Frameworks/StoreKit.framework/StoreKit'
  name 'OBJC_CLASS_$_SKPayment';
initialization
{$IFDEF RELEASE}
  if not TNSFileManager.Wrap(TNSFileManager.OCClass.defaultManager).fileExistsAtPath(
    TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).appStoreReceiptURL.path) then
      _exit(173);
{$ENDIF}
finalization
  if TOSVersion.Major < 0 then // We need any False, which compiler cannot preevaluate
    LibStoreKit;
  {$ELSE}
    {$IFDEF CPUX86}
var
  Module: THandle;
initialization
  Module := dlopen(MarshaledAString(
    '/System/Library/Frameworks/StoreKit.framework/StoreKit'), RTLD_LAZY);
finalization
  dlclose(Module);
    {$ELSE}
initialization
finalization
    {$ENDIF}
  {$ENDIF}
  FreeAndNil(GlobalDelegate);
{$ENDIF}
end.

