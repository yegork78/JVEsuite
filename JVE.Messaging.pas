(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Messaging;

interface

uses {$IF CompilerVersion >= 27} System.Messaging; {$ELSE} FMX.Messages; {$ENDIF}

type
  TJVEAnalyticsCollect = (
    acFormSwitches, // Reports when a new form becomes active
    acExceptions,   // Reports all exceptions (this changes Application.OnException!)
    acJVEAppRater,  // Reports activations of TJVEAppRater control
    acJVEAppStore,  // Reports activations of TJVEAppStore control
    acJVEBrowser,   // Reports all embedded browser executions (with URLs)
    acJVEEmail,     // Reports activations of TJVEEmail control
    acJVEOpenURL,   // Reports all external browser executions (with URLs)
    acJVEPayment,   // Reports all TJVEPayment payments as app revenues
    acJVESocial,    // Reports activations of TJVESocial control
    acJVEPushes);   // Reports native ad clicks (with target URLs)
  TJVEAnalyticsCollects = set of TJVEAnalyticsCollect;
  // Notice, when collecting events from TJVEPayment, be sure to set the
  // Title and PPPrice. For a restored payment, for example, these item-specific
  // parameters might not be available from Apple, so PayPal information is
  // used as a best available substitute.

const
  TJVEAnalyticsInvalidCollect = TJVEAnalyticsCollect(Ord(High(TJVEAnalyticsCollect)) + 1);

type
  // Use one of the below classes for tracking.
  TJVETrackMessage = class abstract(TMessage)
  private
    FCollect: TJVEAnalyticsCollect;
  public
    constructor Create(ACollect: TJVEAnalyticsCollect);
    property Collect: TJVEAnalyticsCollect read FCollect;
  end;

  // Track a screen change. Skip the Screen parameter to use the current
  // application active form's name. Until the name of screen's change
  // all subsequent calls will pass this screen's name.
  TJVETrackScreen = class(TJVETrackMessage)
  private
    FScreen: String;
  public
    constructor Create(const AScreen: String;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property Screen: String read FScreen;
  end;

  // Track a message reporting abstract application event.
  TJVETrackEvent = class(TJVETrackMessage)
  private
    FEvLabel: String;
    FValue: Integer;
    FCategory: String;
    FAction: String;
  public
    constructor Create(const ACategory, AAction: String;
      const AEvLabel: String = ''; const AValue: Integer = 0;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property Category: String read FCategory;
    property Action: String read FAction;
    property EvLabel: String read FEvLabel;
    property Value: Integer read FValue;
  end;

  // Create a message to report social network behaviour.
  TJVETrackSocial = class(TJVETrackMessage)
  private
    FNetwork: String;
    FTarget: String;
    FAction: String;
  public
    constructor Create(const ANetwork, AAction, ATarget: String;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property Network: String read FNetwork;
    property Action: String read FAction;
    property Target: String read FTarget;
  end;

  // Create an Exception notification message.
  TJVETrackException = class(TJVETrackMessage)
  private
    FFatal: Boolean;
    FException: String;
  public
    constructor Create(const AException: String; AFatal: Boolean = True;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property Exception: String read FException;
    property Fatal: Boolean read FFatal;
  end;

  // Create a Transaction message. Each transaction message should be followed
  // by a number of Item messages (next function); these should pass the same
  // unique transaction identifier.
  TJVETrackTransaction = class(TJVETrackMessage)
  private
    FTransactionId: String;
    FTax: Double;
    FAffiliation: String;
    FRevenue: Double;
    FShipping: Double;
    FCurrency: String;
  public
    constructor Create(const ATransactionId, AAffiliation: String;
      ARevenue, AShipping, ATax: Double; const ACurrency: String;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property TransactionId: String read FTransactionId;
    property Affiliation: String read FAffiliation;
    property Revenue: Double read FRevenue;
    property Shipping: Double read FShipping;
    property Tax: Double read FTax;
    property Currency: String read FCurrency;
  end;

  // Create individual items' messages for a given transaction.
  TJVETrackItem = class(TJVETrackMessage)
  private
    FTransactionId: String;
    FCode: String;
    FPrice: Double;
    FQuantity: Integer;
    FItemName: String;
    FCategory: String;
    FCurrency: String;
  public
    constructor Create(const ATransactionId, AItemName: String; APrice: Double;
      AQuantity: Integer; const ACode, ACategory, ACurrency: String;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property TransactionId: String read FTransactionId;
    property ItemName: String read FItemName;
    property Price: Double read FPrice;
    property Quantity: Integer read FQuantity;
    property Code: String read FCode;
    property Category: String read FCategory;
    property Currency: String read FCurrency;
  end;

  // Internally used by other functions. Could be used to send information,
  // not published by other functions (like document URL or timing).
  // The information published follows all the rules of other functions
  // and is queued appropriately.
  // See source code to see the list of parameters internally added to Data
  // (you should not send parameters twice!).
  TJVETrackRaw = class(TJVETrackMessage)
  private
    FData: String;
  public
    constructor Create(const AData: String;
      ACollect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);
    property Data: String read FData;
  end;

implementation

{ TJVETrackMessage }

constructor TJVETrackMessage.Create(ACollect: TJVEAnalyticsCollect);
begin
  inherited Create;
  FCollect := ACollect;
end;

{ TJVETrackItem }

constructor TJVETrackItem.Create(const ATransactionId, AItemName: String;
  APrice: Double; AQuantity: Integer; const ACode, ACategory, ACurrency: String;
  ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FTransactionId := ATransactionId;
  FItemName := AItemName;
  FPrice := APrice;
  FQuantity := AQuantity;
  FCode := ACode;
  FCategory := ACategory;
  FCurrency := ACurrency;
end;

{ TJVETrackRaw }

constructor TJVETrackRaw.Create(const AData: String; ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FData := AData;
end;

{ TJVETrackTransaction }

constructor TJVETrackTransaction.Create(const ATransactionId,
  AAffiliation: String; ARevenue, AShipping, ATax: Double;
  const ACurrency: String; ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FTransactionId := ATransactionId;
  FAffiliation := AAffiliation;
  FRevenue := ARevenue;
  FShipping := AShipping;
  FTax := ATax;
  FCurrency := ACurrency;
end;

{ TJVETrackException }

constructor TJVETrackException.Create(const AException: String; AFatal: Boolean;
  ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FException := AException;
  FFatal := AFatal;
end;

{ TJVETrackSocial }

constructor TJVETrackSocial.Create(const ANetwork, AAction, ATarget: String;
  ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FNetwork := ANetwork;
  FAction := AAction;
  FTarget := ATarget;
end;

{ TJVETrackEvent }

constructor TJVETrackEvent.Create(const ACategory, AAction, AEvLabel: String;
  const AValue: Integer; ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FCategory := ACategory;
  FAction := AAction;
  FEvLabel := AEvLabel;
  FValue := AValue;
end;

{ TJVETrackScreen }

constructor TJVETrackScreen.Create(const AScreen: String;
  ACollect: TJVEAnalyticsCollect);
begin
  inherited Create(ACollect);
  FScreen := AScreen;
end;

end.

