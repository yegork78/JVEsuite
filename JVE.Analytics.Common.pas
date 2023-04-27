(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Analytics.Common;

interface

uses System.SysUtils, System.Types, System.Classes, JVE.Messaging,
  {$IF CompilerVersion >= 27} System.Messaging; {$ELSE} FMX.Messages; {$ENDIF}

type
  // This is a neutral class; use either TJVEAnalytics or TJVEAnalyticsVCL
  // to properly report form and exception events.
  // This class encapsulates Google Analytics reporting on the user's actions.
  // It should be noted that to use Google Analytics you should appropriately
  // notify the user that this data is collected. For details, see:
  // developers.google.com/analytics/devguides/collection/protocol/policy
  TJVEAnalyticsCommon = class(TComponent)
  private
    FCollect: TJVEAnalyticsCollects;
    FPropertyId: String;
    FFormName: String;
    FInSession: Boolean;
    FActive: Boolean;
    procedure MessageTrackingHandler(const Sender: TObject; const Msg: TMessage);
  protected
    procedure CreateThread;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    // The procedures below send (queue for sending) messages to Google Analytics.
    // Use Track prefix to send messages to the current Analytics instance.
    // Use Broadcast prefix to send messages to all active instances (in this case
    // Collect parameter could be used to classify the messages).

    // Report a screen change. Skip the Screen parameter to use the current
    // application active form's name. Until the name of screen's change
    // all subsequent calls will pass this screen's name.
    procedure TrackScreen(const Screen: String = '');
    class procedure BroadcastScreen(const Screen: String = '';
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Create a message reporting abstract application event.
    procedure TrackEvent(const Category, Action: String;
      const EvLabel: String = ''; Value: Integer = 0);
    class procedure BroadcastEvent(const Category, Action: String;
      const EvLabel: String = ''; Value: Integer = 0;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Create a message to report social network behaviour.
    procedure TrackSocial(const Network, Action, Target: String);
    class procedure BroadcastSocial(const Network, Action, Target: String;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Create an Exception notification message.
    procedure TrackException(const Exception: String; Fatal: Boolean = True);
    class procedure BroadcastException(const Exception: String; Fatal: Boolean = True;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Create a Transaction message. Each transaction message should be followed
    // by a number of Item messages (next function); these should pass the same
    // unique transaction identifier.
    procedure TrackTransaction(const TransactionId, Affiliation: String;
      Revenue, Shipping, Tax: Double; const Currency: String);
    class procedure BroadcastTransaction(const TransactionId, Affiliation: String;
      Revenue, Shipping, Tax: Double; const Currency: String;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Create individual items' messages for a given transaction.
    procedure TrackItem(const TransactionId, ItemName: String; Price: Double;
      Quantity: Integer; const Code, Category, Currency: String);
    class procedure BroadcastItem(const TransactionId, ItemName: String; Price: Double;
      Quantity: Integer; const Code, Category, Currency: String;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // Internally used by other functions. Could be used to send information,
    // not published by other functions (like document URL or timing).
    // The information published follows all the rules of other functions
    // and is queued appropriately.
    // See source code to see the list of parameters internally added to Data
    // (you should not send parameters twice!).
    procedure TrackRaw(const Data: String);
    procedure BroadcastRaw(const Data: String;
      Collect: TJVEAnalyticsCollect = TJVEAnalyticsInvalidCollect);

    // These are helpers, which construct parts of the raw data string.
    // Could be used to simplify TrackRaw and BroadcastRaw data preparation.
    // See other Track functions to see usage examples.
    class function Param(const Name, Value: String; Mandatory: Boolean): String; overload;
    class function Param(const Name: String; Value: Integer;
      Mandatory: Boolean): String; overload;
    class function Param(const Name: String; Value: Double;
      Mandatory: Boolean): String; overload;
  published
    // Indicates whether analytics reporting is enabled.
    property Active: Boolean read FActive write FActive default True;

    // The Property ID (also known as Tracking ID).
    property PropertyId: String read FPropertyId write FPropertyId;

    // The types of events, which are automatically collected (in addition to
    // the events you send by dedicated functions). See TJVEAnalyticsCollect.
    property Collect: TJVEAnalyticsCollects read FCollect write FCollect
      default [Low(TJVEAnalyticsCollect)..High(TJVEAnalyticsCollect)];
  end;

var
  JVEAnalyticsExceptions: Boolean;
  JVEAnalyticsScreenSize, JVEAnalyticsFormSize: TFunc<TSize>;

implementation

uses System.SyncObjs, System.Math, System.Generics.Collections,
  System.StrUtils, JVE.Configuration
  {$IF Defined(IOS)}, Macapi.ObjectiveC, iOSapi.UIKit, JVE.OpenURL,
  iOSapi.Foundation, iOSapi.CocoaTypes, JVE.Utils
  {$ELSEIF Defined(MACOS)}, Macapi.ObjectiveC, Macapi.Foundation, JVE.OpenURL,
  Macapi.CocoaTypes, JVE.Utils
  {$ELSEIF Defined(ANDROID)},  AndroidApi.JNI.JavaTypes, AndroidApi.JNIBridge,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.App, JVE.Utils, JVE.OpenURL
  {$IF CompilerVersion >= 27}, AndroidApi.Helpers{$ENDIF}, FMX.Helpers.Android
  {$ELSEIF Defined(MSWINDOWS)}, Winapi.Windows {$IF CompilerVersion >= 29},
  System.Net.HttpClient, System.Net.URLClient{$ELSE}, JVE.OpenURL{$ENDIF}
  {$ENDIF};

{$IF Defined(ANDROID)}
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

{$ENDIF}

type
  TAnalyticsThread = class(TThread)
  protected
    procedure Execute; override;
    function Call(const Data: String): Boolean;
  public
    // Message queue management
    class procedure UnpopMessage;
    class procedure PushMessage(const Data: String);
    class function PopMessage(var Data: String; var Time: TDateTime): Boolean;
  end;

var
  AnalyticsThread: TAnalyticsThread;
  AnalyticsPrefix: String;
  AnalyticsUserAgent: String;
  AnalyticsCanUnpop: Boolean;

const
  TJVEAnalyticsClientId = 'TJVEAnalyticsClientId';
  TJVEAnalyticsHead = 'TJVEAnalyticsHead';
  TJVEAnalyticsTail = 'TJVEAnalyticsTail';
  TJVEAnalyticsData = 'TJVEAnalyticsData';
  TJVEAnalyticsTime = 'TJVEAnalyticsTime';

{ TAnalyticsThread }

class procedure TAnalyticsThread.PushMessage(const Data: String);
var
  Tail: Integer;
begin
  Tail := TJVEConfiguration.ReadInteger(TJVEAnalyticsTail, 0);
  if (Tail - TJVEConfiguration.ReadInteger(TJVEAnalyticsHead, 0)) > 98 then
    AnalyticsCanUnpop := False;

  TJVEConfiguration.WriteString(TJVEAnalyticsData + IntToStr(Tail mod 100), Data);
  TJVEConfiguration.WriteDateTime(TJVEAnalyticsTime + IntToStr(Tail mod 100), Now);
  TJVEConfiguration.WriteInteger(TJVEAnalyticsTail, Tail + 1);
end;

class function TAnalyticsThread.PopMessage(var Data: String;
  var Time: TDateTime): Boolean;
var
  Head: Integer;
begin
  Head := TJVEConfiguration.ReadInteger(TJVEAnalyticsHead, 0);

  if Head = TJVEConfiguration.ReadInteger(TJVEAnalyticsTail, 0) then
    Exit(False);

  Data := TJVEConfiguration.ReadString(TJVEAnalyticsData + IntToStr(Head mod 100), '');
  Time := TJVEConfiguration.ReadDateTime(TJVEAnalyticsTime + IntToStr(Head mod 100), 0);
  TJVEConfiguration.WriteInteger(TJVEAnalyticsHead, Head + 1);
  Result := True;
  AnalyticsCanUnpop := True;
end;

class procedure TAnalyticsThread.UnpopMessage;
begin
  if AnalyticsCanUnpop then
    TJVEConfiguration.WriteInteger(TJVEAnalyticsHead,
      TJVEConfiguration.ReadInteger(TJVEAnalyticsHead, 0) - 1);
end;

procedure TAnalyticsThread.Execute;
var
  Data: String;
  Time: TDateTime;
{$IFDEF MACOS}
  Pool: NSAutoreleasePool;
{$ENDIF}
begin
  {$IFDEF MACOS}Pool := TNSAutoreleasePool.Create;{$ENDIF}
  try
    try
      repeat
        if PopMessage(Data, Time) and
          not Call('https://www.google-analytics.com/collect?v=1&aip=1&' + Data +
            'qt=' + IntToStr(Round((Now - Time) * 24 * 60 * 60 * 1000)) +
            '&z=' + IntToStr(Random(MaxInt))) then
        begin
          UnpopMessage;
          Sleep(15000);
        end;

        Sleep(1000);
      until Terminated;
    except
      // This is an analytics job: all exceptions can be silently ignored.
    end;
  finally
    FreeOnTerminate := True;
    {$IFDEF MACOS}Pool.drain;{$ENDIF}
  end;
end;

function TAnalyticsThread.Call(const Data: String): Boolean;
{$IF Defined(MSWINDOWS) and (CompilerVersion >= 29)}
var
  Headers: TNetHeaders;
  Client: THTTPClient;
  Response: IHTTPResponse;
begin
  SetLength(Headers, 1);
  Headers[0] := TNameValuePair.Create('User-Agent', AnalyticsUserAgent);
  Client := THTTPClient.Create;
  try
    try
      Response := Client.Get(Data, nil, Headers);
      Result := (Response <> nil) and (Response.StatusCode = 200);
    finally
      FreeAndNil(Client);
    end;
  except
    Result := False;
  end;
end;
{$ELSE}
var
  Error: String;
  Headers: TStrings;
  Reply: TMemoryStream;
begin
  Error := '';
  Headers := TStringList.Create;
  Headers.Append('User-Agent=' + AnalyticsUserAgent);

  Reply := TJVEOpenURL.DownloadURL(Data, Headers, Error);
  FreeAndNil(Reply);
  Result := Error = '';
end;
{$ENDIF}

{ TJVEAnalytics }

// Out of fear for destroying the old Application.OnException if there is no need,
// we are delaying background thread construction until after loading.
procedure TJVEAnalyticsCommon.AfterConstruction;
begin
  inherited AfterConstruction;
  if not (csDesigning in ComponentState) then
  begin
    if acExceptions in FCollect then
      JVEAnalyticsExceptions := True;

    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackMessage, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackScreen, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackEvent, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackSocial, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackException, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackTransaction, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackItem, MessageTrackingHandler);
    TMessageManager.DefaultManager.SubscribeToMessage(TJVETrackRaw, MessageTrackingHandler);
  end;
end;

procedure TJVEAnalyticsCommon.MessageTrackingHandler(const Sender: TObject;
  const Msg: TMessage);
var
  Screen: TJVETrackScreen absolute Msg;
  Event: TJVETrackEvent absolute Msg;
  Social: TJVETrackSocial absolute Msg;
  Exception: TJVETrackException absolute Msg;
  Transaction: TJVETrackTransaction absolute Msg;
  Item: TJVETrackItem absolute Msg;
  Raw: TJVETrackRaw absolute Msg;
begin
  if (TJVETrackMessage(Msg).Collect <> TJVEAnalyticsInvalidCollect) and
    not (TJVETrackMessage(Msg).Collect in FCollect) then
      Exit;

  if Msg is TJVETrackScreen then
    TrackScreen(Screen.Screen)
  else if Msg is TJVETrackEvent then
    TrackEvent(Event.Category, Event.Action, Event.EvLabel, Event.Value)
  else if Msg is TJVETrackSocial then
    TrackSocial(Social.Network, Social.Action, Social.Target)
  else if Msg is TJVETrackException then
    TrackException(Exception.Exception, Exception.Fatal)
  else if Msg is TJVETrackTransaction then
    TrackTransaction(Transaction.TransactionId, Transaction.Affiliation,
      Transaction.Revenue, Transaction.Shipping, Transaction.Tax, Transaction.Currency)
  else if Msg is TJVETrackItem then
    TrackItem(Item.TransactionId, Item.ItemName,
      Item.Price, Item.Quantity, Item.Code, Item.Category, Item.Currency)
  else if Msg is TJVETrackRaw then
    TrackRaw(Raw.Data);
end;

class procedure TJVEAnalyticsCommon.BroadcastEvent(const Category, Action,
  EvLabel: String; Value: Integer; Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackEvent.Create(Category, Action, EvLabel, Value, Collect));
end;

class procedure TJVEAnalyticsCommon.BroadcastException(const Exception: String;
  Fatal: Boolean; Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackException.Create(Exception, Fatal, Collect));
end;

class procedure TJVEAnalyticsCommon.BroadcastItem(const TransactionId,
  ItemName: String; Price: Double; Quantity: Integer; const Code, Category,
  Currency: String; Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TJVETrackItem.Create(
    TransactionId, ItemName, Price, Quantity, Code, Category, Currency, Collect));
end;

procedure TJVEAnalyticsCommon.BroadcastRaw(const Data: String;
  Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackRaw.Create(Data, Collect));
end;

class procedure TJVEAnalyticsCommon.BroadcastScreen(const Screen: String;
  Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackScreen.Create(Screen, Collect));
end;

class procedure TJVEAnalyticsCommon.BroadcastSocial(const Network, Action,
  Target: String; Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil,
    TJVETrackSocial.Create(Network, Action, Target, Collect));
end;

class procedure TJVEAnalyticsCommon.BroadcastTransaction(const TransactionId,
  Affiliation: String; Revenue, Shipping, Tax: Double; const Currency: String;
  Collect: TJVEAnalyticsCollect);
begin
  TMessageManager.DefaultManager.SendMessage(nil, TJVETrackTransaction.Create(
    TransactionId, Affiliation, Revenue, Shipping, Tax, Currency, Collect));
end;

constructor TJVEAnalyticsCommon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := True;
  FCollect := [Low(TJVEAnalyticsCollect)..High(TJVEAnalyticsCollect)];
end;

procedure TJVEAnalyticsCommon.CreateThread;
begin
  if not (csDesigning in ComponentState) and (AnalyticsThread = nil) then
    AnalyticsThread := TAnalyticsThread.Create(False);
end;

destructor TJVEAnalyticsCommon.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackMessage, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackScreen, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackEvent, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackSocial, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackException, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackTransaction, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackItem, MessageTrackingHandler);
  TMessageManager.DefaultManager.Unsubscribe(TJVETrackRaw, MessageTrackingHandler);
  inherited Destroy;
end;

class function TJVEAnalyticsCommon.Param(const Name: String; Value: Integer;
  Mandatory: Boolean): String;
begin
  if Mandatory or (Value <> 0) then
    Result := Name + '=' + IntToStr(Value) + '&'
  else
    Result := '';
end;

class function TJVEAnalyticsCommon.Param(const Name, Value: String; Mandatory: Boolean): String;
  {$IF Defined(MSWINDOWS)}
  function URLEncode(Const Value: String): String;
  var
    Ch: Byte;
  begin
    Result := '';
    for Ch in TEncoding.UTF8.GetBytes(Value) do
      if Ch in [Ord('A')..Ord('Z'), Ord('a')..Ord('z'), Ord('_'),
        Ord('0')..Ord('9'), Ord('*'), Ord('-'), Ord('.')] then
          Result := Result + Char(Ch)
        else
          Result := Result + '%' + IntToHex(Ord(Ch), 2);
  end;
  {$ENDIF}
begin
  if Value <> '' then
    Result := Name + '=' +
      {$IF not Defined(MSWINDOWS)}TJVEOpenURL.{$ENDIF}URLEncode(Value) + '&'
  else if Mandatory then
    Result := Name + '=None&'
  else
    Result := '';
end;

class function TJVEAnalyticsCommon.Param(const Name: String; Value: Double;
  Mandatory: Boolean): String;
begin
  if Mandatory or (Value <> 0) then
    Result := Name + '=' + FloatToStr(Value) + '&'
  else
    Result := '';
end;

procedure TJVEAnalyticsCommon.TrackEvent(const Category, Action, EvLabel: String;
  Value: Integer);
begin
  TrackRaw('t=event&' +
    Param('ec', Category, True) +
    Param('ea', Action, True) +
    Param('el', EvLabel, False) +
    Param('ev', Value, False));
end;

procedure TJVEAnalyticsCommon.TrackException(const Exception: String; Fatal: Boolean);
begin
  TrackRaw('t=exception&' +
    Param('exd', Exception, True) +
    Param('exf', IfThen(Fatal, 1, 0), True));
end;

procedure TJVEAnalyticsCommon.TrackItem(const TransactionId, ItemName: String;
  Price: Double; Quantity: Integer; const Code, Category, Currency: String);
begin
  TrackRaw('t=item&' +
    Param('ti', TransactionId, True) +
    Param('in', ItemName, True) +
    Param('ip', Price, False) +
    Param('iq', Quantity, False) +
    Param('ic', Code, False) +
    Param('iv', Category, False) +
    Param('cu', Currency, False));
end;

procedure TJVEAnalyticsCommon.TrackScreen(const Screen: String);
begin
  if Screen <> '' then
    FFormName := Screen;
  TrackRaw('t=appview&');
end;

procedure TJVEAnalyticsCommon.TrackSocial(const Network, Action, Target: String);
begin
  TrackRaw('t=social&' +
    Param('sn', Network, True) +
    Param('sa', Action, True) +
    Param('st', Target, True));
end;

procedure TJVEAnalyticsCommon.TrackTransaction(const TransactionId,
  Affiliation: String; Revenue, Shipping, Tax: Double; const Currency: String);
begin
  TrackRaw('t=transaction&' +
    Param('ti', TransactionId, True) +
    Param('ta', Affiliation, False) +
    Param('tr', Revenue, False) +
    Param('ts', Shipping, False) +
    Param('tt', Tax, False) +
    Param('cu', Currency, False));
end;

procedure TJVEAnalyticsCommon.TrackRaw(const Data: String);
var
  Acc: String;
  Size: TSize;
  {$IF not Defined(MACOS) and not Defined(ANDROID)}
  LocaleName: array [0..LOCALE_NAME_MAX_LENGTH] of WideChar;
  {$ENDIF}
begin
  if not FActive then
    Exit;

  {$IF Defined(MACOS)}
  Acc := FromNSSTR(TNSLocale.Wrap(TNSLocale.OCClass.
    currentLocale).localeIdentifier).Replace('_', '-');
  if Acc.Contains('@') then
    Acc := Acc.Split(['@'])[0];
  {$ELSEIF Defined(ANDROID)}
  Acc := JStringToString(TJLocale.JavaClass.getDefault.getLanguage) + '-' +
    JStringToString(TJLocale.JavaClass.getDefault.getCountry);
  if (Pos('-', Acc) = 1) or (Pos('-', Acc) = Length(Acc)) then
    Acc := 'en-us';
  {$ELSE}
  if Languages.IndexOf(TLanguages.UserDefaultLocale) = -1 then
  begin
    Winapi.Windows.GetUserDefaultLocaleName(LocaleName, LOCALE_NAME_MAX_LENGTH);
    Acc := LocaleName;
  end else
    Acc := Languages.LocaleName[Languages.IndexOf(TLanguages.UserDefaultLocale)];
  {$ENDIF}

  Acc := AnalyticsPrefix + Data + Param('ul', Acc, True) +
    Param('cd', FFormName, True) + Param('tid', FPropertyId, True);

  if Assigned(JVEAnalyticsScreenSize) then
  begin
    Size := JVEAnalyticsScreenSize;
    Acc := Acc + Format('sr=%dx%d&', [Size.Width, Size.Height]);
  end;

  if Assigned(JVEAnalyticsFormSize) then
  begin
    Size := JVEAnalyticsFormSize;
    if (Size.cx <> 0) and (Size.cy <> 0) then
      Acc := Acc + Format('vp=%dx%d&', [Size.Width, Size.Height]);
  end;

  if not FInSession then
  begin
    FInSession := True;
    Acc := Acc + 'sc=start&';
  end;

  TAnalyticsThread.PushMessage(Acc);
end;

procedure InitAnalyticsVariables;
var
  Client, App, Version: String;
  {$IFDEF MSWINDOWS}
  Size, Len: Cardinal;
  Buffer, Value: PChar;
  {$ENDIF}
begin
  Client := TJVEConfiguration.ReadString(TJVEAnalyticsClientId, '');
  if Length(Client) <> 36 then
  begin
    {$IFDEF ANDROID}
    Client := JStringToString(TJUUID.JavaClass.randomUUID.toString).Trim(['{', '}']);
    {$ELSE}
    Client := TGUID.NewGuid.ToString.Trim(['{', '}']);
    {$ENDIF}
    TJVEConfiguration.WriteString(TJVEAnalyticsClientId, Client);
  end;
  {$IF Defined(MACOS)}
  App := FromNSSTR(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).
    infoDictionary.objectForKey(PointerNSSTR('CFBundleName')));
  Version := FromNSSTR(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).
    infoDictionary.objectForKey(PointerNSSTR('CFBundleVersion')));

  AnalyticsUserAgent := Format(
    {$IFDEF IOS}'Mozilla/5.0 (%s; CPU OS %d_%d_%d like Mac OS X)',
      [FromNSSTR(TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).model),
    {$ELSE}'Mozilla/5.0 (Macintosh; Intel Mac OS X %d.%d.%d)', [
    {$ENDIF}TOSVersion.Major, TOSVersion.Minor, TOSVersion.ServicePackMajor]);

  {$ELSEIF Defined(ANDROID)}
  App := {$IF CompilerVersion >= 30} TAndroidHelper.ApplicationTitle;{$ELSE} GetApplicationTitle;{$ENDIF}
  try
    Version := JStringToString(GetSharedActivity.getPackageManager().getPackageInfo(
      GetSharedActivity.getPackageName(), 0).versionName);
  except
    on {JPackageManager_NameNotFoundExceptionClass} Exception do
    begin
      Version := '';
    end;
  end;

  AnalyticsUserAgent := JStringToString(TJSystem.JavaClass.getProperty(
    StringToJString('http.agent'), StringToJString(Format(
    'Mozilla/5.0 (Linux; Android %d.%d)', [TOSVersion.Major, TOSVersion.Minor]))));

  {$ELSEIF Defined(MSWINDOWS)}
  App := ExtractFileName(ParamStr(0));
  if App.Contains('.') then
    App := App.Split(['.'])[0];

  Version := '1.0';
  Size := GetFileVersionInfoSize(PChar(ParamStr(0)), Len);
  if Size > 0 then
  begin
    Buffer := AllocMem(Size);
    try
      GetFileVersionInfo(PChar(ParamStr(0)), 0, Size, Buffer);
      if VerQueryValue(Buffer, 'StringFileInfo\040904E4\FileVersion',
        Pointer(Value), Len) then
          Version := Trim(Value);
    finally
      FreeMem(Buffer, Size);
    end;
  end;

  AnalyticsUserAgent := Format('Mozilla/5.0 (compatible; Windows NT %d.%d)',
    [TOSVersion.Major, TOSVersion.Minor]);
  {$ENDIF}

  AnalyticsPrefix := TJVEAnalyticsCommon.Param('cid', Client, True) +
    TJVEAnalyticsCommon.Param('an', App, True) +
    TJVEAnalyticsCommon.Param('av', Version, True);
end;

initialization
  InitAnalyticsVariables;
finalization
  if AnalyticsThread <> nil then
    AnalyticsThread.Terminate;
end.

