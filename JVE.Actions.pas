(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Actions;

interface

uses System.Classes, System.SysUtils, FMX.Types, FMX.Dialogs, JVE.Utils
  {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

type
  TJVECustomAd = class;
  TJVEAction = class;

  TJVEAppDetails = class(TPersistent)
  private
    FBeacons: TStrings;
    FBitmap: TBitmap;
    FRating: Single;
    FTitle: String;
    FIcon: TBitmap;
    FAdvertiser: String;
    FDescription: String;
    FCallToAction: String;
    FURL: String;
    FOwner: TJVEAction;
    FBeaconHeaders: TStringList;
    procedure SetAdvertiser(const Value: String);
    procedure SetBeacons(const Value: TStrings);
    procedure SetBitmap(const Value: TBitmap);
    procedure SetCallToAction(const Value: String);
    procedure SetDescription(const Value: String);
    procedure SetIcon(const Value: TBitmap);
    procedure SetRating(const Value: Single);
    procedure SetTitle(const Value: String);
    procedure SetURL(const Value: String);
    procedure SetBeaconHeaders(const Value: TStringList);
    procedure OpenBeaconURLs(URLs, Headers: TStrings);
  public
    constructor Create(Owner: TJVEAction = nil);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Owner: TJVEAction read FOwner;

    // Perform the action prescribed by the app: opens the URL. Returns True
    // if successfully opened.
    function Open: Boolean;

    // Sequentially reports each of the beacons (should normally be called
    // when the app is presented).
    // Upon a call clears the Beacons list, thus multiple consecutive calls are ignored.
    // The Background parameter can be used to specify whether the reports
    // should be sent in a background thread.
    procedure Presented(Background: Boolean);
    property BeaconHeaders: TStringList read FBeaconHeaders write SetBeaconHeaders;
  published
    property Title: String read FTitle write SetTitle;
    property Description: String read FDescription write SetDescription;
    property CallToAction: String read FCallToAction write SetCallToAction;
    property Advertiser: String read FAdvertiser write SetAdvertiser;
    property Rating: Single read FRating write SetRating;
    property Icon: TBitmap read FIcon write SetIcon;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property URL: String read FURL write SetURL;
    property Beacons: TStrings read FBeacons write SetBeacons;
  end;

  IJVEInterstitial = interface
    ['{2717A3F4-6FDA-4FA4-A954-45E2A900A205}']
    // Caches the interstitial image.
    procedure Cache(Sender: TJVEAction);
    // Shows the interstitial, if cached.
    procedure Show(Sender: TJVEAction);
    // Checks whether the interstitial is cached.
    function IsCached(Sender: TJVEAction): Boolean;
  end;

  IJVEExecutable = interface
    ['{F86F1CF6-99C0-42E6-A1BB-7EB19AC04F4E}']
    // Starts the action prescribed by the interstitial.
    procedure Open(Sender: TJVEAction);
    // Checks whether the interstitial is valid.
    function CanOpen(Sender: TJVEAction): Boolean;
  end;

  IJVEActionsUpdated = interface
    ['{DDC24C34-9875-4C77-972E-CD4857A2AE11}']
    procedure Update(Item: TJVEAction);
  end;

  // This is an executable which manages App Details by itself and supports
  // consecutive requests (i.e. Native Ad can repeatedly present these ads,
  // if CanOpen returns True).
  IJVEManagedExecutable = interface(IJVEExecutable)
    ['{4ADD8F43-577B-411B-ADF7-C9E930E69FBF}']
  end;

  TJVEActionNotification = procedure(Sender: TJVEAction) of object;
  TJVECustomAdShowEvent = procedure(CustomAd: TJVECustomAd;
    Item: TJVEAction; var Show: Boolean) of object;
  TJVECustomAdClickEvent = procedure(CustomAd: TJVECustomAd;
    Item: TJVEAction) of object;

  // Use this component to show custom ads through the TJVEAction.
  // You can also simply create a class which implements the IJVEExecutable class
  // (then you can use this implementation as an example).
  [ComponentPlatformsAttribute($000B945F)]
  TJVECustomAd = class(TComponent, IJVEExecutable)
  private
    FOnShouldShow: TJVECustomAdShowEvent;
    FOnClick: TJVECustomAdClickEvent;
    FOnShowCustom: TJVECustomAdShowEvent;
  protected
    // IJVEExecutable
    procedure Open(Sender: TJVEAction);
    function CanOpen(Sender: TJVEAction): Boolean;
  published
    // Use this event the decide whether the ad is applicable now (return False
    // in Show to prevent the ad from showing; if you return True, the ad
    // is presented immediate after this call).
    // You can also use this event to update the bitmap. To ease this task, the
    // Item for which the interstitial is about to be shown is also passed here.
    property OnShouldShow: TJVECustomAdShowEvent read FOnShouldShow write FOnShouldShow;

    // This event will be called, if the user clicked the ad.
    property OnClick: TJVECustomAdClickEvent read FOnClick write FOnClick;

    // Use this event to specify custom presentation for your interstitial.
    // If default processing is still prefered, set Show to True.
    property OnShowCustom: TJVECustomAdShowEvent read FOnShowCustom write FOnShowCustom;
  end;

  TJVEAction = class(TCollectionItem)
  private
    [Weak]FAction: TComponent;
    FWeight: Integer;
    FIdentifier: String;
    FImpressions: Integer;
    FChecked: Boolean;
    FOnLoad: TJVEActionNotification;
    FOnUnload: TJVEActionNotification;
    FAppDetails: TJVEAppDetails;
    FWasPresented: Boolean;
    function GetBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetAction(const Value: TComponent);
    procedure SetAppDetails(const Value: TJVEAppDetails);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Do not use. Used internally by TJVEInterstitials and TJVEBanners.
    property Checked: Boolean read FChecked write FChecked;
    // The number of impressions this interstitial got (plus 1, for internal
    // convinience). Set this value to affect the frequency this interstitial
    // will be presented (set to 0 to make it the very next one).
    property Impressions: Integer read FImpressions write FImpressions;
    // Indicates whether the action was already presented. This could be used
    // to update the message to be presented the next time.
    // It is the responsibility of the action to clear this flag.
    property WasPresented: Boolean read FWasPresented write FWasPresented;
  published
    // The action to take on an interstitial click.
    property Action: TComponent read FAction write SetAction;
    // The weight of this interstitial in a random queue (Weight=2 will be shown
    // twice as often as Weight=1). Statistics are reset on each start.
    // What counts is an attempt to show, not whether the ad was actually shown.
    // So, if one ad was unavailable and then will become available, its weight
    // will be used starting at that moment, not retroactively.
    property Weight: Integer read FWeight write FWeight default 1;
    // For IJVEExecutables only, the bitmap to present.
    property Bitmap: TBitmap read GetBitmap write SetBitmap stored False;
    // For TPayment specifically, this chooses the TJVEPaymentItem.
    property Identifier: String read FIdentifier write FIdentifier;

    // Used for native ads. Provides all the details about the ad for presentation.
    // Copied when preserved: any further changes will not update the ad.
    property AppDetails: TJVEAppDetails read FAppDetails write SetAppDetails;

    // This event is called immediately before the interstitial is about to be
    // presented and just after it was closed. These events can be used to load
    // the image and dispose of them after showing.
    // These events are only called for IJVEExecutable (IJVEInterstitial objects
    // usually manage loading internally).
    property OnLoad: TJVEActionNotification read FOnLoad write FOnLoad;
    // If you want to unload images, do it with Sender.Bitmap.SetSize(0,0);
    property OnUnload: TJVEActionNotification read FOnUnload write FOnUnload;
  end;

  TJVEActions = class(TOwnedCollection)
  protected
    function GetItem(Index: Integer): TJVEAction;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    procedure ComponentRemoved(AComponent: TComponent);
    property Items[Index: Integer]: TJVEAction read GetItem; default;
  end;

implementation

uses System.Generics.Collections, System.UITypes, System.UIConsts,
  System.Types, FMX.Platform, FMX.Objects, FMX.Forms, FMX.StdCtrls, FMX.Effects,
  FMX.Ani, JVE.OpenURL {$IFDEF ANDROID}, FMX.Platform.Android{$ENDIF};

{ TJVEActions }

constructor TJVEActions.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJVEAction);
end;

function TJVEActions.GetItem(Index: Integer): TJVEAction;
begin
  Result := TJVEAction(inherited GetItem(Index));
end;

procedure TJVEActions.ComponentRemoved(AComponent: TComponent);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    if Items[Index].FAction = AComponent then
    begin
      Items[Index].FAction := nil;
      Items[Index].Changed(False);
    end;
end;

procedure TJVEActions.Update(Item: TCollectionItem);
var
  Intr: IJVEActionsUpdated;
begin
  if Owner.GetInterface(IJVEActionsUpdated, Intr) then
    Intr.Update(TJVEAction(Item));
end;

{ TJVEAction }

constructor TJVEAction.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FWeight := 1;
  // Factors work much better, if we are starting counting at 1.
  FImpressions := 1;
  FAppDetails := TJVEAppDetails.Create(Self);
end;

destructor TJVEAction.Destroy;
begin
  // Using the setter for the insert/remove notification
  Action := nil;
  FreeAndNil(FAppDetails);
  inherited Destroy;
end;

procedure TJVEAction.Assign(Source: TPersistent);
var
  Item: TJVEAction absolute Source;
begin
  if Source is TJVEAction then
  begin
    // Using the setter for the insert/remove notification
    Action := Item.FAction;
    FWeight := Item.FWeight;
    FAppDetails.Assign(Item.FAppDetails);
  end else
    inherited Assign(Source);
end;

function TJVEAction.GetBitmap: TBitmap;
begin
  Result := FAppDetails.FBitmap;
end;

function TJVEAction.GetDisplayName: string;
begin
  if Action = nil then
    Result := 'Unassigned'
  else if UpperCase(Action.ClassName) <> 'TJVEPAYMENT' then
    Result := Action.Name
  else if Identifier <> '' then
    Result := Action.Name + ' - ' + FIdentifier
  else
    Result := Action.Name + ' (Unassigned)';
end;

procedure TJVEAction.SetBitmap(const Value: TBitmap);
begin
  FAppDetails.FBitmap.Assign(Value);
  Changed(False);
end;

procedure TJVEAction.SetAction(const Value: TComponent);
begin
  if (FAction <> nil) and (Collection <> nil) and
    (Collection.Owner <> nil) and (Collection.Owner is TComponent) then
      FAction.RemoveFreeNotification(TComponent(Collection.Owner));
  FAction := Value;
  if (FAction <> nil) and (Collection <> nil) and
    (Collection.Owner <> nil) and (Collection.Owner is TComponent) then
      FAction.FreeNotification(TComponent(Collection.Owner));
  Changed(False);
end;

procedure TJVEAction.SetAppDetails(const Value: TJVEAppDetails);
begin
  FAppDetails.Assign(Value);
  Changed(False);
end;

{ TJVECustomAd }

function TJVECustomAd.CanOpen(Sender: TJVEAction): Boolean;
begin
  Result := True;
  if Assigned(FOnShouldShow) then
    FOnShouldShow(Self, Sender, Result);
end;

procedure TJVECustomAd.Open(Sender: TJVEAction);
begin
  if Assigned(FOnClick) then
    FOnClick(Self, Sender);
end;

{ TJVEAppDetails }

procedure TJVEAppDetails.Assign(Source: TPersistent);
var
  Item: TJVEAppDetails absolute Source;
begin
  if Source is TJVEAppDetails then
  begin
    FTitle := Item.FTitle;
    FDescription := Item.FDescription;
    FCallToAction := Item.FCallToAction;
    FAdvertiser := Item.FAdvertiser;
    FRating := Item.FRating;
    FURL := Item.FURL;
    FIcon.Assign(Item.FIcon);
    FBitmap.Assign(Item.FBitmap);
    FBeacons.Assign(Item.FBeacons);
  end else
    inherited Assign(Source);
end;

constructor TJVEAppDetails.Create(Owner: TJVEAction);
begin
  inherited Create;
  FOwner := Owner;
  FIcon := TBitmap.Create(0, 0);
  FBitmap := TBitmap.Create(0, 0);
  FBeacons := TStringList.Create;
  FBeaconHeaders := TStringList.Create;
end;

destructor TJVEAppDetails.Destroy;
begin
  FreeAndNil(FBeaconHeaders);
  FreeAndNil(FBeacons);
  FreeAndNil(FBitmap);
  FreeAndNil(FIcon);
  inherited Destroy;
end;

function TJVEAppDetails.Open: Boolean;
begin
  Result := (URL <> '') and TJVEOpenURL.CanOpenURL(URL);
  if Result then
    TJVEOpenURL.OpenURL(URL);
end;

procedure TJVEAppDetails.OpenBeaconURLs(URLs, Headers: TStrings);
var
  URL: String;
  Stream: TMemoryStream;
begin
  for URL in URLs do
    if URL.Contains('://') then
    begin
      Stream := TJVEOpenURL.DownloadURL(URL, Headers);
      FreeAndNil(Stream);
    end;
end;

procedure TJVEAppDetails.Presented(Background: Boolean);
var
  URLs, Headers: TStringList;
begin
  if Beacons.Count = 0 then
    Exit;

  if Background then
  begin
    URLs := TStringList.Create;
    URLs.Assign(FBeacons);
    FBeacons.Clear;
    Headers := TStringList.Create;
    Headers.Assign(FBeaconHeaders);
    FBeaconHeaders.Clear;

    DetachThread(procedure(Synchronize: TProc<TThreadProcedure>)
    begin
      OpenBeaconURLs(URLs, Headers);
      FreeAndNil(URLs);
      FreeAndNil(Headers);
    end);
  end else
  begin
    OpenBeaconURLs(FBeacons, FBeaconHeaders);
    FBeacons.Clear;
  end;
end;

procedure TJVEAppDetails.SetAdvertiser(const Value: String);
begin
  FAdvertiser := Value;
end;

procedure TJVEAppDetails.SetBeaconHeaders(const Value: TStringList);
begin
  FBeaconHeaders.Assign(Value);
end;

procedure TJVEAppDetails.SetBeacons(const Value: TStrings);
begin
  FBeacons.Assign(Value);
end;

procedure TJVEAppDetails.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TJVEAppDetails.SetCallToAction(const Value: String);
begin
  FCallToAction := Value;
end;

procedure TJVEAppDetails.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TJVEAppDetails.SetIcon(const Value: TBitmap);
begin
  FIcon.Assign(Value);
end;

procedure TJVEAppDetails.SetRating(const Value: Single);
begin
  FRating := Value;
end;

procedure TJVEAppDetails.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TJVEAppDetails.SetURL(const Value: String);
begin
  FURL := Value;
end;

end.

