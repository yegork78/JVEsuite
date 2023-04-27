(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Interstitials;

interface

uses System.Classes, System.SysUtils, FMX.Types, JVE.Actions, JVE.Utils, FMX.Forms,
  FMX.Ani, FMX.Effects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, JVE.Layout, System.Types
  {$IF (CompilerVersion >= 27) and Defined(ANDROID)}, AndroidApi.Helpers{$ENDIF}
  {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

// This control presents interstitials (ads and promos) on major events.
// For example, finishing a level is a major event in your game and you want
// to show ads / promos when the user finishes a level, you can use this component.
//
// An interstitial is presented on major events (call to a ProcessMajorEvent
// function), but not before a specified time has passed (see properties).
// You can specify a given time between ads (or 0, in case every major event
// deserves an ad).
// You can also specify a time penalty for major events, so you can specify:
// * Time Between Ads    = 60 seconds
// * Major Event Penalty = 20 seconds
// A usual user will see an ad every 60 seconds or more, but a user, which opens
// a level and closes it immediately will see an ad sooner: maybe after just
// 20 seconds, if in that time he managed to open and close levels 3 times.
//
// This component can present interstitials, choosing them at random, out of the
// Interstitials collection, based on the weight provided for each. There are
// two types of interstitials supported:
// * IJVEInterstitial (like TJVEChartBoost, TJVERevMob and TJVEAppBasicData) -
//   provide a self contained implementation. You can simply select the
//   interstitial compoment in the collection (and, optionally, set its weight)
//   and it will present itself.
// * IJVEExecutable (like TJVESocial, TJVEAppStore, TJVEBrowser, TJVEPaymentItem,
//   TJVEOpenURL and TJVEEmail) - only take care of the click implementation,
//   so you also need to provide an interstitial image (TBitmap) to present.
//
// Interstitials are capable of checking whether they need to be shown:
// * Ads will only be presented if they are cached.
// * TJVEPaymentItem might not be presented if it was already bought.
// * TJVEAppStore might not be presented if the app is already installed.

type
  [ComponentPlatformsAttribute($000B945F)]
  TJVEInterstitials = class(TComponent)
  private
    FFirstTimeToAd: Integer;
    FEachTimeToAd: Integer;
    FTimeBetweenAds: Integer;
    FEventPenalty: Integer;
    FInterstitials: TJVEActions;
    FCountdown: Integer;
    FInitialized: Boolean;
    FTimerHandle: TFmxHandle;
    procedure Timer;
    procedure SetInterstitials(const Value: TJVEActions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    // Use this function to mark the major event within the app. This function
    // will show the interstitial (if appropriate). Returns True, if the
    // interstitial was (and thus, is being) presented; returns immediately.
    // Since clicking the interstitial might take the user out of the app and
    // it is not known when (if) he will be back, if this function returns True,
    // make sure the app is idle (waiting for user input) and its state is saved.
    function ProcessMajorEvent: Boolean;

    // Made public, as it might be usefull outside the scope of this component:
    // shows a bitmap as an interstitial and calls Click, if it was clicked.
    class procedure ShowInterstitial(Bitmap: TBitmap; Click, Close: TProc);
  published
    // The number of seconds, which should pass, before an interstitial is shown
    // for the very first time (running the app for the first time).
    property FirstTimeToAd: Integer read FFirstTimeToAd write FFirstTimeToAd default 300;
    // Seconds before an interstitial is shown after an app is opened (but not
    // for the first time: see above).
    property EachTimeToAd: Integer read FEachTimeToAd write FEachTimeToAd default 60;
    // Seconds between interstitials presentation.
    property TimeBetweenAds: Integer read FTimeBetweenAds write FTimeBetweenAds default 60;
    // Penalty seconds for each major event.
    property EventPenalty: Integer read FEventPenalty write FEventPenalty default 20;
    // This is the list of interstitials to be presented.
    property Interstitials: TJVEActions read FInterstitials write SetInterstitials;
  end;

type
  // This is an internal class. Please don't use it directly.
  TJVEFormInterstitials = class(TForm)
    Shade: TRectangle;
    Image: TImage;
    Cancel: TLabel;
    Animation: TFloatAnimation;
    Layout: TJVELayout;
    Shadow: TShadowEffect;
    procedure AnimationFinish(Sender: TObject);
    procedure ImageClick(Sender: TObject);
    procedure LayoutClick(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
  private
    FClick, FClose: TProc;
  end;

implementation

{$R *.fmx}

{ $DEFINE FULL_VERSION}

uses System.Generics.Collections, System.UITypes, System.UIConsts,
  FMX.Platform, JVE.Banners, {$IFDEF FULL_VERSION}JVE.AppStore,{$ENDIF}
  {$IF Defined(IOS)} iOSapi.Foundation, iosAPI.CocoaTypes;
  {$ELSEIF Defined(MACOS)} MacApi.Foundation;
  {$ELSEIF Defined(ANDROID)} FMX.Platform.Android, AndroidApi.JNI.GraphicsContentViewText,
    AndroidApi.JNI.App, Androidapi.JNI.JavaTypes;
  {$ELSEIF Defined(MSWINDOWS)} System.Win.Registry; {$ENDIF}

{$IF Defined(MACOS)}

var
  PreferencesObject: NSUserDefaults;

function ReadBool(const Ident: String): Boolean; inline;
var
  lPtr: Pointer;
begin
  if PreferencesObject = nil then
    PreferencesObject := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  lPtr := PreferencesObject.objectForKey(ToNSSTR(Ident));
  Result := Assigned(lPtr) and TNSNumber.Wrap(lPtr).boolValue;
end;

procedure WriteBool(const Ident: String; Value: Boolean); inline;
begin
  PreferencesObject.setObject(TNSNumber.OCClass.numberWithBool(Value), ToNSSTR(Ident));
  PreferencesObject.synchronize;
end;

{$ELSEIF Defined(ANDROID)}

var
  PreferencesObject: JSharedPreferences;

function ReadBool(const Ident: String): Boolean;
begin
  if PreferencesObject = nil then
    PreferencesObject := GetSharedActivityContext.getSharedPreferences(
      GetSharedActivity.getLocalClassName, TJContext.JavaClass.MODE_PRIVATE);
  Result := PreferencesObject.getBoolean(StringToJString(Ident), False);
end;

procedure WriteBool(const Ident: String; Value: Boolean); inline;
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := PreferencesObject.edit;
  Editor.putBoolean(StringToJString(Ident), Value);
  Editor.apply;
end;

{$ELSE}

var
  PreferencesObject: TRegistry;

function ReadBool(const Ident: String): Boolean; inline;
var
  App: String;
begin
  if PreferencesObject = nil then
  begin
    App := ExtractFileName(ParamStr(0));
    if App.Contains('.') then
      App := App.Split(['.'])[0];
    PreferencesObject := TRegistry.Create;
    PreferencesObject.OpenKey('Software\' + App, True);
  end;

  Result := (PreferencesObject.GetDataType(Ident) = rdInteger)
    and PreferencesObject.ReadBool(Ident);
end;

procedure WriteBool(const Ident: String; Value: Boolean); inline;
begin
  PreferencesObject.WriteBool(Ident, Value);
end;

{$ENDIF}

{ TJVEInterstitials }

constructor TJVEInterstitials.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInterstitials := TJVEActions.Create(Self);
  FFirstTimeToAd := 300;
  FEachTimeToAd := 60;
  FTimeBetweenAds := 60;
  FEventPenalty := 20;
end;

destructor TJVEInterstitials.Destroy;
begin
  if FTimerHandle <> 0 then
  begin
    (TPlatformServices.Current.GetPlatformService(IFMXTimerService)
      as IFMXTimerService).DestroyTimer(FTimerHandle);
    FTimerHandle := 0;
  end;

  FreeAndNil(FInterstitials);
  inherited Destroy;
end;

procedure TJVEInterstitials.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (FInterstitials <> nil) and (Operation = opRemove) then
    FInterstitials.ComponentRemoved(AComponent);
end;

procedure TJVEInterstitials.AfterConstruction;
begin
  inherited AfterConstruction;
  if not (csDesigning in ComponentState) then
    FTimerHandle := (TPlatformServices.Current.GetPlatformService
      (IFMXTimerService) as IFMXTimerService).CreateTimer(1000, Timer);
end;

procedure TJVEInterstitials.SetInterstitials(const Value: TJVEActions);
begin
  FInterstitials.Assign(Value);
end;

procedure TJVEInterstitials.Timer;
var
  CollectionItem: TCollectionItem;
  Item: TJVEAction absolute CollectionItem;
  Intf: IJVEInterstitial;
  Index: String;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Index := 'TJVEInterstitials-' + Name;
    if not ReadBool(Index) then
    begin
      FCountdown := FFirstTimeToAd;
      WriteBool(Index, True);
    end else
      FCountdown := FEachTimeToAd;
  end else
    Dec(FCountdown);

  // We are starting to cache interstitials 30 seconds before we can need them,
  // repeating caching attempts every 3 seconds. A well behaved IJVEInterstitial
  // should not cause any penalties for the extra calls.
  if (FCountdown <= 30) and (FCountdown mod 3 = 0) then
    for CollectionItem in FInterstitials do
      if (Item.Action <> nil) and
        Item.Action.GetInterface(IJVEInterstitial, Intf) and
        not Intf.IsCached(Item) then
          Intf.Cache(Item);
end;

function TJVEInterstitials.ProcessMajorEvent: Boolean;
var
  MinValue, Value: Single;
  Index: Integer;
  Items: TList<TJVEAction>;
  Item: TJVEAction;
  Intr: IJVEInterstitial;
  Exec: IJVEExecutable;
  DefaultProc: Boolean;
begin
  if not FInitialized then
    Exit(False);

  if FCountdown > 0 then
  begin
    Dec(FCountdown, FEventPenalty);
    Exit(False);
  end else
    FCountdown := FTimeBetweenAds;

  for Index := 0 to FInterstitials.Count - 1 do
    FInterstitials[Index].Checked := FInterstitials[Index].Action = nil;

  Items := TList<TJVEAction>.Create;
  try
    while True do
    begin
      MinValue := MaxInt;
      Items.Clear;
      for Index := 0 to FInterstitials.Count - 1 do
      begin
        Item := FInterstitials[Index];
        if Item.Checked then
          Continue;

        // If TJVEAppRater can show its message now, it should. We are giving this
        // boost because App Rater is the only service, which can be run once every
        // several days, so when it is available, we should give it a change.
{$IFDEF FULL_VERSION}
        if (Item.Action is TJVEAppRater) and
          (TJVEAppRater(Item.Action).ReminderDaysDelay > 0) then
            Value := 0
          else
{$ENDIF}
            Value := Item.Impressions / Item.Weight;

        if MinValue > Value then
        begin
          MinValue := Value;
          Items.Clear;
          Items.Add(Item);
        end else if MinValue = Value then
          Items.Add(Item);
      end;

      if Items.Count = 0 then
        Exit(False);

      // Selecting one at random is particularly important at the very first
      // calls, when several interstitials have the same relative weight.
      Item := Items[Random(Items.Count)];
      Item.Checked := True;
      Item.Impressions := Item.Impressions + 1;

      if Item.Action.GetInterface(IJVEInterstitial, Intr) then
      begin
        if Intr.IsCached(Item) then
        begin
          Item.WasPresented := True;
          Intr.Show(Item);
          Exit(True);
        end;
      end else
      if Item.Action.GetInterface(IJVEExecutable, Exec) then
      begin
        if Exec.CanOpen(Item) then
        begin
          DefaultProc := False;
          if (Item.Action is TJVECustomAd) and Assigned(TJVECustomAd(Item.Action).OnShowCustom) then
          begin
            TJVECustomAd(Item.Action).OnShowCustom(TJVECustomAd(Item.Action), Item, DefaultProc);
            if not DefaultProc then
              Exit(True);
          end;

          if Assigned(Item.OnLoad) then
            Item.OnLoad(Item);

          if not Item.Bitmap.IsEmpty then
          begin
            ShowInterstitial(Item.Bitmap,
              procedure
              begin
                Item.WasPresented := True;
                Exec.Open(Item);
              end,
              procedure
              begin
                if Assigned(Item.OnUnload) then
                  Item.OnUnload(Item);
              end);
            Exit(True);
          end else
          if DefaultProc then
            Exit(True);
        end;
      end;
    end;
  finally
    FreeAndNil(Items);
  end;
end;

{ TJVEFormInterstitials }

var
  GlobalInterstitial: TJVEFormInterstitials;

procedure TJVEFormInterstitials.AnimationFinish(Sender: TObject);
begin
  if Animation.Inverse then
  begin
    Hide;
    Close;
    DisposeOf;
    if Assigned(FClose) then
      FClose;
    GlobalInterstitial := nil;
  end;
end;

procedure TJVEFormInterstitials.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  OnPaint := nil;
  Animation.Start;
end;

procedure TJVEFormInterstitials.ImageClick(Sender: TObject);
begin
  LayoutClick(nil);
  if Assigned(FClick) then
    FClick;
end;

procedure TJVEFormInterstitials.LayoutClick(Sender: TObject);
begin
  Animation.Inverse := True;
  Animation.Start;
end;

class procedure TJVEInterstitials.ShowInterstitial(Bitmap: TBitmap; Click, Close: TProc);
begin
  if GlobalInterstitial <> nil then
    Exit;

  GlobalInterstitial := TJVEFormInterstitials.Create(nil);
  {$IFDEF IOS}
  // BorderStyle = bsNone is the way FireMonkey shows or hides the status bar.
  // Actually, a very interesting idea. Wish it was documented!
  if not SharedUIApplication.isStatusBarHidden then
    GlobalInterstitial.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsSizeable{$ELSE}TFmxFormBorderStyle.Sizeable{$ENDIF}
  else
  {$ELSEIF Defined(ANDROID)}
  if MainActivity.{$IF CompilerVersion >= 33}getFullScreenManager.{$ENDIF}getStatusBarVisibility then
    GlobalInterstitial.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsSizeable{$ELSE}TFmxFormBorderStyle.Sizeable{$ENDIF}
  else
  {$ENDIF}
  begin
    GlobalInterstitial.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsNone{$ELSE}TFmxFormBorderStyle.None{$ENDIF};
{$IF CompilerVersion >= 32}
    GlobalInterstitial.SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Invisible;
{$ENDIF}
  end;

  {$IF not Defined(MACOS) or Defined(IOS)}
  GlobalInterstitial.FullScreen := True;
  {$ENDIF}
  GlobalInterstitial.SetBounds(0, 0, Round(Screen.Size.Width), Round(Screen.Size.Height));
  GlobalInterstitial.FClick := Click;
  GlobalInterstitial.FClose := Close;

  GlobalInterstitial.Image.Bitmap.Assign(Bitmap);
  GlobalInterstitial.Image.Width := Bitmap.Width;
  GlobalInterstitial.Image.Height := Bitmap.Height;
  GlobalInterstitial.Image.Parent := GlobalInterstitial.Layout;

  {$IFDEF IOS}GlobalInterstitial.Cancel.Font.Size := 48;{$ENDIF}
  GlobalInterstitial.Cancel.Parent := GlobalInterstitial.Layout;

  GlobalInterstitial.Show;
  GlobalInterstitial.BringToFront;
end;

initialization
finalization
{$IF not Defined(MACOS) and not Defined(ANDROID)}
  FreeAndNil(PreferencesObject);
{$ENDIF}
end.

