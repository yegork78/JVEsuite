(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Browser;

interface

uses System.Classes, System.SysUtils, FMX.Dialogs, FMX.Types, FMX.Controls,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  System.StrUtils, System.UITypes, System.Math, JVE.Messaging, JVE.Actions,
  JVE.Utils;

type
  TJVEBrowserAction = (// Override Action parameter within OnStarting event with:
    baLoad,   // Load the new page normally (default).
    baIgnore, // Ignore load request (previous page remains), i.e. click is ignored.
    baClose); // Close the browser completely (safer than calling Close within the event).

  TJVEBrowser = class;
  TJVEBrowserNotify = procedure(Browser: TJVEBrowser) of object;
  TJVEBrowserStarting = procedure(Browser: TJVEBrowser;
    const URL: string; var Action: TJVEBrowserAction) of object;
  TJVEBrowserError = procedure(Browser: TJVEBrowser;
    const Error: String; var DefaultProc: Boolean) of object;

  [ComponentPlatformsAttribute($000B945F)]
  TJVEBrowser = class(TComponent, IJVEExecutable)
  private
    FURL, FOpenURL: String;
    FTitle: string;
    FOnClosed: TJVEBrowserNotify;
    FOnLoaded: TJVEBrowserNotify;
    FOnError: TJVEBrowserError;
    FOnStarting: TJVEBrowserStarting;
    FStyleChangedId: Integer;
    FAutoTranslate: Boolean;
    FFreeOnClose: Boolean;
    function GetURL: String;
    procedure SetURL(const Value: String);
    procedure SetTitle(const Value: string);
    function GetIsOpen: Boolean;
    {$IF CompilerVersion >= 27}
    procedure StyleChangedHandler(const Sender: TObject; const Msg: System.Messaging.TMessage);
    {$ELSE}
    procedure StyleChangedHandler(const Sender: TObject; const Msg: FMX.Messages.TMessage);
    {$ENDIF}
  protected
    // IJVEExecutable support
    function CanOpen(Sender: TJVEAction): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;

    // Opens the browser with a given URL and Title (shortcut function).
    class procedure OpenURL(const URL, Title: String);

    // Opens the browser window. Notice, only one browser can be opened at any
    // given time. Launching another one will forcibly close the previous one.
    // The Sender parameter is for IJVEExecutable support and should not be used.
    procedure Open(Sender: TJVEAction = nil);

    // Force closes the browser window.
    procedure Close;
    // Checks whether this browser is now open.
    property IsOpen: Boolean read GetIsOpen;
  published
    // The title to give the browser window.
    property Title: string read FTitle write SetTitle;

    // The URL currently shown or the initial URL to open the browser with.
    property URL: String read GetURL write SetURL;

    // Occurs when the browser has closed (by user or in code).
    property OnClosed: TJVEBrowserNotify read FOnClosed write FOnClosed;

    // Occurs just before the page should be opened. Action is under event
    // control: see TJVEBrowserAction description above.
    property OnStarting: TJVEBrowserStarting read FOnStarting write FOnStarting;

    // Occurs when page finished loading without errors.
    property OnLoaded: TJVEBrowserNotify read FOnLoaded write FOnLoaded;

    // Occurs when error occured loading the website. DefaultProc (default: True)
    // indicates whether default processing is needed, i.e. present the error in
    // a message box and close the browser.
    // If you return False in Default Proc, then you should perform these actions.
    property OnError: TJVEBrowserError read FOnError write FOnError;
  end;

implementation

{$IF Defined(IOS)}

uses System.TypInfo, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation,
  iOSapi.CocoaTypes, iOSapi.CoreGraphics, Macapi.ObjCRuntime;

type
  WKWebView = interface(UIView)
    procedure setNavigationDelegate(navigationDelegate: Pointer); cdecl;
    function loadRequest(request: NSURLRequest): Pointer; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function goBack: Pointer; cdecl;
    function goForward: Pointer; cdecl;
  end;
  WKWebViewClass = interface(UIViewClass)
  end;
  TWKWebView = class(TOCGenericImport<WKWebViewClass, WKWebView>) end;

  WKNavigationAction = interface(NSObject)
    function request: NSURLRequest; cdecl;
  end;
  WKNavigationActionClass = interface(NSObjectClass)
  end;
  TWKNavigationAction = class(TOCGenericImport<WKNavigationActionClass, WKNavigationAction>) end;

  TWebKitDecisionHandler = procedure(policy: Integer) of object;
  WKNavigationDelegate = interface(IObjectiveC)
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecide(webView: WKWebView; decidePolicyForNavigationAction:
      WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisional(webView: WKWebView;
      didFailProvisionalNavigation: Pointer; withError: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView;
      didFailNavigation: Pointer; withError: NSError); cdecl;
  end;


type
  IButtonClicks = interface(NSObject)
    procedure BackClick(sender: Pointer); cdecl;
    procedure ForwardClick(sender: Pointer); cdecl;
  end;

  UINavigationBarDelegate = interface(IObjectiveC)
    function navigationBar(navigationBar: UINavigationBar;
      shouldPopItem: UINavigationItem): Boolean; cdecl; overload;
  end;

  TJVEBrowserDelegate = class(TOCLocal, UINavigationBarDelegate, WKNavigationDelegate)
  protected
    procedure UpdateButtons;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure BackClick(sender: Pointer); cdecl;
    procedure ForwardClick(sender: Pointer); cdecl;
    function navigationBar(navigationBar: UINavigationBar;
      shouldPopItem: UINavigationItem): Boolean; overload; cdecl;
    procedure webViewDecide(webView: WKWebView; decidePolicyForNavigationAction:
      WKNavigationAction; decisionHandler: Pointer); cdecl;
    procedure webViewDidFailProvisional(webView: WKWebView;
      didFailProvisionalNavigation: Pointer; withError: NSError); cdecl;
    procedure webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer); cdecl;
    procedure webViewDidFailNavigation(webView: WKWebView;
      didFailNavigation: Pointer; withError: NSError); cdecl;
  end;

var
  ActiveBrowser: TJVEBrowser;
  ActivePopup: UIViewController;
  ActiveView: UIView;
  ActiveWeb: WKWebView;
  ActiveBar: UINavigationBar;
  ActiveDelegate: TJVEBrowserDelegate;
  ActiveButtons: Integer;

function TJVEBrowserDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IButtonClicks);
end;

function TJVEBrowserDelegate.navigationBar(navigationBar: UINavigationBar;
  shouldPopItem: UINavigationItem): Boolean;
begin
  Result := True;
  if Assigned(ActiveBrowser) then
    ActiveBrowser.Close;
end;

procedure TJVEBrowserDelegate.webViewDidFailProvisional(webView: WKWebView;
  didFailProvisionalNavigation: Pointer; withError: NSError);
begin
  webViewDidFailNavigation(webView, didFailProvisionalNavigation, withError);
end;

procedure TJVEBrowserDelegate.webViewDidFailNavigation(webView: WKWebView;
  didFailNavigation: Pointer; withError: NSError);
var
  Error: String;
  DefaultProc: Boolean;
begin
  { Ignore software-based cancelled redirections (i.e. baIgnore results in 102) }
  if Assigned(ActiveBrowser) and ((withError.code <> 102) or
    (FromNSSTR(withError.domain) <> 'WebKitErrorDomain')) then
  begin
    DefaultProc := True;
    Error := FromNSSTR(withError.localizedDescription);

    if Assigned(ActiveBrowser.FOnError) then
      ActiveBrowser.FOnError(ActiveBrowser, Error, DefaultProc);

    if DefaultProc and Assigned(ActiveBrowser) then
    begin
      ShowDialog(Error, TMsgDlgType.mtError);
      ActiveBrowser.Close;
    end;
  end;
end;

procedure TJVEBrowserDelegate.UpdateButtons;
var
  NewButtons: Integer;
  Buttons: NSMutableArray;
begin
  if ActiveWeb = nil then
    Exit;

  NewButtons := IfThen(ActiveWeb.canGoBack, 1, 0) + IfThen(ActiveWeb.canGoForward, 2, 0);
  if NewButtons = ActiveButtons then
    Exit;
  ActiveButtons := NewButtons;

  Buttons := TNSMutableArray.Create;
  if ActiveWeb.canGoForward then
    Buttons.addObject(TUIBarButtonItem.Alloc.initWithTitle(ToNSSTR(#$27A1),
      UIBarButtonItemStylePlain, GetObjectID, sel_getUid('ForwardClick:')));

  if ActiveWeb.canGoBack then
    Buttons.addObject(TUIBarButtonItem.Alloc.initWithTitle(ToNSSTR(#$2B05),
      UIBarButtonItemStylePlain, GetObjectID, sel_getUid('BackClick:')));

  ActiveBar.topItem.setRightBarButtonItems(Buttons, True);
end;

procedure TJVEBrowserDelegate.webViewDecide(webView: WKWebView;
  decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  Action: TJVEBrowserAction;
  URL: String;
  Allow: Boolean;
var
  LDecisionHandlerBlock: procedure(policy: Integer); cdecl;
begin
  Allow := True;
  if ActiveBrowser = nil then
    Exit;

  URL := FromNSSTR(decidePolicyForNavigationAction.request.URL.absoluteString);

  if Assigned(ActiveBrowser.FOnStarting) then
  begin
    Action := baLoad;
    ActiveBrowser.FOnStarting(ActiveBrowser, URL, Action);
    Allow := Action = baLoad;
    if Action = baClose then
      ActiveBrowser.Close;
  end;

  UpdateButtons;
  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(IfThen(Allow, 1, 0));
  imp_removeBlock(@LDecisionHandlerBlock);
end;

procedure TJVEBrowserDelegate.webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer);
begin
  if ActiveBrowser = nil then
    Exit;

  UpdateButtons;
  if Assigned(ActiveBrowser.FOnLoaded) then
    ActiveBrowser.FOnLoaded(ActiveBrowser);
end;

procedure TJVEBrowserDelegate.BackClick(sender: Pointer);
begin
  if Assigned(ActiveWeb) and ActiveWeb.canGoBack then
    ActiveWeb.goBack;
end;

procedure TJVEBrowserDelegate.ForwardClick(sender: Pointer);
begin
  if Assigned(ActiveWeb) and ActiveWeb.canGoForward then
    ActiveWeb.goForward;
end;

{$ELSEIF Defined(MACOS)}

uses System.TypInfo, Posix.Dlfcn, FMX.Forms, Macapi.ObjectiveC,
  Macapi.CocoaTypes, Macapi.AppKit, Macapi.Foundation, FMX.Platform.Mac,
  FMX.Platform, Macapi.ObjCRuntime, Macapi.CoreGraphics;

type
  WKWebView = interface(NSView)
    procedure setNavigationDelegate(navigationDelegate: Pointer); cdecl;
    function loadRequest(request: NSURLRequest): Pointer; cdecl;
    function canGoBack: Boolean; cdecl;
    function canGoForward: Boolean; cdecl;
    function goBack: Pointer; cdecl;
    function goForward: Pointer; cdecl;
  end;
  WKWebViewClass = interface(NSViewClass)
  end;
  TWKWebView = class(TOCGenericImport<WKWebViewClass, WKWebView>) end;

  WKNavigationAction = interface(NSObject)
    function request: NSURLRequest; cdecl;
  end;
  WKNavigationActionClass = interface(NSObjectClass)
  end;
  TWKNavigationAction = class(TOCGenericImport<WKNavigationActionClass, WKNavigationAction>) end;

  TWebKitDecisionHandler = procedure(policy: Integer) of object;
  WKNavigationDelegate = interface(IObjectiveC)
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecide(webView: WKWebView; decidePolicyForNavigationAction:
      WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisional(webView: WKWebView;
      didFailProvisionalNavigation: Pointer; withError: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView;
      didFailNavigation: Pointer; withError: NSError); cdecl;
  end;

type
  IButtonClicks = interface(NSObject)
    procedure ButtonClick(sender: Pointer); cdecl;
  end;

  TJVEBrowserDelegate = class(TOCLocal, WKNavigationDelegate)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    procedure ButtonClick(sender: Pointer); cdecl;
    [MethodName('webView:decidePolicyForNavigationAction:decisionHandler:')]
    procedure webViewDecide(webView: WKWebView; decidePolicyForNavigationAction:
      WKNavigationAction; decisionHandler: Pointer); cdecl;
    [MethodName('webView:didFailProvisionalNavigation:withError:')]
    procedure webViewDidFailProvisional(webView: WKWebView;
      didFailProvisionalNavigation: Pointer; withError: NSError); cdecl;
    [MethodName('webView:didFinishNavigation:')]
    procedure webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer); cdecl;
    [MethodName('webView:didFailNavigation:withError:')]
    procedure webViewDidFailNavigation(webView: WKWebView;
      didFailNavigation: Pointer; withError: NSError); cdecl;
  end;

var
  ActiveBrowser: TJVEBrowser;
  ActivePopup: TCommonCustomForm;
  ActiveWeb: WKWebView;
  ActiveButtons: NSSegmentedControl;
  ActiveDelegate: TJVEBrowserDelegate;

function TJVEBrowserDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IButtonClicks);
end;

procedure TJVEBrowserDelegate.webViewDidFailProvisional(webView: WKWebView;
  didFailProvisionalNavigation: Pointer; withError: NSError);
begin
  webViewDidFailNavigation(webView, didFailProvisionalNavigation, withError);
end;

procedure TJVEBrowserDelegate.webViewDidFailNavigation(webView: WKWebView;
  didFailNavigation: Pointer; withError: NSError);
var
  Error: String;
  DefaultProc: Boolean;
begin
  { Ignore software-based cancelled redirections (i.e. baIgnore results in 102) }
  if Assigned(ActiveBrowser) and ((withError.code <> 102) or
    (FromNSSTR(withError.domain) <> 'WebKitErrorDomain')) then
  begin
    DefaultProc := True;
    Error := FromNSSTR(withError.localizedDescription);

    if Assigned(ActiveBrowser.FOnError) then
      ActiveBrowser.FOnError(ActiveBrowser, Error, DefaultProc);

    if DefaultProc and Assigned(ActiveBrowser) then
    begin
      ShowDialog(Error, TMsgDlgType.mtError);
      ActiveBrowser.Close;
    end;
  end;
end;

procedure TJVEBrowserDelegate.webViewDecide(webView: WKWebView;
  decidePolicyForNavigationAction: WKNavigationAction; decisionHandler: Pointer);
var
  Action: TJVEBrowserAction;
  URL: String;
  Allow: Boolean;
var
  LDecisionHandlerBlock: procedure(policy: Integer); cdecl;
begin
  Allow := True;
  if ActiveBrowser = nil then
    Exit;

  URL := FromNSSTR(decidePolicyForNavigationAction.request.URL.absoluteString);

  if Assigned(ActiveBrowser.FOnStarting) then
  begin
    Action := baLoad;
    ActiveBrowser.FOnStarting(ActiveBrowser, URL, Action);
    Allow := Action = baLoad;
    if Action = baClose then
      ActiveBrowser.Close;
  end;

  ActiveButtons.setEnabled(ActiveWeb.canGoBack, 0);
  ActiveButtons.setEnabled(ActiveWeb.canGoForward, 1);

  @LDecisionHandlerBlock := imp_implementationWithBlock(decisionHandler);
  LDecisionHandlerBlock(Ifthen(Allow, 1, 0));
  imp_removeBlock(@LDecisionHandlerBlock);
end;

procedure TJVEBrowserDelegate.webViewDidFinish(webView: WKWebView; didFinishNavigation: Pointer);
begin
  if ActiveBrowser = nil then
    Exit;

  ActiveButtons.setEnabled(ActiveWeb.canGoBack, 0);
  ActiveButtons.setEnabled(ActiveWeb.canGoForward, 1);
  if Assigned(ActiveBrowser.FOnLoaded) then
    ActiveBrowser.FOnLoaded(ActiveBrowser);
end;

procedure TJVEBrowserDelegate.ButtonClick(sender: Pointer);
begin
  if ActiveButtons.selectedSegment = 0 then
    ActiveWeb.goBack
  else
    ActiveWeb.goForward;
end;

{$ELSEIF Defined(ANDROID)}

uses System.Types, FMX.Platform, FMX.Forms, FMX.Platform.Android, FMX.ZOrder.Android,
  {$IF CompilerVersion >= 27} AndroidApi.Helpers,{$ENDIF} FMX.Helpers.Android,
  FMX.StdCtrls, AndroidApi.JNI.GraphicsContentViewText, AndroidApi.JNI.JavaTypes,
  AndroidApi.JNI.Webkit, AndroidApi.JNIBridge, FMX.WebBrowser, AndroidApi.JNI.Os,
  AndroidApi.JNI.Embarcadero, AndroidApi.JNI.Net, Androidapi.JNI.App,
  AndroidApi.JNI.Util {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

type
  { TActionBar }
  TActionBar = class
  public
    constructor Create(AParent: TCommonCustomForm);
    procedure UpdateButtons;
    procedure OnTimer(Sender: TObject);
    procedure SetTitle(AValue: String);
    procedure Hide;
    procedure Show;
    function IsVisible: Boolean;
  private
    BackBtn, ForwardBtn, CloseBtn: TButton;
    TitleLbl: TLabel;
    UpdateTimer: TTimer;
    ToolBar: TToolBar;
    Title: String;
    TitleTextWidth, MaxTitleWidth: Single;
    procedure OnBackBtnClick(Sender: TObject);
    procedure OnForwardBtnClick(Sender: TObject);
    procedure OnCloseBtnClick(Sender: TObject);
  end;

  { TAndroidWebBrowserService }
  TWebBrowserServiceUncontrolled = class(TInterfacedObject)
  private
    type
      TWBClient = class(TJavaLocal, JOnWebViewListener)
      private
        FWBService : TWebBrowserServiceUncontrolled;
      public
        constructor Create(AWBService : TWebBrowserServiceUncontrolled);
        procedure doUpdateVisitedHistory(P1: JWebView; P2: JString; P3: Boolean); cdecl;
        procedure onFormResubmission(P1: JWebView; P2: JMessage; P3: JMessage); cdecl;
        procedure onLoadResource(P1: JWebView; P2: JString); cdecl;
        procedure onPageFinished(P1: JWebView; P2: JString); cdecl;
        procedure onPageStarted(P1: JWebView; P2: JString; P3: JBitmap); cdecl;
        procedure onReceivedError(P1: JWebView; P2: Integer; P3: JString; P4: JString); cdecl;
        procedure onReceivedHttpAuthRequest(P1: JWebView; P2: JHttpAuthHandler; P3: JString; P4: JString); cdecl;
        procedure onReceivedSslError(P1: JWebView; P2: JSslErrorHandler; P3: JSslError); cdecl;
        procedure onScaleChanged(P1: JWebView; P2: Single; P3: Single); cdecl;
        procedure onUnhandledKeyEvent(P1: JWebView; P2: JKeyEvent); cdecl;
        function shouldOverrideKeyEvent(P1: JWebView; P2: JKeyEvent): Boolean; cdecl;
        function shouldOverrideUrlLoading(P1: JWebView; P2: JString): Boolean; cdecl;
      end;
  private
    FClient: TWBClient;               // WebViewClient wrapper
    FJWebBrowser: JWebBrowser;        // WebView wrapper
    FURL: string;
    ScreenSize: JRect;

    function GetScreenSize: JRect;
    procedure InitUIThread;
    procedure UpdateButtons;
    procedure Hide;
    procedure Show;
  protected
    procedure DoNavigate(const URL: string);
    function GetURL: string;
    function GetCanGoBack: Boolean;
    function GetCanGoForward: Boolean;
    procedure SetURL(const AValue: string);
    procedure Navigate;
    procedure GoBack;
    procedure GoForward;

    procedure FinishLoading;
    procedure FailLoadingWithError(const ErrorCode: Integer; const Description, URL: string);
    function ShouldStartLoading(const URL: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsBrowserVisible: Boolean;
    property URL: string read GetURL write SetURL;
    property CanGoBack: Boolean read GetCanGoBack;
    property CanGoForward: Boolean read GetCanGoForward;
  end;

var
  ActiveBrowser: TJVEBrowser;     // this browser
  BrowserService: TWebBrowserServiceUncontrolled;
  ActiveBar: TActionBar;
  ActivePanel: TPanel;

{ TActionBar }

constructor TActionBar.Create(AParent: TCommonCustomForm);
begin
  ToolBar := TToolBar.Create(nil);
  ToolBar.Parent := AParent;
  ToolBar.Align := {$IF CompilerVersion < 27}TAlignLayout.alNone{$ELSE}TAlignLayout.None{$ENDIF};

  ForwardBtn := TButton.Create(ToolBar);
  ForwardBtn.Text := #$25B7;      // WHITE RIGHT-POINTING TRIANGLE
  ForwardBtn.Width := ForwardBtn.Width / 2;
  ForwardBtn.Align := {$IF CompilerVersion < 27}TAlignLayout.alMostRight{$ELSE}TAlignLayout.MostRight{$ENDIF};
  //ForwardBtn.StyleLookup := 'priortoolbutton';
  ForwardBtn.Parent := ToolBar;
  ForwardBtn.Visible := False;
  ForwardBtn.CanFocus := True;
  ForwardBtn.OnClick := OnForwardBtnClick;
  ForwardBtn.Enabled := True;

  BackBtn := TButton.Create(ToolBar);
  BackBtn.Text := #$25C1;         // WHITE LEFT-POINTING TRIANGLE
  BackBtn.Width := BackBtn.Width / 2;
  BackBtn.Align := {$IF CompilerVersion < 27}TAlignLayout.alRight{$ELSE}TAlignLayout.Right{$ENDIF};
  //BackBtn.StyleLookup := 'nexttoolbutton';
  BackBtn.Parent := ToolBar;
  BackBtn.Visible := False;
  BackBtn.CanFocus := True;
  BackBtn.OnClick := OnBackBtnClick;
  BackBtn.Enabled := True;

  CloseBtn := TButton.Create(ToolBar);
  CloseBtn.Text := '< Back';
  CloseBtn.Align := {$IF CompilerVersion < 27}TAlignLayout.alLeft{$ELSE}TAlignLayout.Left{$ENDIF};
  CloseBtn.Parent := ToolBar;
  CloseBtn.Visible := True;
  CloseBtn.CanFocus := True;
  CloseBtn.OnClick := OnCloseBtnClick;
  CloseBtn.Enabled := True;

  TitleLbl := TLabel.Create(ToolBar);
  TitleLbl.Align := {$IF CompilerVersion < 27}TAlignLayout.alClient{$ELSE}TAlignLayout.Client{$ENDIF};
  TitleLbl.TextAlign := {$IF CompilerVersion < 27}TTextAlign.taCenter{$ELSE}TTextAlign.Center{$ENDIF};
  TitleLbl.Font.Style := [TFontStyle.fsBold];
  TitleLbl.Parent := ToolBar;
  TitleLbl.Visible := True;
  TitleLbl.CanFocus := False;

  //ToolBar.Height := 2 * (BackBtn.Height + BackBtn.Position.Y * 2);

  UpdateTimer := TTimer.Create(ToolBar);
  UpdateTimer.Interval := 500;
  UpdateTimer.OnTimer := OnTimer;
  UpdateTimer.Enabled := False;
end;

procedure TActionBar.OnTimer(Sender: TObject);
var
  VisibleTitleLength: Integer;
begin
  MaxTitleWidth := TitleLbl.Width;
  if (TitleTextWidth = 0) or (Length(Title) = 0) then
    VisibleTitleLength := 0
  else
    VisibleTitleLength := Round(MaxTitleWidth / TitleTextWidth * Length(Title));
  if VisibleTitleLength > Length(Title) then
    VisibleTitleLength := Length(Title);

  TitleLbl.Text := Copy(Title, 1, VisibleTitleLength);
  UpdateTimer.Enabled := False;
  ToolBar.Repaint;
  ToolBar.BringToFront;
end;

procedure TActionBar.OnBackBtnClick(Sender: TObject);
begin
  if Assigned(BrowserService) then
  begin
    BrowserService.GoBack;
    UpdateButtons;
  end
end;

procedure TActionBar.OnForwardBtnClick(Sender: TObject);
begin
  if Assigned(BrowserService) then
  begin
    BrowserService.GoForward;
    UpdateButtons;
  end;
end;

procedure TActionBar.OnCloseBtnClick(Sender: TObject);
begin
  ActiveBrowser.Close;
end;

procedure TActionBar.UpdateButtons;
begin
  ForwardBtn.Visible := BrowserService.GetCanGoForward;
  BackBtn.Visible := BrowserService.GetCanGoBack;
end;

procedure TActionBar.Hide;
begin
  ToolBar.Visible := False;
end;

procedure TActionBar.Show;
begin
  ToolBar.Visible := True;
end;

function TActionBar.IsVisible: Boolean;
begin
  Result := ToolBar.Visible;
end;

procedure TActionBar.SetTitle(AValue: String);
begin
  Title := AValue;
  TitleLbl.Text := Title;
  TitleTextWidth := TitleLbl.Canvas.TextWidth(Title);
  UpdateTimer.Enabled := True;
end;

{ TWebBrowserServiceUncontrolled }

function TWebBrowserServiceUncontrolled.IsBrowserVisible: Boolean;
begin
  Result := (ActiveBar <> nil) and (ActiveBar.IsVisible);
end;

function TWebBrowserServiceUncontrolled.GetCanGoBack: Boolean;
var
  CanGoBack: Boolean;
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      CanGoBack := FJWebBrowser.canGoBack;
    end);
  Result := CanGoBack;
end;

function TWebBrowserServiceUncontrolled.GetCanGoForward: Boolean;
var
  CanGoForward: Boolean;
begin
  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      CanGoForward := FJWebBrowser.canGoForward;
    end);
  Result := CanGoForward;
end;

function TWebBrowserServiceUncontrolled.GetURL: string;
begin
  Result := FURL;
end;

procedure TWebBrowserServiceUncontrolled.GoBack;
begin
  CallInUIThread(
    procedure
    begin
      FJWebBrowser.goBack;
    end);
end;

procedure TWebBrowserServiceUncontrolled.GoForward;
begin
  CallInUIThread(
    procedure
    begin
      FJWebBrowser.goForward;
    end);
end;

procedure TWebBrowserServiceUncontrolled.InitUIThread;
begin
  FJWebBrowser := TJWebBrowser.JavaClass.init(MainActivity);
  FJWebBrowser.getSettings.setJavaScriptEnabled(True);

  FClient := TWBClient.Create(Self);
  FJWebBrowser.SetWebViewListener(FClient);
end;

procedure TWebBrowserServiceUncontrolled.Navigate;
begin
  DoNavigate(URL);
end;

procedure TWebBrowserServiceUncontrolled.SetURL(const AValue: string);
begin
  if FURL <> AValue then
    FURL := AValue;
end;

function TWebBrowserServiceUncontrolled.ShouldStartLoading(const URL: string): Boolean;
var
  Action: TJVEBrowserAction;
  KeepOnLoading: Boolean;
begin
  if Assigned(ActiveBrowser) then
  begin
    Action := baLoad;
    if Assigned(ActiveBrowser.FOnStarting) then
    begin
      ActiveBrowser.FOnStarting(ActiveBrowser, URL, Action);
      KeepOnLoading := Action = baLoad;
      if Action = baClose then
        ActiveBrowser.Close;
    end
    else
      KeepOnLoading := True;
  end
  else
    KeepOnLoading := False;

  UpdateButtons;
  if KeepOnLoading then
    ActiveBrowser.FURL := URL;

  Result := KeepOnLoading;
end;

constructor TWebBrowserServiceUncontrolled.Create;
const
  TITLEBAR_HEIGHT = 24.0; // Default size for MDPI
var
  ScreenSrv: IFMXScreenService;
  ScreenScale: Single;
  Metrics: JDisplayMetrics;
begin
  ScreenSize := GetScreenSize;

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenSrv)) then
    ScreenScale := ScreenSrv.GetScreenScale
  else
    ScreenScale := 1.0;

  CallInUIThreadAndWaitFinishing(InitUIThread);

  ActiveBar := TActionBar.Create(Application.MainForm);
  ActiveBar.ToolBar.SetBounds(0, 0, ActiveBar.ToolBar.Width, ActiveBar.ToolBar.Height + 7);
  ActiveBar.SetTitle(ActiveBrowser.Title);

  Metrics := GetSharedActivity.getResources.getDisplayMetrics;

  ActivePanel := TPanel.Create(nil);
  ActivePanel.Parent := Application.MainForm;
  ActivePanel.SetBounds(0, ActiveBar.ToolBar.Height + 7, ActiveBar.ToolBar.Width,
    ScreenSize.height / ScreenScale - ActiveBar.ToolBar.Height + 7);

  Show;
end;

procedure TWebBrowserServiceUncontrolled.DoNavigate(const URL: string);
var
  NewURL: string;
begin
  NewURL := URL;
  if Pos('file://', URL) <> 0 then
  begin
    NewURL := ReplaceStr(NewURL, 'file://', 'file:///');
    CallInUIThread(
      procedure
      begin
        FJWebBrowser.loadUrl(StringToJString(NewURL));
      end);
  end
  else
  begin
    if Pos('http', URL) = 0 then
      Insert('https://', NewURL, 0);
    CallInUIThread(
      procedure
      begin
        FJWebBrowser.loadUrl(StringToJString(NewURL));
      end);
  end;
end;

procedure TWebBrowserServiceUncontrolled.FailLoadingWithError(const ErrorCode: Integer; const Description, URL: string);
var
  DefaultProc: Boolean;
  ErrorText: String;
begin
  DefaultProc := True;
  ErrorText := String.Format('%s (%d)', [Description, ErrorCode]);

  if Assigned(ActiveBrowser) and Assigned(ActiveBrowser.FOnError) then
    ActiveBrowser.FOnError(ActiveBrowser, ErrorText, DefaultProc);

  if not Assigned(ActiveBrowser) then Exit;
  //ActiveButtons.setEnabled(ActiveWeb.canGoBack, 0);
  //ActiveButtons.setEnabled(ActiveWeb.canGoForward, 1);

  if DefaultProc then
  begin
    ShowDialog(ErrorText, TMsgDlgType.mtError);
    ActiveBrowser.Close;
  end;
end;

procedure TWebBrowserServiceUncontrolled.FinishLoading;
begin
  if Assigned(ActiveBrowser) and Assigned(ActiveBrowser.FOnLoaded) then
    ActiveBrowser.FOnLoaded(ActiveBrowser);
end;

procedure TWebBrowserServiceUncontrolled.UpdateButtons;
begin
  if (Assigned(ActiveBar)) then
    ActiveBar.UpdateButtons;
end;

function TWebBrowserServiceUncontrolled.GetScreenSize: JRect;
var
  NativeWindow: JWindow;
  ScreenRect: JRect;
begin
  NativeWindow := GetSharedActivity.getWindow;
  if Assigned(NativeWindow) then
  begin
    ScreenRect := TJRect.Create;
    NativeWindow.getDecorView.getWindowVisibleDisplayFrame(ScreenRect);
    Result := ScreenRect;
  end;
end;

procedure TWebBrowserServiceUncontrolled.Hide;
begin
  if ActiveBar = nil then
    Exit;
  ActiveBar.Hide;
  ActivePanel.Visible := False;

  CallInUIThreadAndWaitFinishing(
    procedure
    begin
      if Assigned(FJWebBrowser) then
      begin
        FJWebBrowser.loadUrl(StringToJString('about:blank'));
        FJWebBrowser.stopLoading;

        if FJWebBrowser.getVisibility <> TJView.JavaClass.INVISIBLE then
          FJWebBrowser.setVisibility(TJView.JavaClass.INVISIBLE);
      end;
    end);
end;

procedure TWebBrowserServiceUncontrolled.Show;
begin
  ActiveBar.Show;
  ActivePanel.Visible := True;

  CallInUIThread(
    procedure
    var
      ZOrder: TAndroidZOrderManager;
    begin
      if FJWebBrowser.getVisibility <> TJView.JavaClass.VISIBLE then
        FJWebBrowser.setVisibility(TJView.JavaClass.VISIBLE);

      ZOrder := WindowHandleToPlatform(TCommonCustomForm(ActivePanel.Root).Handle).ZOrderManager;
      ZOrder.AddOrSetLink(ActivePanel, JView(FJWebBrowser), nil);
      ZOrder.UpdateOrderAndBounds(ActivePanel);
    end);
end;

destructor TWebBrowserServiceUncontrolled.Destroy;
begin
  Hide;
  FreeAndNil(FClient);
  if FJWebBrowser <> nil then
  begin
    FJWebBrowser.destroy;
    FJWebBrowser := nil;
  end;
end;

{ TAndroidWebBrowserService.TWebBrowserListener }

constructor TWebBrowserServiceUncontrolled.TWBClient.Create(
  AWBService: TWebBrowserServiceUncontrolled);
begin
  inherited Create;
  FWBService := AWBService;
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.doUpdateVisitedHistory(
  P1: JWebView; P2: JString; P3: Boolean);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onFormResubmission(
  P1: JWebView; P2, P3: JMessage);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onLoadResource(
  P1: JWebView; P2: JString);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onPageFinished(
  P1: JWebView; P2: JString);
begin
  FWBService.FURL := JStringToString(P2);
  FWBService.FinishLoading;
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onPageStarted(
  P1: JWebView; P2: JString; P3: JBitmap);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onReceivedError(
  P1: JWebView; P2: Integer; P3, P4: JString);
begin
  FWBService.FailLoadingWithError(P2, JStringToString(P3), JStringToString(P4));
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onReceivedHttpAuthRequest(
  P1: JWebView; P2: JHttpAuthHandler; P3, P4: JString);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onReceivedSslError(
  P1: JWebView; P2: JSslErrorHandler; P3: JSslError);
begin
  FWBService.FailLoadingWithError(P3.getPrimaryError, JStringToString(P3.toString), JStringToString(P3.getUrl));
  P2.proceed;
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onScaleChanged(
  P1: JWebView; P2, P3: Single);
begin
end;

procedure TWebBrowserServiceUncontrolled.TWBClient.onUnhandledKeyEvent(
  P1: JWebView; P2: JKeyEvent);
begin
end;

function TWebBrowserServiceUncontrolled.TWBClient.shouldOverrideKeyEvent(
  P1: JWebView; P2: JKeyEvent): Boolean;
begin
  Result := False;
end;

// known WebViewClient bugs: https://code.google.com/p/android/issues/detail?id=2887
function TWebBrowserServiceUncontrolled.TWBClient.shouldOverrideUrlLoading(
  P1: JWebView; P2: JString): Boolean;
begin
  if (not BrowserService.ShouldStartLoading(JStringToString(P2))) then
    P1.stopLoading;
  Result := False;  // result doesn't affect anything! strange.
end;

{$ELSEIF Defined(MSWINDOWS)}

uses FMX.Platform, FMX.Forms, FMX.StdCtrls, System.Win.InternetExplorer,
  FMX.Controls.Ole, Winapi.ActiveX, Winapi.Windows, Winapi.Messages,
  FMX.ZOrder.Win, FMX.Platform.Win, System.Types;

type
  TJVEBrowserDelegate = class(TOLEFrameworkDelegate)
  private
    FButtons: TForm;
    FTimer: TTimer;
    FURL: String;
  public
    procedure DefaultHandler(var Message); override;
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormBack(Sender: TObject);
    procedure FormForward(Sender: TObject);
    procedure MoveTimer(Sender: TObject);
    procedure WebBrowserBeforeNavigate(ASender: TObject; const pDisp: IDispatch;
      const URL, Flags, Target, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure WebBrowserNavigateComplete(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
    procedure WebBrowserNavigateError(ASender: TObject; const pDisp: IDispatch;
      const URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
  end;

var
  ActiveBrowser: TJVEBrowser;
  ActivePopup: TForm;
  ActiveScale: Single;
  ActiveDelegate: TJVEBrowserDelegate;
  ActiveWeb: TOleWebBrowser;

procedure TJVEBrowserDelegate.WebBrowserBeforeNavigate(
  ASender: TObject; const pDisp: IDispatch; const URL, Flags,
  Target, PostData, Headers: OleVariant; var Cancel: WordBool);
var
  Action: TJVEBrowserAction;
begin
  if Assigned(ActiveBrowser) and Assigned(ActiveBrowser.FOnStarting) then
  begin
    Action := baLoad;
    ActiveBrowser.FOnStarting(ActiveBrowser, URL, Action);
    Cancel := Action <> baLoad;
    if Action = baClose then
      ActiveBrowser.Close;
  end else
    Cancel := False;

  if not Cancel and Assigned(ActiveBrowser) then
    ActiveBrowser.FOpenURL := URL;
end;

procedure TJVEBrowserDelegate.WebBrowserNavigateComplete(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  if Assigned(ActiveBrowser) and Assigned(ActiveBrowser.FOnLoaded) then
    ActiveBrowser.FOnLoaded(ActiveBrowser);
end;

procedure TJVEBrowserDelegate.WebBrowserNavigateError(ASender: TObject;
  const pDisp: IDispatch; const URL, Frame, StatusCode: OleVariant; var Cancel: WordBool);
  function ErrorText(StatusCode: Integer): String;
  begin
    case StatusCode of
      400: Result := 'HTTP error: bad request (400)';
      401: Result := 'HTTP error: denied (401)';
      402: Result := 'HTTP error: payment required (402)';
      403: Result := 'HTTP error: forbidden (403)';
      404: Result := 'HTTP error: not found (404)';
      405: Result := 'HTTP error: bad method (405)';
      406: Result := 'HTTP error: none acceptable (406)';
      407: Result := 'HTTP error: proxy autherization required (407)';
      408: Result := 'HTTP error: request timeout (408)';
      409: Result := 'HTTP error: conflict (409)';
      410: Result := 'HTTP error: gone (410)';
      411: Result := 'HTTP error: length required (411)';
      412: Result := 'HTTP error: precondition failed (412)';
      413: Result := 'HTTP error: request too large (413)';
      414: Result := 'HTTP error: URI too long (414)';
      415: Result := 'HTTP error: unsupported media (415)';
      449: Result := 'HTTP error: retry with (449)';
      500: Result := 'HTTP error: server error (500)';
      501: Result := 'HTTP error: not supported (501)';
      502: Result := 'HTTP error: bad gateway (502)';
      503: Result := 'HTTP error: service unavailable (503)';
      504: Result := 'HTTP error: gateway timeout (504)';
      505: Result := 'HTTP error: version not supported (505)';
      -2146697214: Result := 'Invalid URL (-2146697214)';
      -2146697213: Result := 'No session (-2146697213)';
      -2146697212: Result := 'Cannot connect (-2146697212)';
      -2146697211: Result := 'Resource not found (-2146697211)';
      -2146697210: Result := 'Object not found (-2146697210)';
      -2146697209: Result := 'Data not available (-2146697209)';
      -2146697208: Result := 'Download failure (-2146697208)';
      -2146697207: Result := 'Authentication required (-2146697207)';
      -2146697206: Result := 'No valid media (-2146697206)';
      -2146697205: Result := 'Connection timeout (-2146697205)';
      -2146697204: Result := 'Invalid request (-2146697204)';
      -2146697203: Result := 'Unknown protocol (-2146697203)';
      -2146697202: Result := 'Security problem (-2146697202)';
      -2146697201: Result := 'Cannot load data (-2146697201)';
      -2146697200: Result := 'Cannot instantiate object (-2146697200)';
      -2146697196: Result := 'Redirect failed (-2146697196)';
      -2146697195: Result := 'Redirect to directory (-2146697195)';
      -2146697194: Result := 'Cannot lock request (-2146697194)';
      -2146697193: Result := 'Use extend binding (-2146697193)';
      -2146697192: Result := 'Terminated bind (-2146697192)';
      -2146697191: Result := 'Invalid certificate (-2146697191)';
      -2146696960: Result := 'Code download declined (-2146696960)';
      -2146696704: Result := 'Result dispatched (-2146696704)';
      -2146696448: Result := 'Cannot replace SFP file (-2146696448)';
      -2146695936: Result := 'Code install blocked by HASH policy (-2146695936)';
      -2146696192: Result := 'Code install suppressed (-2146696192)';
    else
      Result := Format('Unknown error (%d)', [StatusCode]);
    end;
  end;
var
  DefaultProc: Boolean;
begin
  if Assigned(ActiveBrowser) then
  begin
    DefaultProc := True;
    if Assigned(ActiveBrowser.FOnError) then
      ActiveBrowser.FOnError(ActiveBrowser, ErrorText(StatusCode), DefaultProc);

    if DefaultProc and Assigned(ActiveBrowser) then
    begin
      ShowDialog(ErrorText(StatusCode), TMsgDlgType.mtError);
      ActiveBrowser.Close;
    end;
  end;
end;

procedure TJVEBrowserDelegate.FormShow(Sender: TObject);
var
  Button: TButton;
begin
  if ActiveBrowser = nil then
  begin
    ExecuteInIdle(procedure
    begin
      ActivePopup.ModalResult := mrOk;
    end);
    Exit;
  end;

  ActivePopup.OnShow := nil;
  Parent := ActivePopup;
  Size.PlatformDefault := False;
  Size.Width := ActivePopup.ClientWidth;
  Size.Height := ActivePopup.ClientHeight;

  ActiveWeb := TOleWebBrowser.Create(Self);
  ActiveWeb.OnBeforeNavigate2 := WebBrowserBeforeNavigate;
  ActiveWeb.OnNavigateComplete2 := WebBrowserNavigateComplete;
  ActiveWeb.OnNavigateError := WebBrowserNavigateError;
  ActiveWeb.Silent := True;
  ActiveWeb.Visible := True;
  ActiveWeb.Navigate(FURL);

  {$IF CompilerVersion >= 33}
  Winapi.Windows.SetParent(GetHandle, FmxHandleToHWND(ActivePopup.Handle));
  SetWindowPos(GetHandle, HWND_TOP, 0, 0, Round(ActivePopup.ClientWidth * ActiveScale),
    Round(ActivePopup.ClientHeight * ActiveScale), SWP_NOACTIVATE);
  {$ELSE}
  WindowHandleToPlatform(ActivePopup.Handle).ZOrderManager.UpdateOrderAndBounds(Self, GetHandle);
  {$ENDIF}

  // This is by far not the best way to introduce buttons to the title bar,
  // but bearing in mind that the form is modal - this is the easiest way.
  FButtons := TForm.CreateNew(Self);
  FButtons.SetBounds(0, 0, 44, 22);
  FButtons.FormStyle := TFormStyle.StayOnTop;
  FButtons.Position := TFormPosition.Designed;
  FButtons.BorderStyle := TFmxFormBorderStyle.None;
  FButtons.Show;
  FormResize(nil);

  Button := TButton.Create(FButtons);
  Button.SetBounds(22, 0, 22, 22);
  Button.Text := #$25B6;
  Button.Parent := FButtons;
  Button.OnClick := FormForward;

  Button := TButton.Create(FButtons);
  Button.SetBounds(0, 0, 22, 22);
  Button.Text := #$25C0;
  Button.Parent := FButtons;
  Button.OnClick := FormBack;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 50;
  FTimer.OnTimer := MoveTimer;
end;

procedure TJVEBrowserDelegate.DefaultHandler(var Message);
var
  LMessage: TMessage;
begin
  if ActiveWeb = nil then
    Exit;
  if HandleAllocated and (ActiveWeb.MiscStatus and OLEMISC_SIMPLEFRAME = 0) then
  begin
    LMessage := TMessage(Message);
    LMessage.Result := CallWindowProc(DefWndProc, Handle,
      LMessage.Msg, LMessage.WParam, LMessage.LParam);
  end else
    inherited DefaultHandler(Message);
end;

procedure TJVEBrowserDelegate.FormResize(Sender: TObject);
var
  Value: Integer;
begin
  if FButtons = nil then
    Exit;

  Value := Round(ActivePopup.Left + ActivePopup.Width - 200);
  if FButtons.Left <> Value then
    FButtons.Left := Value;
  Value := ActivePopup.Top + 4;
  if FButtons.Top <> Value then
    FButtons.Top := Value;

  Size.Width := ActivePopup.ClientWidth * ActiveScale;
  Size.Height := ActivePopup.ClientHeight * ActiveScale;

  {$IF CompilerVersion >= 33}
  SetWindowPos(GetHandle, HWND_TOP, 0, 0, Round(ActivePopup.ClientWidth * ActiveScale),
    Round(ActivePopup.ClientHeight * ActiveScale), SWP_NOACTIVATE);
  {$ELSE}
  WindowHandleToPlatform(ActivePopup.Handle).ZOrderManager.UpdateOrderAndBounds(Self, GetHandle);
  {$ENDIF}
end;

procedure TJVEBrowserDelegate.MoveTimer(Sender: TObject);
var
  Value: Integer;
begin
  if FButtons = nil then
    Exit;

  Value := Round(ActivePopup.Left + ActivePopup.Width) - 200;
  if FButtons.Left <> Value then
    FButtons.Left := Value;
  Value := ActivePopup.Top + 4;
  if FButtons.Top <> Value then
    FButtons.Top := Value;
end;

procedure TJVEBrowserDelegate.FormBack(Sender: TObject);
begin
  // Sadly, TWebControl does not seem to have an ability to tell whether
  // there is a history, furthermore, if there is no history, calling this
  // function results in an exception (other platforms silently ignore invalid
  // calls.
  // This try-except-end hides unneeded exception messages.
  // If debugger stopped here, simply continue, ignoring the error.
  try
    ActiveWeb.GoBack;
  except
  end;
end;

procedure TJVEBrowserDelegate.FormForward(Sender: TObject);
begin
  // See just above the GoBack function call's comment: same applies.
  try
    ActiveWeb.GoForward;
  except
  end;
end;

{$ENDIF}

{ TJVEBrowser }

constructor TJVEBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAutoTranslate := True;
  FStyleChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
  SetTitle(FTitle);
end;

destructor TJVEBrowser.Destroy;
begin
  Close;

  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  inherited Destroy;
end;

{$IF CompilerVersion >= 27}
procedure TJVEBrowser.StyleChangedHandler(const Sender: TObject;
  const Msg: System.Messaging.TMessage);
{$ELSE}
procedure TJVEBrowser.StyleChangedHandler(const Sender: TObject;
  const Msg: FMX.Messages.TMessage);
{$ENDIF}
var
  Translation: String;
begin
  if FAutoTranslate and (FTitle <> '') then
  begin
    Translation := Translate(FTitle); // Mandatory for texts collection
    if not (csDesigning in ComponentState) then
      Title := Translation;
  end;
end;

class procedure TJVEBrowser.OpenURL(const URL, Title: String);
var
  Browser: TJVEBrowser;
begin
  Browser := TJVEBrowser.Create(nil);
  Browser.URL := URL;
  Browser.Title := Title;
  Browser.FFreeOnClose := True;
  Browser.Open;
end;

procedure TJVEBrowser.Close;
begin
  if IsOpen then
{$IF Defined(IOS)}
  begin
    ActivePopup.dismissModalViewControllerAnimated(True);
    ActiveBrowser := nil;
    ActivePopup := nil;
    ActiveView := nil;
    ActiveBar := nil;
    ActiveWeb := nil;
    if Assigned(FOnClosed) then
      FOnClosed(Self);
  end;
{$ELSEIF Defined(ANDROID)}
  begin
    if (ActivePanel.Root <> nil) and (ActivePanel.Root.GetObject is TCommonCustomForm) then
      WindowHandleToPlatform(TCommonCustomForm(ActivePanel.Root).Handle).
        ZOrderManager.RemoveLink(ActivePanel);
    BrowserService.Hide;
    ActiveBar.UpdateTimer.Enabled := False;
    ActiveBar.ToolBar.Parent := nil;
    ActivePanel.Parent := nil;
    FreeAndNil(ActivePanel);
    FreeAndNil(ActiveBar);
    ActiveBrowser := nil;
    FreeAndNil(BrowserService);
    if Assigned(FOnClosed) then
      FOnClosed(Self);
  end;
{$ELSE}
  begin
    ActiveBrowser := nil;
    ActivePopup.ModalResult := 1;
  end;
{$ENDIF}
end;

function TJVEBrowser.GetIsOpen: Boolean;
begin
{$IF Defined(ANDROID)}
  Result := (Self = ActiveBrowser) and (BrowserService <> nil) and (BrowserService.IsBrowserVisible);
{$ELSE not ANDROID}
  Result := (Self = ActiveBrowser) and (ActivePopup <> nil);
{$ENDIF}
end;

function TJVEBrowser.GetURL: String;
begin
  Result := IfThen(IsOpen, FOpenURL, FURL);
end;

function TJVEBrowser.CanOpen(Sender: TJVEAction): Boolean;
begin
  Result := True;
end;

procedure TJVEBrowser.SetURL(const Value: String);
begin
  if IsOpen then
  begin
    FOpenURL := Value;
{$IF Defined(MACOS)}
    ActiveWeb.loadRequest(TNSURLRequest.Wrap(TNSURLRequest.alloc.initWithURL(
      TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(Value))))));
{$ELSEIF Defined(ANDROID)}
    BrowserService.DoNavigate(Value);
{$ELSEIF Defined(MSWINDOWS)}
    ActiveWeb.Navigate(Value);
{$ENDIF}
  end else
    FURL := Value;
end;

procedure TJVEBrowser.Open;
{$IF Defined(MACOS) and not Defined(IOS)}
var
  WindowSize: NSSize;
  Header: Single;
  Cell: NSSegmentedCell;
  CurrentForm: TCommonCustomForm;
{$ENDIF}
begin
  if (ActiveBrowser <> nil) and (ActiveBrowser <> Self) then
    ActiveBrowser.Close;

  if not IsOpen then
  begin
    FOpenURL := FURL;
    ActiveBrowser := Self;

{$IF Defined(IOS)}
    if ActiveDelegate = nil then
      ActiveDelegate := TJVEBrowserDelegate.Create;

    ActiveView := TUIView.Create;
    ActiveView.setFrame(CGRectMake(0, 0, 100, 144));

    ActiveBar := TUINavigationBar.Create;
    ActiveBar.pushNavigationItem(TUINavigationItem.Create, False);
    ActiveBar.pushNavigationItem(TUINavigationItem.Wrap(
      TUINavigationItem.Alloc.initWithTitle(ToNSSTR(FTitle))), True);
    ActiveBar.setDelegate(ActiveDelegate.GetObjectID);
    ActiveBar.setBarStyle(UIBarStyleBlack);
    ActiveBar.setFrame(CGRectMake(0, 0, 100, 44));
    ActiveBar.setAutoresizingMask(UIViewAutoresizingFlexibleWidth);
    ActiveView.addSubview(ActiveBar);

    ActiveWeb := TWKWebView.Create;
    ActiveWeb.setNavigationDelegate(ActiveDelegate.GetObjectID);
    ActiveWeb.loadRequest(TNSURLRequest.Wrap(TNSURLRequest.alloc.initWithURL(
      TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(FURL))))));
    ActiveWeb.setFrame(CGRectMake(0, 44, 100, 100));
    ActiveWeb.setAutoresizingMask(UIViewAutoresizingFlexibleWidth or
      UIViewAutoresizingFlexibleHeight);
    ActiveView.addSubview(ActiveWeb);

    { We cannot use regular modal animation due to seldom race condition   }
    { (which are complicated to solve in general case), when closing modal }
    { view controller, before it finished opening.                         }
    ActiveView.setAlpha(0);
    TUIView.OCClass.beginAnimations(nil, nil);
    TUIView.OCClass.setAnimationDuration(0.5);
    ActiveView.setAlpha(1);
    TUIView.OCClass.commitAnimations;
    { If you are not planning to close the popup from code, before the     }
    { popup opens, you can delete above 5 lines and change the last        }
    { parameter in presentModalViewController to True.                     }

    ActivePopup := TUIViewController.Create;
    ActivePopup.setView(ActiveView);
    GetRootViewController.presentModalViewController(ActivePopup, False);
{$ELSEIF Defined(MACOS)}
    if ActiveDelegate = nil then
      ActiveDelegate := TJVEBrowserDelegate.Create;

    ActivePopup := TForm.CreateNew(nil);
    ActivePopup.SetBounds(Round(Screen.Size.cx / 10), Round(Screen.Size.cy / 10),
      Round(Screen.Size.cx * 0.8), Round(Screen.Size.cy * 0.8));
    ActivePopup.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMaximize];
    ActivePopup.Caption := FTitle;
    ActivePopup.Show;
    ActivePopup.Hide;

    CurrentForm := Screen.ActiveForm;
    if Assigned(CurrentForm) and (CurrentForm.WindowState = TWindowState.wsMaximized) then
      ActivePopup.WindowState := TWindowState.wsMaximized;

    WindowSize := TNSView.Wrap(TNSView.Wrap(WindowHandleToPlatform(
      ActivePopup.Handle).Wnd.contentView).superview).frame.size;
    Header := WindowSize.height - TNSView.Wrap(WindowHandleToPlatform(
      ActivePopup.Handle).Wnd.contentView).frame.size.height;

    ActiveButtons := TNSSegmentedControl.Create;
    ActiveButtons.setFrame(CGRectMake(WindowSize.width - Header * 3 - 5,
      WindowSize.height - Header - 1, Header * 3 + 5, Header));
    ActiveButtons.setAutoresizingMask(NSViewMinXMargin or NSViewMinYMargin);
    ActiveButtons.setSegmentCount(2);
    ActiveButtons.setWidth(Header * 1.5, 0);
    ActiveButtons.setWidth(Header * 1.5, 1);
    ActiveButtons.setLabel(ToNSSTR(#$2B05), 0);
    ActiveButtons.setLabel(ToNSSTR(#$27A1), 1);
    ActiveButtons.setEnabled(False, 0);
    ActiveButtons.setEnabled(False, 1);
    Cell := TNSSegmentedCell.Wrap(ActiveButtons.cell);
    Cell.setTrackingMode(NSSegmentSwitchTrackingMomentary);
    Cell.setTarget(ActiveDelegate.GetObjectID);
    Cell.setAction(sel_getUid('ButtonClick:'));
    TNSView.Wrap(TNSView.Wrap(WindowHandleToPlatform(
      ActivePopup.Handle).Wnd.contentView).superview).addSubview(ActiveButtons);

    ActiveWeb := TWKWebView.Create;
    ActiveWeb.setNavigationDelegate(ActiveDelegate.GetObjectID);
    ActiveWeb.setFrame(CGRectMake(0, 0, ActivePopup.ClientWidth, ActivePopup.ClientHeight));
    ActiveWeb.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
    WindowHandleToPlatform(ActivePopup.Handle).View.addSubview(ActiveWeb);

    ExecuteInIdle(procedure
      begin
        // This should be here, as otherwise some errors arrive too early.
        ActiveWeb.loadRequest(TNSURLRequest.Wrap(TNSURLRequest.alloc.initWithURL(
          TNSURL.Wrap(TNSURL.OCClass.URLWithString(ToNSSTR(ActiveBrowser.FURL))))));

        ActivePopup.ShowModal;
        ActiveBrowser := nil;
        ActiveButtons := nil;
        ActiveWeb := nil;
        FreeAndNil(ActivePopup);

        if Assigned(FOnClosed) then
          FOnClosed(Self);
        if FFreeOnClose then
          FreeAndNil(Self);
      end);
{$ELSEIF Defined(ANDROID)}
    if BrowserService = nil then
      BrowserService := TWebBrowserServiceUncontrolled.Create
    else
      BrowserService.Show;
    with BrowserService do
    begin
      URL := FOpenURL;
      Navigate;
    end;

{$ELSEIF Defined(MSWINDOWS)}
    ActivePopup := TForm.CreateNew(nil);
    ActiveScale := ActivePopup.Handle.Scale;
    if ActiveScale < 0.1 then
      ActiveScale := 1;

    ActivePopup.SetBounds(
      Round(Screen.Width / ActiveScale) div 10,
      Round(Screen.Height / ActiveScale) div 10,
      Round(Screen.Width * 8 / ActiveScale) div 10,
      Round(Screen.Height * 8 / ActiveScale) div 10);
    ActivePopup.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMaximize];
    ActivePopup.Caption := FTitle;
    if (FMX.Forms.Screen.ActiveForm <> nil) and
      (FMX.Forms.Screen.ActiveForm.WindowState = TWindowState.wsMaximized) then
        ActivePopup.WindowState := TWindowState.wsMaximized;

    ExecuteInIdle(procedure
      begin
        ActiveDelegate := TJVEBrowserDelegate.Create(nil);
        ActivePopup.OnShow := ActiveDelegate.FormShow;
        ActivePopup.OnResize := ActiveDelegate.FormResize;
        ActiveDelegate.FURL := FURL;

        ActivePopup.ShowModal;
        ActiveBrowser := nil;
        FreeAndNil(ActivePopup);
        FreeAndNil(ActiveWeb);
        FreeAndNil(ActiveDelegate);

        if Assigned(FOnClosed) then
          FOnClosed(Self);
        if FFreeOnClose then
          FreeAndNil(Self);
      end);
{$ENDIF}
    TMessageManager.DefaultManager.SendMessage(Self,
      TJVETrackEvent.Create('Browser', 'Embedded', FOpenURL, 0, acJVEBrowser));
  end else
    URL := FURL;
end;

procedure TJVEBrowser.SetTitle(const Value: string);
begin
  FTitle := Value;
  if IsOpen then
{$IF Defined(IOS)}
    ActiveBar.topItem.setTitle(ToNSSTR(FTitle));
{$ELSEIF Defined(ANDROID)}
    ActiveBar.SetTitle(FTitle);
{$ELSE}
    ActivePopup.Caption := FTitle;
{$ENDIF}
end;

{$IF Defined(IOS)}
initialization
finalization
  FreeAndNil(ActiveDelegate);
{$ELSEIF Defined(MACOS)}
// Running away from direct calls to "dlopen" for the app review (just in case).
function LibWebKit: Pointer; cdecl;
  external '/System/Library/Frameworks/WebKit.framework/WebKit'
  name 'OBJC_CLASS_$_WKWebView';
initialization
finalization
  FreeAndNil(ActiveDelegate);
  if TOSVersion.Major < 0 then // We need any False, which compiler cannot preevaluate
    LibWebKit;
{$ENDIF}
end.

