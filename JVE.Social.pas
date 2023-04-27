(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Social;

interface

uses System.Classes, System.SysUtils, FMX.Types, FMX.Controls, FMX.Dialogs,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  {$IF CompilerVersion >= 26} FMX.Graphics,{$ENDIF} JVE.OpenURL, JVE.Browser,
  JVE.Messaging, JVE.Actions, System.StrUtils, JVE.Utils, FMX.Forms, System.Types;

type
  TJVESocialNetwork = (snTwitter, snFacebook, snSinaWeibo);

  [ComponentPlatformsAttribute($000B945F)]
  TJVESocial = class(TComponent, IJVEExecutable)
  private
    FText: String;
    FURL: String;
    FBitmap: TBitmap;
    FEmbed: Boolean;
    FNetwork: TJVESocialNetwork;
    FStyleChangedId: Integer;
    FAutoTranslate: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
  protected
    procedure Open(Sender: TJVEAction); overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;
    // Opens the native or web posting window; returns immediately. True result
    // indicates no program failure, but user could have cancelled the action.
    function Open: Boolean; overload;
    // Indicates the availability of the native interface. Even if there is
    // no native interface, the class will fall back to posting via a browser.
    // The Sender parameter is for IJVEExecutable support and should not be used.
    function CanOpen(Sender: TJVEAction = nil): Boolean; overload;
    class function CanOpen(Network: TJVESocialNetwork): Boolean; overload;
  published
    // The bitmap property is NOT supported, except in iOS-native mode.
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    // If fallback is required, whether to embed browser or open external.
    property Embed: Boolean read FEmbed write FEmbed default True;
    property Network: TJVESocialNetwork read FNetwork write FNetwork default snTwitter;
    // The default text and URL can be edited by the user before posting.
    property Text: String read FText write FText;
    property URL: String read FURL write FURL;
  end;

const
  // Might be usefull outside this unit.
  TJVESocialNetworkNames: array[TJVESocialNetwork] of String =
    ('Twitter', 'Facebook', 'SinaWeibo');

implementation

{$IF Defined(IOS)}

uses Macapi.ObjectiveC, iOSapi.CoreGraphics, iOSapi.UIKit, iOSapi.Foundation,
  Posix.Dlfcn;

var
  SocialNetworkServices: array[TJVESocialNetwork] of NSString;

type
  TSLComposeViewControllerBlock = procedure(result: Pointer) of object;

  SLComposeViewController = interface(UIViewController)
    function serviceType: NSString; cdecl;
    function setInitialText(text: NSString): Boolean; cdecl;
    function addImage(image: UIImage): Boolean; cdecl;
    function removeAllImages: Boolean; cdecl;
    function addURL(url: NSURL): Boolean; cdecl;
    function removeAllURLs: Boolean; cdecl;
    function completionHandler: TSLComposeViewControllerBlock; cdecl;
    procedure setCompletionHandler(handler: TSLComposeViewControllerBlock); cdecl;
  end;

  SLComposeViewControllerClass = interface(UIViewControllerClass)
    function isAvailableForServiceType(serviceType: NSString): Boolean; cdecl;
    function composeViewControllerForServiceType(
      serviceType: NSString): SLComposeViewController; cdecl;
  end;

  TSLComposeViewController = class(TOCGenericImport
    <SLComposeViewControllerClass, SLComposeViewController>) end;

  TSLComposeWrapper = class
    Controller: SLComposeViewController;
    procedure ViewControllerBlock(result: Pointer);
  end;

var
  SLComposeWrapper: TSLComposeWrapper;
  ActivitiesArray: NSMutableArray;
  ActivityViewController: UIActivityViewController;
  PopoverController: UIPopoverController;

{ TSLComposeViewControllerBlockWrapper }

// Actually the parameter should be an enum, but an exception is reported
// if it is and we don't really need it. Probably a Delphi bug in Objective C
// blocks interface.
// But then, blocks' interface is implemented incorrectly anyway: they
// should have been "references to procedures", not "procedures of objects"
// (hence the TSLComposeWrapper class).
procedure TSLComposeWrapper.ViewControllerBlock(result: Pointer);
begin
  if Controller <> nil then
    Controller.dismissModalViewControllerAnimated(True);
  Controller := nil;
end;

{$ELSEIF Defined(ANDROID)}

uses Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText;

function GetIntent(Network: TJVESocialNetwork): JIntent;
var
  List: JList;
  Index: Integer;
  Activity: String;
  Resolve: JResolveInfo;
begin
  Result := TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SEND);
  Result.setType(StringToJString('text/plain'));

  List := GetSharedActivity.getPackageManager.queryIntentActivities(Result, 0);
  for Index := 0 to List.size - 1 do
  begin
    Resolve := TJResolveInfo.Wrap(List.get(Index));
    case Network of
      snTwitter: Activity := 'twitter';
      snFacebook: Activity := 'facebook';
      snSinaWeibo: Activity := 'weibo';
    end;

    if JStringToString(Resolve.activityInfo.packageName).ToLower.Contains(Activity) or
      JStringToString(Resolve.activityInfo.name).ToLower.Contains(Activity) then
    begin
      Result.setPackage(Resolve.activityInfo.packageName);
      Exit;
    end;
  end;
  Result := nil;
end;

{$ENDIF}

{ TJVESocial }

constructor TJVESocial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBitmap := TBitmap.Create(0, 0);
  FEmbed := True;
  FNetwork := snTwitter;
  FAutoTranslate := True;
  FStyleChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TJVESocial.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  FreeAndNil(FBitmap);
  inherited Destroy;
end;

procedure TJVESocial.Open(Sender: TJVEAction);
begin
  Open;
end;

procedure TJVESocial.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  Translation: String;
begin
  if FAutoTranslate and (FText <> '') then
  begin
    Translation := Translate(FText); // Mandatory for texts collection
    if not (csDesigning in ComponentState) then
      Text := Translation;
  end;
end;

procedure TJVESocial.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

function TJVESocial.Open: Boolean;
var
  OpenURL: String;
  {$IF Defined(ANDROID)}
  Intent: JIntent;
  {$ELSEIF Defined(IOS)}
  ParsedURL: Pointer;
  Control: IControl;
  Rect: CGRect;

  // This function is available as part of the Delphi RTL, in file
  // FMX.MediaLibrary.iOS.pas, but it's private there (for some reason).
  // By the way, its name is misspelt there as "BitampToUIImage".
  function BitmapToUIImage(const Bitmap: TBitmap): UIImage;
  var
    ImageRef: CGImageRef;
    CtxRef: CGContextRef;
    ColorSpace: CGColorSpaceRef;
    BitmapData: TBitmapData;
  begin
    if Bitmap.IsEmpty then
      Result := TUIImage.Create
    else
    begin
      ColorSpace := CGColorSpaceCreateDeviceRGB;
      try
        if Bitmap.Map({$IF CompilerVersion < 27}TMapAccess.maRead
          {$ELSE}TMapAccess.Read{$ENDIF}, BitmapData) then
        begin
          CtxRef := CGBitmapContextCreate(BitmapData.Data,
            Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace,
            kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big);
          try
            ImageRef := CGBitmapContextCreateImage(CtxRef);
            try
              Result := TUIImage.Alloc;
              Result.initWithCGImage(ImageRef);
            finally
              CGImageRelease(ImageRef);
            end;
          finally
            CGContextRelease(CtxRef);
          end;
        end;
      finally
        CGColorSpaceRelease(ColorSpace);
      end;
    end;
  end;
  {$ENDIF}
begin
  Result := True;
  try
    {$IF Defined(IOS)}
    if CanOpen then
    begin
      ParsedURL := TNSURL.OCClass.URLWithString(ToNSSTR(FURL));
      if TSLComposeViewController.Defined and TSLComposeViewController.
        OCClass.isAvailableForServiceType(SocialNetworkServices[Network]) then
      begin
        if SLComposeWrapper = nil then
          SLComposeWrapper := TSLComposeWrapper.Create;

        SLComposeWrapper.Controller := TSLComposeViewController.OCClass.
          composeViewControllerForServiceType(SocialNetworkServices[FNetwork]);
        SLComposeWrapper.Controller.setInitialText(ToNSSTR(FText));
        SLComposeWrapper.Controller.setCompletionHandler(SLComposeWrapper.ViewControllerBlock);
        if ParsedURL <> nil then
          SLComposeWrapper.Controller.addURL(TNSURL.Wrap(ParsedURL));
        if (FBitmap <> nil) and not FBitmap.IsEmpty then
          SLComposeWrapper.Controller.addImage(BitmapToUIImage(FBitmap));
        GetRootViewController.presentModalViewController(SLComposeWrapper.Controller, True);
      end else
      begin
        if ActivityViewController <> nil then
          ActivityViewController.release;
        if ActivitiesArray <> nil then
          ActivitiesArray.removeAllObjects
        else
          ActivitiesArray := TNSMutableArray.Wrap(TNSMutableArray.Alloc.init);

        if ParsedURL <> nil then
          ActivitiesArray.addObject(ParsedURL);
        if (FBitmap <> nil) and not FBitmap.IsEmpty then
          ActivitiesArray.addObject(PointerNSObject(BitmapToUIImage(FBitmap)));
        if FText <> '' then
          ActivitiesArray.addObject(PointerNSSTR(FText));

        ActivityViewController := TUIActivityViewController.alloc;
        ActivityViewController.initWithActivityItems(ActivitiesArray, nil);

        if TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad then
        begin
          Control := Screen.FocusControl;
          if Control <> nil then
          begin
            Rect.origin.x := Control.LocalToScreen(PointF(0, 0)).X;
            Rect.origin.y := Control.LocalToScreen(PointF(0, 0)).Y;
            Rect.size.width := TControl(Control.GetObject).Width;
            Rect.size.height := TControl(Control.GetObject).Height;
          end else
            Rect := CGRectMake(0, 0, 0, 0);

          PopoverController := TUIPopoverController.Alloc;
          PopoverController.initWithContentViewController(ActivityViewController);
          PopoverController.presentPopoverFromRect(Rect,
            GetRootViewController.View, UIPopoverArrowDirectionAny, True);
        end else
          GetRootViewController.presentModalViewController(ActivityViewController, True);
      end;
    end else
    {$ELSEIF Defined(ANDROID)}
    Intent := GetIntent(FNetwork);
    if Intent <> nil then
    begin
      Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(
        IfThen(FNetwork = snFacebook, FURL, FText + ' ' + FURL)));
      GetSharedActivityContext.startActivity(TJIntent.JavaClass.
        createChooser(Intent, StrToJCharSequence('Select:')));
      Exit;
    end;
    {$ENDIF}
    begin
      case FNetwork of
        snTwitter: OpenURL := 'https://twitter.com/intent/tweet?text=' +
          TJVEOpenURL.URLEncode(FText + ' ' + FURL);
        snFacebook: OpenURL := 'https://www.facebook.com/sharer/sharer.php?u=' +
          TJVEOpenURL.URLEncode(FURL) + '&t=' + TJVEOpenURL.URLEncode(FText);
        snSinaWeibo: OpenURL := 'https://service.weibo.com/share/share.php?url=' +
          TJVEOpenURL.URLEncode(FURL) + '&title=' + TJVEOpenURL.URLEncode(FText);
      end;

      if FEmbed then
        TJVEBrowser.OpenURL(OpenURL, TJVESocialNetworkNames[FNetwork])
      else
        Result := TJVEOpenURL.OpenURL(OpenURL);
    end;
  finally
    if Result then
      TMessageManager.DefaultManager.SendMessage(Self, TJVETrackSocial.Create(
        TJVESocialNetworkNames[FNetwork], 'Post', FURL, acJVESocial));
  end;
end;

function TJVESocial.CanOpen(Sender: TJVEAction = nil): Boolean;
begin
  Result := CanOpen(FNetwork);
end;

// Social framework is only available as of iOS 6, we are compatible with iOS 5.
class function TJVESocial.CanOpen(Network: TJVESocialNetwork): Boolean;
begin
  Result :=
{$IF Defined(IOS)}
    (TSLComposeViewController.Defined and TSLComposeViewController.
    OCClass.isAvailableForServiceType(SocialNetworkServices[Network])) or
    TUIActivityViewController.Defined;
{$ELSEIF Defined(ANDROID)}
    GetIntent(Network) <> nil;
{$ELSE}
    True;
{$ENDIF}
end;

{$IFDEF IOS}
const
  Framework = '/System/Library/Frameworks/Social.framework/Social';
{$IFDEF CPUX86}
var
  Module: THandle;
{$ENDIF}
initialization
{$IFDEF CPUX86}
  Module := dlopen(MarshaledAString(Framework), RTLD_LAZY);
{$ENDIF}
  SocialNetworkServices[snTwitter] :=
    CocoaNSStringConst(Framework, 'SLServiceTypeTwitter');
  SocialNetworkServices[snFacebook] :=
    CocoaNSStringConst(Framework, 'SLServiceTypeFacebook');
  SocialNetworkServices[snSinaWeibo] :=
    CocoaNSStringConst(Framework, 'SLServiceTypeSinaWeibo');
finalization
{$IFDEF CPUX86}
  dlclose(Module);
{$ENDIF}
{$ENDIF}
end.


