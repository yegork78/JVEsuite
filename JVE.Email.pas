(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Email;

interface

uses System.Classes, System.SysUtils, JVE.Utils,
  {$IF CompilerVersion >= 27} System.Messaging,{$ELSE} FMX.Messages,{$ENDIF}
  FMX.Controls, FMX.Types, JVE.OpenURL, JVE.Messaging, JVE.Actions;

type
  [ComponentPlatformsAttribute($000B945F)]
  TJVEEmail = class(TComponent, IJVEExecutable)
  private
    FCcRecipients: TStrings;
    FBccRecipients: TStrings;
    FBody: TStrings;
    FSubject: String;
    FToRecipients: TStrings;
    FIsHTML: Boolean;
    FStyleChangedId: Integer;
    FAutoTranslate: Boolean;
    procedure SetBody(const Value: TStrings);
    procedure SetToRecipients(const Value: TStrings);
    procedure SetCcRecipients(const Value: TStrings);
    procedure SetBccRecipients(const Value: TStrings);
    procedure StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
  protected
    // IJVEExecutable support
    procedure Open(Sender: TJVEAction);
    function CanOpen(Sender: TJVEAction): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property AutoTranslate: Boolean read FAutoTranslate write FAutoTranslate;
    // Opens the email composition UI.
    function Send: Boolean;
    // Indicates the availability of the native interface. Even if there is
    // no native interface, the class will fall back to posting via a browser.
    class function CanSend: Boolean;
  published
    property Subject: String read FSubject write FSubject;
    property Body: TStrings read FBody write SetBody;
    property IsHTML: Boolean read FIsHTML write FIsHTML default False;
    property ToRecipients: TStrings read FToRecipients write SetToRecipients;
    property CcRecipients: TStrings read FCcRecipients write SetCcRecipients;
    property BccRecipients: TStrings read FBccRecipients write SetBccRecipients;
  end;

implementation

{$IF Defined(IOS)}

uses Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation, Posix.Dlfcn,
  Macapi.CoreFoundation, iOSapi.CocoaTypes;

type
  MFMailComposeViewControllerClass = interface(UINavigationControllerClass)
    function canSendMail: Boolean; cdecl;
  end;

  MFMailComposeViewController = interface(UINavigationController)
    function mailComposeDelegate: Pointer; cdecl;
    procedure setMailComposeDelegate(delegate: Pointer); cdecl;
    procedure setSubject(subject: NSString); cdecl;
    procedure setToRecipients(toRecipients: NSArray); cdecl;
    procedure setCcRecipients(ccRecipients: NSArray); cdecl;
    procedure setBccRecipients(bccRecipients: NSArray); cdecl;
    procedure setMessageBody(body: NSString; isHTML: Boolean); cdecl;
    procedure addAttachmentData(attachment: NSData; mimeType: NSString;
      fileName: NSString); cdecl;
  end;

  TMFMailComposeViewController = class(TOCGenericImport
    <MFMailComposeViewControllerClass, MFMailComposeViewController>) end;

  MFMailComposeViewControllerDelegate = interface(IObjectiveC)
    procedure mailComposeController(controller: MFMailComposeViewController;
      didFinishWithResult: NSInteger; error: NSError); cdecl;
  end;

  TMFMailComposeViewControllerDelegate = class(TOCLocal, MFMailComposeViewControllerDelegate)
  public
    procedure mailComposeController(controller: MFMailComposeViewController;
      didFinishWithResult: NSInteger; error: NSError); cdecl;
  end;

var
  EmailDelegate: TMFMailComposeViewControllerDelegate;

{ TMFMailComposeViewControllerDelegate }

procedure TMFMailComposeViewControllerDelegate.mailComposeController(
  controller: MFMailComposeViewController; didFinishWithResult: NSInteger; error: NSError);
begin
  GetRootViewController.dismissModalViewControllerAnimated(True);
end;

{$ELSEIF Defined(MACOS)}

uses Macapi.CoreFoundation;

{$ELSEIF Defined(ANDROID)}

uses System.Generics.Collections, AndroidApi.JNI.GraphicsContentViewText,
  AndroidApi.JNI.Net, AndroidApi.JNIBridge, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Os,
  {$IF CompilerVersion >= 27} AndroidApi.Helpers,{$ENDIF} FMX.Helpers.Android,
  Androidapi.JNI.App;

type
  {Class forward declarations}
  JSpanned = interface; //android.text.Spanned
  JHtml = interface;  //android.text.Html

  JSpannedClass = interface(JCharSequenceClass)
  ['{C400CAF8-32FC-4774-8260-4EF68CA4305C}']
    {Methods}
  end;

  [JavaSignature('android/text/Spanned')]
  JSpanned = interface(JCharSequence)
  ['{F308D039-DA54-499F-9CC1-5C12E73F9FA6}']
  end;
  TJSpanned = class(TJavaGenericImport<JSpannedClass, JSpanned>) end;

  JHtmlClass = interface(JObjectClass)
  ['{534ACC8D-8693-4A5B-B494-CD84E44BC040}']
    {Methods}
    function fromHtml(source: JString): JSpanned;
    function toHtml(text: JSpanned): JString;
  end;

  [JavaSignature('android/text/Html')]
  JHtml = interface(JObject)
  ['{775589B4-685F-483F-80DA-71BBCCC6D2E5}']
  end;
  TJHtml = class(TJavaGenericImport<JHtmlClass, JHtml>) end;

{$ENDIF}

{ TJVEEmail }

function TJVEEmail.CanOpen(Sender: TJVEAction): Boolean;
begin
  Result := CanSend;
end;

class function TJVEEmail.CanSend: Boolean;
begin
  {$IFDEF IOS}
  Result := TMFMailComposeViewController.Defined and
    TMFMailComposeViewController.OCClass.canSendMail;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

constructor TJVEEmail.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoTranslate := True;
  FBody := TStringList.Create;
  FToRecipients := TStringList.Create;
  FCcRecipients := TStringList.Create;
  FBccRecipients := TStringList.Create;
  FStyleChangedId := TMessageManager.DefaultManager.
    SubscribeToMessage(TStyleChangedMessage, StyleChangedHandler);
end;

destructor TJVEEmail.Destroy;
begin
  FreeAndNil(FBody);
  FreeAndNil(FToRecipients);
  FreeAndNil(FCcRecipients);
  FreeAndNil(FBccRecipients);
  TMessageManager.DefaultManager.Unsubscribe(TStyleChangedMessage, FStyleChangedId);
  inherited Destroy;
end;

procedure TJVEEmail.Open(Sender: TJVEAction);
begin
  Send;
end;

procedure TJVEEmail.StyleChangedHandler(const Sender: TObject; const Msg: TMessage);
var
  Translation: String;
begin
  if not FAutoTranslate then
    Exit;

  if FSubject <> '' then
  begin
    Translation := Translate(FSubject); // Mandatory for texts collection
    if not (csDesigning in ComponentState) then
      Subject := Translation;
  end;

  Translation := FBody.Text.Trim.Replace(#13#10, '$LF$');
  if Translation <> '' then
  begin
    Translation := Translate(Translation).Replace('$LF$', #13#10); // Mandatory for texts collection
    if not (csDesigning in ComponentState) then
      Body.Text := Translation;
  end;
end;

function TJVEEmail.Send: Boolean;
  {$IF Defined(IOS)}
  function SendiOS: Boolean;
  var
    Mail: MFMailComposeViewController;
    function CreateArray(const List: TStrings): NSMutableArray;
    var Item: String;
    begin
      Result := TNSMutableArray.Create;
      for Item in List do Result.addObject(PointerNSSTR(Item));
    end;
  begin
    if not CanSend then
      Exit(False);

    if EmailDelegate = nil then
      EmailDelegate := TMFMailComposeViewControllerDelegate.Create;

    Mail := TMFMailComposeViewController.Create;
    Mail.setSubject(ToNSSTR(FSubject));
    Mail.setMessageBody(ToNSSTR(FBody.Text), FIsHTML);
    Mail.setToRecipients(CreateArray(FToRecipients));
    Mail.setCcRecipients(CreateArray(FCcRecipients));
    Mail.setBccRecipients(CreateArray(FBccRecipients));
    Mail.setModalPresentationStyle(1{UIModalPresentationPageSheet});
    Mail.setMailComposeDelegate(EmailDelegate.GetObjectID);
    GetRootViewController.presentModalViewController(Mail, True);
    Result := True;
  end;
  {$ELSEIF Defined(ANDROID)}
  function CreateEmailOnlyChooserIntent(Source: JIntent; ChooserTitle: JCharSequence): JIntent;
  var
    Intents: TStack<JIntent>;
    ActivityIt: JIterator;
    function ArrayToJavaParcelableArray(const Values: TArray<JIntent>): TJavaObjectArray<JParcelable>;
    var i: Integer;
    begin
      Result := TJavaObjectArray<JParcelable>.Create(Length(Values));
      for i := 0 to Length(Values) - 1 do
        Result[i] := TJParcelable.Wrap((Values[i] as ILocalObject).GetObjectID);
    end;
  begin
    Intents := TStack<JIntent>.Create;
    ActivityIt := GetSharedActivity.getPackageManager().queryIntentActivities(
      TJIntent.JavaClass.init(TJIntent.JavaClass.ACTION_SENDTO,
      TJNet_Uri.JavaClass.fromParts(StringToJString('mailto'),
      StringToJString('test@domain.com'), Nil)), 0).iterator;
    while ActivityIt.hasNext do
      Intents.Push(TJIntent.JavaClass.init(Source).setPackage(TJResolveInfo.
        Wrap((ActivityIt.next as ILocalObject).GetObjectID).activityInfo.packageName));

    if Intents.Count > 0 then
      Result := TJIntent.JavaClass.createChooser(Intents.Pop, ChooserTitle).putExtra(
        TJIntent.JavaClass.EXTRA_INITIAL_INTENTS, ArrayToJavaParcelableArray(Intents.toArray))
    else
      Result := TJIntent.JavaClass.createChooser(Source, ChooserTitle);

    Intents.Clear;
    Intents.Free;
  end;

  function SendAndroid: Boolean;
  var
    MailIntent: JIntent;
    function CreateArray(const List: TStrings): TJavaObjectArray<JString>;
    var i: Integer;
    begin
      Result := TJavaObjectArray<JString>.Create(List.Count);
      for i := 0 to List.Count - 1 do
        Result[i] := StringToJstring(List.Strings[i]);
    end;
  begin
    if not CanSend then
      Exit(False);

    MailIntent := TJIntent.Create;
    try
      MailIntent.setAction(TJIntent.JavaClass.ACTION_SEND);
      MailIntent.putExtra(TJIntent.JavaClass.EXTRA_EMAIL, CreateArray(FToRecipients));
      MailIntent.putExtra(TJIntent.JavaClass.EXTRA_SUBJECT, StringToJString(FSubject));
      MailIntent.putExtra(TJIntent.JavaClass.EXTRA_BCC, CreateArray(FBccRecipients));
      MailIntent.putExtra(TJIntent.JavaClass.EXTRA_CC, CreateArray(FCcRecipients));
      if IsHTML then
        MailIntent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, TJHtml.JavaClass.fromHtml(StringToJString(FBody.Text)))
      else
        MailIntent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(FBody.Text));
      MailIntent.setType(StringToJString('message/rfc822'));

      GetSharedActivity.startActivity(
        CreateEmailOnlyChooserIntent(MailIntent, StrToJCharSequence('Choose E-mail App')));
      Result := True;
    except
      on E: Exception do
      begin
        Result := False;
      end;
    end;
  end;
  {$ENDIF}

  function Encode(Key, Value: String): String;
  begin
    if Value = '' then
      Exit('');

    Result := TJVEOpenURL.URLEncode(Value);
    if Key <> '' then
      Result := '&' + Key + '=' + Result;
  end;

  function EmailBody: String;
  var
    Value: String;
    Index, Upper: Integer;
  begin
    if not FIsHTML then
      Exit(FBody.Text);

    Value := FBody.Text.Replace(#13, ' ').Replace(#10, ' ');
    repeat
      Index := Value.Length;
      Value := Value.Replace('  ', ' ');
    until Index = Value.Length;

    Index := Value.ToUpper.IndexOf('<BR');
    while Index <> -1 do
    begin
      Upper := Value.IndexOf('>', Index);
      if Upper = -1 then
        Upper := Index + 3;
      Value := Value.Remove(Index, Upper - Index + 1).Insert(Index, sLineBreak);
      Index := Value.ToUpper.IndexOf('<BR');
    end;

    Index := Value.IndexOf('<');
    while Index <> -1 do
    begin
      Upper := Value.IndexOf('>', Index);
      if Upper = -1 then
        Upper := Index + 1;
      Value := Value.Remove(Index, Upper - Index + 1);
      Index := Value.IndexOf('<');
    end;
    Result := Value;
  end;

begin
  Result := {$IFDEF IOS}SendiOS or {$ENDIF}
    {$IFDEF ANDROID}SendAndroid or {$ENDIF}
    TJVEOpenURL.OpenURL(StringReplace('mailto:' +
    Encode('', FToRecipients.CommaText) + '?' +
    Encode('cc', FCcRecipients.CommaText) +
    Encode('bcc', FBccRecipients.CommaText) +
    Encode('subject', FSubject) +
    Encode('body', EmailBody), '?&', '?', []));

  if Result then
    TMessageManager.DefaultManager.SendMessage(Self, TJVETrackSocial.Create(
      'Email', 'Send', FToRecipients.CommaText + ' / ' + FSubject, acJVEEmail));
end;

procedure TJVEEmail.SetBccRecipients(const Value: TStrings);
begin
  FBccRecipients.Assign(Value);
end;

procedure TJVEEmail.SetBody(const Value: TStrings);
begin
  FBody.Assign(Value);
end;

procedure TJVEEmail.SetCcRecipients(const Value: TStrings);
begin
  FCcRecipients.Assign(Value);
end;

procedure TJVEEmail.SetToRecipients(const Value: TStrings);
begin
  FToRecipients.Assign(Value);
end;

{$IFDEF IOS}
  {$IFDEF CPUX86}
var
  Module: THandle;
initialization
  Module := dlopen(MarshaledAString(
    '/System/Library/Frameworks/MessageUI.framework/MessageUI'), RTLD_LAZY);
finalization
  dlclose(Module);
  {$ELSE}
initialization
finalization
  {$ENDIF}
  FreeAndNil(EmailDelegate);
{$ENDIF}
end.



