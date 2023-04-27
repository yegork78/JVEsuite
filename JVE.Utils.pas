(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Utils;

interface

uses System.Classes, System.SysUtils, FMX.Types, System.Generics.Collections,
  System.UITypes, FMX.Dialogs, FMX.Consts, System.JSON
  {$IF Defined(IOS)}, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.Foundation, iOSapi.CocoaTypes
  {$ELSEIF Defined(MACOS)}, Macapi.ObjectiveC, Macapi.Foundation, Macapi.CocoaTypes
  {$ELSEIF Defined(ANDROID)}, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App
  {$IF CompilerVersion >= 27}, AndroidApi.Helpers {$ENDIF}
  {$ELSEIF Defined(MSWINDOWS)}, Winapi.Windows {$ENDIF};

// This function returns an array of identifiers, which are unique for this machine.
// On Mac OS X and Windows these are the MAC addresses of all the network adapters.
// On iOS 6 this is the [identifierForVendor]; on earlier iOS devices this is a
// persistent random number (meaning on iOS this value change on reinstall);
// On Android this is the IMEI number.
// Usually the first array entry is good enough for most uses.
{$DEFINE CONFIG_VERSION}
{$IFDEF CONFIG_VERSION}
function GetDeviceUniqueIds: TArray<String>;
{$ENDIF}

// This function detaches a thread. On mobile platforms the detached thread will
// have the scope-release protection, so that you don't need to save any global
// variables. On Mac/iOS the thread will have its own autorelease pool.
// If you need special synchronization, you may use it as follows:
// DetachThread(procedure(Synchronize: TProc<TThreadProcedure>)
//   begin
//     Your_Code_Here;
//     Synchronize(procedure
//       begin
//         Your_Synchronized_Code_Here;
//       end);
//     More_of_Your_Code_Here;
//   end);
procedure DetachThread(ThreadProc: TProc); overload;
procedure DetachThread(ThreadProc: TProc<TProc<TThreadProcedure>>); overload;

// This function queues the procedure to be executed exactly once during
// the next idle processing.
// The action is executed via a very short timer, because in recent Delphi
// versions the function will never be called if any animation is in progress.
procedure ExecuteInIdle(IdleProc: TProc);

// The following group of procedures provides message boxes, whose invocation
// does not block the calling thread (on iOS without an additional message loop).
// The user choice (if applicable) is provided via a callback.
// If Title is not provided, a default for the dialog type will be used.
// -- Regular message dialog; OK button only; no notification.
procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType = TMsgDlgType.mtCustom;
  const Title: string = ''); inline; overload;
// -- Confirmation message; OK and Cancel buttons only; Result gets True if confirmed.
procedure ShowConfirmation(const Msg: string; Result: TProc<Boolean>); inline; overload;
procedure ShowConfirmation(const Msg: string; const Title: string;
  Result: TProc<Boolean>); overload;
// -- These provide all the available options.
procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Result: TProc<TModalResult>); inline; overload;
procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
  Result: TProc<TModalResult>); inline; overload;
procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  const Title: string; Buttons: TMsgDlgButtons;
  Result: TProc<TModalResult>); inline; overload;
procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  Title: string; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn;
  Result: TProc<TModalResult>); overload;

{$IFDEF MACOS}
// This helper allows you to verify that the platform implements the given class.
// You can use this, for example, to check whether an iOS 6-only class is present.
type
  TOCImportHelper = class helper for TOCImport
    class function Defined: Boolean;
  end;

// Various conversions functions. The functionality is immediately clear from
// the parameter and result types.
function FromNSSTR(Value: NSString): String; overload;
function FromNSSTR(Value: Pointer): String; overload;
function ToNSSTR(Str: String): NSString;
function PointerNSSTR(Str: String): Pointer;
function PointerNSObject(AObject: NSObject): Pointer;

// This function converts an NSDictionary to a convinient TStrings to use with
// the Values property. The result might look like this:
//   Name1=Value1
//   Name2=Value2
//   Arr#=2       <= An array will have a count record with '#' suffix, followed by data
//   Arr[0].Name1=Value3
//   Arr[0].Name2=Value4
//   Arr[1]=Value5
// Only NSDictionary and NSArray have specific converters, other types are simply
// directly converted to Strings (using Objective-C means).
function NSDictionaryToStrings(Dict: NSDictionary): TStrings;
{$ENDIF}

function JSONObjectToStrings(Dict: TJSONObject): TStrings;

{$IF Defined(IOS)}
function GetRootViewController: UIViewController;
function SharedUIApplication: UIApplication;
{$ENDIF}

{$IF Defined(ANDROID)}
function IsRunningInForeground: Boolean;
// Fighting deprecation warnings, while staying backward compatible...
function GetSharedActivityContext: JContext; inline;
function GetSharedActivity: JActivity; inline;
{$ENDIF}

procedure JVERequestTrackingAuthorization;

implementation

uses System.Types, System.Math, FMX.Types3D, System.DateUtils, FMX.Forms//, JVE.OpenURL
  {$IF CompilerVersion >= 27}, System.Messaging {$ELSE}, FMX.Messages {$ENDIF}
  {$IF Defined(IOS)}, Macapi.ObjCRuntime, JVE.Mac.CodeBlocks
  {$ELSEIF Defined(MACOS)}, Macapi.ObjCRuntime, Macapi.AppKit, FMX.Platform.Mac,
  System.TypInfo, FMX.Controls
  {$ELSEIF Defined(ANDROID)}, AndroidApi.JNI.Telephony, AndroidApi.JNIBridge,
  FMX.Helpers.Android, FMX.Platform, AndroidApi.JNI.JavaTypes, AndroidApi.JNI.Net,
  AndroidApi.JNI.Provider, AndroidApi.JNI.Embarcadero, AndroidApi.JNI.Os
  {$IF CompilerVersion >= 31}, FMX.DialogService{$ENDIF}
  {$ELSEIF Defined(MSWINDOWS)}, Winapi.IpTypes, Winapi.IpHlpApi, System.Win.Registry{$ENDIF}
  {$IFDEF CONFIG_VERSION}, JVE.Configuration{$ENDIF};

{$IFDEF CONFIG_VERSION}
var
  DeviceUniqueIds: TArray<String>;

{$IF Defined(IOS)}
type
  NSUUID = interface(NSObject)
    function UUIDString: NSString; cdecl;
  end;
  UIDevice = interface(NSObject)
    function identifierForVendor: NSUUID; cdecl;
  end;
  UIDeviceClass = interface(NSObjectClass)
    function currentDevice: UIDevice; cdecl;
  end;
  TUIDevice = class(TOCGenericImport<UIDeviceClass, UIDevice>)
  end;
{$ELSEIF Defined(MACOS32)}
const
  SystemConfiguration =
    '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';
function SCNetworkInterfaceCopyAll: Pointer; cdecl;
  external SystemConfiguration name '_SCNetworkInterfaceCopyAll';
function SCNetworkInterfaceGetHardwareAddressString(Int: Pointer): Pointer; cdecl;
  external SystemConfiguration name '_SCNetworkInterfaceGetHardwareAddressString';
{$ELSEIF Defined(MACOS64)}
const
  SystemConfiguration =
    '/System/Library/Frameworks/SystemConfiguration.framework/SystemConfiguration';
function SCNetworkInterfaceCopyAll: Pointer; cdecl;
  external SystemConfiguration name 'SCNetworkInterfaceCopyAll';
function SCNetworkInterfaceGetHardwareAddressString(Int: Pointer): Pointer; cdecl;
  external SystemConfiguration name 'SCNetworkInterfaceGetHardwareAddressString';
{$ELSEIF Defined(ANDROID)}
type
  JSystem = interface;  //java.lang.System

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

  [JavaSignature('java/lang/System')]
  JSystem = interface(JObject)
  ['{A1046CA2-6B5C-446A-878F-5CB0B91E29A1}']

  end;
  TJSystem = class(TJavaGenericImport<JSystemClass, JSystem>) end;

{$ENDIF}

function CreateDeviceUniqueIds: TArray<String>;
{$IF Defined(IOS)}
const
  TJVEDeviceUniqueId = 'TJVEDeviceUniqueId';
begin
  SetLength(Result, 1);
  if TOSVersion.Check(6) then
    Result[0] := FromNSSTR(TUIDevice.OCClass.currentDevice.
      identifierForVendor.UUIDString).Replace('-', '', [rfReplaceAll]);

  if Result[0] = '' then
    Result[0] := TJVEConfiguration.ReadString(TJVEDeviceUniqueId, '');

  if Result[0] = '' then
  begin
    Result[0] := IntToStr(Random(90000000) + 10000000) +
      IntToStr(Random(90000000) + 10000000);
    TJVEConfiguration.WriteString(TJVEDeviceUniqueId, Result[0]);
  end;
end;
{$ELSEIF Defined(MACOS)}
var
  Idx: Integer;
  Current: String;
  Interfaces: NSArray;
begin
  SetLength(Result, 0);
  Interfaces := TNSArray.Wrap(SCNetworkInterfaceCopyAll());
  for Idx := 0 to Interfaces.count - 1 do
  begin
    Current := FromNSSTR(TNSString.Wrap(SCNetworkInterfaceGetHardwareAddressString(
      Interfaces.objectAtIndex(Idx)))).Replace(':', '', [rfReplaceAll]);
    if (Length(Current) >= 8) and not Current.Contains('000000') then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Current;
    end;
  end;

  if Length(Result) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := '1234567890BB';
  end;
end;
{$ELSEIF Defined(ANDROID)}
const
  TJVEDeviceUniqueId = 'TJVEDeviceUniqueId';
begin
  SetLength(Result, 1);
  // Thanks to Marco Cirinei for this one
  Result[0] := JStringToString(TJSettings_Secure.JavaClass.getString(
    GetSharedActivity.getContentResolver, TJSettings_Secure.JavaClass.ANDROID_ID));

  if Result[0] = '' then
    Result[0] := TJVEConfiguration.ReadString(TJVEDeviceUniqueId, '');

  if Result[0] = '' then
  begin
    Result[0] := IntToStr(Random(90000000) + 10000000) +
      IntToStr(Random(90000000) + 10000000);
    TJVEConfiguration.WriteString(TJVEDeviceUniqueId, Result[0]);
  end;
end;
{$ELSEIF Defined(MSWINDOWS)}
var
  Name: String;
  Size: Cardinal;
  Info, Current: PIP_ADAPTER_INFO;
begin
  Info := nil;
  Size := 0;
  GetAdaptersInfo(Info, Size);
  GetMem(Info, Size);
  GetAdaptersInfo(Info, Size);
  Current := Info;

  SetLength(Result, 0);
  while Current <> nil do
  begin
    if Current.AddressLength >= 4 then
    begin
      Name := '';
      for Size := 0 to Current.AddressLength - 1 do
        Name := Name + IntToHex(Current.Address[Size], 2);

      if not Name.Contains('00000000') then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Name;
      end;
    end;
    Current := Current.Next;
  end;
  FreeMem(Info);

  if Length(Result) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := '1234567890AA';
  end;
end;
{$ENDIF}

function GetDeviceUniqueIds: TArray<String>;
begin
  if Length(DeviceUniqueIds) = 0 then
    DeviceUniqueIds := CreateDeviceUniqueIds;
  Result := DeviceUniqueIds;
end;
{$ENDIF}

function JSONObjectToStrings(Dict: TJSONObject): TStrings;

  procedure AddObject(Obj: TJSONValue; Name: String); forward;

  procedure AddArray(Arr: TJSONArray; Prefix: String);
  var
    Index: Integer;
  begin
    Result.Add(Format('%s#=%d', [Prefix, Arr.Count]));
    for Index := 0 to Arr.Count - 1 do
      AddObject(Arr.Items[Index], Format('%s[%d]', [Prefix, Index]));
  end;

  procedure AddDict(Dict: TJSONObject; Prefix: String);
  var
    Index: Integer;
  begin
    for Index := 0 to Dict.Count - 1 do
      AddObject(Dict.Pairs[Index].JsonValue, Prefix + Dict.Pairs[Index].JsonString.Value);
  end;

  procedure AddObject(Obj: TJSONValue; Name: String);
  begin
    if Obj is TJSONObject then
      AddDict(TJSONObject(Obj), Name + '.')
    else if Obj is TJSONArray then
      AddArray(TJSONArray(Obj), Name)
    else
      Result.Add(Name + '=' + Obj.Value);
  end;

begin
  Result := TStringList.Create;
  AddDict(Dict, '');
end;

{$IFDEF MACOS}

function NSDictionaryToStrings(Dict: NSDictionary): TStrings;

  procedure AddObject(Obj: Pointer; Name: String); forward;

  procedure AddArray(Arr: NSArray; Prefix: String);
  var
    Index: Integer;
  begin
    Result.Add(Format('%s#=%d', [Prefix, Arr.count]));
    for Index := 0 to Arr.count - 1 do
      AddObject(Arr.objectAtIndex(Index), Format('%s[%d]', [Prefix, Index]));
  end;

  procedure AddDict(Dict: NSDictionary; Prefix: String);
  var
    Keys: NSArray;
    Index: Integer;
    Obj: Pointer;
  begin
    Keys := Dict.allKeys;
    for Index := 0 to Keys.count - 1 do
    begin
      Obj := Keys.objectAtIndex(Index);
      AddObject(Dict.objectForKey(Obj), Prefix + FromNSStr(Obj));
    end;
  end;

  procedure AddObject(Obj: Pointer; Name: String);
  begin
    if TNSObject.Wrap(Obj).isKindOfClass(objc_getClass('NSDictionary')) then
      AddDict(TNSDictionary.Wrap(Obj), Name + '.')
    else if TNSObject.Wrap(Obj).isKindOfClass(objc_getClass('NSArray')) then
      AddArray(TNSArray.Wrap(Obj), Name)
    else // For some reason Delphi's NSObject does not expose @sel(description)
      Result.Add(Name + '=' + FromNSStr(objc_msgSend(Obj, sel_getUid('description'))));
  end;

begin
  Result := TStringList.Create;
  AddDict(Dict, '');
end;

class function TOCImportHelper.Defined: Boolean;
var
  M: TMarshaller;
begin
  Result := objc_getClass(M.AsAnsi(ClassName.SubString(1), CP_UTF8).ToPointer) <> nil;
end;

function FromNSSTR(Value: NSString): String;
begin
  if Value <> nil then
    Result := UTF8ToString(Value.UTF8String)
  else
    Result := '';
end;

function FromNSSTR(Value: Pointer): String;
begin
  if Value <> nil then
    Result := UTF8ToString(TNSString.Wrap(Value).UTF8String)
  else
    Result := '';
end;

function ToNSSTR(Str: String): NSString;
var
  M: TMarshaller;
begin
  Result := TNSString.Wrap(TNSString.OCClass.
    StringWithUTF8String(M.AsAnsi(Str, CP_UTF8).ToPointer));
end;

function PointerNSSTR(Str: String): Pointer;
var
  M: TMarshaller;
begin
  Result := TNSString.OCClass.StringWithUTF8String(M.AsAnsi(Str, CP_UTF8).ToPointer);
end;

function PointerNSObject(AObject: NSObject): Pointer;
begin
  Result := (AObject as ILocalObject).GetObjectID;
end;

{$ENDIF}

{$IFDEF IOS}

function SharedUIApplication: UIApplication;
begin
  Result := TUIApplication.wrap(TUIApplication.OCClass.SharedApplication);
end;

function GetRootViewController: UIViewController;
begin
  Result := SharedUIApplication.keyWindow.rootViewController;
end;

type
  ATTrackingManager = interface(NSObject)
  end;
  ATTrackingManagerClass = interface(NSObjectClass)
    procedure requestTrackingAuthorizationWithCompletionHandler(Handler: Pointer); cdecl;
  end;
  TATTrackingManager = class(TOCGenericImport<ATTrackingManagerClass, ATTrackingManager>)
  end;

procedure JVERequestTrackingAuthorization;
begin
  if TATTrackingManager.Defined then
    ExecuteInIdle(procedure
    begin
      if TOSVersion.Check(14) and (SharedUIApplication.applicationState = UIApplicationStateActive) then
        TATTrackingManager.OCClass.requestTrackingAuthorizationWithCompletionHandler(
          TObjCBlock.CreateBlockWithProcedure(procedure(status: NSInteger)
          begin
          end))
      else
        JVERequestTrackingAuthorization;
    end);
end;

{$ELSE}

procedure JVERequestTrackingAuthorization;
begin
end;

{$ENDIF}

{$IFDEF ANDROID}

type
  JActivityManagerClass = interface(JObjectClass)
  ['{C1AC95F1-BEB2-445B-8613-8FFE675AE79A}']
  end;

  [JavaSignature('android/app/ActivityManager')]
  JActivityManager = interface(JObject)
  ['{8592587D-1193-4B37-BB4B-738BF1848624}']
    function getRunningTasks(maxNum: Integer): JList;
  end;
  TJActivityManager = class(TJavaGenericImport<JActivityManagerClass, JActivityManager>) end;

  JActivityManager_RunningTaskInfoClass = interface(JObjectClass)
  ['{74F28ED6-F67E-45FE-815B-5DB2A3611C66}']
    function _GetCREATOR: JParcelable_ClassLoaderCreator;
    property CREATOR: JParcelable_ClassLoaderCreator read _GetCREATOR;
  end;

  [JavaSignature('android/app/ActivityManager$RunningTaskInfo')]
  JActivityManager_RunningTaskInfo = interface(JObject)
  ['{3721CED3-E592-426A-B8B9-284CF401E488}']
    function _GetbaseActivity: JComponentName;
    procedure _SetbaseActivity(Value: JComponentName);
    function _Getdescription: JCharSequence;
    procedure _Setdescription(Value: JCharSequence);
    function _Getid: Integer;
    procedure _Setid(Value: Integer);
    function _GetnumActivities: Integer;
    procedure _SetnumActivities(Value: Integer);
    function _GetnumRunning: Integer;
    procedure _SetnumRunning(Value: Integer);
    function _Getthumbnail: JBitmap;
    procedure _Setthumbnail(Value: JBitmap);
    function _GettopActivity: JComponentName;
    procedure _SettopActivity(Value: JComponentName);
      // The component launched as the first activity in the task.
    property baseActivity: JComponentName read _GetbaseActivity write _SetbaseActivity;
      // Description of the task's current state.
    property description: JCharSequence read _Getdescription write _Setdescription;
      // A unique identifier for this task.
    property id: Integer read _Getid write _Setid;
      // Number of activities in this task.
    property numActivities: Integer read _GetnumActivities write _SetnumActivities;
      // Number of activities that are currently running (not stopped and persisted) in this task.
    property numRunning: Integer read _GetnumRunning write _SetnumRunning;
      // Thumbnail representation of the task's current state.
    property thumbnail: JBitmap read _Getthumbnail write _Setthumbnail;
      // The activity component at the top of the history stack of the task.
    property topActivity: JComponentName read _GettopActivity write _SettopActivity;
  end;
  TJActivityManager_RunningTaskInfo = class(TJavaGenericImport<JActivityManager_RunningTaskInfoClass, JActivityManager_RunningTaskInfo>) end;

function IsRunningInForeground: Boolean;
begin
  try
    Result := TJActivityManager_RunningTaskInfo.Wrap((TJActivityManager.Wrap(
      (GetSharedActivityContext.getSystemService(TJContext.JavaClass.ACTIVITY_SERVICE) as
      ILocalObject).GetObjectID).getRunningTasks(1).get(0) as ILocalObject).GetObjectID).
      topActivity.getPackageName.equals(GetSharedActivityContext.getPackageName);
  except
    Result := False;
  end;
end;

function GetSharedActivityContext: JContext;
begin
  {$IF CompilerVersion >= 30}
  Result := TAndroidHelper.Context;
  {$ELSE}
  Result := SharedActivityContext;
  {$ENDIF}
end;

function GetSharedActivity: JActivity;
begin
  {$IF CompilerVersion >= 30}
  Result := TAndroidHelper.Activity;
  {$ELSE}
  Result := SharedActivity;
  {$ENDIF}
end;

{$ENDIF}

// This section is based, in part, on the Apple.AnonThread.pas, included as part of
// the \Samples\Delphi\RTL\CrossPlatform Utils\
// Replaced because:
// - "Cross Platform" should mean ALL platforms, not "something outside Windows".
// - No Synchronization support is given in the original.
// - RunningThreads is kept on Mac even though there is no reason for that.

type
  TAnonymousThread = class(TThread)
{$IF Defined(IOS) or Defined(ANDROID)}
  private
    class var RunningThreads: TList<TThread>;
    procedure ThreadTerminate(Sender: TObject);
  public
    class constructor Create;
    class destructor Destroy;
{$ENDIF}
  private
    FPlainProc: TProc;
    FSyncProc: TProc<TProc<TThreadProcedure>>;
  protected
    procedure Execute; override;
 end;

{$IF Defined(IOS) or Defined(ANDROID)}

class constructor TAnonymousThread.Create;
begin
  inherited;
  RunningThreads := TList<TThread>.Create;
end;

class destructor TAnonymousThread.Destroy;
begin
  FreeAndNil(RunningThreads);
  inherited;
end;

procedure TAnonymousThread.ThreadTerminate(Sender: TObject);
begin
  RunningThreads.Remove(Self);
end;

{$ENDIF}

procedure TAnonymousThread.Execute;
{$IFDEF MACOS}
var
  lPool: NSAutoreleasePool;
{$ENDIF}
begin
{$IFDEF MACOS}
  lPool := TNSAutoreleasePool.Create;
  try
{$ENDIF}
    if Assigned(FPlainProc) then
      FPlainProc
    else
      FSyncProc(procedure(Proc: TThreadProcedure) begin Synchronize(Proc); end);
{$IFDEF MACOS}
  finally
    lPool.drain;
  end;
{$ENDIF}
end;

procedure DetachThread(ThreadProc: TProc);
var
  Thread: TAnonymousThread;
begin
  Thread := TAnonymousThread.Create(True);
{$IF Defined(IOS) or Defined(ANDROID)}
  Thread.OnTerminate := Thread.ThreadTerminate;
  TAnonymousThread.RunningThreads.Add(Thread);
{$ENDIF}
  Thread.FPlainProc := ThreadProc;
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

procedure DetachThread(ThreadProc: TProc<TProc<TThreadProcedure>>);
var
  Thread: TAnonymousThread;
begin
  Thread := TAnonymousThread.Create(True);
{$IF Defined(IOS) or Defined(ANDROID)}
  Thread.OnTerminate := Thread.ThreadTerminate;
  TAnonymousThread.RunningThreads.Add(Thread);
{$ENDIF}
  Thread.FSyncProc := ThreadProc;
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

type
  TIdleExecution = class
    Procs: TQueue<TProc>;
    Timer: TTimer;
    procedure IdleTimer(Sender: TObject);
  end;

var
  IdleExecution: TIdleExecution;

procedure TIdleExecution.IdleTimer(Sender: TObject);
var
  OldQueue: TQueue<TProc>;
begin
  OldQueue := Procs;
  Procs := TQueue<TProc>.Create;
  while OldQueue.Count > 0 do
    OldQueue.Dequeue()();

  if Procs.Count = 0 then
    Timer.Enabled := False;
end;

procedure ExecuteInIdle(IdleProc: TProc);
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
  begin
    if IdleExecution = nil then
    begin
      IdleExecution := TIdleExecution.Create;
      IdleExecution.Procs := TQueue<TProc>.Create;
      IdleExecution.Timer := TTimer.Create(nil);
      IdleExecution.Timer.OnTimer := IdleExecution.IdleTimer;
      IdleExecution.Timer.Interval := 10;
    end;

    IdleExecution.Procs.Enqueue(IdleProc);
    IdleExecution.Timer.Enabled := True;
  end else
    TThread.Synchronize(nil, procedure
    begin
      ExecuteInIdle(IdleProc);
    end);
end;

procedure ShowDialog(const Msg: string;
  DlgType: TMsgDlgType = TMsgDlgType.mtCustom; const Title: string = '');
begin
  ShowDialog(Msg, DlgType, Title, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, nil);
end;

procedure ShowConfirmation(const Msg: string; Result: TProc<Boolean>);
begin
  ShowConfirmation(Msg, '', Result);
end;

procedure ShowConfirmation(const Msg: string; const Title: string;
  Result: TProc<Boolean>);
begin
  ShowDialog(Msg, TMsgDlgType.mtConfirmation, Title, [TMsgDlgBtn.mbOK,
    TMsgDlgBtn.mbCancel], TMsgDlgBtn.mbOK, procedure(Choice: TModalResult)
    begin
      if Assigned(Result) then
        Result(Choice = mrOk);
    end);
end;

procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; Result: TProc<TModalResult>);
begin
  ShowDialog(Msg, DlgType, '', Buttons, TMsgDlgBtn.mbOK, Result);
end;

procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; Result: TProc<TModalResult>);
begin
  ShowDialog(Msg, DlgType, '', Buttons, DefaultButton, Result);
end;

procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType;
  const Title: string; Buttons: TMsgDlgButtons; Result: TProc<TModalResult>);
begin
  ShowDialog(Msg, DlgType, Title, Buttons, TMsgDlgBtn.mbOK, Result);
end;

var
  MsgCaptions: array[TMsgDlgType] of string =
    (SMsgDlgWarning, SMsgDlgError, SMsgDlgInformation, SMsgDlgConfirm, '');
{$IF Defined(MSWINDOWS)}
  MsgIcons: array[TMsgDlgType] of UInt =
    (MB_ICONWARNING, MB_ICONERROR, MB_ICONINFORMATION, MB_ICONQUESTION, 0);
{$ELSEIF Defined(MACOS)}
  ModalResults: array[TMsgDlgBtn] of Integer = (mrYes, mrNo, mrOk, mrCancel,
    mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp, mrClose);
  ButtonCaptions: array[TMsgDlgBtn] of string = (SMsgDlgYes, SMsgDlgNo,
    SMsgDlgOK, SMsgDlgCancel, SMsgDlgAbort, SMsgDlgRetry, SMsgDlgIgnore,
    SMsgDlgAll, SMsgDlgNoToAll, SMsgDlgYesToAll, SMsgDlgHelp, SMsgDlgClose);
{$ENDIF}

{$IF Defined(IOS)}
type
  TJVEAlertViewDelegate = class(TOCLocal, UIAlertViewDelegate)
  private
    Alert: UIAlertView;
    Cancel: TModalResult;
    Results: TDictionary<Integer, TModalResult>;
    Callback: TProc<TModalResult>;
  public
    procedure didPresentAlertView(alertView: UIAlertView); cdecl;
    procedure alertView(alertView: UIAlertView; clickedButtonAtIndex: NSInteger); cdecl;
    procedure alertViewCancel(alertView: UIAlertView); cdecl;
    procedure alertViewDidDismissWithButtonIndex(alertView: UIAlertView;
      didDismissWithButtonIndex: NSInteger); cdecl;
  end;

// This is used to keep references and thus prevent the reference counter from
// releasing the objects and also to prevent double callback invocation
// in case of, for example, an alertViewCancel: being followed by the
// alertView:clickedButtonAtIndex: (which is possible according to Apple docs).
var
  ActiveAlerts: TList<TJVEAlertViewDelegate>;

procedure TJVEAlertViewDelegate.alertView(alertView: UIAlertView;
  clickedButtonAtIndex: NSInteger);
begin
  if ActiveAlerts.Contains(Self) then
  begin
    if Assigned(Callback) then
      if Results.ContainsKey(clickedButtonAtIndex) then
        Callback(Results[clickedButtonAtIndex])
      else
        Callback(Cancel);

    ActiveAlerts.Remove(Self)
  end;
end;

procedure TJVEAlertViewDelegate.alertViewCancel(alertView: UIAlertView);
begin
  if ActiveAlerts.Contains(Self) then
  begin
    if Assigned(Callback) then
      Callback(Cancel);
    ActiveAlerts.Remove(Self)
  end;
end;

procedure TJVEAlertViewDelegate.alertViewDidDismissWithButtonIndex(alertView: UIAlertView;
  didDismissWithButtonIndex: NSInteger);
begin
end;

procedure TJVEAlertViewDelegate.didPresentAlertView(alertView: UIAlertView); cdecl;
begin
end;

{$ELSEIF Defined(MACOS)}

// Based on the FMX.Platform.Mac.pas code, but the message loops, the modality
// and the delegate management are implemented differently.
type
  TAlertStack = record
    Result: TProc<TModalResult>;
    Results: array of Integer;
    Session: NSModalSession;
  end;

  AlertDelegate = interface(NSObject)
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

  TAlertDelegate = class(TOCLocal, NSAlertDelegate)
  public
    Stack: TStack<TAlertStack>;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

var
  GlobalAlertDelegate: TAlertDelegate;

function TAlertDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(AlertDelegate);
end;

procedure TAlertDelegate.alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer);
var
  Return: Integer;
  Callback: TAlertStack;
begin
  Callback := Stack.Pop;
  PlatformAlertReleased;
  if Callback.Session <> nil then
    TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).endModalSession(Callback.Session);

  if Assigned(Callback.Result) then
  begin
    Return := returnCode - NSAlertFirstButtonReturn;
    if (Return >= 0) and (Return < Length(Callback.Results)) then
      Callback.Result(Callback.Results[Return])
    else
      Callback.Result(mrCancel);
  end;
end;

{$ENDIF}

procedure ShowDialog(const Msg: string; DlgType: TMsgDlgType; Title: string;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; Result: TProc<TModalResult>);
{$IF Defined(IOS)}
var
  Button, CancelBtn: TMsgDlgBtn;
  CancelString: NSString;
  Delegate: TJVEAlertViewDelegate;
begin
  if ActiveAlerts = nil then
    ActiveAlerts := TList<TJVEAlertViewDelegate>.Create;

  Delegate := TJVEAlertViewDelegate.Create;
  Delegate.Callback := Result;
  ActiveAlerts.Add(Delegate);

  if Title = '' then
    Title := MsgCaptions[DlgType];

  if TMsgDlgBtn.mbCancel in Buttons then CancelBtn := TMsgDlgBtn.mbCancel else
  if TMsgDlgBtn.mbNo in Buttons then CancelBtn := TMsgDlgBtn.mbNo else
  if TMsgDlgBtn.mbNoToAll in Buttons then CancelBtn := TMsgDlgBtn.mbNoToAll else
  if TMsgDlgBtn.mbClose in Buttons then CancelBtn := TMsgDlgBtn.mbClose else
  if TMsgDlgBtn.mbOK in Buttons then CancelBtn := TMsgDlgBtn.mbOK else
    CancelBtn := TMsgDlgBtn(-1);

  CancelString := nil;
  if CancelBtn <> TMsgDlgBtn(-1) then
  begin
    Delegate.Cancel := ModalResults[CancelBtn];
    CancelString := ToNSSTR(ButtonCaptions[CancelBtn]);
  end else
    Delegate.Cancel := mrCancel;

  Delegate.Results := TDictionary<Integer, TModalResult>.Create;
  Delegate.Alert := TUIAlertView.Wrap(TUIAlertView.Alloc.initWithTitle(ToNSSTR(Title),
    ToNSSTR(Msg), ILocalObject(Delegate).GetObjectID, CancelString, nil));

  for Button := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if (Button in Buttons) and (Button <> CancelBtn) then
      Delegate.Results.Add(Delegate.Alert.addButtonWithTitle(
        ToNSSTR(ButtonCaptions[Button])), ModalResults[Button]);
  Delegate.Alert.show;
{$ELSEIF Defined(MACOS)}
begin
  ExecuteInIdle(procedure
  var
    Alert: NSAlert;
    Button: TMsgDlgBtn;
    ActiveForm: TCommonCustomForm;
    NSWin: NSWindow;
    Choice: TModalResult;
    Stack: TAlertStack;

    procedure AddButton(Button: TMsgDlgBtn);
    begin
      SetLength(Stack.Results, Length(Stack.Results) + 1);
      Stack.Results[High(Stack.Results)] := ModalResults[Button];
      Alert.addButtonWithTitle(ToNSSTR(ButtonCaptions[Button]));
    end;

  begin
    ActiveForm := nil;
    if GlobalAlertDelegate = nil then
    begin
      GlobalAlertDelegate := TAlertDelegate.Create;
      GlobalAlertDelegate.Stack := TStack<TAlertStack>.Create;
    end;

    if Screen <> nil then
    begin
      Screen.PrepareClosePopups(nil);
      Screen.ClosePopupForms;
      ActiveForm := Screen.ActiveForm;
      if (ActiveForm <> nil) and (ActiveForm.Visible) and (ActiveForm.Handle <> nil) and
        not (ActiveForm.Owner is TPopup) then
      begin
        NSWin := WindowHandleToPlatform(ActiveForm.Handle).Wnd;
        if NSWin <> nil then
          NSWin.retain;
      end;
    end;

    Alert := TNSAlert.Create;
    Alert.setInformativeText(ToNSSTR(Msg));
    if (Title = '') and (DlgType = TMsgDlgType.mtCustom) then
      Alert.setMessageText(ToNSSTR(Application.Title))
    else if Title = '' then
      Alert.setMessageText(ToNSSTR(MsgCaptions[DlgType]))
    else
      Alert.setMessageText(ToNSSTR(Title));

    if DlgType = TMsgDlgType.mtWarning then
      Alert.setAlertStyle(NSWarningAlertStyle)
    else if DlgType = TMsgDlgType.mtError then
      Alert.setAlertStyle(NSCriticalAlertStyle)
    else
      Alert.setAlertStyle(NSInformationalAlertStyle);

    SetLength(Stack.Results, 0);
    if DefaultButton in Buttons then
      AddButton(DefaultButton);
    for Button := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if (Button in Buttons) and (Button <> DefaultButton) then
        AddButton(Button);
    if Length(Stack.Results) = 0 then
      AddButton(TMsgDlgBtn.mbOK);

    if NSWin <> nil then
    begin
      Alert.beginSheetModalForWindow(NSWin, GlobalAlertDelegate.GetObjectID,
        sel_getUid('alertDidEndSelector:returnCode:contextInfo:'), nil);
      if TFmxFormState.Modal in ActiveForm.FormState then
        Stack.Session := nil
      else
        Stack.Session := TNSApplication.Wrap(
          TNSApplication.OCClass.sharedApplication).beginModalSessionForWindow(NSWin);

      PlatformAlertCreated;
      Stack.Result := Result;
      GlobalAlertDelegate.Stack.Push(Stack);
    end else
    begin
      Choice := Alert.runModal - NSAlertFirstButtonReturn;
      if Assigned(Result) then
      begin
        if (Choice >= 0) and (Choice < Length(Stack.Results)) then
          Result(Stack.Results[Choice])
        else
          Result(mrCancel);
      end;
    end;
  end);
{$ELSEIF Defined(ANDROID)}
  {$IF CompilerVersion >= 31}
begin
  ExecuteInIdle(procedure
  begin
    TDialogService.MessageDialog(Msg, DlgType, Buttons, DefaultButton, 0,
      procedure(const Choice: TModalResult)
      begin
        if Assigned(Result) then
          Result(Choice);
      end);
  end);
  {$ELSE}
var
  DialogService: IFMXDialogService;
begin
  ExecuteInIdle(procedure
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXDialogService,
      IInterface(DialogService)) then
        DialogService.MessageDialog(Msg, DlgType, Buttons, DefaultButton,
          0, 0, 0, '', procedure(const Choice: TModalResult)
          begin
            if Assigned(Result) then
              Result(Choice);
          end);
  end);
  {$ENDIF}
{$ELSEIF Defined(MSWINDOWS)}
var
  Flags: UInt;
begin
  Flags := MsgIcons[DlgType] or MB_TASKMODAL;
  if Buttons >= [TMsgDlgBtn.mbAbort, TMsgDlgBtn.mbRetry, TMsgDlgBtn.mbIgnore] then
  begin
    Flags := Flags or MB_ABORTRETRYIGNORE;
    if DefaultButton = TMsgDlgBtn.mbRetry then
      Flags := Flags or MB_DEFBUTTON2
    else if DefaultButton = TMsgDlgBtn.mbIgnore then
      Flags := Flags or MB_DEFBUTTON3;
  end else
  if Buttons >= [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo, TMsgDlgBtn.mbCancel] then
  begin
    Flags := Flags or MB_YESNOCANCEL;
    if DefaultButton = TMsgDlgBtn.mbNo then
      Flags := Flags or MB_DEFBUTTON2
    else if DefaultButton = TMsgDlgBtn.mbCancel then
      Flags := Flags or MB_DEFBUTTON3;
  end else
  if Buttons >= [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo] then
  begin
    Flags := Flags or MB_YESNO;
    if DefaultButton = TMsgDlgBtn.mbNo then
      Flags := Flags or MB_DEFBUTTON2;
  end else
  if Buttons >= [TMsgDlgBtn.mbRetry, TMsgDlgBtn.mbCancel] then
  begin
    Flags := Flags or MB_RETRYCANCEL;
    if DefaultButton = TMsgDlgBtn.mbCancel then
      Flags := Flags or MB_DEFBUTTON2;
  end else
  if Buttons >= [TMsgDlgBtn.mbOk, TMsgDlgBtn.mbCancel] then
  begin
    Flags := Flags or MB_OKCANCEL;
    if DefaultButton = TMsgDlgBtn.mbCancel then
      Flags := Flags or MB_DEFBUTTON2;
  end else
  if TMsgDlgBtn.mbHelp in Buttons then
    Flags := Flags or MB_HELP;

  if Title = '' then
    Title := MsgCaptions[DlgType];

  if Title = '' then
    Title := Application.Title;

  ExecuteInIdle(procedure
    var
      Choice: TModalResult;
    begin
      Choice := MessageBox(0, PChar(Msg), PChar(Title), Flags);
      if Assigned(Result) then
        Result(Choice);
    end);
{$ENDIF}
end;

end.


