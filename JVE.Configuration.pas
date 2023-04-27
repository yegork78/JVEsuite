(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Configuration;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, System.Math,
  JVE.Utils;

type
  TJVEConfiguration = class;

  TJVEConfigurationValue = class(TComponent)
  private
    FParent: TJVEConfiguration;
    FIdentifier: String;
  public
    destructor Destroy; override;
    procedure SetParentComponent(Value: TComponent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  published
    // This is the identifier used by the underlying OS to store the value.
    property Identifier: String read FIdentifier write FIdentifier;
  end;

  TJVEConfigurationInteger = class(TJVEConfigurationValue)
  private
    FDefault: Integer;
    procedure SetValue(const Value: Integer); inline;
  protected
    function GetValue: Integer; virtual;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: Integer read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: Integer read FDefault write FDefault default 0;
  end;

  TJVEConfigurationIntegerRange = class(TJVEConfigurationInteger)
  private
    FMaximum: Integer;
    FMinimum: Integer;
  protected
    function GetValue: Integer; override;
  published
    // The valid range for the value.
    property Minimum: Integer read FMinimum write FMinimum default 0;
    property Maximum: Integer read FMaximum write FMaximum default 0;
  end;

  TJVEConfigurationFloat = class(TJVEConfigurationValue)
  private
    FDefault: Double;
    procedure SetValue(const Value: Double); inline;
  protected
    function GetValue: Double; virtual;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: Double read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: Double read FDefault write FDefault;
  end;

  TJVEConfigurationFloatRange = class(TJVEConfigurationFloat)
  private
    FMaximum: Double;
    FMinimum: Double;
  protected
    function GetValue: Double; override;
  published
    // The valid range for the value.
    property Minimum: Double read FMinimum write FMinimum;
    property Maximum: Double read FMaximum write FMaximum;
  end;

  TJVEConfigurationBoolean = class(TJVEConfigurationValue)
  private
    FDefault: Boolean;
    function GetValue: Boolean; inline;
    procedure SetValue(const Value: Boolean); inline;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: Boolean read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: Boolean read FDefault write FDefault default False;
  end;

  TJVEConfigurationDate = class(TJVEConfigurationValue)
  private
    FDefault: TDate;
    function GetValue: TDate; inline;
    procedure SetValue(const Value: TDate); inline;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: TDate read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: TDate read FDefault write FDefault;
  end;

  TJVEConfigurationTime = class(TJVEConfigurationValue)
  private
    FDefault: TTime;
    function GetValue: TTime; inline;
    procedure SetValue(const Value: TTime); inline;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: TTime read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: TTime read FDefault write FDefault;
  end;

  TJVEConfigurationDateTime = class(TJVEConfigurationValue)
  private
    FDefault: TDateTime;
    function GetValue: TDateTime; inline;
    procedure SetValue(const Value: TDateTime); inline;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: TDateTime read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: TDateTime read FDefault write FDefault;
  end;

  TJVEConfigurationString = class(TJVEConfigurationValue)
  private
    FDefault: String;
    function GetValue: String; inline;
    procedure SetValue(const Value: String); inline;
  public
    // Resets the value back to the default;
    procedure Reset; inline;
    // Provides access to the property value, as currently recorded;
    // automatically returns the default value, if another value is not set.
    property Value: String read GetValue write SetValue;
  published
    // The default value for the property (returned if the value is not set).
    property Default: String read FDefault write FDefault;
  end;

  // This class provides a user-based persistent data storage.
  // On Windows this is based on the Registry; on Mac and iOS - NSUserDefaults;
  // on Android - SharedPreferences.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEConfiguration = class(TComponent)
  private
    FValues: TList<TJVEConfigurationValue>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    // These functions provide direct access to the saved values.
    // These could be used directly to avoid components overhead.
    class procedure WriteBool(const Ident: String; Value: Boolean);
    class procedure WriteString(const Ident, Value: String);
    class procedure WriteInteger(const Ident: String; Value: Integer);
    class procedure WriteDate(const Ident: String; Value: TDateTime);
    class procedure WriteDateTime(const Ident: String; Value: TDateTime);
    class procedure WriteFloat(const Ident: String; Value: Double);
    class procedure WriteTime(const Ident: String; Value: TDateTime);

    class function ReadBool(const Ident: String; Default: Boolean): Boolean;
    class function ReadString(const Ident, Default: String): String;
    class function ReadInteger(const Ident: String; Default: Integer): Integer;
    class function ReadDate(const Ident: String; Default: TDateTime): TDateTime;
    class function ReadDateTime(const Ident: String; Default: TDateTime): TDateTime;
    class function ReadFloat(const Ident: String; Default: Double): Double;
    class function ReadTime(const Ident: String; Default: TDateTime): TDateTime;

    // I have seen this specific function fail on Mac. Suggest not using it,
    // rather falling back to overriding old value with default.
    class procedure DeleteKey(const Ident: String);
  end;

implementation

{$IF Defined(MACOS)}
uses System.DateUtils {$IFDEF IOS}, iOSapi.Foundation,
  iosAPI.CocoaTypes {$ELSE}, MacApi.Foundation {$ENDIF};
{$ELSEIF Defined(ANDROID)}
uses DateUtils, AndroidApi.Jni.JavaTypes, AndroidApi.Jni.App,
  AndroidApi.Jni.GraphicsContentViewText, FMX.Helpers.Android
  {$IF CompilerVersion >= 27}, AndroidApi.Helpers{$ENDIF};
{$ELSE}
uses System.DateUtils, System.Win.Registry;
{$IFEND}

{ TJVEConfigurationValue }

destructor TJVEConfigurationValue.Destroy;
begin
  SetParentComponent(nil);
  inherited Destroy;
end;

function TJVEConfigurationValue.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

function TJVEConfigurationValue.HasParent: Boolean;
begin
  Result := Assigned(FParent);
end;

procedure TJVEConfigurationValue.SetParentComponent(Value: TComponent);
begin
  if FParent <> Value then
  begin
    if Assigned(FParent) then
      FParent.FValues.Remove(Self);
    FParent := TJVEConfiguration(Value);
    if Assigned(FParent) then
      FParent.FValues.Add(Self);
  end;
  SetSubComponent(Assigned(FParent));
end;

{ TJVEConfigurationInteger }

function TJVEConfigurationInteger.GetValue: Integer;
begin
  Result := TJVEConfiguration.ReadInteger(Identifier, Default);
end;

procedure TJVEConfigurationInteger.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationInteger.SetValue(const Value: Integer);
begin
  TJVEConfiguration.WriteInteger(Identifier, Value);
end;

{ TJVEConfigurationIntegerRange }

function TJVEConfigurationIntegerRange.GetValue: Integer;
begin
  Result := EnsureRange(inherited GetValue, FMinimum, FMaximum);
end;

{ TJVEConfigurationFloat }

function TJVEConfigurationFloat.GetValue: Double;
begin
  Result := TJVEConfiguration.ReadFloat(Identifier, Default);
end;

procedure TJVEConfigurationFloat.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationFloat.SetValue(const Value: Double);
begin
  TJVEConfiguration.WriteFloat(Identifier, Value);
end;

{ TJVEConfigurationFloatRange }

function TJVEConfigurationFloatRange.GetValue: Double;
begin
  Result := EnsureRange(inherited GetValue, FMinimum, FMaximum);
end;

{ TJVEConfigurationBoolean }

function TJVEConfigurationBoolean.GetValue: Boolean;
begin
  Result := TJVEConfiguration.ReadBool(Identifier, Default);
end;

procedure TJVEConfigurationBoolean.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationBoolean.SetValue(const Value: Boolean);
begin
  TJVEConfiguration.WriteBool(Identifier, Value);
end;

{ TJVEConfigurationDate }

function TJVEConfigurationDate.GetValue: TDate;
begin
  Result := TJVEConfiguration.ReadDate(Identifier, Default);
end;

procedure TJVEConfigurationDate.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationDate.SetValue(const Value: TDate);
begin
  TJVEConfiguration.WriteDate(Identifier, Value);
end;

{ TJVEConfigurationTime }

function TJVEConfigurationTime.GetValue: TTime;
begin
  Result := TJVEConfiguration.ReadTime(Identifier, Default);
end;

procedure TJVEConfigurationTime.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationTime.SetValue(const Value: TTime);
begin
  TJVEConfiguration.WriteTime(Identifier, Value);
end;

{ TJVEConfigurationDateTime }

function TJVEConfigurationDateTime.GetValue: TDateTime;
begin
  Result := TJVEConfiguration.ReadDateTime(Identifier, Default);
end;

procedure TJVEConfigurationDateTime.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationDateTime.SetValue(const Value: TDateTime);
begin
  TJVEConfiguration.WriteDateTime(Identifier, Value);
end;

{ TJVEConfigurationString }

function TJVEConfigurationString.GetValue: String;
begin
  Result := TJVEConfiguration.ReadString(Identifier, Default);
end;

procedure TJVEConfigurationString.Reset;
begin
  Value := Default;
end;

procedure TJVEConfigurationString.SetValue(const Value: String);
begin
  TJVEConfiguration.WriteString(Identifier, Value);
end;

{ TJVEConfiguration }

constructor TJVEConfiguration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FValues := TList<TJVEConfigurationValue>.Create;
end;

destructor TJVEConfiguration.Destroy;
var
  Value: TJVEConfigurationValue;
begin
  while FValues.Count <> 0 do
  begin
    Value := FValues[0];
    Value.SetParentComponent(nil);
    Value.Free;
  end;

  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TJVEConfiguration.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  Value: TJVEConfigurationValue;
begin
  for Value in FValues do
    Proc(Value);
end;

{$IF Defined(MACOS)}

// This section is based, in part, on the Apple.Inifiles.pas, included as part of
// the \Samples\Delphi\RTL\CrossPlatform Utils\
// Replaced because:
// - IniFile orientation is actually bad for all platforms (none support sections).
// - "Cross Platform" should mean ALL platforms, not "something outside Windows".

var
  FUserDefaults: NSUserDefaults;

function UserDefaults: NSUserDefaults;
begin
  if FUserDefaults = nil then
    FUserDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  Result := FUserDefaults;
end;

function ReadPointer(const Ident: String): Pointer;
begin
  Result := UserDefaults.objectForKey(ToNSSTR(Ident));
end;

procedure WritePointer(const Ident: String; Value: Pointer);
begin
  UserDefaults.setObject(Value, ToNSSTR(Ident));
  UserDefaults.synchronize;
end;

{ TJVEConfiguration }

class procedure TJVEConfiguration.DeleteKey(const Ident: String);
begin
  UserDefaults.removeObjectForKey(ToNSStr(Ident));
end;

class function TJVEConfiguration.ReadBool(const Ident: String; Default: Boolean): Boolean;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Ident);
  if Assigned(lPtr) then
    Result := TNSNumber.Wrap(lPtr).boolValue
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadDate(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := DateOf(ReadDateTime(Ident, Default));
end;

class function TJVEConfiguration.ReadDateTime(const Ident: String;
  Default: TDateTime): TDateTime;
var
  lPtr: Pointer;
  lCalendar: NSCalendar;
  lComps: NSDateComponents;
  lUnits: Cardinal;
begin
  lPtr := ReadPointer(Ident);
  if Assigned(lPtr) then
  begin
    lCalendar := TNSCalendar.Wrap(TNSCalendar.OCClass.currentCalendar);
    lUnits := NSYearCalendarUnit or NSMonthCalendarUnit or NSDayCalendarUnit or
      NSHourCalendarUnit or NSMinuteCalendarUnit or NSSecondCalendarUnit;
    lComps := lCalendar.components(lUnits, TNSDate.Wrap(lPtr));

    Result := EncodeDate(lComps.year, lComps.month, lComps.day) +
      EncodeTime(lComps.hour, lComps.minute, lComps.second, 0);
  end else
    Result := Default;
end;

class function TJVEConfiguration.ReadFloat(const Ident: String; Default: Double): Double;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Ident);
  if Assigned(lPtr) then
    Result := TNSNumber.Wrap(lPtr).doubleValue
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadInteger(const Ident: String;
  Default: Integer): Integer;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Ident);
  if Assigned(lPtr) then
    Result := TNSNumber.Wrap(lPtr).intValue
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadString(const Ident, Default: String): String;
var
  lPtr: Pointer;
begin
  lPtr := ReadPointer(Ident);
  if Assigned(lPtr) then
    Result := FromNSSTR(lPtr)
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadTime(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := TimeOf(ReadDateTime(Ident, Default));
end;

class procedure TJVEConfiguration.WriteBool(const Ident: String; Value: Boolean);
begin
  WritePointer(Ident, TNSNumber.OCClass.numberWithBool(Value));
end;

class procedure TJVEConfiguration.WriteDate(const Ident: String; Value: TDateTime);
begin
  WriteDateTime(Ident, DateOf(Value));
end;

class procedure TJVEConfiguration.WriteDateTime(const Ident: String; Value: TDateTime);
var
  Day, Month, Year, Hour, Min, Sec, MSec: Word;
  Formatter: NSDateFormatter;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Min, Sec, MSec);
  Formatter := TNSDateFormatter.Create;
  try
    Formatter.setDateFormat(ToNSStr('YYYY-MM-dd HH:mm:ss'));
    WritePointer(Ident, PointerNSObject(formatter.dateFromString(ToNSStr(Format(
      '%.4d-%.2d-%.2d %.2d:%.2d:%.2d', [Year, Month, Day, Hour, Min, Sec, 0])))));
  finally
    Formatter.release;
  end;
end;

class procedure TJVEConfiguration.WriteFloat(const Ident: String; Value: Double);
begin
  WritePointer(Ident, TNSNumber.OCClass.numberWithDouble(Value));
end;

class procedure TJVEConfiguration.WriteInteger(const Ident: String; Value: Integer);
begin
  WritePointer(Ident, TNSNumber.OCClass.numberWithInt(Value));
end;

class procedure TJVEConfiguration.WriteString(const Ident, Value: String);
begin
  WritePointer(Ident, PointerNSSTR(Value));
end;

class procedure TJVEConfiguration.WriteTime(const Ident: String; Value: TDateTime);
begin
  //Need to ensure we're writing a TDateTime value that will correspond to
  //>= 1/1/1900, otherwise we get garbage back when reading the time
  WriteDateTime(Ident, 2 + TimeOf(Value));
end;

initialization
finalization
  FUserDefaults := nil;

{$ELSEIF Defined(ANDROID)}

var
  FPreferences: JSharedPreferences;

function Preferences: JSharedPreferences;
begin
  if FPreferences = nil then
    FPreferences := GetSharedActivityContext.getSharedPreferences(
      GetSharedActivity.getLocalClassName, TJContext.JavaClass.MODE_PRIVATE);

  Result := FPreferences;
end;

{ TJVEConfiguration }

class procedure TJVEConfiguration.DeleteKey(const Ident: String);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  Editor.remove(StringToJString(Ident));
  Editor.apply;  // apply() is asynchronous unlike commit()
end;

class procedure TJVEConfiguration.WriteBool(const Ident: String; Value: Boolean);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  Editor.putBoolean(StringToJString(Ident), Value);
  Editor.apply;
end;

class procedure TJVEConfiguration.WriteString(const Ident, Value: String);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  Editor.putString(StringToJString(Ident), StringToJString(Value));
  Editor.apply;
end;

class procedure TJVEConfiguration.WriteInteger(const Ident: String;
  Value: Integer);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  Editor.putInt(StringToJString(Ident), Value);
  Editor.apply;
end;

class procedure TJVEConfiguration.WriteDate(const Ident: String;
  Value: TDateTime);
begin
  WriteDateTime(Ident, DateOf(Value));
end;

class procedure TJVEConfiguration.WriteDateTime(const Ident: String;
  Value: TDateTime);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  // The number of milliseconds since Jan. 1, 1970 GMT, as Int64
  Editor.putLong(StringToJString(Ident), DateTimeToUnix(Value));
  Editor.apply;
end;

class procedure TJVEConfiguration.WriteFloat(const Ident: String; Value: Double);
var
  Editor: JSharedPreferences_Editor;
begin
  Editor := Preferences.edit;
  Editor.putFloat(StringToJString(Ident), Value);
  Editor.apply;
end;

class procedure TJVEConfiguration.WriteTime(const Ident: String;
  Value: TDateTime);
begin
  WriteDateTime(Ident, TimeOf(Value));
end;

class function TJVEConfiguration.ReadBool(const Ident: String;
  Default: Boolean): Boolean;
begin
  Result := Preferences.getBoolean(StringToJString(Ident), Default);
end;

class function TJVEConfiguration.ReadString(const Ident, Default: String): String;
begin
  Result := JStringToString(
    Preferences.getString(StringToJString(Ident), StringToJString(Default)));
end;

class function TJVEConfiguration.ReadInteger(const Ident: String;
  Default: Integer): Integer;
begin
  Result := Preferences.getInt(StringToJString(Ident), Default);
end;

class function TJVEConfiguration.ReadDate(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := DateOf(ReadDateTime(Ident, Default));
end;

class function TJVEConfiguration.ReadDateTime(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  // The number of milliseconds since Jan. 1, 1970 GMT
  Result := UnixToDateTime(
    Preferences.getLong(StringToJString(Ident), DateTimeToUnix(Default)));
end;

class function TJVEConfiguration.ReadFloat(const Ident: String;
  Default: Double): Double;
begin
  Result := Preferences.getFloat(StringToJString(Ident), Default);
end;

class function TJVEConfiguration.ReadTime(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := TimeOf(ReadDateTime(Ident, Default));
end;

initialization
finalization
  FPreferences := nil;

{$ELSE}

var
  FRegistry: TRegistry;

function Registry: TRegistry;
var
  App: String;
begin
  if FRegistry = nil then
  begin
    App := ExtractFileName(ParamStr(0));
    if App.Contains('.') then
      App := App.Split(['.'])[0];

    FRegistry := TRegistry.Create;
    FRegistry.OpenKey('Software\' + App, True);
  end;
  Result := FRegistry;
end;

{ TJVEConfiguration }

class procedure TJVEConfiguration.DeleteKey(const Ident: String);
begin
  Registry.DeleteValue(Ident);
end;

class function TJVEConfiguration.ReadBool(const Ident: String; Default: Boolean): Boolean;
begin
  if Registry.GetDataType(Ident) = rdInteger then
    Result := Registry.ReadBool(Ident)
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadDate(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := DateOf(ReadDateTime(Ident, Default));
end;

class function TJVEConfiguration.ReadDateTime(const Ident: String;
  Default: TDateTime): TDateTime;
var
  Info: TRegDataInfo;
begin
  if Registry.GetDataInfo(Ident, Info) and (Info.RegData = rdBinary) and
    (Info.DataSize = SizeOf(TDateTime)) then
      Result := Registry.ReadDateTime(Ident)
    else
      Result := Default;
end;

class function TJVEConfiguration.ReadFloat(const Ident: String; Default: Double): Double;
var
  Info: TRegDataInfo;
begin
  if Registry.GetDataInfo(Ident, Info) and (Info.RegData = rdBinary) and
    (Info.DataSize = SizeOf(Double)) then
      Result := Registry.ReadFloat(Ident)
    else
      Result := Default;
end;

class function TJVEConfiguration.ReadInteger(const Ident: String;
  Default: Integer): Integer;
begin
  if Registry.GetDataType(Ident) = rdInteger then
    Result := Registry.ReadInteger(Ident)
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadString(const Ident, Default: String): String;
begin
  if Registry.GetDataType(Ident) in [rdString, rdExpandString] then
    Result := Registry.ReadString(Ident)
  else
    Result := Default;
end;

class function TJVEConfiguration.ReadTime(const Ident: String;
  Default: TDateTime): TDateTime;
begin
  Result := TimeOf(ReadDateTime(Ident, Default));
end;

class procedure TJVEConfiguration.WriteBool(const Ident: String; Value: Boolean);
begin
  Registry.WriteBool(Ident, Value);
end;

class procedure TJVEConfiguration.WriteDate(const Ident: String; Value: TDateTime);
begin
  Registry.WriteDate(Ident, Value);
end;

class procedure TJVEConfiguration.WriteDateTime(const Ident: String; Value: TDateTime);
begin
  Registry.WriteDateTime(Ident, Value);
end;

class procedure TJVEConfiguration.WriteFloat(const Ident: String; Value: Double);
begin
  Registry.WriteFloat(Ident, Value);
end;

class procedure TJVEConfiguration.WriteInteger(const Ident: String; Value: Integer);
begin
  Registry.WriteInteger(Ident, Value);
end;

class procedure TJVEConfiguration.WriteString(const Ident, Value: String);
begin
  Registry.WriteString(Ident, Value);
end;

class procedure TJVEConfiguration.WriteTime(const Ident: String; Value: TDateTime);
begin
  Registry.WriteTime(Ident, Value);
end;

initialization
finalization
  FreeAndNil(FRegistry);

{$IFEND}

end.

