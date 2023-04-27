(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Register;

interface

// To use this library on iOS, there are some frameworks, which Delphi should
// be aware of, but by default doesn't, so you need to perform the following changes
// in the IDE (you need to do this just once):
//
// 1. Go to Tools -> Options -> SDK Manager -> iOS Device, for each iPhoneOS device:
// 2. Click on any framework (just a click: there is a bug in IDE).
// 3. Click "Add a new path item" image button (top one)
//    a. Set "Path on remote machine" to "/System/Library/Frameworks"
//    b. Set "Framework name" to "System Configuration"
//    c. Click Ok (nothing should be selected in the radio and the checkbox unchecked).
// 4. Repeat step 3 with Framework name "AdSupport", "StoreKit", "Social", "CoreData",
//    "CoreTelephony", "CoreMedia", "SystemConfiguration", “Security”, “EventKit”,
//    “EventKitUI”, “AVFoundation”, "iAd", "AudioToolbox" and "MessageUI"
//    (no need to duplicate already existing items: Delphi XE5, for example,
//    already includes StoreKit and iAd).
// 5. Click “Update Local File Cache” button.
//
// In each project, you are using this library, you need to perform the following
// changes:
//
// 1. Go to Project -> Options -> Delphi Compiler -> Linking.
// 2. Choose Target "All Configurations - iOS Device platform".
// 3. Enter the following value in "Options passed to the LD linked":
//	-lz -framework SystemConfiguration -weak_framework Social
//	-weak_framework iAd -framework StoreKit -framework MessageUI
//	-framework AudioToolbox -weak_framework AdSupport
//	-framework Security -framework CoreData -framework CoreTelephony
//	-framework AVFoundation -framework EventKit
//	-framework EventKitUI -framework CoreMedia
// This should be written as a single line.
//
// It would be better to use SDK version at least 6.0, as otherwise you might
// not have Social and AdSupport frameworks (notice, they are weak-linked,
// so the app will work in iOS 5 even if you use SDK 6.0).

{$IF not Defined(CPUX86) or not Defined(MSWINDOWS)}
{$MESSAGE Fatal 'This is a Design Time unit; it should only be compiled for Win32.'}
{$IFEND}

{$DEFINE PACKAGE_ANALYTICS}
{$DEFINE PACKAGE_ADS}
{$DEFINE PACKAGE_NATIVE}

{$IF Defined(PACKAGE_ANALYTICS) and Defined(PACKAGE_ADS) and Defined(PACKAGE_NATIVE)}
{$DEFINE PACKAGE_FULL}
{$UNDEF PACKAGE_ANALYTICS}
{$UNDEF PACKAGE_ADS}
{$UNDEF PACKAGE_NATIVE}
{$ENDIF}

procedure Register;

implementation

uses System.Classes, System.RTLConsts, DesignEditors, DesignIntf, System.TypInfo,
  FMX.Types, FMX.Dialogs, FMX.Styles, System.SysUtils, System.UITypes,
  System.Math, System.Character,
{$IF Defined(PACKAGE_FULL)}
  JVE.Email, JVE.Layout, JVE.Social, JVE.OpenURL, JVE.Browser, JVE.Payment,
  JVE.AppStore, JVE.Analytics, JVE.Analytics.VCL, JVE.ChartBoost, JVE.Facebook,
  JVE.Interstitials, JVE.Banners, JVE.AdMob, JVE.Pushes, JVE.PushWoosh, JVE.Actions,
  JVE.Native, JVE.PubNative, JVE.MobFox, JVE.Shade, JVE.Configuration, JVE.RegConfigEdit;
{$ELSEIF Defined(PACKAGE_ANALYTICS)}
  JVE.OpenURL, JVE.Analytics, JVE.Analytics.VCL, JVE.Configuration, JVE.RegConfigEdit;
{$ELSEIF Defined(PACKAGE_ADS)}
  JVE.OpenURL, JVE.ChartBoost, JVE.Interstitials, JVE.Banners, JVE.AdMob,
  JVE.Actions, JVE.Native, JVE.PubNative, JVE.MobFox, JVE.Layout, JVE.Facebook;
{$ELSEIF Defined(PACKAGE_NATIVE)}
  JVE.Email, JVE.Social, JVE.OpenURL, JVE.Browser, JVE.AppStore,
  JVE.Shade, JVE.Configuration, JVE.RegConfigEdit, JVE.Layout;
{$ENDIF}

{$IF Defined(PACKAGE_FULL)}
type
  TJVEPaymentItemFilter = class(TSelectionEditor, ISelectionpropertyFilter)
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections;
      const ASelectionproperties: IInterfaceList);
  end;

{ TJVEPaymentItemFilter }

procedure TJVEPaymentItemFilter.FilterProperties(const ASelection:
  IDesignerSelections; const ASelectionproperties: IInterfaceList);
var
  Item, Prop: Integer;
  SelectedItem: TPersistent;
  Action: TJVEAction absolute SelectedItem;
begin
  for Item := 0 to ASelection.Count - 1 do
  begin
    SelectedItem := ASelection[Item];
    if SelectedItem is TJVEPaymentItem then
      for Prop := ASelectionProperties.Count - 1 downto 0 do
        if (ASelectionProperties[Prop] as IProperty).GetName = 'Identifier' then
          ASelectionProperties.Delete(Prop);
  end;
end;

type
  TJVEIdentifierProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TJVEIdentifierProperty }

function TJVEIdentifierProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TJVEIdentifierProperty.GetValues(Proc: TGetStrProc);
var
  Payment: TJVEPayment;
  Col: TCollectionItem;
  Item: TJVEPaymentItem absolute Col;
  Acc: String;
begin
  if (PropCount = 0) or not (GetComponent(0) is TJVEAction) or
    not (TJVEAction(GetComponent(0)).Action is TJVEPayment) then
    Exit;

  Payment := TJVEPayment(TJVEAction(GetComponent(0)).Action);
  for Col in Payment.Items do
  begin
    Acc := '';
    if Item.iOSIdentifier <> '' then
      Acc := Item.iOSIdentifier + ',';
    if Item.MacIdentifier <> '' then
      Acc := Item.MacIdentifier + ',';
    if Item.AndroidIdentifier <> '' then
      Acc := Item.AndroidIdentifier + ',';
    if Item.PPIdentifier <> '' then
      Acc := Item.PPIdentifier + ',';

    if Acc <> '' then
      Proc(Copy(Acc, 1, Length(Acc) - 1));
  end;
end;
{$ENDIF}

{$IF Defined(PACKAGE_FULL) or Defined(PACKAGE_ADS)}
type
  TJVEActionFilter = class(TSelectionEditor, ISelectionpropertyFilter)
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections;
      const ASelectionproperties: IInterfaceList);
  end;

{ TJVEActionFilter }

procedure TJVEActionFilter.FilterProperties(const ASelection:
  IDesignerSelections; const ASelectionproperties: IInterfaceList);
var
  Name: String;
  Item, Prop: Integer;
  SelectedItem: TPersistent;
  Action: TJVEAction absolute SelectedItem;
  Intf: IInterface;
begin
  for Item := 0 to ASelection.Count - 1 do
  begin
    SelectedItem := ASelection[Item];
    if SelectedItem is TJVEAction then
    begin
      if (Action.Action = nil)
        {$IF Defined(PACKAGE_FULL)}or not (Action.Action is TJVEPayment){$ENDIF} then
          for Prop := ASelectionProperties.Count - 1 downto 0 do
            if (ASelectionProperties[Prop] as IProperty).GetName = 'Identifier' then
              ASelectionProperties.Delete(Prop);

      if (Action.Action = nil) or Action.Action.GetInterface(IJVEInterstitial, Intf) or
        Action.Action.GetInterface(IJVEManagedExecutable, Intf) then
          for Prop := ASelectionProperties.Count - 1 downto 0 do
          begin
            Name := (ASelectionProperties[Prop] as IProperty).GetName;
            if (Name = 'Bitmap') or (Name = 'AppDetails') or
              (Name = 'OnLoad') or (Name = 'OnUnload') then
                ASelectionProperties.Delete(Prop);
          end;
    end;
  end;
end;

type
  TJVENativeImageFilter = class(TSelectionEditor, ISelectionpropertyFilter)
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections;
      const ASelectionproperties: IInterfaceList);
  end;

{ TJVENativeImageFilter }

procedure TJVENativeImageFilter.FilterProperties(const ASelection:
  IDesignerSelections; const ASelectionproperties: IInterfaceList);
var
  Item, Prop: Integer;
  SelectedItem: TPersistent;
begin
  for Item := 0 to ASelection.Count - 1 do
  begin
    SelectedItem := ASelection[Item];
    if SelectedItem is TJVENativeImage then
      for Prop := ASelectionProperties.Count - 1 downto 0 do
        if ((ASelectionProperties[Prop] as IProperty).GetName = 'MultiResBitmap') or
          ((ASelectionProperties[Prop] as IProperty).GetName = 'Bitmap') then
            ASelectionProperties.Delete(Prop);
  end;
end;

type
  TJVENativeLabelFilter = class(TSelectionEditor, ISelectionpropertyFilter)
  protected
    procedure FilterProperties(const ASelection: IDesignerSelections;
      const ASelectionproperties: IInterfaceList);
  end;

{ TJVENativeLabelFilter }

procedure TJVENativeLabelFilter.FilterProperties(const ASelection:
  IDesignerSelections; const ASelectionproperties: IInterfaceList);
var
  Item, Prop: Integer;
  SelectedItem: TPersistent;
begin
  for Item := 0 to ASelection.Count - 1 do
  begin
    SelectedItem := ASelection[Item];
    if SelectedItem is TJVENativeLabel then
      for Prop := ASelectionProperties.Count - 1 downto 0 do
        if (ASelectionProperties[Prop] as IProperty).GetName = 'Text' then
          ASelectionProperties.Delete(Prop);
  end;
end;

type
  TJVEActionProperty = class(TComponentProperty)
  private
    FGetValuesStrProc: TGetStrProc;
  protected
    procedure ReceiveComponentNames(const S: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

{ TJVEActionProperty }

procedure TJVEActionProperty.ReceiveComponentNames(const S: string);
var
  Component: TComponent;
  Owner: TPersistent;
  Intf: IInterface;
begin
  if (PropCount = 0) or not (GetComponent(0) is TJVEAction) then
    Exit;

  Component := Designer.GetComponent(S);
  Owner := TJVEActions(TJVEAction(GetComponent(0)).Collection).Owner;
  if Assigned(FGetValuesStrProc) and Assigned(Component) and
    (((Component is TJVENativeAd) and (Owner is TJVENativeAd)) or
    (Component.GetInterface(IJVEInterstitial, Intf) and (Owner is TJVEInterstitials)) or
    Component.GetInterface(IJVEExecutable, Intf)) then
      FGetValuesStrProc(S);
end;

procedure TJVEActionProperty.GetValues(Proc: TGetStrProc);
begin
  FGetValuesStrProc := Proc;
  try
    Designer.GetComponentNames(GetTypeData(TypeInfo(TComponent)), ReceiveComponentNames);
  finally
    FGetValuesStrProc := nil;
  end;
end;

procedure TJVEActionProperty.SetValue(const Value: string);
var
  Component: TComponent;
  Owner: TPersistent;
  Intf: IInterface;
  List: IDesignerSelections;
begin
  if (PropCount = 0) or not (GetComponent(0) is TJVEAction) then
    Exit;

  if not Value.IsEmpty then
  begin
    Component := Designer.GetComponent(Value);
    Owner := TJVEActions(TJVEAction(GetComponent(0)).Collection).Owner;
    if not (((Component is TJVENativeAd) and (Owner is TJVENativeAd)) or
      (Component.GetInterface(IJVEInterstitial, Intf) and (Owner is TJVEInterstitials)) or
      Component.GetInterface(IJVEExecutable, Intf)) then
        raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end else
    Component := nil;
  SetOrdValue(LongInt(Component));

  List := TDesignerSelections.Create;
  Designer.GetSelections(List);
  Designer.SetSelections(TDesignerSelections.Create);
  Designer.SetSelections(List);
end;
{$ENDIF}

{$IF not Defined(PACKAGE_ADS)}
type
  TJVEConfigIdentifierProperty = class(TStringProperty)
  public
    procedure SetValue(const Value: string); override;
  end;

{ TJVEConfigIdentifierProperty }

procedure TJVEConfigIdentifierProperty.SetValue(const Value: string);
var
  Config: TJVEConfigurationValue;
  Short: String;
  Idx: Integer;
begin
  inherited SetValue(Value);
  if PropCount <> 1 then
    Exit;

  Short := Value;
  for Idx := Length(Short) downto 1 do
    if not Short[Idx].IsLetterOrDigit then
      Delete(Short, Idx, 1);

  Config := TJVEConfigurationValue(GetComponent(0));
  Config.Name := Designer.UniqueName(Config.GetParentComponent.Name + Short);
  Designer.Modified;
end;

{ TJVEConfigurationEditor }

type
  TJVEConfigurationEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TJVEConfigurationEditor.Edit;
begin
  ShowConfigEditor(Designer, Component);
end;

procedure TJVEConfigurationEditor.ExecuteVerb(Index: Integer);
begin
  ShowConfigEditor(Designer, Component);
end;

function TJVEConfigurationEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Variables...';
end;

function TJVEConfigurationEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
{$ENDIF}

{ TLangSaver }

type
  TLangSaver = class(TComponentEditor)
  protected
    procedure Save;
    procedure Load;
    procedure PropertyProc(const Prop: IProperty);
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TLangSaver.Save;
var
  Dialog: TSaveDialog;
  Output, Line, Resources, Original: TStrings;
  Current, Acc: String;
  Ref: Integer;
begin
  Dialog := TSaveDialog.Create(nil);
  try
    Dialog.Title := 'Save TLang as an Excel friendly file';
    Dialog.Filter := 'All files (*.*)|*.*|Language .csv files (*.csv)|*.csv';
    Dialog.FilterIndex := 2;
    Dialog.Options := [TOpenOption.ofOverwritePrompt,
      TOpenOption.ofHideReadOnly, TOpenOption.ofPathMustExist,
      TOpenOption.ofNoReadOnlyReturn, TOpenOption.ofEnableSizing];

    if Dialog.Execute then
    begin
      Output := TStringList.Create;
      try
        Resources := TLang(Component).Resources;
        Original := TLang(Component).Original;
        Acc := 'en';
        for Current in Resources do
          Acc := Acc + #9 + Current;
        Output.Add(Acc);

        Line := TStringList.Create;
        Line.Delimiter := #9;
        try
          CollectLangStart;
          TStyleManager.UpdateScenes;
          for Current in CollectLangStrings do
            if Original.IndexOf(Current) < 0 then
              Original.Add(Current);
          CollectLangFinish;

          for Current in Original do
          begin
            Line.Clear;
            Line.Add(Current);
            for Ref := 0 to Resources.Count - 1 do
              Line.Add(TStrings(Resources.Objects[Ref]).Values[Current]);
            Output.Add(Line.DelimitedText);
          end;
        finally
          FreeAndNil(Line);
        end;
        Output.SaveToFile(Dialog.FileName, TEncoding.Unicode);
      finally
        FreeAndNil(Output);
      end;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TLangSaver.Load;
var
  Dialog: TOpenDialog;
  Input, Resources, Languages, Line: TStrings;
  Current: String;
  Idx: Integer;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := 'Load TLang from a .csv file';
    Dialog.Filter := 'All files (*.*)|*.*|Language .csv files (*.csv)|*.csv';
    Dialog.FilterIndex := 2;
    Dialog.Options := [TOpenOption.ofHideReadOnly, TOpenOption.ofPathMustExist,
      TOpenOption.ofFileMustExist, TOpenOption.ofEnableSizing];
    if Dialog.Execute then
    begin
      Input := TStringList.Create;
      try
        Input.LoadFromFile(Dialog.FileName);
        if (Input.Count < 2) or (Input[0].Length < 3) then
        begin
          ShowMessage('The input file contains no translations in a recognized format');
          Exit;
        end;

        Input[0] := LowerCase(Input[0]);
        if not Input[0].StartsWith('en') then
        begin
          ShowMessage('The first column must represent the English language ("en")');
          Exit;
        end;

        // This is, in part, to make sure that the third character is indeed a separator
        if not CharInSet(Input[0].Chars[2], [',', ';', #9]) then
        begin
          ShowMessage('Only comma, semicolon and tab separated formats are supported');
          Exit;
        end;

        Languages := TStringList.Create;
        try
          Languages.StrictDelimiter := True;
          Languages.Delimiter := Input[0].Chars[2];
          Languages.DelimitedText := Input[0].Substring(3);
          if Languages.Count = 0 then
          begin
            ShowMessage('At least one language in addition to english is needed');
            Exit;
          end;

          for Current in Languages do
            if Current.Length <> 2 then
            begin
              ShowMessage('All language codes should have 2 character (en, de, fr, ru, etc.)');
              Exit;
            end;

          // All tests have passed; now we are loading the file
          Resources := TLang(Component).Resources;
          for Idx := 0 to Resources.Count - 1 do
            TStrings(Resources.Objects[Idx]).DisposeOf;
          Resources.Clear;
          TLang(Component).Original.Clear;

          for Current in Languages do
          begin
            Line := TStringList.Create;
            TStringList(Line).CaseSensitive := True;
            Resources.AddObject(Current, Line);
          end;

          Line := TStringList.Create;
          try
            Line.StrictDelimiter := True;
            Line.Delimiter := Languages.Delimiter;
            Input.Delete(0);
            for Current in Input do
            begin
              Line.DelimitedText := Current;
              if (Line.Count = 0) or Line[0].IsEmpty then
                Continue;

              TLang(Component).Original.Add(Line[0]);
              for Idx := 1 to Min(Line.Count - 1, Languages.Count) do
                if not Line[Idx].IsEmpty then
                  TStrings(Resources.Objects[Idx - 1]).Add(Line[0] + '=' + Line[Idx]);
            end;
          finally
            FreeAndNil(Line);
          end;
        finally
          FreeAndNil(Languages);
        end;
      finally
        FreeAndNil(Input);
      end;
    end;
  finally
    FreeAndNil(Dialog);
  end;
end;

procedure TLangSaver.PropertyProc(const Prop: IProperty);
begin
  if Prop.GetName = 'Lang' then
    Prop.Edit;
end;

// There is no proper way (that I know) to add a component editor, rather than
// to replace it. The original TLang editor exposes double-click to edit the
// component; that editing is defined within dclfmxstd???.bpl, which we cannot
// use directly.
// Thankfully they are also exposing this editor via a Lang property, so we can
// invoke it indirectly. Otherwise we would have had to include those sources.
procedure TLangSaver.Edit;
var
  Selection: IDesignerSelections;
begin
  Selection := CreateSelectionList;
  Selection.Add(Component);
  GetComponentProperties(Selection,
    [Low(TTypeKind)..High(TTypeKind)], Designer, PropertyProc);
end;

procedure TLangSaver.ExecuteVerb(Index: Integer);
begin
  case Index of
    1: Save;
    2: Load;
    else Edit;
  end;
end;

function TLangSaver.GetVerb(Index: Integer): string;
begin
  case Index of
    1: Result := 'Save as CSV...';
    2: Result := 'Load from CSV...';
    else Result := 'Edit...';
  end;
end;

function TLangSaver.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ Register }

procedure Register;
begin
{$IF Defined(PACKAGE_FULL)}
  RegisterComponents('JVE Pack', [TJVEBrowser, TJVEOpenURL, TJVEEmail,
    TJVESocial, TJVEPayment, TJVEAppStore, TJVEAppRater, TJVEAnalytics,
    TJVEChartBoost, TJVEAdMobInterstitial, TJVEFacebookInterstitial,
    TJVEInterstitials, TJVEBanners, TJVECustomAd, TJVEAdMob, TJVEFacebookBanner,
    TJVEPubNative, TJVEMobFox, TJVENativeAd, TJVENativeImage,
    TJVENativeLabel, TJVEPushNotification, TJVEPushWoosh,
    TJVEConfiguration, TJVEShade, TJVELayout, TJVESpan, TJVEAnalyticsForVCL]);
{$ELSEIF Defined(PACKAGE_ANALYTICS)}
  RegisterComponents('JVE Analytics', [TJVEOpenURL, TJVEAnalytics,
    TJVEConfiguration, TJVEAnalyticsForVCL]);
{$ELSEIF Defined(PACKAGE_ADS)}
  RegisterComponents('JVE Ads Support', [TJVEOpenURL, TJVEChartBoost,
    TJVEAdMobInterstitial, TJVEFacebookInterstitial, TJVEInterstitials,
    TJVEBanners, TJVECustomAd, TJVEAdMob, TJVEFacebookBanner,
    TJVEPubNative, TJVEMobFox, TJVENativeAd, TJVENativeImage, TJVENativeLabel,
    TJVELayout, TJVESpan]);
{$ELSEIF Defined(PACKAGE_NATIVE)}
  RegisterComponents('JVE Native', [TJVEBrowser, TJVEOpenURL, TJVEEmail, TJVESocial,
    TJVEAppStore, TJVEAppRater, TJVEConfiguration, TJVEShade, TJVELayout, TJVESpan]);
{$ENDIF}

{$IF not Defined(PACKAGE_ADS)}
  RegisterClass(TJVEConfigurationValue);
  RegisterClass(TJVEConfigurationInteger);
  RegisterClass(TJVEConfigurationIntegerRange);
  RegisterClass(TJVEConfigurationFloat);
  RegisterClass(TJVEConfigurationFloatRange);
  RegisterClass(TJVEConfigurationDate);
  RegisterClass(TJVEConfigurationTime);
  RegisterClass(TJVEConfigurationDateTime);
  RegisterClass(TJVEConfigurationBoolean);
  RegisterClass(TJVEConfigurationString);
  RegisterNoIcon([TJVEConfigurationValue, TJVEConfigurationInteger,
    TJVEConfigurationFloat, TJVEConfigurationDate, TJVEConfigurationTime,
    TJVEConfigurationDateTime, TJVEConfigurationBoolean, TJVEConfigurationString]);

  RegisterComponentEditor(TJVEConfiguration, TJVEConfigurationEditor);
  RegisterPropertyEditor(TypeInfo(String), TJVEConfigurationValue,
    'Identifier', TJVEConfigIdentifierProperty);
{$ENDIF}

  RegisterComponentEditor(TLang, TLangSaver);

{$IF Defined(PACKAGE_FULL) or Defined(PACKAGE_ADS)}
  RegisterSelectionEditor(TJVEAction, TJVEActionFilter);
  RegisterSelectionEditor(TJVENativeLabel, TJVENativeLabelFilter);
  RegisterSelectionEditor(TJVENativeImage, TJVENativeImageFilter);
  RegisterPropertyEditor(TypeInfo(TComponent), TJVEAction, 'Action', TJVEActionProperty);
{$ENDIF}

{$IF Defined(PACKAGE_FULL)}
  RegisterSelectionEditor(TJVEPaymentItem, TJVEPaymentItemFilter);
  RegisterPropertyEditor(TypeInfo(String), TJVEAction, 'Identifier', TJVEIdentifierProperty);
{$ENDIF}
end;

end.

