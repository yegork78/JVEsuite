(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Shade;

interface

uses System.SysUtils, System.Classes, JVE.Utils, FMX.Ani, FMX.Objects, FMX.Forms,
  System.Types, FMX.Types, FMX.Controls, FMX.Layouts, JVE.Layout {$IF CompilerVersion >= 26}, FMX.Graphics {$ENDIF};

type
  // This class allows you to shade the screen, while your app is contacting
  // the server or performing another lengthy task.
  [ComponentPlatformsAttribute($000B945F)]
  TJVEShade = class(TComponent)
  private
    FKey: Integer;
    FActive: Boolean;
    FOnCancel: TNotifyEvent;
    FOnShow: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
  public
    destructor Destroy; override;

    // Show the shade. The value returned is the value you should pass
    // to the Hide function below.
    // Provide OnCancel callback, if you want your callback to be called,
    // when the user clicks that gray background. It will NOT hide the shade
    // automatically, you should do that normally, using the Hide function.
    class function Show(OnCancel: TProc = nil): Integer; overload; inline;

    // As above. The shade is opened in background; OnShow method is called
    // once the shade is visible (the OnShow sole parameter is the same as
    // the return value; could be used to Hide from within OnShow).
    // Useful to start heavy work in the main thread processes.
    class function Show(OnShow: TProc<Integer>;
      OnCancel: TProc = nil): Integer; overload;

    // Call to hide the shade, presented with the matching Show call.
    class procedure Hide(Key: Integer);

    // Runs the specified code in main thread, while presenting a shade.
    // The shade is removed automatically when done.
    // Useful to start heavy work in the main thread processes.
    class procedure RunShaded(OnShow: TProc; OnCancel: TProc = nil);

  published
    // Use this property to show the shade or hide it. The shade will remain
    // visible if it is requested by at least one component.
    property Active: Boolean read FActive write SetActive default False;

    // This event is called when the shade actually became visible on the screen.
    // If you want to perform heavy calculations on the main thread, you should
    // do that after this event is fired.
    property OnShow: TNotifyEvent read FOnShow write FOnShow;

    // This event is called when the user clicks on the shade (usually because
    // he is impatient).
    // Notice, this does not hide the shade automatically.
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
  end;

type
  // This is an internal class. Please don't use it directly.
  TJVEFormShade = class(TForm)
    Shade: TRectangle;
    Animation: TFloatAnimation;
    Layout: TJVELayout;
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure ShadeClick(Sender: TObject);
    procedure AnimationFinish(Sender: TObject);
  private
    FShown: Boolean;
  end;

implementation

{$R *.fmx}

uses System.Generics.Collections, System.UITypes, FMX.Platform, FMX.StdCtrls
  {$IF CompilerVersion >= 27}, System.Math.Vectors {$ENDIF}
  {$IFDEF ANDROID}, FMX.Platform.Android, FMX.Helpers.Android {$ENDIF};

{$IF Defined(IOS) and (CompilerVersion < 30)}

type
  TJVEAniIndicator = class(TAniIndicator)
    function GetAbsoluteMatrix: TMatrix; override;
  end;

// This effectively means that the scale will be applied from the center of
// the control, rather than from the top-left corner.
// IMHO applying it from the top-left corner is the greatest mistake of FMX.
function TJVEAniIndicator.GetAbsoluteMatrix: TMatrix;
begin
  Result := inherited GetAbsoluteMatrix;
  Result.m31 := Result.m31 - Width * (Scale.X - 1) / 2;
  Result.m32 := Result.m32 - Height * (Scale.Y - 1) / 2;
end;

{$ENDIF}

var
  RequestIndex: Integer;
  Requests: TDictionary<Integer, TProc>;
  NotifyList: TDictionary<Integer, TProc<Integer>>;
  GlobalShade: TJVEFormShade;
{$IF Defined(MACOS) and not Defined(IOS)}
  GlobalShadeOwner: TPopup;
{$ENDIF}

procedure TJVEFormShade.AnimationFinish(Sender: TObject);
var
  Key: TPair<Integer, TProc<Integer>>;
begin
  FShown := True;
  for Key in NotifyList do
    Key.Value(Key.Key);
  NotifyList.Clear;
end;

procedure TJVEFormShade.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  OnPaint := nil;
  Animation.Start;
end;

procedure TJVEFormShade.ShadeClick(Sender: TObject);
var
  Key: TPair<Integer, TProc>;
begin
  for Key in Requests do
    if Key.Value <> nil then
      Key.Value();
end;

{ TShadeForm }

procedure ShowShade;
var
  Indicator: TAniIndicator;
begin
  if GlobalShade <> nil then
    Exit;

{$IF Defined(MACOS) and not Defined(IOS)}
  // This variable is needed in order to trick FMX.Platform.Mac into thinking
  // that the Shade is not a real window (which it actually isn't).
  // This will not let it host alerts, allowing it to be hidden independently.
  if GlobalShadeOwner = nil then
    GlobalShadeOwner := TPopup.Create(nil);
  GlobalShade := TJVEFormShade.Create(GlobalShadeOwner);
{$ELSE}
  GlobalShade := TJVEFormShade.Create(nil);
{$ENDIF}

  {$IF Defined(IOS)}
  // BorderStyle = bsNone is the way FireMonkey shows or hides the status bar.
  // Actually, a very intresting idea. Wish it was documented!
  if not SharedUIApplication.isStatusBarHidden then
    GlobalShade.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsSizeable{$ELSE}TFmxFormBorderStyle.Sizeable{$ENDIF}
  else
  {$ELSEIF Defined(ANDROID)}
  if MainActivity.{$IF CompilerVersion >= 33}getFullScreenManager.{$ENDIF}getStatusBarVisibility then
    GlobalShade.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsSizeable{$ELSE}TFmxFormBorderStyle.Sizeable{$ENDIF}
  else
  {$ENDIF}
  begin
    GlobalShade.BorderStyle := {$IF CompilerVersion < 27}TFmxFormBorderStyle.bsNone{$ELSE}TFmxFormBorderStyle.None{$ENDIF};
{$IF CompilerVersion >= 32}
    GlobalShade.SystemStatusBar.Visibility := TFormSystemStatusBar.TVisibilityMode.Invisible;
{$ENDIF}
  end;

{$IF not Defined(MACOS) or Defined(IOS)}
  GlobalShade.FullScreen := True;
{$ENDIF}
  GlobalShade.SetBounds(0, 0, Round(Screen.Size.Width), Round(Screen.Size.Height));

{$IF Defined(IOS) and (CompilerVersion < 30)}
  Indicator := TJVEAniIndicator.Create(GlobalShade);
  Indicator.Scale.X := 4;
  Indicator.Scale.Y := 4;
{$ELSE}
  Indicator := TAniIndicator.Create(GlobalShade);
{$ENDIF}
  Indicator.Enabled := True;
  Indicator.HitTest := False;
  Indicator.Parent := GlobalShade.Layout;

{$IF (Defined(MACOS) and not Defined(IOS)) or Defined(MSWINDOWS)}
  ExecuteInIdle(procedure
  begin
    GlobalShade.ShowModal;
  end);
{$ELSE}
  GlobalShade.Show;
  GlobalShade.BringToFront;
{$ENDIF}
end;

{ TJVEShade }

class function TJVEShade.Show(OnCancel: TProc): Integer;
begin
  Result := Show(nil, OnCancel);
end;

class function TJVEShade.Show(OnShow: TProc<Integer>; OnCancel: TProc): Integer;
begin
  if Requests = nil then
    Requests := TDictionary<Integer, TProc>.Create;
  if NotifyList = nil then
    NotifyList := TDictionary<Integer, TProc<Integer>>.Create;

  Inc(RequestIndex);
  Requests.Add(RequestIndex, OnCancel);
  Result := RequestIndex;

  if Requests.Count = 1 then
  begin
    if Assigned(OnShow) then
      NotifyList.Add(Result, OnShow);
{$IF Defined(ANDROID)}
    CallInUIThreadAndWaitFinishing(procedure
    begin
      ShowShade;
    end);
{$ELSE}
    ShowShade;
{$ENDIF}
  end else if Assigned(OnShow) then
    if GlobalShade.FShown then
      OnShow(Result)
    else
      NotifyList.Add(Result, OnShow);
end;

class procedure TJVEShade.Hide(Key: Integer);
begin
  if Requests = nil then
    Exit;

  Requests.Remove(Key);
  if (Requests.Count = 0) and (GlobalShade <> nil) then
  begin
    if GlobalShade.Animation.Running then
      GlobalShade.Animation.Stop;
    GlobalShade.Close;

    ExecuteInIdle(procedure
      begin
        GlobalShade.DisposeOf;
        GlobalShade := nil;
      end);
  end;
end;

class procedure TJVEShade.RunShaded(OnShow: TProc; OnCancel: TProc = nil);
begin
  Show(procedure(Shade: Integer)
  begin
    try
      OnShow;
    finally
      Hide(Shade);
    end;
  end, OnCancel);
end;

destructor TJVEShade.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TJVEShade.SetActive(const Value: Boolean);
begin
  if Value = FActive then
    Exit;
  FActive := Value;

  if csDesigning in ComponentState then
    Exit;

  if FActive then
    ExecuteInIdle(procedure
    begin
      if (FKey <> 0) or not FActive then
        Exit;

      FKey := Show(
        procedure(Key: Integer)
        begin
          FKey := Key;
          if Assigned(FOnShow) then
            FOnShow(Self);
        end,
        procedure
        begin
          if Assigned(FOnCancel) then
            FOnCancel(Self);
        end);
    end)
  else if FKey <> 0 then
  begin
    Hide(FKey);
    FKey := 0;
  end;
end;

end.

