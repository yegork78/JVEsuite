(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Analytics.VCL;

interface

uses System.SysUtils, System.Types, System.Classes, JVE.Analytics.Common,
  {$IF Defined(MSWINDOWS)} Vcl.Forms, Vcl.AppEvnts, {$ENDIF}
  JVE.Messaging, System.Messaging, Vcl.ExtCtrls;

type
  // This class is defined to force Delphi to use the correct source (FMX vs. VCL).
  // In the end this provides support for VCL platform.
  // It should be noted that to use Google Analytics you should appropriately
  // notify the user that this data is collected. For details, see:
  // developers.google.com/analytics/devguides/collection/protocol/policy
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  TJVEAnalyticsForVCL = class(TJVEAnalyticsCommon)
  private
    FDelay: TTimer;
    procedure StartupTimer(Sender: TObject);
  public
    procedure AfterConstruction; override;
  end;

implementation

{ TJVEAnalyticsForVCL }

procedure TJVEAnalyticsForVCL.AfterConstruction;
begin
  inherited AfterConstruction;
  FDelay := TTimer.Create(Self);
  FDelay.Interval := 10;
  FDelay.Enabled := True;
  FDelay.OnTimer := StartupTimer;
end;

procedure TJVEAnalyticsForVCL.StartupTimer(Sender: TObject);
begin
  FDelay.Enabled := False;
  FreeAndNil(FDelay);
  CreateThread;
end;

{$IF Defined(MSWINDOWS)}

{ TVCLTracker }

type
  TVCLTracker = class
  private
    FFormName: String;
    FEvents: TApplicationEvents;
  public
    procedure IdleListener(Sender: TObject; var Done: Boolean);
    procedure HandleException(Sender: TObject; E: Exception);
  end;

var
  VCLTracker: TVCLTracker;

procedure TVCLTracker.HandleException(Sender: TObject; E: Exception);
begin
  if not (E is EAbort) then
    TJVEAnalyticsCommon.BroadcastException(E.Message, False, acExceptions);
end;

procedure TVCLTracker.IdleListener(Sender: TObject; var Done: Boolean);
begin
  if (Screen.ActiveForm <> nil) and (FFormName <> Screen.ActiveForm.Name) then
  begin
    FFormName := Screen.ActiveForm.Name;
    TJVEAnalyticsCommon.BroadcastScreen(FFormName, acFormSwitches);
  end;
end;

initialization
  JVEAnalyticsScreenSize := function: TSize
    begin
      Result := TSize.Create(Screen.Width, Screen.Height);
    end;
  JVEAnalyticsFormSize := function: TSize
    begin
      if Screen.ActiveForm <> nil then
        Result := TSize.Create(Screen.ActiveForm.Width, Screen.ActiveForm.Height)
      else
        Result := TSize.Create(0, 0);
    end;
  VCLTracker := TVCLTracker.Create;
  VCLTracker.FEvents := TApplicationEvents.Create(nil);
  VCLTracker.FEvents.OnIdle := VCLTracker.IdleListener;
  VCLTracker.FEvents.OnException := VCLTracker.HandleException;
finalization
  FreeAndNil(VCLTracker.FEvents);
  FreeAndNil(VCLTracker);
{$ENDIF}
end.

