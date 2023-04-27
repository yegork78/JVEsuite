(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2017 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit JVE.Analytics;

interface

uses System.SysUtils, System.Types, System.Classes, FMX.Forms,
  JVE.Analytics.Common, FMX.Types, JVE.Messaging, JVE.Utils,
  {$IF CompilerVersion >= 27} System.Messaging; {$ELSE} FMX.Messages; {$ENDIF}

type
  // This class is defined to force Delphi to use the correct source (FMX vs. VCL).
  // In the end this provides support for FireMonkey platform.
  // It should be noted that to use Google Analytics you should appropriately
  // notify the user that this data is collected. For details, see:
  // developers.google.com/analytics/devguides/collection/protocol/policy
  [ComponentPlatformsAttribute($000B945F)]
  TJVEAnalytics = class(TJVEAnalyticsCommon)
  public
    procedure AfterConstruction; override;
  end;

// This class overrides the Application.OnException: this event does not send
// messages, so there is no other way to catch global exceptions (that I know),
// and, from my experiene, this event is rarely used in regular apps.
// This event will only be overridden once the acExceptions is selected in any
// TJVEAnalytics class. There is a global counter, responsible for this.
// Once overridden, it will stay overridden (background thread will force it
// staying overridden.
// If you are using TJVEAnalytics to track exceptions, you have to use the
// following event handler for exceptions instead:
var
  ApplicationException: TExceptionEvent;

implementation

{ TFMXTracker }

type
  TFMXTracker = class
  private
    FFormName: String;
  public
    procedure IdleListener(const Sender: TObject; const Msg: TMessage);
    procedure HandleException(Sender: TObject; E: Exception);
  end;

var
  FMXTracker: TFMXTracker;

procedure TFMXTracker.HandleException(Sender: TObject; E: Exception);
begin
  if E is EAbort then
    Exit;

  TJVEAnalyticsCommon.BroadcastException(E.Message, False, acExceptions);

  if Assigned(ApplicationException) then
    ApplicationException(Sender, E)
  else
    Application.ShowException(E);
end;

procedure TFMXTracker.IdleListener(const Sender: TObject; const Msg: TMessage);
begin
  if JVEAnalyticsExceptions then
    Application.OnException := HandleException;

  if (Screen.ActiveForm <> nil) and (FFormName <> Screen.ActiveForm.Name) then
  begin
    FFormName := Screen.ActiveForm.Name;
    TJVEAnalyticsCommon.BroadcastScreen(FFormName, acFormSwitches);
  end;
end;

{ TJVEAnalytics }

procedure TJVEAnalytics.AfterConstruction;
begin
  inherited AfterConstruction;
  ExecuteInIdle(procedure
  begin
    CreateThread;
  end);
end;

initialization
  JVEAnalyticsScreenSize := function: TSize
    begin
      Result := TSize.Create(Round(Screen.Size.Width), Round(Screen.Size.Height));
    end;
  JVEAnalyticsFormSize := function: TSize
    begin
      if Screen.ActiveForm <> nil then
        Result := TSize.Create(Screen.ActiveForm.Width, Screen.ActiveForm.Height)
      else
        Result := TSize.Create(0, 0);
    end;
  FMXTracker := TFMXTracker.Create;
  TMessageManager.DefaultManager.SubscribeToMessage(TIdleMessage, FMXTracker.IdleListener);
finalization
  TMessageManager.DefaultManager.Unsubscribe(TIdleMessage, FMXTracker.IdleListener);
  FreeAndNil(FMXTracker);
end.

