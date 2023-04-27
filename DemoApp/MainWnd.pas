(**************************************************************************)
(* JVEsoft Cross Platform Infrastructure Component Suite                  *)
(* Copyright (C) 2013-2016 JVEsoft ltd. All Rights Reserved               *)
(*                                                                        *)
(* This file is subject to the terms and conditions defined in file       *)
(* 'Manual.pdf', section License Agreement, which is part of this         *)
(* source code package.                                                   *)
(**************************************************************************)

unit MainWnd;

interface

uses
  System.SysUtils, FMX.Forms, System.UITypes, FMX.Dialogs, JVE.ChartBoost,
  JVE.AdMob, JVE.Facebook, JVE.Analytics.Common, JVE.Analytics, JVE.Shade,
  JVE.Interstitials, JVE.AppStore, JVE.Payment, JVE.Social, JVE.Email,
  JVE.OpenURL, JVE.Browser, FMX.Controls, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts, JVE.Layout, FMX.Types,
  System.Classes, JVE.Banners, JVE.Actions, JVE.Utils;

type
  TMainWindow = class(TForm)
    JVEBrowser: TJVEBrowser;
    JVEOpenURL: TJVEOpenURL;
    JVEEmail: TJVEEmail;
    JVESocialT: TJVESocial;
    JVESocialF: TJVESocial;
    JVESocialS: TJVESocial;
    JVEPayment: TJVEPayment;
    JVEAppStore: TJVEAppStore;
    JVEAppRater: TJVEAppRater;
    JVEBanners: TJVEBanners;
    JVEInterstitials: TJVEInterstitials;
    StoreImg: TImage;
    TwitterImg: TImage;
    RateImg: TImage;
    FacebookImg: TImage;
    EmailImg: TImage;
    SinaWeiboImg: TImage;
    PaymentImg: TImage;
    InterstitialImg: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BrowserImg: TImage;
    OpenImg: TImage;
    Timer: TTimer;
    JVEShade: TJVEShade;
    JVELayout: TJVELayout;
    ShadeImg: TImage;
    JVEAnalytics: TJVEAnalytics;
    JVEAdMob: TJVEAdMob;
    Label1: TLabel;
    Label5: TLabel;
    JVEFacebookBanner: TJVEFacebookBanner;
    JVEFacebookReward: TJVEFacebookInterstitial;
    JVEFacebookInterstitial: TJVEFacebookInterstitial;
    ADInterstitialImg: TImage;
    ADRewardImg: TImage;
    AdMobInterstitialImg: TImage;
    Label7: TLabel;
    JVEAdMobInterstitial: TJVEAdMobInterstitial;
    JVEAdMobReward: TJVEAdMobInterstitial;
    AdMobRewardImg: TImage;
    Label6: TLabel;
    CBInterstitialImg: TImage;
    CBRewardImg: TImage;
    Label8: TLabel;
    JVEChartBoostInterstitial: TJVEChartBoost;
    JVEChartBoostReward: TJVEChartBoost;
    procedure BrowserImgClick(Sender: TObject);
    procedure TwitterImgClick(Sender: TObject);
    procedure FacebookImgClick(Sender: TObject);
    procedure SinaWeiboImgClick(Sender: TObject);
    procedure EmailImgClick(Sender: TObject);
    procedure PaymentImgClick(Sender: TObject);
    procedure InterstitialImgClick(Sender: TObject);
    procedure RateImgClick(Sender: TObject);
    procedure StoreImgClick(Sender: TObject);
    procedure OpenImgClick(Sender: TObject);
    procedure ShadeImgClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure JVEPaymentFinished(Payment: TJVEPayment;
      Status: TJVEPaymentStatus; const Error: string);
    procedure JVECustomAd1Click(CustomAd: TJVECustomAd; Item: TJVEAction);
    procedure JVEAdMobNoPaidAd(Sender: TJVEAdMob; Error: string);
    procedure JVEChartBoost1Reward(Sender: TJVEChartBoost; Reward: Integer);
    procedure JVEChartBoost1NoAd(Sender: TJVEChartBoost; Error: string);
    procedure JVEPaymentItems0Purchased(Payment: TJVEPayment;
      Item: TJVEPaymentItem; Receipt: string; PurchaseDate: TDateTime);
    procedure JVEFacebookBannerNoPaidAd(Sender: TJVEFacebookBanner;
      Error: string);
    procedure JVEFacebookRewardNoAd(Sender: TJVEFacebookInterstitial;
      Error: string);
    procedure JVEFacebookRewardReward(Sender: TJVEFacebookInterstitial);
    procedure AdMobInterstitialImgClick(Sender: TObject);
    procedure AdMobRewardImgClick(Sender: TObject);
    procedure JVEAdMobRewardReward(Sender: TJVEAdMobInterstitial;
      Kind: string; Amount: Double);
    procedure JVEAdMobRewardNoAd(Sender: TJVEAdMobInterstitial;
      Error: string);
    procedure JVEChartBoostInterstitialReward(Sender: TJVEChartBoost;
      Reward: Integer);
    procedure JVEChartBoostInterstitialNoAd(Sender: TJVEChartBoost;
      Error: string);
    procedure ADInterstitialImgClick(Sender: TObject);
    procedure ADRewardImgClick(Sender: TObject);
    procedure CBInterstitialImgClick(Sender: TObject);
    procedure CBRewardImgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.fmx}

procedure TMainWindow.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  JVEShade.Active := False;
end;

procedure TMainWindow.TwitterImgClick(Sender: TObject);
begin
  JVESocialT.Open;
end;

procedure TMainWindow.FacebookImgClick(Sender: TObject);
begin
  JVESocialF.Open;
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  JVERequestTrackingAuthorization;
  JVEAdMobInitAndShowConsent(False);
  JVEFacebookSetAdvertiserTracking(True);
  JVEChartBoostSetGDPRConsent(True);
end;

procedure TMainWindow.AdMobRewardImgClick(Sender: TObject);
begin
  JVEAdMobReward.Show;
end;

procedure TMainWindow.ADRewardImgClick(Sender: TObject);
begin
  JVEFacebookReward.Show;
end;

procedure TMainWindow.InterstitialImgClick(Sender: TObject);
begin
  JVEInterstitials.ProcessMajorEvent;
end;

procedure TMainWindow.JVEAdMobNoPaidAd(Sender: TJVEAdMob; Error: string);
begin
  ShowMessage('Admob No: ' + Error);
end;

procedure TMainWindow.JVEAdMobRewardNoAd(Sender: TJVEAdMobInterstitial;
  Error: string);
begin
  ShowMessage('Admob No: ' + Error);
end;

procedure TMainWindow.JVEAdMobRewardReward(Sender: TJVEAdMobInterstitial;
  Kind: string; Amount: Double);
begin
  ShowMessage('Admob Yes: ' + Kind + ' x ' + FloatToStr(Amount));
end;

procedure TMainWindow.JVEChartBoost1NoAd(Sender: TJVEChartBoost; Error: string);
begin
  ShowMessage('ChartBoost No: ' + Error);
end;

procedure TMainWindow.JVEChartBoost1Reward(Sender: TJVEChartBoost;
  Reward: Integer);
begin
  ShowMessage('ChartBoost Yes: ' + IntToStr(Reward));
end;

procedure TMainWindow.JVEChartBoostInterstitialNoAd(Sender: TJVEChartBoost;
  Error: string);
begin
  ShowMessage('ChartBoost No: ' + Error);
end;

procedure TMainWindow.JVEChartBoostInterstitialReward(Sender: TJVEChartBoost;
  Reward: Integer);
begin
  ShowMessage('ChartBoost Yes: ' + IntToStr(Reward));
end;

procedure TMainWindow.JVECustomAd1Click(CustomAd: TJVECustomAd;
  Item: TJVEAction);
begin
  TJVEOpenURL.OpenURL(Item.AppDetails.URL);
end;

procedure TMainWindow.JVEFacebookBannerNoPaidAd(Sender: TJVEFacebookBanner;
  Error: string);
begin
  ShowMessage('Faceboook No: ' + Error);
end;

procedure TMainWindow.JVEFacebookRewardNoAd(
  Sender: TJVEFacebookInterstitial; Error: string);
begin
  ShowMessage('Faceboook No: ' + Error);
end;

procedure TMainWindow.JVEFacebookRewardReward(
  Sender: TJVEFacebookInterstitial);
begin
  ShowMessage('Faceboook Yes!');
end;

procedure TMainWindow.JVEPaymentFinished(Payment: TJVEPayment;
  Status: TJVEPaymentStatus; const Error: string);
begin
  ShowDialog(Format('Done. Status: %d, %s', [Ord(Status), Error]));
end;

procedure TMainWindow.JVEPaymentItems0Purchased(Payment: TJVEPayment;
  Item: TJVEPaymentItem; Receipt: string; PurchaseDate: TDateTime);
begin
  ShowDialog('Purchased!');
end;

procedure TMainWindow.OpenImgClick(Sender: TObject);
begin
  JVEOpenURL.Open;
end;

procedure TMainWindow.EmailImgClick(Sender: TObject);
begin
  JVEEmail.Send;
end;

procedure TMainWindow.ShadeImgClick(Sender: TObject);
begin
  JVEShade.Active := True;
  Timer.Enabled := True;
end;

procedure TMainWindow.SinaWeiboImgClick(Sender: TObject);
begin
  JVESocialS.Open;
end;

procedure TMainWindow.StoreImgClick(Sender: TObject);
begin
  JVEAppStore.Open;
end;

procedure TMainWindow.ADInterstitialImgClick(Sender: TObject);
begin
  JVEFacebookInterstitial.Show;
end;

procedure TMainWindow.AdMobInterstitialImgClick(Sender: TObject);
begin
  JVEAdMobInterstitial.Show;
end;

procedure TMainWindow.BrowserImgClick(Sender: TObject);
begin
  JVEBrowser.Open;
end;

procedure TMainWindow.CBInterstitialImgClick(Sender: TObject);
begin
  JVEChartBoostInterstitial.Show;
end;

procedure TMainWindow.CBRewardImgClick(Sender: TObject);
begin
  JVEChartBoostReward.Show;
end;

procedure TMainWindow.PaymentImgClick(Sender: TObject);
begin
  JVEPayment.Items[0].Buy;
end;

procedure TMainWindow.RateImgClick(Sender: TObject);
begin
  JVEAppRater.QueryForFeedback(procedure(Open: Boolean)
    begin
      if Open then
        JVEAppRater.Open;
    end);
end;

end.

