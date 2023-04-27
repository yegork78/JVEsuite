package com.jvesoft.admob;

import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.rewardedinterstitial.RewardedInterstitialAd;
import com.google.android.gms.ads.rewardedinterstitial.RewardedInterstitialAdLoadCallback;

public class RewardedInterstitialAdLoadListener extends RewardedInterstitialAdLoadCallback
{
  public native void error(String error);
  public native void loaded(Object interstitial);

  @Override
  public void onAdLoaded(RewardedInterstitialAd interstitial)
  {
    loaded(interstitial);
  }

  @Override
  public void onAdFailedToLoad(LoadAdError error)
  {
    error(error.getMessage());
  }
}
