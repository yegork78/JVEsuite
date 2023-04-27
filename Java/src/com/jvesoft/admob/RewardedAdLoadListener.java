package com.jvesoft.admob;

import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.rewarded.RewardedAd;
import com.google.android.gms.ads.rewarded.RewardedAdLoadCallback;

public class RewardedAdLoadListener extends RewardedAdLoadCallback
{
  public native void error(String error);
  public native void loaded(Object interstitial);

  @Override
  public void onAdLoaded(RewardedAd interstitial)
  {
    loaded(interstitial);
  }

  @Override
  public void onAdFailedToLoad(LoadAdError error)
  {
    error(error.getMessage());
  }
}
