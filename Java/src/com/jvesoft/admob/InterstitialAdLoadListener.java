package com.jvesoft.admob;

import com.google.android.gms.ads.LoadAdError;
import com.google.android.gms.ads.interstitial.InterstitialAd;
import com.google.android.gms.ads.interstitial.InterstitialAdLoadCallback;

public class InterstitialAdLoadListener extends InterstitialAdLoadCallback
{
  public native void error(String error);
  public native void loaded(Object interstitial);

  @Override
  public void onAdLoaded(InterstitialAd interstitial)
  {
    loaded(interstitial);
  }

  @Override
  public void onAdFailedToLoad(LoadAdError error)
  {
    error(error.getMessage());
  }
}
