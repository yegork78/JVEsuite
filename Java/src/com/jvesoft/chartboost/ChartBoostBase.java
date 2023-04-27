package com.jvesoft.chartboost;

import com.chartboost.sdk.Chartboost;
import com.chartboost.sdk.ChartboostDelegate;
import com.chartboost.sdk.Model.CBError.CBImpressionError;

public class ChartBoostBase extends ChartboostDelegate
{
  public native void ChartBoostNotReceived(String location, String message);
  public native void ChartBoostClosed(String location);
  public native void ChartBoostReward(String location, int reward);

  @Override
  public void didFailToLoadInterstitial(String location, CBImpressionError error)
  {
    ChartBoostNotReceived(location, error.name());
  }

  @Override
  public void didDismissInterstitial(String location)
  {
    ChartBoostClosed(location);
  }

  @Override
  public void didFailToLoadRewardedVideo(String location, CBImpressionError error)
  {
    ChartBoostNotReceived(location, error.name());
  }

  @Override
  public void didDismissRewardedVideo(String location)
  {
    ChartBoostClosed(location);
  }

  @Override
  public void didCompleteRewardedVideo(String location, int reward)
  {
    ChartBoostReward(location, reward);
  }
}
