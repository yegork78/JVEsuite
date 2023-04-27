package com.jvesoft.admob;

import com.google.android.gms.ads.AdError;
import com.google.android.gms.ads.FullScreenContentCallback;
import com.google.android.gms.ads.MobileAds;
import com.google.android.gms.ads.OnUserEarnedRewardListener;
import com.google.android.gms.ads.RequestConfiguration;
import com.google.android.ump.ConsentInformation;
import com.google.android.ump.ConsentRequestParameters;
import com.google.android.ump.UserMessagingPlatform;
import android.app.Activity;
import android.util.Log;

public class FullScreenContentListener extends FullScreenContentCallback
{
  public native void done(String error);

  @Override
  public void onAdFailedToShowFullScreenContent(AdError error)
  {
    done(error.getMessage());
  }

  @Override
  public void onAdShowedFullScreenContent()
  {
    done("#");
  }

  @Override
  public void onAdDismissedFullScreenContent()
  {
    done("^");
  }

  public OnUserEarnedRewardListener getRewardListener()
  {
    return reward -> done("$" + reward.getType() + "/" + reward.getAmount());
  }


  private static void loadConsentForm(Activity activity, ConsentInformation consent)
  {
    UserMessagingPlatform.loadConsentForm(activity, consentForm ->
    {
      if(consent.getConsentStatus() == ConsentInformation.ConsentStatus.REQUIRED)
        consentForm.show(activity, formError -> loadConsentForm(activity, consent));
    }, formError -> Log.wtf("Consent Form", "Error: " + formError.getMessage()));
  }

  public static void initAdMob(Activity activity, boolean minors)
  {
    if(minors)
      MobileAds.setRequestConfiguration(new RequestConfiguration.Builder().
        setTagForChildDirectedTreatment(RequestConfiguration.TAG_FOR_CHILD_DIRECTED_TREATMENT_TRUE).
        setMaxAdContentRating(RequestConfiguration.MAX_AD_CONTENT_RATING_G).
        setTagForUnderAgeOfConsent(RequestConfiguration.TAG_FOR_UNDER_AGE_OF_CONSENT_TRUE).build());
    MobileAds.initialize(activity);

    ConsentInformation consent = UserMessagingPlatform.getConsentInformation(activity);
    consent.requestConsentInfoUpdate(activity,
      new ConsentRequestParameters.Builder().setTagForUnderAgeOfConsent(minors).build(), () -> {
        if(consent.isConsentFormAvailable())
          loadConsentForm(activity, consent);
      }, error -> Log.wtf("Consent Load", "Error: " + error.getMessage()));
  }
}
