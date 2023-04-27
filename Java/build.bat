javac -Xlint:deprecation -source 1.6 -target 1.6 -cp lib/android.jar -cp ../chartboost.jar -d output/jve-chartboost src/com/jvesoft/chartboost/*.java
jar cf output/jar/jve-chartboost.jar -C output/jve-chartboost com
jar cf output/jar/jve-admob.jar -C output/jve-admob com
