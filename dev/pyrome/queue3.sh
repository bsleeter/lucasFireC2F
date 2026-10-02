#!/usr/bin/env bash
# 1.7.0 re-runs: P27 and P28 = C5 refit on their seed runs + model; then the P29 full build
R="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"; D="F:/lucas-master/Models/lucasFireC2F Dev"; LIB="$D/lucasFireC2F Sierra Nevada & Foothills.ssim"; OUT=/f/lucasFireC2F/dev/pyrome/zones
cd /f/lucasFireC2F/dev/pyrome
zone() { TAG=$1; CS=$2; CR=$3; PR=$4; MS=$5
  "$R" refit_c5.R "$LIB" $TAG $CS $CR $PR > $OUT/${TAG}_refit.log 2>&1
  RF=$(grep -o "==== refit result [0-9]*" $OUT/${TAG}_refit.log | grep -o "[0-9]*$")
  if [ -z "$RF" ] || grep -q "Failure\|run error" $OUT/${TAG}_refit.log; then echo "$TAG refit FAILED"; return; fi
  echo "$TAG refit result $RF"
  "$R" run_with_curves.R "$LIB" $MS $RF $CR $PR "$TAG Run 1.7.0" > $OUT/${TAG}_v170.log 2>&1; echo "$TAG model exit $? $(grep -o '==== model result [0-9]*' $OUT/${TAG}_v170.log)"; }
zone P27 22 26 2 24
zone P28 41 42 40 43
"$R" build_zone.R "P29 Northern Sierran Foothills and Tuscan Flows" P29 1 "$D/lucasFireC2F P29.ssim" 18 > $OUT/P29.log 2>&1; echo "P29 exit $? $(grep -o 'RESULTS P29.*' $OUT/P29.log)"
