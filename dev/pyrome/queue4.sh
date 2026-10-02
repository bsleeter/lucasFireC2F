#!/usr/bin/env bash
# foothills with ONE pooled spread-day curve (1.7.0 stack otherwise): refit C5 on the existing seed runs, then the models. P29 first, then P28.
R="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"; D="F:/lucas-master/Models/lucasFireC2F Dev"; LIB="$D/lucasFireC2F Sierra Nevada & Foothills.ssim"; OUT=/f/lucasFireC2F/dev/pyrome/zones
cd /f/lucasFireC2F/dev/pyrome
zone() { TAG=$1; CS=$2; CR=$3; PR=$4; MS=$5
  "$R" refit_c5.R "$LIB" "$TAG" $CS $CR $PR NA "One curve for all fires" > "$OUT/${TAG// /_}_refit.log" 2>&1
  RF=$(grep -o "==== refit result [0-9]*" "$OUT/${TAG// /_}_refit.log" | grep -o "[0-9]*$")
  if [ -z "$RF" ] || grep -q "Failure\|run error" "$OUT/${TAG// /_}_refit.log"; then echo "$TAG refit FAILED"; return; fi
  echo "$TAG refit result $RF"
  "$R" run_with_curves.R "$LIB" $MS $RF $CR $PR "$TAG Run" > "$OUT/${TAG// /_}_run.log" 2>&1; echo "$TAG model exit $? $(grep -o '==== model result [0-9]*' "$OUT/${TAG// /_}_run.log")"; }
zone "P29 pooled" 65 66 64 67
zone "P28 pooled" 41 42 40 43
