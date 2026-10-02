#!/usr/bin/env bash
# after rerun_after_fix.sh: P29 full build (its prep failed on a temp-folder clash), then P28 model only (scenario 43 on calibration 42, prep 40)
R="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"; D="F:/lucas-master/Models/lucasFireC2F Dev"; LIB="$D/lucasFireC2F Sierra Nevada & Foothills.ssim"; OUT=/f/lucasFireC2F/dev/pyrome/zones
cd /f/lucasFireC2F/dev/pyrome
until grep -aq "==== model result\|Execution halted" $OUT/P26_rerun.log 2>/dev/null; do sleep 60; done
"$R" build_zone.R "P29 Northern Sierran Foothills and Tuscan Flows" P29 1 "$D/lucasFireC2F P29.ssim" 18 > $OUT/P29.log 2>&1; echo "P29 exit $?"
"$R" run_model.R "$LIB" 43 42 40 > $OUT/P28_model.log 2>&1; echo "P28 model exit $?"
