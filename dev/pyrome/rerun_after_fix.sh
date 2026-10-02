#!/usr/bin/env bash
# after build_three.sh: model-only re-runs of P27 (scenario 24 on calibration 26) and P26 (scenario 35 on calibration 34, prep 32)
# with the stage 3 curve-matching fix
R="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"; LIB="F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim"; OUT=/f/lucasFireC2F/dev/pyrome/zones
cd /f/lucasFireC2F/dev/pyrome
until grep -aq "RESULTS P28\|Execution halted" $OUT/P28.log 2>/dev/null; do sleep 60; done
"$R" run_model.R "$LIB" 24 26 2 > $OUT/P27_rerun.log 2>&1; echo "P27 rerun exit $?"
"$R" run_model.R "$LIB" 35 34 32 > $OUT/P26_rerun.log 2>&1; echo "P26 rerun exit $?"
