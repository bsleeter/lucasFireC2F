#!/usr/bin/env bash
# P26, P29, P28 in turn with build_zone.R (Sierra Nevada & Foothills library); one log per zone in p27_diag/../zones
R="/c/Program Files/R/R-4.6.1/bin/Rscript.exe"; D="F:/lucas-master/Models/lucasFireC2F Dev"; OUT=/f/lucasFireC2F/dev/pyrome/zones; mkdir -p $OUT
cd /f/lucasFireC2F/dev/pyrome
"$R" build_zone.R "P26 Northern Sierra Nevada" P26 2 "$D/lucasFireC2F P26.ssim" 17 > $OUT/P26.log 2>&1; echo "P26 exit $?"
"$R" build_zone.R "P29 Northern Sierran Foothills and Tuscan Flows" P29 1 "$D/lucasFireC2F P29.ssim" 18 > $OUT/P29.log 2>&1; echo "P29 exit $?"
"$R" build_zone.R "P28 Southern Sierran and Tehachapi Foothills" P28 1 "$D/lucasFireC2F P29.ssim" 18 > $OUT/P28.log 2>&1; echo "P28 exit $?"
