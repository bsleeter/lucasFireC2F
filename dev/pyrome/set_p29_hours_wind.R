# P29 second calibration: 1 burning hour per spread day (the 2 h day cannot make the record's 30-100 ha fires in the foothills)
# and P29's own RAWS burning-period wind table, in the baseline and the calibration scenario alike.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P29.ssim")); P <- rsyncrosim::project(L, 1)
wr <- fread("F:/lucas-master/Models/California 30m BurnP3/data/wind/p029_raws_ratio_quantiles.csv")
for (nm in c("P29 Baseline LF2020", "P29 Calibration 1992-2020")) { S <- rsyncrosim::scenario(P, nm)
  saveDatasheet(S, data.frame(Hours = 1L), "lucasFireC2F_BurningHours")
  saveDatasheet(S, as.data.frame(wr[, .(LowerWindSpeed = lo, UpperWindSpeed = ifelse(is.finite(hi), hi, NA), Quantile = p, Ratio = ratio)]), "lucasFireC2F_WindRatio")
  cat(sprintf("%s: burning hours %d; wind ratio rows %d\n", nm, datasheet(S, "lucasFireC2F_BurningHours")$Hours, nrow(datasheet(S, "lucasFireC2F_WindRatio")))) }
