# =============================================================================
# C3 - Fit Ignitions
# =============================================================================
# Fits the fire population from the historical record over CalibrationPeriod's fire-record years (validated P26 scripts
# 184 and 158), for the fires whose ignition point is in the defined fire zone(s):
#   threshold   the large-fire size: FSim's rule (the zone's MEAN fire size over all recorded fires, 73-acre floor;
#               CalibrationPeriod.MinimumSizeRule 1) or ResampleOption.MinimumFireSize as given (rule 0). Written back to
#               ResampleOption.MinimumFireSize, so resampling, the count and the size target all use the same population.
#   count       fires >= threshold per year, zero years filled: Gamma mean and SD (CalibrationOption.FitIgnitionCount)
#   mix         season x cause shares of those fires; unknown causes split within each season by its human share
#               (CalibrationOption.FitIgnitionDistribution). Season from the discovery month (Season table months).
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); source(file.path(env$PackageDirectory, "src", "calibration.R"))
cp <- need("CalibrationPeriod", c("FireFirstYear", "FireLastYear")); co <- need("CalibrationOption")
H <- need("HistoricalFire", c("Date", "AreaHa")); rso <- read_ds("ResampleOption")
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); se <- as.data.table(datasheet(prj, ds("Season"), lookupsAsFactors = FALSE))
if (any(!is.na(H$FireZone)) && nrow(fzt)) H <- H[FireZone %in% fzt$Name]
H[, yr := as.integer(substr(Date, 1, 4))]
Y <- cp$FireFirstYear:cp$FireLastYear; h <- H[yr %in% Y]
if (!nrow(h)) fail("no historical fires in %d-%d", min(Y), max(Y))
rule <- if (is.character(cp$MinimumSizeRule)) as.integer(grepl("^FSim", cp$MinimumSizeRule)) else as.integer(cp$MinimumSizeRule)
thr <- if (isTRUE(rule == 1)) fsim_threshold(h$AreaHa) else { if (!nrow(rso) || is.na(rso$MinimumFireSize)) fail("give Resampling a minimum fire size, or choose the FSim rule"); rso$MinimumFireSize }
say("%d-%d: %s recorded fires (mean %.1f ha); large-fire threshold %.2f ha (%s)", min(Y), max(Y), format(nrow(h), big.mark = ","), mean(h$AreaHa), thr,
    if (isTRUE(rule == 1)) "FSim rule: mean fire size, 73-ac floor" else "Resampling minimum as given")
px <- if (nrow(rso) && !is.na(rso$ProportionExtraIgnition)) rso$ProportionExtraIgnition else 0.5
write_ds(data.frame(MinimumFireSize = round(thr, 3), ProportionExtraIgnition = px), "ResampleOption")
if (isTRUE(co$FitIgnitionCount)) {
  cf <- count_fit(h$yr, h$AreaHa, thr, Y)
  write_ds(data.frame(DistributionType = "Gamma", Mean = round(cf$mean, 3), DistributionSD = round(cf$sd, 3), DistributionMin = 1), "IgnitionCount")
  say("ignition count: %d fires >= threshold, Gamma mean %.3f SD %.3f per season (range %d-%d)", sum(cf$n), cf$mean, cf$sd, min(cf$n), max(cf$n))
} else say("ignition count kept as given")
if (isTRUE(co$FitIgnitionDistribution)) {
  mix <- cause_mix(season_of(h$Date, se), h$Cause, h$AreaHa, thr)
  write_ds(mix[, c("Season", "Cause", "RelativeLikelihood")], "IgnitionDistribution")
  say("season x cause: %s", paste(sprintf("%s/%s %.3f", mix$Season, mix$Cause, mix$RelativeLikelihood), collapse = "; "))
} else say("ignition distribution kept as given")
