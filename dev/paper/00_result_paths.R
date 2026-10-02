# Writes the output file paths and key tables of a lucasFireC2F result scenario to dev/paper/result_<id>.rds for the paper scripts
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
id <- as.integer(commandArgs(TRUE)[1]); OUT <- "F:/lucasFireC2F/dev/paper"
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); S <- rsyncrosim::scenario(L, id)
P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
g <- function(n, x = S) as.data.table(datasheet(x, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
code <- function(sheet, d) { m <- g(sheet, P); d[, Code := m$Code[match(Metric, m$Name)]]; d }
R <- list(id = id,
  bp = g("OutputBurnProbability")$FileName[1], bc = g("OutputBurnCount")$FileName[1],
  effects = code("EffectsMetric", g("OutputFireEffects")), risk = code("RiskMetric", g("OutputRisk")),
  severity = code("SeverityMetric", g("OutputSeverityMap")), engine = g("OutputEngineLandscape"),
  landscape = g("LandscapeRasters"), canopy = g("CanopyRasters"), burn_table = g("OutputBurnTable")$FileName[1],
  burn_effects = g("OutputBurnEffects")$FileName[1], perimeters = g("OutputFirePerimeter")$FileName[1],
  fire_stat = g("OutputFireStatistic"), fire_list = g("OutputFireList"), effects_summary = g("OutputEffectsSummary"),
  severity_fit = g("OutputSeverityFit"), severity_class = g("SeverityClass"), run_control = g("RunControl"),
  resample = g("ResampleOption"), spread_days = g("SpreadDays"), ignition_count = g("IgnitionCount"),
  ignition_dist = g("IgnitionDistribution"), effects_option = g("EffectsOption"), growth_option = g("GrowthOption"),
  fuel_type = g("FuelType", P), lifeform = g("Lifeform", P), log = strsplit(paste(runLog(S), collapse = "\n"), "\n")[[1]])
saveRDS(R, file.path(OUT, sprintf("result_%d.rds", id)))
cat("saved", file.path(OUT, sprintf("result_%d.rds", id)), "\n"); str(R[c("bp", "burn_table", "burn_effects", "perimeters")])
cat(grep("Total time|done in|kept", R$log, value = TRUE), sep = "\n")
