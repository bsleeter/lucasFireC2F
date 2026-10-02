# Refit spread days (C5 only) on an existing calibration result's seed run - no new seed run needed (1.7.0: vegetation-group
# fall-back, MinimumFiresPerCurve). New scenario "<tag> Refit spread days" = copy of the zone's calibration scenario with pipeline C5,
# depending on the calibration result (seed run fires + fitted sheets) and the prep result.
# Usage: Rscript refit_c5.R "<library>" <tag> <calibration scenario> <calibration result> <prep result> [minimum fires per curve]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); TAG <- a[2]; ids <- as.integer(a[3:5])
vc <- datasheet(P, "lucasFireC2F_VegetationClass", optional = TRUE); print(vc[, c("Name", "FallbackGroup")])
S <- rsyncrosim::scenario(P, paste(TAG, "Refit spread days"), sourceScenario = rsyncrosim::scenario(L, ids[1]), overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = "C5 - Fit Spread Days", RunOrder = 1L), "core_Pipeline")
co <- datasheet(S, "lucasFireC2F_CalibrationOption", optional = TRUE, lookupsAsFactors = FALSE)
if (length(a) > 5 && a[6] != "NA") co$MinimumFiresPerCurve <- as.integer(a[6])
if (length(a) > 6) co$SpreadDaysBy <- a[7]
if (length(a) > 5) saveDatasheet(S, co, "lucasFireC2F_CalibrationOption")
dependency(S) <- ids[2:3]
cat(sprintf("refit scenario %d, depends on %s, SpreadDaysBy %s, MinimumFiresPerCurve %s\n", scenarioId(S), paste(dependency(S)$ScenarioId, collapse = ", "),
            co$SpreadDaysBy, co$MinimumFiresPerCurve))
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg)
cat(sprintf("\n==== refit result %d (%.1f min)\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat(unique(lg[grepl("fitted by|a curve needs|too few|group curve|recorded fires, mean|Failure|Error|error", lg)]), sep = "\n")
sd <- as.data.table(datasheet(R, "lucasFireC2F_SpreadDays", optional = TRUE, lookupsAsFactors = FALSE))
print(sd[, .(meanDays = round(sum(Days * RelativeFrequency) / sum(RelativeFrequency), 2)), by = .(Cause, Vegetation)])
