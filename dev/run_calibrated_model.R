# A model run on the package's own calibration: a copy of the baseline scenario with the calibrated sheets EMPTIED and the
# calibration result scenario added as a DEPENDENCY (SyncroSim's way to take another scenario's datasheets), then
# stages 1-5 + C6 over 2,500 seasons. Acceptance: sizes ~1.0x the 1992-2020 record by class.
# Usage: Rscript dev/run_calibrated_model.R <calibration result scenario id>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
cid <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
base <- rsyncrosim::scenario(P, "P26 Baseline LF2020 (1992-2020 calibration)"); CR <- rsyncrosim::scenario(L, cid)
S <- rsyncrosim::scenario(P, "P26 Model on package calibration", sourceScenario = base, overwrite = TRUE)
cal <- c("SpreadDays", "IgnitionCount", "IgnitionDistribution", "ResampleOption", "WeatherStream")
for (k in cal) delete(S, datasheet = paste0("lucasFireC2F_", k), force = TRUE)
dependency(S) <- CR
got <- sapply(cal, function(k) nrow(datasheet(S, paste0("lucasFireC2F_", k))))
cat("calibrated sheets seen through the dependency:", paste(names(got), got, collapse = ", "), "\n")
sd <- as.data.table(datasheet(S, "lucasFireC2F_SpreadDays")); cat(sprintf("spread days via dependency: mean %.2f\n", sum(sd$Days * sd$RelativeFrequency) / sum(sd$RelativeFrequency)))
saveDatasheet(S, data.frame(StageNameId = c("1 - Prepare Landscape", "2 - Sample Ignitions", "3 - Sample Burning Conditions", "4 - Grow Fires (C2F-W)",
                                            "5 - Summarize Burn Probability", "C6 - Score Against Fire Records"), RunOrder = 1:6), "core_Pipeline")
t0 <- Sys.time(); tryCatch(run(S), error = function(e) NULL)
ids <- rsyncrosim::scenario(P)[, "ScenarioId"]; R <- rsyncrosim::scenario(L, max(ids))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("spread days:|seasons:|done in|kept|vs the record|fires/yr|Failure|Error|error", lg, value = TRUE))), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), max(ids)))
