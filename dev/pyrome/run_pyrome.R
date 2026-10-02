# Runs a pyrome library end to end: the calibration scenario (C1-C3, 500-season seed run, C5, C6), then the baseline model on
# that calibration through a scenario dependency (stages 1-8 + C6, 2,500 seasons). Prints the key log lines of each.
# Usage: Rscript run_pyrome.R <pyrome number> [calibration result id: skip calibration and use it]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); PY <- as.integer(a[1]); CID <- if (length(a) > 1) as.integer(a[2]) else NA
L <- suppressWarnings(ssimLibrary(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P%02d.ssim", PY)))
P <- rsyncrosim::project(L, 1)
logs <- function(R, pat) { lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]; cat(unique(trimws(grep(pat, lg, value = TRUE))), sep = "\n") }
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
PAT <- "imported|threshold|ignition count|season x cause|stream|spread-day|target|fitted|observed|fires/yr|vs the record|seasons:|SEED|kept|done in|Failure|Error|error|TRANSFORMER"
if (is.na(CID)) {
  C <- rsyncrosim::scenario(P, sprintf("P%02d Calibration 1992-2020", PY)); t0 <- Sys.time()
  tryCatch(run(C), error = function(e) cat("run error:", conditionMessage(e), "\n")); CR <- newest(); CID <- scenarioId(CR)
  cat(sprintf("\n==== calibration result %d (%.1f min)\n", CID, as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(CR, PAT)
} else CR <- rsyncrosim::scenario(L, CID)
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
sd <- g(CR, "SpreadDays"); ic <- g(CR, "IgnitionCount"); ro <- g(CR, "ResampleOption")
if (!nrow(sd) || !nrow(ic)) stop("calibration result has no SpreadDays / IgnitionCount - see its log")
cat(sprintf("calibrated: threshold %.1f ha; Gamma %.2f / %.2f; spread days mean %.2f; stream rows %s\n", ro$MinimumFireSize, ic$Mean, ic$DistributionSD,
            sum(sd$Days * sd$RelativeFrequency) / sum(sd$RelativeFrequency), format(nrow(g(CR, "WeatherStream")), big.mark = ",")))
base <- rsyncrosim::scenario(P, sprintf("P%02d Baseline LF2020", PY))
S <- rsyncrosim::scenario(P, sprintf("P%02d Model on calibration %d", PY, CID), sourceScenario = base, overwrite = TRUE)
for (k in c("SpreadDays", "IgnitionCount", "IgnitionDistribution", "ResampleOption", "WeatherStream")) delete(S, datasheet = paste0("lucasFireC2F_", k), force = TRUE)
dependency(S) <- CR
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n")); R <- newest()
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(R, PAT)
cat(sprintf("RESULT_SCENARIO %d\n", scenarioId(R)))
