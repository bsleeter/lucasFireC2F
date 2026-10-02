# Stage 6 (refactored) + stage 8 on a copy of a finished result; checks stage 8's per-burn table aggregates to stage 6's maps.
# Usage: Rscript dev/run_stage8.R <result scenario id> [fit=1] [severity seasons=10]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra); library(arrow); library(sf) })
a <- commandArgs(TRUE); src <- as.integer(a[1]); FIT <- if (length(a) > 1) a[2] == "1" else TRUE; NSEV <- if (length(a) > 2) as.integer(a[3]) else 10L
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
S <- rsyncrosim::scenario(P, sprintf("dev: stage 6+8 from [%d]", src), sourceScenario = rsyncrosim::scenario(L, src))
saveDatasheet(S, data.frame(StageNameId = c("6 - Fire Effects", "8 - Fire Detail (optional)"), RunOrder = 1:2), "core_Pipeline")
saveDatasheet(S, data.frame(BurnEffectsTable = TRUE, Perimeters = TRUE, SeverityMapSeasons = NSEV, FitSeverityClasses = FIT), "lucasFireC2F_FireDetailOption")
t0 <- Sys.time(); tryCatch(run(S), error = function(e) NULL)
ids <- rsyncrosim::scenario(P)[, "ScenarioId"]; R <- rsyncrosim::scenario(L, max(ids))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("burn events|thresholds|model |per-burn|perimeters|severity maps|Failure|Error|error|Total time: 0", lg, value = TRUE))), sep = "\n")
cat(sprintf("%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), max(ids)))
g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
be <- as.data.table(arrow::read_parquet(g("OutputBurnEffects")$FileName[1], col_select = c("Cell", "FlameLengthFt", "FireType")))
fx <- g("OutputFireEffects"); cfl <- values(rast(fx[Metric == "CFL"]$FileName[1]), mat = FALSE); pc <- values(rast(fx[Metric == "PCROWN"]$FileName[1]), mat = FALSE)
a8 <- be[, .(n = .N, cfl = mean(FlameLengthFt), pc = mean(FireType >= 2)), by = Cell][n >= 10]
cat(sprintf("stage 8 table aggregated vs stage 6 maps (cells >= 10 burns: %d): max |CFL diff| %.3f ft (table rounds to 0.01), max |P(crown) diff| %.2e\n",
            nrow(a8), max(abs(a8$cfl - cfl[a8$Cell])), max(abs(a8$pc - pc[a8$Cell]))))
pf <- st_read(g("OutputFirePerimeter")$FileName[1], quiet = TRUE); st <- g("OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned")]
cat(sprintf("perimeters %d (kept fires %d); polygon area / fire area: median %.3f\n", nrow(pf), nrow(st), median(as.numeric(st_area(pf)) / 1e4 / pf$AreaHa)))
print(g("OutputSeverityFit")[, .(FuelGroup, SeverityClass, Target = round(Target, 3), Model = round(Model, 3))])
cat("severity maps:", nrow(g("OutputSeverityMap")), "\n")
