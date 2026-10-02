# Builds and runs a merge library: zone results combined by stage M, then stages 5-8 on the union (no fire is regrown).
# Project lists = the union of the zone libraries' Fire Zones and Weather Zones; options copied from the first zone's baseline.
# Usage: Rscript build_merge.R <out library> <lib1> <result id1> <lib2> <result id2> [...]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); OUT <- a[1]; src <- matrix(a[-1], ncol = 2, byrow = TRUE)
rd <- function(lib, sid, sheet, prj = FALSE) { L <- suppressWarnings(ssimLibrary(lib)); x <- if (prj) rsyncrosim::project(L, 1) else rsyncrosim::scenario(L, as.integer(sid))
  as.data.table(datasheet(x, paste0("lucasFireC2F_", sheet), lookupsAsFactors = FALSE)) }
fz <- unique(rbindlist(lapply(seq_len(nrow(src)), function(i) rd(src[i, 1], NA, "FireZone", TRUE))), by = "Name")
wz <- unique(rbindlist(lapply(seq_len(nrow(src)), function(i) rd(src[i, 1], NA, "WeatherZone", TRUE))), by = "Name")
if (anyDuplicated(wz$ID)) stop("weather zone IDs collide across the zone libraries")
L <- suppressWarnings(ssimLibrary(OUT, packages = "lucasFireC2F", overwrite = TRUE)); P <- rsyncrosim::project(L, "Merged zones")
saveDatasheet(P, as.data.frame(fz[, .(Name, ID, Description)]), "lucasFireC2F_FireZone"); saveDatasheet(P, as.data.frame(wz[, .(Name, ID, Latitude)]), "lucasFireC2F_WeatherZone")
S <- rsyncrosim::scenario(P, sprintf("Merge %s", paste(fz$Name, collapse = " + ")))
saveDatasheet(S, data.frame(StageNameId = c("M - Merge Zone Results", "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk", "8 - Fire Detail (optional)"), RunOrder = 1:5), "core_Pipeline")
NS <- rd(src[1, 1], src[1, 2], "RunControl")$MaximumIteration
saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = NS, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(S, data.frame(LibraryFileName = src[, 1], SourceScenarioId = as.integer(src[, 2])), "lucasFireC2F_MergeSource")
for (sh in c("EffectsOption", "WeatherOption", "RiskOption", "FireDetailOption", "OutputOption", "SeverityTarget")) {
  x <- rd(src[1, 1], src[1, 2], sh); x <- x[, setdiff(names(x), c(paste0(sh, "Id"), "ScenarioId")), with = FALSE]
  if (nrow(x)) saveDatasheet(S, as.data.frame(x), paste0("lucasFireC2F_", sh)) }
cat(sprintf("merge library %s: %d fire zones, %d weather zones, %d sources, %d seasons\n", OUT, nrow(fz), nrow(wz), nrow(src), NS))
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("source \\d|union|overlapping|merged|wind ratio|seasons:|burned cell|burn events|wind:|risk|cRPS|severity|Failure|Error|error", lg, value = TRUE))), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
