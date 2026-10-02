# Test: a merge set up the way a user would in Studio - a new library, one scenario, stages M + 5-8, Merge Sources only.
# Nothing else is filled: zone lists come from the package defaults, option sheets from source 1 via stage M.
# Then compares the burn count with the earlier merge (P26+P29 library, result 2): must be identical.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F merge test.ssim"), packages = "lucasFireC2F", overwrite = TRUE)); P <- rsyncrosim::project(L, "California")
cat(sprintf("new project lists: %d fire zones, %d weather zones\n", nrow(datasheet(P, "lucasFireC2F_FireZone")), nrow(datasheet(P, "lucasFireC2F_WeatherZone"))))
S <- rsyncrosim::scenario(P, "Merge P26 + P29 (defaults)")
saveDatasheet(S, data.frame(StageNameId = c("M - Merge Zone Results", "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk", "8 - Fire Detail (optional)"), RunOrder = 1:5), "core_Pipeline")
saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = 2500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(S, data.frame(LibraryFileName = file.path(D, c("lucasFireC2F P26.ssim", "lucasFireC2F P29.ssim")), SourceScenarioId = c(12L, 12L)), "lucasFireC2F_MergeSource")
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("taken from source|differs|lack|union|overlapping cells|merged:|seasons:|Failure|Error|error", lg, value = TRUE))), sep = "\n")
cat(sprintf("%.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
for (sh in c("EffectsOption", "RiskOption", "FireDetailOption", "SeverityTarget")) cat(sh, "rows in result:", nrow(datasheet(R, paste0("lucasFireC2F_", sh))), "\n")
a <- rast(datasheet(R, "lucasFireC2F_OutputBurnCount")$FileName[1])
b <- rast(datasheet(rsyncrosim::scenario(suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F P26+P29.ssim"))), 2), "lucasFireC2F_OutputBurnCount")$FileName[1])
d <- values(a, mat = FALSE) - values(b, mat = FALSE); cat(sprintf("burn count vs the earlier merge: same grid %s; cells differing %d\n", compareGeom(a, b, stopOnError = FALSE), sum(d != 0, na.rm = TRUE)))
