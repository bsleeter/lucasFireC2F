# Test: a merge scenario inside a zone library (P29), its own result given with a BLANK library path, P26 from its library.
# P29's project predates the statewide defaults: the first run must stop with the missing-zone message; then only the missing
# names are APPENDED (existing rows untouched, so saved references keep their keys) and the merge runs from the same library.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F P29.ssim"))); P <- rsyncrosim::project(L, 1)
S <- rsyncrosim::scenario(P, "Merge P29 (this library) + P26", overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = c("M - Merge Zone Results", "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk", "8 - Fire Detail (optional)"), RunOrder = 1:5), "core_Pipeline")
saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = 2500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(S, data.frame(LibraryFileName = c(NA, file.path(D, "lucasFireC2F P26.ssim")), SourceScenarioId = c(12L, 12L)), "lucasFireC2F_MergeSource")
tryRun <- function() { tryCatch(run(S), error = function(e) NULL); R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
  lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]; cat(unique(trimws(grep("source \\d|lack|union|merged:|seasons:|Failure|Error", lg, value = TRUE))), sep = "\n"); R }
cat("== run 1 (P29 project without P26's zones)\n"); R1 <- tryRun()
def <- function(n) read.csv(file.path("F:/lucasFireC2F/defaults", paste0(n, ".csv")))
for (n in c("FireZone", "WeatherZone")) { have <- datasheet(P, paste0("lucasFireC2F_", n))$Name; add <- def(n)[!def(n)$Name %in% have, ]
  if (nrow(add)) saveDatasheet(P, add, paste0("lucasFireC2F_", n), append = TRUE); cat(sprintf("%s: appended %d missing rows (had %d)\n", n, nrow(add), length(have))) }
cat("== run 2\n"); t0 <- Sys.time(); R2 <- tryRun(); cat(sprintf("%.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
a <- rast(datasheet(R2, "lucasFireC2F_OutputBurnCount")$FileName[1])
b <- rast(datasheet(rsyncrosim::scenario(suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F P26+P29.ssim"))), 2), "lucasFireC2F_OutputBurnCount")$FileName[1])
cat(sprintf("burn count vs the P26-first merge: total %s vs %s; same grid %s\n", format(global(a, "sum", na.rm = TRUE)[1, 1], big.mark = ","), format(global(b, "sum", na.rm = TRUE)[1, 1], big.mark = ","), compareGeom(a, b, stopOnError = FALSE)))
