# Stage M in the zones' own library: a scenario "<name>" with pipeline M, 5, 6, 7, 8 merging results of this library
# (MergeSource.LibraryFileName blank). Options are filled by stage M from source 1. Usage: Rscript run_merge_samelib.R "<library>" "<name>" <result id> ...
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); ids <- as.integer(a[-(1:2)])
S <- rsyncrosim::scenario(P, a[2])
saveDatasheet(S, data.frame(StageNameId = c("M - Merge Zone Results", "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk", "8 - Fire Detail (optional)"), RunOrder = 1:5), "core_Pipeline")
NS <- datasheet(rsyncrosim::scenario(L, ids[1]), "lucasFireC2F_RunControl")$MaximumIteration
saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = NS, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(S, data.frame(LibraryFileName = NA_character_, SourceScenarioId = ids), "lucasFireC2F_MergeSource")
cat(sprintf("merge scenario %d '%s': sources %s, %d seasons\n", scenarioId(S), a[2], paste(ids, collapse = ", "), NS))
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- sub(" +$", "", strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]])
cat(unique(grep("source [0-9]|union|overlap|merged|wind ratio|seasons:|burned cell|burn events|wind:|risk|cRPS|severity|filled|Failure|Error|error", lg, value = TRUE)), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
