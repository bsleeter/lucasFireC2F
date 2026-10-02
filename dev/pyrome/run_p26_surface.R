# P26 with a refitted ignition surface and NOTHING else changed: a copy of scenario 7 (result 8's setup: stages 1-8,
# 2,500 seasons, the baseline's calibrated count / mix / curve), its IgnitionGrid swapped, stages 1-8 + C6 run.
# Usage: Rscript run_p26_surface.R <tag>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
TG <- commandArgs(TRUE)[1]; IN <- "F:/lucas-master/Models/lucasFireC2F Dev/P26/inputs"
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); P <- rsyncrosim::project(L, 1)
S <- rsyncrosim::scenario(P, sprintf("P26 ignition surface %s", TG), sourceScenario = rsyncrosim::scenario(L, 7), overwrite = TRUE)
saveDatasheet(S, data.frame(Cause = c("Human", "Lightning"), FileName = file.path(IN, sprintf("ign_%s%s.tif", c("human", "lightning"), TG))), "lucasFireC2F_IgnitionGrid")
pl <- datasheet(S, "core_Pipeline"); st <- as.character(pl$StageNameId)
if (!any(grepl("^C6", st))) saveDatasheet(S, data.frame(StageNameId = c(st, "C6 - Score Against Fire Records"), RunOrder = seq_len(length(st) + 1)), "core_Pipeline")
cat("pipeline:", paste(datasheet(S, "core_Pipeline")$StageNameId, collapse = " > "), "\n")
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("seasons:|done in|kept \\d|obs .* sim|TOTAL|Error|error|Failure", lg, value = TRUE))), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
