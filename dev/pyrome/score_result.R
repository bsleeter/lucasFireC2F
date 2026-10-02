# Score an existing model result with C6 and C7 only (no fire regrown): a copy of the model scenario running C6 + C7, depending on
# the result, its calibration result and the prep result. Usage: Rscript score_result.R "<library>" <model scenario> <result> <calibration result> <prep result>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); ids <- as.integer(a[2:5])
S <- rsyncrosim::scenario(P, sprintf("Score result %d", ids[2]), sourceScenario = rsyncrosim::scenario(L, ids[1]), overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = c("C6 - Score Against Fire Records", "C7 - Validate Burn Probability"), RunOrder = 1:2), "core_Pipeline")
dependency(S) <- ids[2:4]; cat("depends on:", paste(dependency(S)$ScenarioId, collapse = ", "), "\n")
tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg)
cat(unique(lg[grepl("TOTAL|fires/yr|by cause|by origin|AUC|hottest|correlation|Failure|Error", lg)]), sep = "\n"); cat("score result", scenarioId(R), "\n")
