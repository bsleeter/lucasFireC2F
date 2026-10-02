# Model run with spread-day curves taken from another result (e.g. a C5 refit), set LOCALLY on a copy of the model scenario (local rows
# override dependencies), dependencies unchanged otherwise. Usage: Rscript run_with_curves.R "<library>" <model scenario> <curves result> <calibration result> <prep result> "<new name>"
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); ids <- as.integer(a[2:5])
M <- rsyncrosim::scenario(P, a[6], sourceScenario = rsyncrosim::scenario(L, ids[1]), overwrite = TRUE)
sd <- datasheet(rsyncrosim::scenario(L, ids[2]), "lucasFireC2F_SpreadDays", optional = TRUE, lookupsAsFactors = FALSE)
saveDatasheet(M, sd, "lucasFireC2F_SpreadDays", append = FALSE); dependency(M) <- ids[3:4]
cat(sprintf("model scenario %d '%s': %d curve rows from result %d, depends on %s\n", scenarioId(M), a[6], nrow(sd), ids[2], paste(dependency(M)$ScenarioId, collapse = ", ")))
t0 <- Sys.time(); tryCatch(run(M), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg)
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat(unique(lg[grepl("recent-fire|Recent Fire|spread days,|replacement within|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Failure|Error|error", lg)]), sep = "\n")
