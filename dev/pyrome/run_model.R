# Run an existing model scenario on a given calibration result (dependencies: calibration result, then prep result), optionally
# setting its extra-ignition pool. Usage: Rscript run_model.R "<library>" <model scenario> <calibration result> <prep result> [pool proportion]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); ids <- as.integer(a[2:4])
M <- rsyncrosim::scenario(L, ids[1]); dependency(M) <- ids[2:3]
if (length(a) > 4) { ro <- datasheet(M, "lucasFireC2F_ResampleOption", optional = TRUE); if (!nrow(ro)) ro <- data.frame(MinimumFireSize = NA_real_)
  ro$ProportionExtraIgnition <- as.numeric(a[5]); saveDatasheet(M, ro, "lucasFireC2F_ResampleOption") }
cat("dependencies:", paste(dependency(M)$ScenarioId, collapse = ", "), "| resample:", paste(unlist(datasheet(M, "lucasFireC2F_ResampleOption", optional = TRUE)), collapse = " / "),
    "| seasons:", datasheet(M, "lucasFireC2F_RunControl")$MaximumIteration, "\n")
t0 <- Sys.time(); tryCatch(run(M), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg)
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat(unique(lg[grepl("recent-fire|Recent Fire|spread days,|replacement within|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Failure|Error|error", lg)]), sep = "\n")
