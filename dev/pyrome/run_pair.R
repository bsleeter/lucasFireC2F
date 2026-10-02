# Run an existing calibration scenario, then an existing model scenario on its new result (dependencies: that result, then the
# prep result). Prints the key log lines. Usage: Rscript run_pair.R "<library>" <calibration scenario id> <model scenario id> <prep result id>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1)
CID <- as.integer(a[2]); MID <- as.integer(a[3]); PREP <- as.integer(a[4])
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
logs <- function(R, pat) { lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg); cat(unique(lg[grepl(pat, lg)]), sep = "\n") }
PAT <- "recent-fire|Recent Fire|fitted by|too few|recorded fires, mean|spread-day distribution|spread days,|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Failure|Error|error|Total time: [0-9:]+$"
t0 <- Sys.time(); tryCatch(run(rsyncrosim::scenario(L, CID)), error = function(e) cat("run error:", conditionMessage(e), "\n")); CR <- newest()
cat(sprintf("\n==== calibration result %d (%.1f min)\n", scenarioId(CR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(CR, PAT)
if (!nrow(datasheet(CR, "lucasFireC2F_SpreadDays"))) stop("calibration wrote no spread days - stopping")
M <- rsyncrosim::scenario(L, MID); dependency(M) <- c(scenarioId(CR), PREP); cat("\nmodel dependencies:", paste(dependency(M)$ScenarioId, collapse = ", "), "\n")
t0 <- Sys.time(); tryCatch(run(M), error = function(e) cat("run error:", conditionMessage(e), "\n")); RR <- newest()
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(RR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(RR, PAT)
cat(sprintf("\nRESULTS calibration %d model %d\n", scenarioId(CR), scenarioId(RR)))
