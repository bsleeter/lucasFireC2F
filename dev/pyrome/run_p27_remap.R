# P27 rerun after the engine fuel remaps (TL2->TL3, TL3->TL5, TL4->TL5, GS2->GS1 in the library's Fuel Types sheet):
# calibration (scenario 3) with a 1,500-season seed run, then the model run (scenario 8) on that calibration. Prints the key log
# lines of each; the user's scenarios are run in place (as in Studio), results appear as new result scenarios.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
LIB <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim"
L <- suppressWarnings(ssimLibrary(LIB)); P <- rsyncrosim::project(L, 1)
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
logs <- function(R, pat) { lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg)
  cat(unique(lg[grepl(pat, lg)]), sep = "\n") }
PAT <- "sent to C2F-W|target:|Human|Lightning|spread-day distribution|KS|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Failure|Error|error|Total time: [0-9:]+$"

# optional argument: an existing calibration result id -> skip the calibration and run the model on it
a <- commandArgs(TRUE)
if (length(a)) { CR <- rsyncrosim::scenario(L, as.integer(a[1])); cat("model run only, on calibration result", scenarioId(CR), "\n") } else {
CAL <- rsyncrosim::scenario(L, 3)
rc <- datasheet(CAL, "lucasFireC2F_RunControl"); rc$MaximumIteration <- 1500L; saveDatasheet(CAL, rc, "lucasFireC2F_RunControl")
cat("calibration: seed run set to", datasheet(CAL, "lucasFireC2F_RunControl")$MaximumIteration, "seasons\n")
t0 <- Sys.time(); tryCatch(run(CAL), error = function(e) cat("run error:", conditionMessage(e), "\n")); CR <- newest()
cat(sprintf("\n==== calibration result %d (%.1f min)\n", scenarioId(CR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(CR, PAT) }
if (!nrow(datasheet(CR, "lucasFireC2F_SpreadDays"))) stop("calibration wrote no spread days - stopping before the model run")

RUN <- rsyncrosim::scenario(L, 8); cat("\nmodel run dependencies before:\n"); print(dependency(RUN))
dependency(RUN) <- c(scenarioId(CR), 2L)   # rsyncrosim 2.1.13: the setter replaces the whole list, in priority order
cat("model run dependencies now:\n"); print(dependency(RUN))
t0 <- Sys.time(); tryCatch(run(RUN), error = function(e) cat("run error:", conditionMessage(e), "\n")); RR <- newest()
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(RR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(RR, PAT)
cat(sprintf("\nRESULTS calibration %d model %d\n", scenarioId(CR), scenarioId(RR)))
