# Runs the P26 baseline scenario through chosen stages (a copy with its pipeline cut down) and prints the run log.
# Usage: Rscript dev/run_stages.R "<stage display name>" ["<stage>" ...]   (library: dev/load_p26.R's)
suppressPackageStartupMessages(library(rsyncrosim))
st <- commandArgs(TRUE); IT <- NA
if (length(st) && grepl("^--iters=", st[1])) { IT <- as.integer(sub("^--iters=", "", st[1])); st <- st[-1] }
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
base <- rsyncrosim::scenario(P, "P26 Baseline LF2020 (1992-2020 calibration)")
nm <- sprintf("dev: %s", paste(sub(" - .*", "", st), collapse = "+"))
S <- rsyncrosim::scenario(P, nm, sourceScenario = base, overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = st, RunOrder = seq_along(st)), "core_Pipeline")
if (!is.na(IT)) { rc <- datasheet(S, "lucasFireC2F_RunControl"); rc$MaximumIteration <- IT; saveDatasheet(S, rc, "lucasFireC2F_RunControl")
  nm <- paste(nm, sprintf("(%d seasons)", IT)) }
t0 <- Sys.time(); R <- tryCatch(run(S), error = function(e) NULL)
cat(sprintf("\n%s: %.1f min\n", nm, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
ids <- rsyncrosim::scenario(P)[, "ScenarioId"]; res <- rsyncrosim::scenario(L, max(ids))
lg <- strsplit(paste(runLog(res), collapse = "\n"), "\n")[[1]]
cat(grep("^(RunLog|SyncroSim|Operating|Packages|core ->|lucasFireC2F ->|Parent|Result|\\[Warning\\] The package|This Library|To enable conda|-+|\\s*)$", lg, value = TRUE, invert = TRUE), sep = "\n")
cat(sprintf("RESULT_SCENARIO %d\n", max(ids)))
