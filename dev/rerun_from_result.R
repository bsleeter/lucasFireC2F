# Reruns later stages on a copy of a finished result scenario (its stage 1-5 outputs carried over), e.g. after fixing stage 6.
# Usage: Rscript dev/rerun_from_result.R <result scenario id> "<stage>" ["<stage>" ...]
suppressPackageStartupMessages(library(rsyncrosim))
a <- commandArgs(TRUE); src <- as.integer(a[1]); st <- a[-1]
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
S <- rsyncrosim::scenario(P, sprintf("dev: rerun %s from [%d]", paste(sub(" - .*", "", st), collapse = "+"), src),
                          sourceScenario = rsyncrosim::scenario(L, src), overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = st, RunOrder = seq_along(st)), "core_Pipeline")
n <- nrow(datasheet(S, "lucasFireC2F_OutputFireStatistic")); cat("carried-over fire statistics rows:", n, "\n")
t0 <- Sys.time(); tryCatch(run(S), error = function(e) NULL)
ids <- rsyncrosim::scenario(P)[, "ScenarioId"]; res <- rsyncrosim::scenario(L, max(ids))
lg <- strsplit(paste(runLog(res), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("burn events|wind|Timber|Grass|Shrub|All |cRPS|Failure|Error|error|Total time: 0", lg, value = TRUE))), sep = "\n")
cat(sprintf("%.1f min\nRESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), max(ids)))
