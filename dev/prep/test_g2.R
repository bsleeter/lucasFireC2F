# G2: P2 (FBFM40 fuel groups) for P26 and P29 in the prep test library, each depending on its P1 prep result; compared with the
# adopted surfaces (185, legacy fuel classes, _pNNNL405b20 masked into <dev>/P<NN>/inputs): cross-validated AUC, cell correlation,
# and placement against the recorded large-fire origins (score_ignition_surfaces.R's metrics).
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; M <- "F:/lucas-master/Models/California 30m BurnP3"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F prep test.ssim"))); P <- rsyncrosim::project(L, 1)
ds <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); ds$IgnitionCovariateFolder <- file.path(M, "spatial/ignition_covariates")
saveDatasheet(P, ds, "lucasFireC2F_DataSource", append = FALSE)
sc <- as.data.table(rsyncrosim::scenario(P)); prep <- sc[IsResult == "Yes" & grepl("prep", Name)]
for (py in c(26, 29)) {
  pr <- prep[grepl(sprintf("^P%d prep", py), Name)][order(-ScenarioId)][1]
  S <- rsyncrosim::scenario(P, sprintf("P%d ignitions", py), overwrite = TRUE)
  saveDatasheet(S, data.frame(StageNameId = "P2 - Fit Ignition Surfaces", RunOrder = 1L), "core_Pipeline")
  saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = 1L, MinimumTimestep = 0L, MaximumTimestep = 0L), "lucasFireC2F_RunControl")
  dependency(S) <- rsyncrosim::scenario(L, pr$ScenarioId)
  t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
  R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
  lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
  cat(sprintf("\n==== P%d (%.1f min, result %d)\n", py, as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
  cat(unique(trimws(grep("fit domain|presences|background|cross-validated|surface:|Failure|Error|error", lg, value = TRUE))), sep = "\n")
  ig <- as.data.table(datasheet(R, "lucasFireC2F_IgnitionGrid", lookupsAsFactors = FALSE)); if (!nrow(ig)) next
  for (cz in c("Human", "Lightning")) { a <- rast(ig[Cause == cz]$FileName[1]); b <- rast(file.path(D, sprintf("P%d/inputs/ign_%s_p%03dL405b20.tif", py, tolower(cz), py)))
    va <- values(a, mat = FALSE); vb <- values(b, mat = FALSE); k <- va > 0 | vb > 0
    cat(sprintf("  %-9s vs adopted surface: same grid %s; correlation of shares %.3f; rank correlation %.3f\n", cz, compareGeom(a, b, stopOnError = FALSE),
                cor(va[k] / sum(va[k]), vb[k] / sum(vb[k])), cor(va[k], vb[k], method = "spearman"))) }
  for (cz in c("Human", "Lightning")) file.copy(ig[Cause == cz]$FileName[1], file.path(D, sprintf("P%d/inputs/ign_%s_p%03dP2.tif", py, tolower(cz), py)), overwrite = TRUE)
}
