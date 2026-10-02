# A zone model run with only its ignition grids swapped: a copy of <source scenario> (same stages, calibration, dependencies)
# with IgnitionGrid pointing at <dev>/P<NN>/inputs/ign_{human,lightning}<tag>.tif; then C7 against MTBS and WRC.
# Usage: Rscript run_surface_swap.R <pyrome> <source scenario id> <tag>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); PY <- as.integer(a[1]); SRC <- as.integer(a[2]); TG <- a[3]
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; IN <- file.path(D, sprintf("P%d/inputs", PY))
L <- suppressWarnings(ssimLibrary(file.path(D, sprintf("lucasFireC2F P%d.ssim", PY)))); P <- rsyncrosim::project(L, 1)
ds <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); if (!nrow(ds)) ds <- data.frame(MtbsPerimeterFile = NA_character_)
ds$MtbsPerimeterFile <- "F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp"
ds$WrcBurnProbabilityFile <- "F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"; saveDatasheet(P, ds, "lucasFireC2F_DataSource", append = FALSE)
S <- rsyncrosim::scenario(P, sprintf("P%d ignition surface %s", PY, TG), sourceScenario = rsyncrosim::scenario(L, SRC), overwrite = TRUE)
saveDatasheet(S, data.frame(Cause = c("Human", "Lightning"), FileName = file.path(IN, sprintf("ign_%s%s.tif", c("human", "lightning"), TG))), "lucasFireC2F_IgnitionGrid")
st <- as.character(datasheet(S, "core_Pipeline")$StageNameId); st <- unique(c(setdiff(st, "C7 - Validate Burn Probability"), "C7 - Validate Burn Probability"))
saveDatasheet(S, data.frame(StageNameId = st, RunOrder = seq_along(st)), "core_Pipeline")
saveDatasheet(S, data.frame(Name = c("2021-2024 (after the fuel vintage)", "1984-2010", "1984-2024 (all)"), FirstYear = c(2021L, 1984L, 1984L), LastYear = c(2024L, 2010L, 2024L)), "lucasFireC2F_ValidationPeriod")
cat("pipeline:", paste(st, collapse = " > "), "\n"); print(dependency(S))
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("kept \\d|obs .* sim|AUC|correlation|Failure|Error|error", lg, value = TRUE))), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
