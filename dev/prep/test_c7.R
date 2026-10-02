# C7 acceptance: validate P26 result 12 from its own library (a scenario with only C7, result 12 as a dependency) and compare
# with the paper's Table 5 (script 162: 2021-24 0.695 / WRC 0.593, hottest decile 22% / 5%; 1984-2010 0.735 / 0.540; ...).
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F P26.ssim"))); P <- rsyncrosim::project(L, 1)
ds <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); if (!nrow(ds)) ds <- data.frame(MtbsPerimeterFile = NA)
ds$MtbsPerimeterFile <- "F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp"
ds$WrcBurnProbabilityFile <- "F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"
saveDatasheet(P, ds, "lucasFireC2F_DataSource", append = FALSE)
S <- rsyncrosim::scenario(P, "C7 validation of result 12", overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = "C7 - Validate Burn Probability", RunOrder = 1L), "core_Pipeline")
saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = 2500L, MinimumTimestep = 0L, MaximumTimestep = 0L), "lucasFireC2F_RunControl")
saveDatasheet(S, data.frame(Name = c("2021-2024 (after the fuel vintage)", "1984-2010", "2013-2020 (inside the vintage)", "1984-2024 (all)"),
                            FirstYear = c(2021L, 1984L, 2013L, 1984L), LastYear = c(2024L, 2010L, 2020L, 2024L)), "lucasFireC2F_ValidationPeriod")
dependency(S) <- rsyncrosim::scenario(L, 12)
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("zones scored|perimeters|cells scored|AUC|selection|correlation|Failure|Error|error", lg, value = TRUE))), sep = "\n")
cat(sprintf("%.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
print(as.data.table(datasheet(R, "lucasFireC2F_OutputValidation")))
print(as.data.table(datasheet(R, "lucasFireC2F_OutputSelection"))[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
