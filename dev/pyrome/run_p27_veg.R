# 1.6.0 test: spread days by cause and origin vegetation for P27, on top of the die-off setup (result 19's scenarios 16 / 18).
#   1 Vegetation Classes rows into each library's project (default records only reach new projects); schema check
#   2 Data Sources BpsFile (SN&F library)
#   3 "P27 Prep vegetation": copy of P27 Prep (1) running P1 only -> the vegetation map
#   4 "P27 Calibrate by vegetation": copy of 16 with the vegetation map and SpreadDaysBy = 3; "P27 Run by vegetation": copy of 18 on it
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; BPS <- "F:/lucas-master/Data Sources/BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/Tif/LF2020_BPS_CONUS.tif"
VC <- data.frame(Name = c("Conifer", "Hardwood", "Shrub", "Grass", "Other"), ID = 1:5,
                 Description = c("BpS Conifer, Conifer-Hardwood", "BpS Hardwood, Hardwood-Conifer", "BpS Shrubland", "BpS Grassland, Savanna", "BpS Riparian, Sparse, Peatland and the rest"))
for (n in c("lucasFireC2F Sierra Nevada & Foothills", "lucasFireC2F P26", "lucasFireC2F P29")) {
  L <- suppressWarnings(ssimLibrary(file.path(D, paste0(n, ".ssim")))); P <- rsyncrosim::project(L, 1)
  v <- datasheet(P, "lucasFireC2F_VegetationClass"); if (!nrow(v)) saveDatasheet(P, VC, "lucasFireC2F_VegetationClass")
  s1 <- rsyncrosim::scenario(L, min(rsyncrosim::scenario(P)[, "ScenarioId"]))
  cat(sprintf("%s: package %s; Vegetation Classes %d rows; SpreadDays cols: %s\n", n, paste(as.data.frame(packages(L))[2, c("version")], collapse = ""),
              nrow(datasheet(P, "lucasFireC2F_VegetationClass")), paste(names(datasheet(s1, "lucasFireC2F_SpreadDays", optional = TRUE)), collapse = ","))) }

L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim"))); P <- rsyncrosim::project(L, 1)
ds <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); ds$BpsFile <- BPS; saveDatasheet(P, ds, "lucasFireC2F_DataSource", append = FALSE)
cat("Data Sources BpsFile:", datasheet(P, "lucasFireC2F_DataSource", optional = TRUE)$BpsFile, "\n")
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
logs <- function(R, pat) { lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg); cat(unique(lg[grepl(pat, lg)]), sep = "\n") }
PAT <- "vegetation|fitted by|Any|Human /|Lightning /|spread days,|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|too few|Failure|Error|error|Total time: [0-9:]+$"

S <- rsyncrosim::scenario(P, "P27 Prep vegetation", sourceScenario = rsyncrosim::scenario(L, 1), overwrite = TRUE)
saveDatasheet(S, data.frame(StageNameId = "P1 - Prepare Zone Landscape", RunOrder = 1L), "core_Pipeline")
tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n")); PV <- newest()
cat(sprintf("\n==== vegetation prep result %d\n", scenarioId(PV))); logs(PV, PAT)
vf <- datasheet(PV, "lucasFireC2F_LandscapeRasters", optional = TRUE)$VegetationFileName; if (is.null(vf) || is.na(vf)) stop("P1 wrote no vegetation map")
old <- rast(datasheet(rsyncrosim::scenario(L, 2), "lucasFireC2F_LandscapeRasters")$FuelFileName); new <- rast(datasheet(PV, "lucasFireC2F_LandscapeRasters")$FuelFileName)
cat("P1 fuel grid unchanged vs prep result 2:", compareGeom(old, new, stopOnError = FALSE) && all(values(old, mat = FALSE) == values(new, mat = FALSE), na.rm = TRUE), "\n")

lr <- as.data.table(datasheet(rsyncrosim::scenario(L, 16), "lucasFireC2F_LandscapeRasters", lookupsAsFactors = FALSE, optional = TRUE))
lr$VegetationFileName <- vf
C <- rsyncrosim::scenario(P, "P27 Calibrate by vegetation", sourceScenario = rsyncrosim::scenario(L, 16), overwrite = TRUE)
saveDatasheet(C, as.data.frame(lr), "lucasFireC2F_LandscapeRasters")
co <- datasheet(C, "lucasFireC2F_CalibrationOption", optional = TRUE, lookupsAsFactors = FALSE)
if (!nrow(co)) co <- data.frame(FitIgnitionCount = TRUE, FitIgnitionDistribution = TRUE, FitSpreadDays = TRUE, AreaWeight = 5, MaximumSpreadDays = 25L)
co$SpreadDaysBy <- 3L; saveDatasheet(C, co, "lucasFireC2F_CalibrationOption"); dependency(C) <- 2L
cat("\ncalibration scenario", scenarioId(C), "| SpreadDaysBy:", datasheet(C, "lucasFireC2F_CalibrationOption", optional = TRUE)$SpreadDaysBy,
    "| seasons", datasheet(C, "lucasFireC2F_RunControl")$MaximumIteration, "\n")
t0 <- Sys.time(); tryCatch(run(C), error = function(e) cat("run error:", conditionMessage(e), "\n")); CR <- newest()
cat(sprintf("\n==== calibration result %d (%.1f min)\n", scenarioId(CR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(CR, PAT)
if (!nrow(datasheet(CR, "lucasFireC2F_SpreadDays"))) stop("calibration wrote no spread days - stopping")
R <- rsyncrosim::scenario(P, "P27 Run by vegetation", sourceScenario = rsyncrosim::scenario(L, 18), overwrite = TRUE)
saveDatasheet(R, as.data.frame(lr), "lucasFireC2F_LandscapeRasters"); dependency(R) <- c(scenarioId(CR), 2L)
t0 <- Sys.time(); tryCatch(run(R), error = function(e) cat("run error:", conditionMessage(e), "\n")); RR <- newest()
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(RR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(RR, PAT)
cat(sprintf("\nRESULTS vegetation-prep %d calibration %d model %d\n", scenarioId(PV), scenarioId(CR), scenarioId(RR)))
