# Build and run a zone in the Sierra Nevada & Foothills library with the P27 setup (recent-fire rules, spread days by cause x
# vegetation, within-stratum replacement, 1,500-season seed run, pool 1.25, TL/GS engine remaps from the project's Fuel Types, die-off
# fuel rule). Copies P27's scenarios and replaces everything zone-specific:
#   1 "<tag> Prep"      copy of P27 Prep (1) with Zone Prep Options' FireZone = the zone; P1-P4
#   2 die-off fuel grid from the zone's prep fuel (timber cells with >= 0.5 insect/disease mortality 2012-2019 -> SB2 / SB3)
#   3 "<tag> Calibrate" copy of P27 Calibrate by vegetation (22): landscape = die-off grid + the zone's vegetation map, burning hours,
#                       local Severity Target cleared (the zone's P4 target comes by dependency), depends on the zone prep
#   4 "<tag> Run"       copy of P27 Run by vegetation (24): same landscape, Resample Options = the zone's threshold + pool 1.25, the
#                       zone's burning-period wind table, Severity Target cleared; depends on the calibration result, then the prep
# Usage: Rscript build_zone.R "<zone name>" <tag> <burning hours> "<wind library>" <wind scenario id>
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
a <- commandArgs(TRUE); ZN <- a[1]; TAG <- a[2]; BH <- as.integer(a[3]); WLIB <- a[4]; WSID <- as.integer(a[5])
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; LIB <- file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim"); DD <- "F:/lucas-master/Data Sources/Landfire Disturbance"
L <- suppressWarnings(ssimLibrary(LIB)); P <- rsyncrosim::project(L, 1)
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
logs <- function(R, pat) { lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg); cat(unique(lg[grepl(pat, lg)]), sep = "\n") }
PAT <- "Map Code|vegetation|presences|cross-validated|severity|Failure|Error|error|recent-fire|Recent Fire|threshold|ignition count|fitted by|too few|recorded fires, mean|spread days,|replacement within|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Total time: [0-9:]+$"
runit <- function(S, what) { t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n")); R <- newest()
  cat(sprintf("\n==== %s %s: result %d (%.1f min)\n", TAG, what, scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(R, PAT); R }
dropds <- function(S, n) tryCatch(delete(S, datasheet = paste0("lucasFireC2F_", n), force = TRUE), error = function(e) NULL)

# ---- 1 prep -----------------------------------------------------------------------------------------------------------------------
fz <- as.data.table(datasheet(P, "lucasFireC2F_FireZone")); if (!ZN %in% fz$Name) stop("fire zone not in the project: ", ZN)
S1 <- rsyncrosim::scenario(P, paste(TAG, "Prep"), sourceScenario = rsyncrosim::scenario(L, 1), overwrite = TRUE)
zo <- datasheet(S1, "lucasFireC2F_ZonePrepOption", optional = TRUE, lookupsAsFactors = FALSE); zo$FireZone <- ZN; saveDatasheet(S1, zo, "lucasFireC2F_ZonePrepOption")
PR <- runit(S1, "prep"); lr <- as.data.table(datasheet(PR, "lucasFireC2F_LandscapeRasters", optional = TRUE, lookupsAsFactors = FALSE))
if (!nrow(lr) || is.na(lr$VegetationFileName[1])) stop("prep wrote no landscape / vegetation map - stopping")
has_sev <- nrow(datasheet(PR, "lucasFireC2F_SeverityTarget")) > 0; cat("prep severity target rows:", has_sev, "\n")

# ---- 2 die-off fuel grid -----------------------------------------------------------------------------------------------------------
fm <- as.data.table(datasheet(P, "lucasFireC2F_FuelModel", lookupsAsFactors = FALSE)); fu <- rast(lr$FuelFileName[1]); zv <- as.polygons(ext(fu), crs = crs(fu))
any30 <- NULL; hi30 <- NULL; TYPES <- c("Insects", "Disease", "Insects/Disease")
for (y in 2012:2019) { tf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "[.]tif$", recursive = TRUE, full.names = TRUE); tf <- tf[!grepl("ovr|aux", tf)][1]
  cf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "[.]csv$", recursive = TRUE, full.names = TRUE); cf <- cf[grepl("CSV_Data", cf)][1]
  lut <- fread(cf); setnames(lut, toupper(names(lut))); r <- rast(tf); if (crs(r) == "") crs(r) <- "EPSG:5070"
  r <- crop(r, terra::project(zv, crs(r)), snap = "out"); a1 <- r %in% lut[DIST_TYPE %in% TYPES]$VALUE; h1 <- r %in% lut[DIST_TYPE %in% TYPES & SEVERITY == "High"]$VALUE
  if (!is.null(any30) && !compareGeom(a1, any30, stopOnError = FALSE)) { a1 <- resample(a1, any30, "near"); h1 <- resample(h1, any30, "near") }
  any30 <- if (is.null(any30)) a1 else (any30 | a1); hi30 <- if (is.null(hi30)) h1 else (hi30 | h1) }
mv <- values(terra::project(as.numeric(any30), fu, method = "average"), mat = FALSE); hv <- values(terra::project(as.numeric(hi30), fu, method = "average"), mat = FALSE)
mv[is.na(mv)] <- 0; hv[is.na(hv)] <- 0; fv <- values(fu, mat = FALSE); grp <- fm$FuelGroup[match(fv, fm$ID)]
hit <- grp %in% c("Timber-Understory", "Timber Litter") & mv >= 0.5; new <- fv; new[hit] <- ifelse(hv[hit] >= 0.5 * mv[hit], 203L, 202L)
OD <- file.path(D, TAG, "inputs"); dir.create(OD, FALSE, TRUE); FF <- file.path(OD, "fuel_type_dieoff_m50.tif")
writeRaster(setValues(fu, new), FF, overwrite = TRUE, datatype = datatype(fu))
cat(sprintf("die-off rule: %s timber cells -> SB2 %s, SB3 %s; wrote %s\n", format(sum(hit), big.mark = ","), format(sum(new[hit] == 202), big.mark = ","), format(sum(new[hit] == 203), big.mark = ","), FF))
lr2 <- copy(lr); lr2$FuelFileName <- FF

# ---- 3 calibration ---------------------------------------------------------------------------------------------------------------
S3 <- rsyncrosim::scenario(P, paste(TAG, "Calibrate"), sourceScenario = rsyncrosim::scenario(L, 22), overwrite = TRUE)
saveDatasheet(S3, as.data.frame(lr2), "lucasFireC2F_LandscapeRasters"); saveDatasheet(S3, data.frame(Hours = BH), "lucasFireC2F_BurningHours", append = FALSE)
if (has_sev) dropds(S3, "SeverityTarget"); dependency(S3) <- scenarioId(PR)
cat(sprintf("\ncalibration scenario %d: burning hours %d, seasons %d, SpreadDaysBy %s, depends on %s\n", scenarioId(S3), datasheet(S3, "lucasFireC2F_BurningHours")$Hours,
            datasheet(S3, "lucasFireC2F_RunControl")$MaximumIteration, datasheet(S3, "lucasFireC2F_CalibrationOption", optional = TRUE)$SpreadDaysBy, paste(dependency(S3)$ScenarioId, collapse = ", ")))
CR <- runit(S3, "calibration"); if (!nrow(datasheet(CR, "lucasFireC2F_SpreadDays"))) stop("calibration wrote no spread days - stopping")
MINA <- datasheet(CR, "lucasFireC2F_ResampleOption")$MinimumFireSize[1]

# ---- 4 model run -------------------------------------------------------------------------------------------------------------------
S4 <- rsyncrosim::scenario(P, paste(TAG, "Run"), sourceScenario = rsyncrosim::scenario(L, 24), overwrite = TRUE)
saveDatasheet(S4, as.data.frame(lr2), "lucasFireC2F_LandscapeRasters")
saveDatasheet(S4, data.frame(MinimumFireSize = MINA, ProportionExtraIgnition = 1.25), "lucasFireC2F_ResampleOption")
WL <- suppressWarnings(ssimLibrary(WLIB)); wr <- datasheet(rsyncrosim::scenario(WL, WSID), "lucasFireC2F_WindRatio", optional = TRUE, lookupsAsFactors = FALSE)
if ("FireZone" %in% names(wr)) wr$FireZone <- NULL
saveDatasheet(S4, wr, "lucasFireC2F_WindRatio", append = FALSE)
if (has_sev) dropds(S4, "SeverityTarget"); dependency(S4) <- c(scenarioId(CR), scenarioId(PR))
cat(sprintf("\nmodel scenario %d: threshold %.1f ha, pool 1.25, wind rows %d (from %s scenario %d), depends on %s\n", scenarioId(S4), MINA, nrow(datasheet(S4, "lucasFireC2F_WindRatio")),
            basename(WLIB), WSID, paste(dependency(S4)$ScenarioId, collapse = ", ")))
RR <- runit(S4, "model run")
cat(sprintf("\nRESULTS %s prep %d calibration %d model %d\n", TAG, scenarioId(PR), scenarioId(CR), scenarioId(RR)))
