# Die-off fuel test for P27: timber cells (Timber-Understory / Timber Litter) with >= MINM insect/disease mortality share (LANDFIRE
# disturbance, 2012-2019) get a slash-blowdown model - SB3 where at least half of that mortality is High severity, else SB2 (C2F-W
# grows both as SH5, the package crosswalk; stage 6 uses SB2/SB3 themselves). The modified fuel grid replaces the prep's in two NEW
# scenarios (copies of P27 Calibrate and P27 Run); the calibration is refitted, then the model run takes it. The user's scenarios are
# not changed. Usage: Rscript dieoff_run.R [min mortality share = 0.5]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); MINM <- if (length(a)) as.numeric(a[1]) else 0.5
LIB <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim"; OUTD <- "F:/lucas-master/Models/lucasFireC2F Dev/P27/inputs"
dir.create(OUTD, FALSE, TRUE); DD <- "F:/lucas-master/Data Sources/Landfire Disturbance"; TYPES <- c("Insects", "Disease", "Insects/Disease")
L <- suppressWarnings(ssimLibrary(LIB)); PR <- rsyncrosim::project(L, 1); PREP <- rsyncrosim::scenario(L, 2)
lr <- as.data.table(datasheet(PREP, "lucasFireC2F_LandscapeRasters", lookupsAsFactors = FALSE))
fm <- as.data.table(datasheet(PR, "lucasFireC2F_FuelModel", lookupsAsFactors = FALSE))
fu <- rast(lr$FuelFileName[1])

# ---- mortality share (any severity) and high-severity share on the 150 m grid ------------------------------------------------
zv <- as.polygons(ext(fu), crs = crs(fu)); any30 <- NULL; hi30 <- NULL
for (y in 2012:2019) { tf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "[.]tif$", recursive = TRUE, full.names = TRUE); tf <- tf[!grepl("ovr|aux", tf)][1]
  cf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "[.]csv$", recursive = TRUE, full.names = TRUE); cf <- cf[grepl("CSV_Data", cf)][1]
  lut <- fread(cf); setnames(lut, toupper(names(lut))); r <- rast(tf); if (crs(r) == "") crs(r) <- "EPSG:5070"
  r <- crop(r, terra::project(zv, crs(r)), snap = "out")
  a1 <- r %in% lut[DIST_TYPE %in% TYPES]$VALUE; h1 <- r %in% lut[DIST_TYPE %in% TYPES & SEVERITY == "High"]$VALUE
  if (!is.null(any30) && !compareGeom(a1, any30, stopOnError = FALSE)) { a1 <- resample(a1, any30, "near"); h1 <- resample(h1, any30, "near") }  # products differ in extent
  any30 <- if (is.null(any30)) a1 else (any30 | a1); hi30 <- if (is.null(hi30)) h1 else (hi30 | h1) }
mv <- values(terra::project(as.numeric(any30), fu, method = "average"), mat = FALSE); hv <- values(terra::project(as.numeric(hi30), fu, method = "average"), mat = FALSE)
mv[is.na(mv)] <- 0; hv[is.na(hv)] <- 0

# ---- the rule ------------------------------------------------------------------------------------------------------------------
fv <- values(fu, mat = FALSE); grp <- fm$FuelGroup[match(fv, fm$ID)]
hit <- grp %in% c("Timber-Understory", "Timber Litter") & mv >= MINM
new <- fv; new[hit] <- ifelse(hv[hit] >= 0.5 * mv[hit], 203L, 202L)
cat(sprintf("die-off rule (mortality >= %.2f): %s timber cells -> SB2 %s, SB3 %s (of %s timber cells)\n", MINM, format(sum(hit), big.mark = ","),
            format(sum(new[hit] == 202), big.mark = ","), format(sum(new[hit] == 203), big.mark = ","), format(sum(grp %in% c("Timber-Understory", "Timber Litter")), big.mark = ",")))
print(as.data.table(table(from = fm$Name[match(fv[hit], fm$ID)], to = ifelse(new[hit] == 203, "SB3", "SB2")))[N > 0][order(-N)][1:10])
nf <- setValues(fu, new); FF <- file.path(OUTD, sprintf("fuel_type_dieoff_m%02d.tif", round(100 * MINM)))
writeRaster(nf, FF, overwrite = TRUE, datatype = datatype(fu)); cat("wrote", FF, "\n")

# ---- scenarios -------------------------------------------------------------------------------------------------------------------
lr2 <- copy(lr); lr2$FuelFileName <- FF
P <- rsyncrosim::project(L, 1)
newest <- function() rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
logs <- function(R, pat) { lg <- unlist(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")); lg <- sub(" +$", "", lg); cat(unique(lg[grepl(pat, lg)]), sep = "\n") }
PAT <- "sent to C2F-W|Human:|Lightning:|spread-day distribution|spread days,|kept .* fires|mean BP|TOTAL|fires/yr|AUC|hottest|correlation|Slash|Failure|Error|error|Total time: [0-9:]+$"
C <- rsyncrosim::scenario(P, "P27 Calibrate die-off", sourceScenario = rsyncrosim::scenario(L, 3), overwrite = TRUE)
saveDatasheet(C, as.data.frame(lr2), "lucasFireC2F_LandscapeRasters"); dependency(C) <- 2L
cat("\ncalibration scenario", scenarioId(C), "| seasons", datasheet(C, "lucasFireC2F_RunControl")$MaximumIteration, "| depends on:", paste(dependency(C)$ScenarioId, collapse = ", "), "\n")
t0 <- Sys.time(); tryCatch(run(C), error = function(e) cat("run error:", conditionMessage(e), "\n")); CR <- newest()
cat(sprintf("\n==== calibration result %d (%.1f min)\n", scenarioId(CR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(CR, PAT)
if (!nrow(datasheet(CR, "lucasFireC2F_SpreadDays"))) stop("calibration wrote no spread days - stopping")
R <- rsyncrosim::scenario(P, "P27 Run die-off", sourceScenario = rsyncrosim::scenario(L, 8), overwrite = TRUE)
saveDatasheet(R, as.data.frame(lr2), "lucasFireC2F_LandscapeRasters"); dependency(R) <- c(scenarioId(CR), 2L)
cat("\nmodel scenario", scenarioId(R), "| depends on:", paste(dependency(R)$ScenarioId, collapse = ", "), "\n")
t0 <- Sys.time(); tryCatch(run(R), error = function(e) cat("run error:", conditionMessage(e), "\n")); RR <- newest()
cat(sprintf("\n==== model result %d (%.1f min)\n", scenarioId(RR), as.numeric(difftime(Sys.time(), t0, units = "mins")))); logs(RR, PAT)
cat(sprintf("\nRESULTS calibration %d model %d\n", scenarioId(CR), scenarioId(RR)))
