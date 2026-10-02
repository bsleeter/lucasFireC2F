# Phase 1 CONUS regression: a fresh test library, two projects - "CA inputs" (the SN&F Data Sources) and "National inputs" (LANDFIRE
# 30 m DEM, national pyromes, no fire-zone / fire-age grids, interagency perimeters, NWS zones) - each building P26's prep (CA: P1;
# National: P1, P3, P5). Then every raster is compared with SN&F result 32, and the national wind table with result 81.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; DS <- "F:/lucas-master/Data Sources"; OUT <- "F:/lucasFireC2F/dev/conus"
SNF <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim")))
src_ds <- datasheet(rsyncrosim::project(SNF, 1), "lucasFireC2F_DataSource", optional = TRUE)
zp <- datasheet(rsyncrosim::scenario(SNF, 31), "lucasFireC2F_ZonePrepOption", optional = TRUE, lookupsAsFactors = FALSE); cat("P26 Prep options:\n"); print(zp)
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F CONUS test.ssim"), packages = "lucasFireC2F", overwrite = TRUE))
nat <- src_ds; nat$StatewideElevationFile <- file.path(DS, "DEM/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif")
nat$FireZoneBoundaryFile <- file.path(DS, "Pyromes/Data/Pyromes_CONUS_20200206.shp"); nat$FireZoneGridFile <- NA; nat$FireAgeFile <- NA
nat$InteragencyPerimeterFile <- file.path(DS, "NIFS/InterAgencyFirePerimeterHistory_All_Years_View/InterAgencyFirePerimeterHistory_All_Years_View.shp")
nat$WeatherZoneBoundaryFile <- file.path(DS, "Study Areas/Fire Weather Zones/fz03mr26.shp")
nat$RawsHourlyFolder <- "F:/lucasFireC2F/Data/raws/hourly"; nat$RawsCatalogueFile <- "F:/lucasFireC2F/Data/raws/fems_ca_raws_catalogue.json"
res <- list()
for (pr in list(list(n = "CA inputs", ds = src_ds, pipe = "P1 - Prepare Zone Landscape"),
                list(n = "National inputs", ds = nat, pipe = c("P1 - Prepare Zone Landscape", "P3 - Extract Zone Weather", "P5 - Burning-Period Wind (RAWS)")))) {
  P <- rsyncrosim::project(L, pr$n); dd <- pr$ds; dd$DataSourceId <- NULL; dd$ProjectId <- NULL; saveDatasheet(P, dd, "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
  S <- rsyncrosim::scenario(P, "P26 Prep test"); z <- zp; z$ZonePrepOptionId <- NULL; z$ScenarioId <- NULL; saveDatasheet(S, z, "lucasFireC2F_ZonePrepOption")
  saveDatasheet(S, data.frame(StageNameId = pr$pipe, RunOrder = seq_along(pr$pipe)), "core_Pipeline")
  description(S) <- sprintf("CONUS regression (2.0.0): P26 prep with %s; compared with SN&F result 32 (P1) and 81 (P5).", pr$n)
  t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
  R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"])); res[[pr$n]] <- R
  lg <- sub(" +$", "", strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]])
  cat(sprintf("\n==== %s: result %d (%.1f min)\n", pr$n, scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat(unique(grep("Map Code|elevation|fire zones|fire age|years since|fuel types|forest cells|vegetation|LANDFIRE|time zone|stations|burning-period|daily wind|Table|WARNING|Failure|Error|error", lg, value = TRUE)), sep = "\n") }
saveRDS(lapply(res, scenarioId), file.path(OUT, "test_results.rds"))
# ---- compare with SN&F result 32 --------------------------------------------------------------------------------------------
ref <- rsyncrosim::scenario(SNF, 32); g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), optional = TRUE))
cmp <- function(a, b, nm) { if (is.null(a) || is.na(a) || is.null(b) || is.na(b)) return(cat(sprintf("  %-22s missing (%s / %s)\n", nm, a, b)))
  A <- rast(a); B <- rast(b); if (!compareGeom(A, B, stopOnError = FALSE)) { cat(sprintf("  %-22s GRID DIFFERS: %s vs %s, ext %s vs %s\n", nm, paste(dim(A)[1:2], collapse = "x"), paste(dim(B)[1:2], collapse = "x"),
    paste(round(as.vector(ext(A))), collapse = ","), paste(round(as.vector(ext(B))), collapse = ","))); B <- resample(B, A, "near") }
  va <- values(A, mat = FALSE); vb <- values(B, mat = FALSE); both <- !is.na(va) & !is.na(vb)
  cat(sprintf("  %-22s cells %s | NA only in new %s, only in ref %s | both: %s differ (max |d| %s)\n", nm, format(length(va), big.mark = ","), format(sum(is.na(va) & !is.na(vb)), big.mark = ","),
    format(sum(!is.na(va) & is.na(vb)), big.mark = ","), format(sum(abs(va[both] - vb[both]) > 1e-6), big.mark = ","), signif(max(c(0, abs(va[both] - vb[both]))), 4))) }
for (n in names(res)) { cat("\n==== compare", n, "result", scenarioId(res[[n]]), "with SN&F result 32\n")
  for (sh in c("LandscapeRasters", "CanopyRasters")) { a <- g(res[[n]], sh); b <- g(ref, sh); for (col in intersect(names(a), names(b))) if (grepl("FileName", col)) cmp(a[[col]][1], b[[col]][1], sub("FileName", "", col)) } }
w1 <- g(res[["National inputs"]], "WindRatio"); w0 <- g(rsyncrosim::scenario(SNF, 81), "WindRatio")
if (nrow(w1)) { cat("\n==== wind table: national P5 vs result 81 (median / p90 / p99 by daily-wind bin)\n")
  for (b in sort(unique(w1$LowerWindSpeed))) { q1 <- w1[LowerWindSpeed == b]; q0 <- w0[LowerWindSpeed == b]; f <- function(q, p) approx(q$Quantile, q$Ratio, p, ties = "ordered")$y
    cat(sprintf("  %4g km/h: %.2f / %.2f / %.2f  vs  %.2f / %.2f / %.2f\n", b, f(q1, .5), f(q1, .9), f(q1, .99), f(q0, .5), f(q0, .9), f(q0, .99))) } }
