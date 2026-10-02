# G1 acceptance: stages P1, P3, P4 run from a project's Data Sources for P26 and P29, compared with the inputs the zones
# actually used (158 zone folders; prep_zone_landscape.R outputs; the zone libraries' Daily Weather; the P29 severity csv and
# the P26 package default target).
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; M <- "F:/lucas-master/Models/California 30m BurnP3"; SP <- file.path(M, "spatial")
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F prep test.ssim"), packages = "lucasFireC2F", overwrite = TRUE)); P <- rsyncrosim::project(L, "California")
saveDatasheet(P, data.frame(LandfireFolder = "F:/lucas-master/Data Sources/FBFM", StatewideElevationFile = file.path(SP, "Elevation_CA_150m_LF2020.tif"),
  FireZoneGridFile = file.path(SP, "Pyrome_CA_150m_LF2020.tif"), FireZoneBoundaryFile = file.path(M, "data/zones/pyromes.gpkg"),
  WeatherZoneGridFile = file.path(SP, "WeatherZone_CA_150m_LF2020.tif"), FireAgeFile = file.path(SP, "FireAge_CA_150m_LF2020_age10.tif"),
  ZoneDailyWeatherFile = file.path(M, "data/weather/zone_daily_raw.csv"), FpaFodFile = "F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg",
  MtbsSeverityFolder = "F:/lucas-master/Data Sources/MTBS/Burn Severity"), "lucasFireC2F_DataSource", append = FALSE)   # rsyncrosim 2.1.13 drops an appended row into an empty single-row project sheet
res <- list()
for (py in c(26, 29)) {
  zn <- datasheet(P, "lucasFireC2F_FireZone")$Name; zn <- zn[startsWith(zn, sprintf("P%d ", py))]
  S <- rsyncrosim::scenario(P, sprintf("P%d prep", py))
  saveDatasheet(S, data.frame(StageNameId = c("P1 - Prepare Zone Landscape", "P3 - Extract Zone Weather", "P4 - MTBS Severity Target"), RunOrder = 1:3), "core_Pipeline")
  saveDatasheet(S, data.frame(MinimumIteration = 1L, MaximumIteration = 1L, MinimumTimestep = 0L, MaximumTimestep = 0L), "lucasFireC2F_RunControl")
  saveDatasheet(S, data.frame(FireZone = zn, CanopyHeightVintage = 2022L), "lucasFireC2F_ZonePrepOption")
  t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
  R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"])); res[[as.character(py)]] <- R
  lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
  cat(sprintf("\n==== P%d (%.1f min)\n", py, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat(unique(trimws(grep("Map Code|fuel types|forest cells|years since|daily weather|burned 30 m|px |groups with|Failure|Error|error", lg, value = TRUE))), sep = "\n")
  g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
  lr <- g("LandscapeRasters"); cr <- g("CanopyRasters"); ZD <- file.path(SP, sprintf("pyromes_150m_LF2020/pyrome_%03d", py)); IN <- file.path(D, sprintf("P%d/inputs", py))
  cmp <- function(a, b, lab) { ra <- rast(a); rb <- rast(b); same <- compareGeom(ra, rb, stopOnError = FALSE)
    va <- values(ra, mat = FALSE); vb <- values(rb, mat = FALSE); nd <- if (same) sum(xor(is.na(va), is.na(vb))) + sum(abs(va - vb) > 1e-9, na.rm = TRUE) else NA
    cat(sprintf("  %-22s same grid %-5s differing cells %s\n", lab, same, if (is.na(nd)) "n/a" else format(nd, big.mark = ","))) }
  cmp(lr$ElevationFileName, file.path(ZD, "Elevation.tif"), "elevation"); cmp(lr$FireZoneFileName, file.path(ZD, "FireZone.tif"), "fire zone")
  cmp(lr$WeatherZoneFileName, file.path(ZD, "WeatherZone.tif"), "weather zone"); cmp(lr$FuelFileName, file.path(IN, "fuel_type.tif"), "fuel type")
  cmp(lr$FireAgeFileName, file.path(IN, "years_since_fire.tif"), "years since fire")
  for (k in c("CanopyBaseHeightFileName", "CanopyBulkDensityFileName", "CanopyCoverFileName", "CanopyHeightFileName"))
    cmp(cr[[k]], file.path(IN, c(CanopyBaseHeightFileName = "canopy_base_height_m.tif", CanopyBulkDensityFileName = "canopy_bulk_density.tif",
        CanopyCoverFileName = "canopy_cover_pct.tif", CanopyHeightFileName = "canopy_height_m.tif")[k]), k)
  zl <- suppressWarnings(ssimLibrary(file.path(D, sprintf("lucasFireC2F P%d.ssim", py)))); base <- rsyncrosim::scenario(zl, 1)
  a <- g("DailyWeather"); b <- as.data.table(datasheet(base, "lucasFireC2F_DailyWeather", lookupsAsFactors = FALSE))
  setorder(a, WeatherZone, Date); setorder(b, WeatherZone, Date)
  cat(sprintf("  daily weather: %s rows vs zone library %s; values identical %s\n", format(nrow(a), big.mark = ","), format(nrow(b), big.mark = ","),
      nrow(a) == nrow(b) && isTRUE(all.equal(a[, .(WeatherZone, Date, Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)],
                                               b[, .(WeatherZone, Date, Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)], check.attributes = FALSE))))
  st <- g("SeverityTarget"); print(st[, .(FuelGroup, UnburnedLow, Low, Moderate, High)])
}
