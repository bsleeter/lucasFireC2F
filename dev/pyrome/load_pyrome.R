# =============================================================================
# load_pyrome.R - builds a lucasFireC2F library for any pyrome from the zone prep outputs (generalises dev/load_p26.R)
# =============================================================================
# Development tool (not part of the package). Unlike load_p26.R nothing comes from a BurnP3+ library: the ignition count and
# mix, the weather stream, the large-fire threshold and the spread-day curve are all left to the package's own CALIBRATION
# scenario (C1-C3, a seed run, C5), and the baseline takes them through a scenario dependency after it has run.
#   rasters      prep/prep_zone_landscape.R output in <dev>/P<NN>/inputs (raw FBFM40, canopy, years since fire);
#                elevation, fire zone, weather zone and the pyrome-fitted ignition surfaces from the 158 zone folder
#   effects      RAWS burning-period wind ratios (the pyrome's table if it exists, else P26's), Cruz, CBH x1.5, FMC 85%
#   severity     MTBS 2021-24 target by fuel group (severity_target.R) when given, else the package default (P26)
#   calibration  FPA-FOD 1992-2020 by ignition point in the pyrome; FSim size rule; 500-season seed run
# Usage: Rscript load_pyrome.R <pyrome number> [severity target csv | ""] [wind ratio csv | ""]
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra); library(sf) })
sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); SEVF <- if (length(a) > 1) a[2] else ""; WRF <- if (length(a) > 2) a[3] else ""
M    <- "F:/lucas-master/Models/California 30m BurnP3"; DEV <- sprintf("F:/lucas-master/Models/lucasFireC2F Dev/P%02d", PY)
IN   <- file.path(DEV, "inputs"); ZD <- file.path(M, sprintf("spatial/pyromes_150m_LF2020/pyrome_%03d", PY))
LIBF <- sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P%02d.ssim", PY)
say  <- function(...) cat(sprintf(...), "\n", sep = "")
pz <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); pz <- pz[pz$PYROME == PY, ]; ZNAME <- pz$Name; ZID <- as.integer(pz$ID)
for (f in c(file.path(IN, c("fuel_type.tif", "years_since_fire.tif", "canopy_base_height_m.tif")), file.path(ZD, c("Elevation.tif", "WeatherZone.tif", "Human.tif", "Lightning.tif", "zone.gpkg"))))
  if (!file.exists(f)) stop("missing input: ", f)
wzr <- rast(file.path(ZD, "WeatherZone.tif")); gpk <- as.data.table(st_drop_geometry(read_sf(file.path(M, "data/zones/weather_zones.gpkg")))[, c("ID", "Name")])
codes <- sort(unique(values(wzr, mat = FALSE))); codes <- codes[!is.na(codes)]
if (anyNA(match(codes, gpk$ID))) stop("weather-zone codes not in weather_zones.gpkg")
say("%s (ID %d): %d weather zones", ZNAME, ZID, length(codes))

# ---- library and project ---------------------------------------------------------------------------------------------------
L <- suppressWarnings(ssimLibrary(LIBF, packages = "lucasFireC2F", overwrite = TRUE))
P <- rsyncrosim::project(L, ZNAME)
wz <- data.table(Name = gpk$Name[match(codes, gpk$ID)], ID = as.integer(codes))
gz <- suppressWarnings(st_centroid(read_sf(file.path(M, "data/zones/weather_zones.gpkg")), of_largest_polygon = TRUE) |> st_transform(4326))
wz[, Latitude := st_coordinates(gz)[match(Name, gz$Name), 2]]
saveDatasheet(P, as.data.frame(wz), "lucasFireC2F_WeatherZone")
saveDatasheet(P, data.frame(Name = ZNAME, ID = ZID, Description = sprintf("Pyrome %d (FSim / WRC simulation unit)", PY)), "lucasFireC2F_FireZone")

# ---- baseline scenario (calibrated sheets arrive by dependency) --------------------------------------------------------------
S <- rsyncrosim::scenario(P, sprintf("P%02d Baseline LF2020", PY))
sv <- function(x, nm, sc = S) saveDatasheet(sc, as.data.frame(x), paste0("lucasFireC2F_", nm))
saveDatasheet(S, data.frame(StageNameId = c("1 - Prepare Landscape", "2 - Sample Ignitions", "3 - Sample Burning Conditions", "4 - Grow Fires (C2F-W)",
  "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk", "8 - Fire Detail (optional)", "C6 - Score Against Fire Records"), RunOrder = 1:9), "core_Pipeline")
sv(data.frame(MinimumIteration = 1L, MaximumIteration = 2500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "RunControl")
sv(data.frame(FuelFileName = file.path(IN, "fuel_type.tif"), ElevationFileName = file.path(ZD, "Elevation.tif"), FireZoneFileName = file.path(ZD, "FireZone.tif"),
              WeatherZoneFileName = file.path(ZD, "WeatherZone.tif"), FireAgeFileName = file.path(IN, "years_since_fire.tif")), "LandscapeRasters")
sv(data.frame(CanopyBaseHeightFileName = file.path(IN, "canopy_base_height_m.tif"), CanopyBulkDensityFileName = file.path(IN, "canopy_bulk_density.tif"),
              CanopyCoverFileName = file.path(IN, "canopy_cover_pct.tif"), CanopyHeightFileName = file.path(IN, "canopy_height_m.tif")), "CanopyRasters")
sv(data.frame(MinimumYears = c(0L, 3L, 6L), MaximumYears = c(2L, 5L, 10L), ReplacementFuelType = c("NB9", "TL1", "TL8")), "FireAgeRule")
sv(data.frame(Cause = c("Human", "Lightning"), FileName = file.path(ZD, c("Human.tif", "Lightning.tif"))), "IgnitionGrid")
sv(data.frame(Hours = 2L), "BurningHours")
sv(data.frame(MinimumFWI = 19, WindSpeedTo20ft = 1.15), "WeatherOption")
sv(data.frame(Class = 1:3, MaximumEMC = c(4.5, 7.5, 10.5)), "MoistureDeadClass")
sv(data.frame(FoliarMoisture = 85, CrownFire = TRUE, SaveSpreadNetwork = TRUE, WorkersPerJob = 100L, SeedRun = FALSE), "GrowthOption")
sv(data.frame(WindMode = 0L, WindFactor = 1, CrownModel = 0L, CanopyBaseHeightMultiplier = 1.5, FoliarMoisture = 85, MinimumBurns = 10L), "EffectsOption")
wrf <- if (nzchar(WRF)) WRF else file.path(M, "data/wind/p026_raws_ratio_quantiles.csv"); wr <- fread(wrf)
sv(wr[, .(LowerWindSpeed = lo, UpperWindSpeed = ifelse(is.finite(hi), hi, NA), Quantile = p, Ratio = ratio)], "WindRatio")
sv(data.frame(OozeWindowCells = 5L, OozePasses = 3L, MinimumPatchHa = 500), "RiskOption")
sv(data.frame(BurnEffectsTable = TRUE, Perimeters = TRUE, SeverityMapSeasons = 10L, FitSeverityClasses = TRUE), "FireDetailOption")
sv(data.frame(BurnTable = TRUE, FireStatistics = TRUE), "OutputOption")
if (nzchar(SEVF)) { tg <- fread(SEVF); sv(tg[, .(FuelGroup, UnburnedLow, Low, Moderate, High)], "SeverityTarget"); say("severity target: %s", SEVF) } else say("severity target: package default (P26)")
say("wind ratio table: %s", wrf)
cp <- data.frame(FireFirstYear = 1992L, FireLastYear = 2020L, WeatherFirstYear = 1992L, WeatherLastYear = 2020L, HoldoutFirstYear = 2021L,
                 HoldoutLastYear = 2024L, RecordSource = "FPA-FOD", MinimumSizeRule = 1L)
sv(cp, "CalibrationPeriod")
sv(data.frame(FitIgnitionCount = TRUE, FitIgnitionDistribution = TRUE, FitSpreadDays = TRUE, AreaWeight = 5, MaximumSpreadDays = 25L), "CalibrationOption")
sv(data.frame(ProportionExtraIgnition = 0.5), "ResampleOption")

# historical fire record: every FPA-FOD fire with its ignition point in the pyrome
FPA <- "F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg"
sv(data.frame(FPAFODFileName = FPA, ZoneBoundaryFileName = file.path(ZD, "zone.gpkg")), "FireRecordImport")
zone <- read_sf(file.path(ZD, "zone.gpkg"))
fpa <- read_sf(FPA, query = "SELECT FOD_ID, DISCOVERY_DATE, FIRE_SIZE, NWCG_CAUSE_CLASSIFICATION, LATITUDE, LONGITUDE, Shape FROM Fires WHERE STATE IN ('CA','NV','OR','AZ') AND FIRE_SIZE > 0")
fpa <- fpa[st_within(st_transform(fpa, st_crs(zone)), st_union(zone), sparse = FALSE)[, 1], ]
hf <- data.table(Source = "FPA-FOD", FireID = as.character(fpa$FOD_ID), Date = as.Date(fpa$DISCOVERY_DATE, format = "%m/%d/%Y"),
                 AreaHa = round(fpa$FIRE_SIZE * 0.404686, 2), Cause = ifelse(fpa$NWCG_CAUSE_CLASSIFICATION == "Natural", "Lightning",
                 ifelse(fpa$NWCG_CAUSE_CLASSIFICATION == "Human", "Human", NA)), Latitude = fpa$LATITUDE, Longitude = fpa$LONGITUDE, FireZone = ZNAME)
if (anyNA(hf$Date)) stop(sum(is.na(hf$Date)), " FPA dates did not parse")
sv(hf[, .(Source, FireID, Date = format(Date), AreaHa, Cause, Latitude, Longitude, FireZone)], "HistoricalFire")
say("historical fires: %d (1992-2020 mean size %.1f ha)", nrow(hf), mean(hf[year(Date) <= 2020]$AreaHa))

# full daily weather for the pyrome's zones (unrounded zone-daily gridMET; C2 builds the stream from it)
dw <- fread(file.path(M, "data/weather/zone_daily_raw.csv"))[zone %in% codes]
dw <- dw[, .(WeatherZone = gpk$Name[match(zone, gpk$ID)], Date = as.character(date), Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)]
sv(dw, "DailyWeather")
say("daily weather rows: %s (%d zones, %s to %s)", format(nrow(dw), big.mark = ","), uniqueN(dw$WeatherZone), min(dw$Date), max(dw$Date))

# ---- calibration scenario ---------------------------------------------------------------------------------------------------
C <- rsyncrosim::scenario(P, sprintf("P%02d Calibration 1992-2020", PY), sourceScenario = S)
saveDatasheet(C, data.frame(StageNameId = c("C1 - Import Fire Record", "C2 - Build Weather Stream", "C3 - Fit Ignitions", "1 - Prepare Landscape",
  "2 - Sample Ignitions", "3 - Sample Burning Conditions", "4 - Grow Fires (C2F-W)", "5 - Summarize Burn Probability",
  "C5 - Fit Spread Days", "C6 - Score Against Fire Records"), RunOrder = 1:10), "core_Pipeline")
saveDatasheet(C, data.frame(MinimumIteration = 1L, MaximumIteration = 500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(C, data.frame(FoliarMoisture = 85, CrownFire = TRUE, SaveSpreadNetwork = FALSE, WorkersPerJob = 100L, SeedRun = TRUE), "lucasFireC2F_GrowthOption")
say("library %s: baseline (stages 1-8 + C6, 2,500 seasons, calibrated sheets by dependency) and calibration (C1-C3 > 500-season seed > C5 > C6)", LIBF)
