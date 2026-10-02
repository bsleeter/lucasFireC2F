# =============================================================================
# load_p26.R - builds a lucasFireC2F library for pyrome 26 from the validated development inputs
# =============================================================================
# Development tool (not part of the package): the Phase A acceptance test, "a P26 scenario can be filled entirely with
# the package's datasheets", and the starting library for the Phase B reproduction test. Every value comes from the
# same files the own-draw run (p026_LF2020_own) used:
#   rasters      prep/prep_zone_landscape.R output (raw FBFM40 + fire age; checked cell for cell by dev/check_p26_prep.R),
#                elevation / fire zone from the pyrome folder, weather zone and ignition grids from the P26 LF2020
#                library's scenario 3 (as 221 read them)
#   ignitions    count Gamma(10.93, 7.64), season x cause likelihoods, 0.5 extra pool, 36.8 ha minimum (library scenario 3)
#   spread days  C2F-W's fitted 1992-2020 curve (204, h2_seed/days_curve.csv), 2 burning hours per spread day
#   weather      library scenario 3 stream for the pyrome's weather zones (FWI >= 19, 1992-2020, 07b indices);
#                full daily record for those zones (Daily Weather_ow.csv) for calibration
#   effects      RAWS ratio table (220), stations (219); Cruz crown spread, CBH x1.5, FMC 85%
#   calibration  FPA-FOD fires in P26 >= 1 ha, 1992-2020, by ignition point
# Usage: Rscript dev/load_p26.R [library path]
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(DBI); library(terra); library(sf) })
sf_use_s2(FALSE)
a <- commandArgs(TRUE)
LIBF <- if (length(a)) a[1] else "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"
M    <- "F:/lucas-master/Models/California 30m BurnP3"
IN   <- "F:/lucas-master/Models/lucasFireC2F Dev/P26/inputs"
ZD   <- file.path(M, "spatial/pyromes_150m_LF2020/pyrome_026")
S3   <- file.path(M, "California BurnP3 Pyrome026 LF2020.ssim.data/Scenario-3")
say  <- function(...) cat(sprintf(...), "\n", sep = "")
one  <- function(d, pat) { f <- list.files(file.path(S3, d), pat, full.names = TRUE); stopifnot(length(f) == 1); f }

con <- dbConnect(RSQLite::SQLite(), file.path(M, "California BurnP3 Pyrome026 LF2020.ssim"), flags = RSQLite::SQLITE_RO)
q <- function(s) as.data.table(dbGetQuery(con, s))
seasons <- q("select SeasonId, Name from burnP3Plus_Season"); causes <- q("select CauseId, Name from burnP3Plus_Cause")
wzn <- q("select WeatherZoneId, Name from burnP3Plus_WeatherZone")
idist <- q("select Season, Cause, RelativeLikelihood from burnP3Plus_IgnitionDistribution where ScenarioId = 3")
ipi <- q("select Mean, DistributionSD from burnP3Plus_IgnitionsPerIteration where ScenarioId = 3")
rso <- q("select MinimumFireSize, ProportionExtraIgnition from burnP3Plus_FireResampleOption where ScenarioId = 3")
wzr <- rast(one("burnP3Plus_LandscapeRasters", "^WeatherZone"))
gpk <- as.data.table(st_drop_geometry(read_sf(file.path(M, "data/zones/weather_zones.gpkg")))[, c("ID", "Name")])
codes <- sort(unique(values(wzr, mat = FALSE))); codes <- codes[!is.na(codes)]      # raster codes = weather_zones.gpkg ID
used <- wzn$WeatherZoneId[match(gpk$Name[match(codes, gpk$ID)], wzn$Name)]          # library ids, via the zone name
if (anyNA(used)) stop("weather-zone codes with no library zone: ", paste(codes[is.na(used)], collapse = ", "))
stream <- q(sprintf("select [Order] as ord, Season, WeatherZone, Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation,
  FineFuelMoistureCode, DuffMoistureCode, DroughtCode, InitialSpreadIndex, BuildupIndex, FireWeatherIndex
  from burnP3Plus_WeatherStream where ScenarioId = 3 and WeatherZone in (%s) order by WeatherZone, [Order]", paste(used, collapse = ",")))
dbDisconnect(con)
say("weather zones in the pyrome grid: %d; stream rows %s", length(used), format(nrow(stream), big.mark = ","))

# ---- library and project -------------------------------------------------------------------------------------------
dir.create(dirname(LIBF), showWarnings = FALSE, recursive = TRUE)
L <- suppressWarnings(ssimLibrary(LIBF, packages = "lucasFireC2F", overwrite = TRUE))
P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada")
wz <- data.table(Name = gpk$Name[match(codes, gpk$ID)], ID = as.integer(codes))   # ID = the weather-zone raster's code
gz <- suppressWarnings(st_centroid(read_sf(file.path(M, "data/zones/weather_zones.gpkg")), of_largest_polygon = TRUE) |> st_transform(4326))
wz[, Latitude := st_coordinates(gz)[match(Name, gz$Name), 2]]
saveDatasheet(P, as.data.frame(wz), "lucasFireC2F_WeatherZone")
saveDatasheet(P, data.frame(Name = "P26 Northern Sierra Nevada", ID = 1026L, Description = "Pyrome 26 (FSim / WRC simulation unit)"), "lucasFireC2F_FireZone")

# ---- scenario --------------------------------------------------------------------------------------------------------
S <- rsyncrosim::scenario(P,"P26 Baseline LF2020 (1992-2020 calibration)")
sv <- function(x, nm) saveDatasheet(S, as.data.frame(x), paste0("lucasFireC2F_", nm))
saveDatasheet(S, data.frame(StageNameId = c("1 - Prepare Landscape", "2 - Sample Ignitions", "3 - Sample Burning Conditions", "4 - Grow Fires (C2F-W)",
                              "5 - Summarize Burn Probability", "6 - Fire Effects", "7 - Hazard and Risk"), RunOrder = 1:7), "core_Pipeline")
sv(data.frame(MinimumIteration = 1L, MaximumIteration = 2500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "RunControl")
sv(data.frame(FuelFileName = file.path(IN, "fuel_type.tif"), ElevationFileName = file.path(ZD, "Elevation.tif"),
              FireZoneFileName = file.path(ZD, "FireZone.tif"), WeatherZoneFileName = one("burnP3Plus_LandscapeRasters", "^WeatherZone"),
              FireAgeFileName = file.path(IN, "years_since_fire.tif")), "LandscapeRasters")
sv(data.frame(CanopyBaseHeightFileName = file.path(IN, "canopy_base_height_m.tif"), CanopyBulkDensityFileName = file.path(IN, "canopy_bulk_density.tif"),
              CanopyCoverFileName = file.path(IN, "canopy_cover_pct.tif"), CanopyHeightFileName = file.path(IN, "canopy_height_m.tif")), "CanopyRasters")
sv(data.frame(MinimumYears = c(0L, 3L, 6L), MaximumYears = c(2L, 5L, 10L), ReplacementFuelType = c("NB9", "TL1", "TL8")), "FireAgeRule")
sv(data.frame(DistributionType = "Gamma", Mean = ipi$Mean, DistributionSD = ipi$DistributionSD, DistributionMin = 1), "IgnitionCount")
sv(idist[, .(Season = seasons$Name[match(Season, seasons$SeasonId)], Cause = causes$Name[match(Cause, causes$CauseId)], RelativeLikelihood)], "IgnitionDistribution")
sv(data.frame(Cause = c("Human", "Lightning"), FileName = c(one("burnP3Plus_ProbabilisticIgnitionLocation", "^Human"),
                                                             one("burnP3Plus_ProbabilisticIgnitionLocation", "^Lightning"))), "IgnitionGrid")
sv(data.frame(MinimumFireSize = rso$MinimumFireSize, ProportionExtraIgnition = rso$ProportionExtraIgnition), "ResampleOption")
cv <- fread(file.path(M, "data/c2fw/p026_LF2020_h2_seed/days_curve.csv"))[w > 0]
sv(cv[, .(Days = D, RelativeFrequency = w)], "SpreadDays")
sv(data.frame(Hours = 2L), "BurningHours")
sv(stream[, .(Order = ord, Season = seasons$Name[match(Season, seasons$SeasonId)], WeatherZone = wzn$Name[match(WeatherZone, wzn$WeatherZoneId)],
              Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation, FineFuelMoistureCode, DuffMoistureCode, DroughtCode,
              InitialSpreadIndex, BuildupIndex, FireWeatherIndex)], "WeatherStream")
say("scenario inputs saved; now the single-row option sheets and the calibration / utility inputs")
sv(data.frame(MinimumFWI = 19, WindSpeedTo20ft = 1.15), "WeatherOption")
sv(data.frame(Class = 1:3, MaximumEMC = c(4.5, 7.5, 10.5)), "MoistureDeadClass")
sv(data.frame(FoliarMoisture = 85, CrownFire = TRUE, SaveSpreadNetwork = TRUE, WorkersPerJob = 100L, SeedRun = FALSE), "GrowthOption")
sv(data.frame(WindMode = 0L, WindFactor = 1, CrownModel = 0L, CanopyBaseHeightMultiplier = 1.5, FoliarMoisture = 85, MinimumBurns = 10L), "EffectsOption")
wr <- fread(file.path(M, "data/wind/p026_raws_ratio_quantiles.csv"))
sv(wr[, .(LowerWindSpeed = lo, UpperWindSpeed = ifelse(is.finite(hi), hi, NA), Quantile = p, Ratio = ratio)], "WindRatio")
sv(data.frame(OozeWindowCells = 5L, OozePasses = 3L, MinimumPatchHa = 500), "RiskOption")
sv(data.frame(BurnEffectsTable = TRUE, Perimeters = TRUE, SeverityMapSeasons = 10L, FitSeverityClasses = TRUE), "FireDetailOption")
sv(data.frame(BurnTable = TRUE, FireStatistics = TRUE), "OutputOption")
sv(data.frame(FireFirstYear = 1992L, FireLastYear = 2020L, WeatherFirstYear = 1992L, WeatherLastYear = 2020L, HoldoutFirstYear = 2021L,
              HoldoutLastYear = 2024L, RecordSource = "FPA-FOD", MinimumSizeRule = 0L), "CalibrationPeriod")
sv(data.frame(FitIgnitionCount = TRUE, FitIgnitionDistribution = TRUE, FitSpreadDays = TRUE, AreaWeight = 5, MaximumSpreadDays = 25L), "CalibrationOption")
st <- fread(file.path(M, "data/wind/p026_raws_stations.csv"))
sv(st[, .(StationID = station_id, Name = station_name, Latitude = latitude, Longitude = longitude, Elevation = round(elevation * 0.3048))], "RAWSStation")
sv(data.frame(FirstYear = 2006L, LastYear = 2024L, FirstMonth = 6L, LastMonth = 11L, FirstHour = 12L, LastHour = 19L), "WindTableOption")
sv(data.frame(FirstDirection = 0, LastDirection = 110, MinimumWindSpeed = 25, MinimumGust = 45, MaximumRH = 25, MinimumHours = 3L,
              MinimumStations = 3L, FirstMonth = 6L, LastMonth = 11L), "EventOption")

# historical fire record: every FPA-FOD fire with its ignition point in P26 (C1 re-imports the same from these paths)
FPA <- "F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg"
sv(data.frame(FPAFODFileName = FPA, ZoneBoundaryFileName = file.path(ZD, "zone.gpkg")), "FireRecordImport")
zone <- read_sf(file.path(ZD, "zone.gpkg"))
fpa <- read_sf(FPA, query = "SELECT FOD_ID, DISCOVERY_DATE, FIRE_SIZE, NWCG_CAUSE_CLASSIFICATION, LATITUDE, LONGITUDE, Shape FROM Fires WHERE STATE IN ('CA','NV','OR','AZ') AND FIRE_SIZE > 0")
fpa <- fpa[st_within(st_transform(fpa, st_crs(zone)), st_union(zone), sparse = FALSE)[, 1], ]
hf <- data.table(Source = "FPA-FOD", FireID = as.character(fpa$FOD_ID), Date = as.Date(fpa$DISCOVERY_DATE, format = "%m/%d/%Y"),
                 AreaHa = round(fpa$FIRE_SIZE * 0.404686, 2), Cause = ifelse(fpa$NWCG_CAUSE_CLASSIFICATION == "Natural", "Lightning",
                 ifelse(fpa$NWCG_CAUSE_CLASSIFICATION == "Human", "Human", NA)), Latitude = fpa$LATITUDE, Longitude = fpa$LONGITUDE,
                 FireZone = "P26 Northern Sierra Nevada")
if (anyNA(hf$Date)) stop(sum(is.na(hf$Date)), " FPA dates did not parse")
sv(hf[, .(Source, FireID, Date = format(Date), AreaHa, Cause, Latitude, Longitude, FireZone)], "HistoricalFire")
say("historical fires: %d (>= 36.8 ha: %d, %.1f/yr 1992-2020)", nrow(hf), sum(hf$AreaHa >= 36.8), sum(hf$AreaHa >= 36.8 & year(hf$Date) <= 2020) / 29)

# full daily weather for the pyrome's zones (C2 rebuilds the stream from it; utilities read it)
# the UNROUNDED zone-daily record 07b computed its indices from (Daily Weather_ow.csv is rounded, and 3 borderline P26 days then
# cross FWI 19); zone codes are weather_zones.gpkg IDs
dw <- fread(file.path(M, "data/weather/zone_daily_raw.csv"))[zone %in% codes]
dw <- dw[, .(WeatherZone = gpk$Name[match(zone, gpk$ID)], Date = as.character(date), Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)]
sv(dw, "DailyWeather")
say("daily weather rows: %s (%d zones, %s to %s)", format(nrow(dw), big.mark = ","), uniqueN(dw$WeatherZone), min(dw$Date), max(dw$Date))

# ---- read back: every input sheet has what it should ------------------------------------------------------------------
chk <- c(RunControl = 1, LandscapeRasters = 1, CanopyRasters = 1, FireAgeRule = 3, IgnitionCount = 1, IgnitionDistribution = 7,
         IgnitionGrid = 2, ResampleOption = 1, SpreadDays = nrow(cv), BurningHours = 1, WeatherStream = nrow(stream), WeatherOption = 1,
         GrowthOption = 1, EffectsOption = 1, WindRatio = nrow(wr), RiskOption = 1, CalibrationPeriod = 1, HistoricalFire = nrow(hf),
         DailyWeather = nrow(dw), RAWSStation = nrow(st))
bad <- 0
for (k in names(chk)) { n <- nrow(datasheet(S, paste0("lucasFireC2F_", k), lookupsAsFactors = FALSE)); ok <- n == chk[[k]]
  if (!ok) bad <- bad + 1; say("%-22s %8s rows %s", k, format(n, big.mark = ","), if (ok) "" else sprintf("EXPECTED %d", chk[[k]])) }
lr <- datasheet(S, "lucasFireC2F_LandscapeRasters")
say("raster files stored in the library: %s", paste(basename(unlist(lr[1, ])), collapse = ", "))
say(if (bad) "LOAD FAILED (%d sheets)" else "P26 scenario loaded%s", if (bad) bad else "")

# ---- calibration scenario: a copy of the baseline run as the calibration pipeline --------------------------------------
C <- rsyncrosim::scenario(P, "P26 Calibration 1992-2020", sourceScenario = S)
saveDatasheet(C, data.frame(StageNameId = c("C1 - Import Fire Record", "C2 - Build Weather Stream", "C3 - Fit Ignitions", "1 - Prepare Landscape",
  "2 - Sample Ignitions", "3 - Sample Burning Conditions", "4 - Grow Fires (C2F-W)", "5 - Summarize Burn Probability",
  "C5 - Fit Spread Days", "C6 - Score Against Fire Records"), RunOrder = 1:10), "core_Pipeline")
saveDatasheet(C, data.frame(MinimumIteration = 1L, MaximumIteration = 500L, MinimumTimestep = 0L, MaximumTimestep = 0L, RandomSeed = 20260925L), "lucasFireC2F_RunControl")
saveDatasheet(C, data.frame(FoliarMoisture = 85, CrownFire = TRUE, SaveSpreadNetwork = FALSE, WorkersPerJob = 100L, SeedRun = TRUE), "lucasFireC2F_GrowthOption")
saveDatasheet(C, data.frame(FireFirstYear = 1992L, FireLastYear = 2020L, WeatherFirstYear = 1992L, WeatherLastYear = 2020L, HoldoutFirstYear = 2021L,
                            HoldoutLastYear = 2024L, RecordSource = "FPA-FOD", MinimumSizeRule = 1L), "lucasFireC2F_CalibrationPeriod")
say("calibration scenario: C1-C3 > 500-season seed run (stages 1-5) > C5 > C6, FSim size rule")
