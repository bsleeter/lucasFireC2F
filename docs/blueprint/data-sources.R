# =============================================================================
# data-sources.R - the national datasets of the Data Sources sheet: group, short label, vintage, and what each stage reads them for
# =============================================================================
# Sourced by build.R (the National data column) and build-data.R (the data blueprint). Vintages are what a Data Download fetches (user guide
# section 2.15). u(stage, role, note, alt): note = the setting that switches it; alt = TRUE when only a non-default choice
# reads it - for the shortcut rows, a filled row that replaces what the stage would otherwise build (dashed wire).
# =============================================================================
u <- function(stage, role, note = NA_character_, alt = FALSE) list(stage = stage, role = role, note = note, alt = alt)
shortcut <- "the Data Sources row is filled (it is blank by default, and the stage builds the input itself)"
D <- list(
  list("Landscape", "LandfireFolder", "LANDFIRE fuels and canopy (zips by hand)", "LF 2020 (2.2.0) · FBFM40, CBD, CBH, CC, CH · 30 m", list(
    u("P1", "The fuel map (modal FBFM40 of the 25 cells in each 150 m cell) and the canopy maps (CBD, CBH, CC, CH)", "Zone Preparation Options > Canopy Height Vintage"),
    u("P2", "Fuel-group term of the ignition fit"),
    u("P4", "Pre-fire FBFM40 fuel groups the severity shares are split by"))),
  list("Landscape", "StatewideElevationFile", "LANDFIRE elevation", "LF 2020 · 30 m DEM", list(
    u("P1", "The fixed national 150 m lattice (5 x 5 blocks of 30 m cells) and its mean elevation"),
    u("P2", "Elevation, slope and topographic position covariates"))),
  list("Landscape", "BpsFile", "LANDFIRE Biophysical Settings", "LF 2020 · 30 m · with attribute table", list(
    u("P1", "Origin-vegetation classes (Conifer, Hardwood, Shrub, Grass, Other), modal per cell", "Blank = no vegetation map; C5 then fits by cause only"))),
  list("Zones", "FireZoneBoundaryFile", "Pyromes (fire zones)", "RDS-2020-0020 · field PYROME", list(
    u("P1", "The zone polygon, rasterised to the fire-zone grid (code 1000 + pyrome), and the zone boundary"),
    u("P2", "The zone and the buffer the ignition points are drawn from", "Zone Preparation Options > Ignition Buffer"))),
  list("Zones", "FireZoneGridFile", "Fire-zone grid (prebuilt)", "150 m · optional shortcut", list(
    u("P1", "Read in place of rasterising the pyrome boundaries", shortcut, TRUE))),
  list("Zones", "WeatherZoneBoundaryFile", "NWS fire weather zones", "the latest the NWS lists", list(
    u("P1", "Weather-zone grid: rasterised at 30 m, gaps filled within 1 km, modal to 150 m"),
    u("P3", "The weather zones the daily weather is built for"),
    u("P5", "Each RAWS station's weather zone and time zone"))),
  list("Zones", "WeatherZoneGridFile", "Weather-zone grid (prebuilt)", "150 m · optional shortcut", list(
    u("P1", "Read in place of rasterising the NWS zones", shortcut, TRUE))),
  list("Fire record", "FpaFodFile", "FPA-FOD fire occurrence", "7th edition · 1992-2020 · GeoPackage", list(
    u("P2", "Origins of the large fires (405 ha and up) the ignition surfaces are fitted to", "Zone Preparation Options > Ignition fire size, years, buffer"),
    u("C1", "Every recorded fire whose ignition point is in the zone: date, size, cause"))),
  list("Fire record", "MtbsPerimeterFile", "MTBS burned area boundaries", "latest release · 1984 on", list(
    u("P3", "Fire-activity weights of the gridMET cells (fires / 8 + 0.05)"),
    u("C7", "Where large fires burned after the fuel vintage: the cells burn probability is scored on", "Validation Options > minimum fire size, years"),
    u("R", "Large fires after the fuel vintage, outlined on the burn probability figure"))),
  list("Fire record", "MtbsSeverityFolder", "MTBS burn severity mosaics", "mtbs_CONUS_1984 to 2024 · 30 m", list(
    u("P1", "Moderate and high severity burns of the ten years to the fuel vintage: the fire-age classes 521-523"),
    u("P4", "Severity class shares by fuel group for the fires after the vintage", "Zone Preparation Options > Severity years (default 2021-2024)"))),
  list("Fire record", "InteragencyPerimeterFile", "Interagency fire perimeters", "NIFC · all years", list(
    u("P1", "Fires under 405 ha (too small for MTBS) in the fire-age classes"))),
  list("Fire record", "FireAgeFile", "Fire-age classes (prebuilt)", "150 m · optional shortcut", list(
    u("P1", "Read in place of building the classes from MTBS and NIFC", shortcut, TRUE))),
  list("Weather", "GridMetFolder", "gridMET daily meteorology", "4 km · 1979 to the last full year", list(
    u("P2", "Warm-season rain covariates (wet days and June-September share of rain)"),
    u("P3", "Daily temperature, humidity, wind and precipitation of each weather zone"))),
  list("Weather", "ZoneDailyWeatherFile", "Zone-daily weather table (prebuilt)", "CSV · optional shortcut", list(
    u("P3", "Read as is in place of the gridMET build", shortcut, TRUE))),
  list("Weather", "RawsHourlyFolder", "RAWS hourly observations", "FEMS · 2006-2024", list(
    u("P5", "Hourly wind of the stations within 10 km; missing station-years are downloaded into the folder", "Wind Table Options (months, years, hours)"))),
  list("Weather", "RawsCatalogueFile", "RAWS station catalogue", "FEMS JSON · optional", list(
    u("P5", "Station locations; blank = fetched from FEMS"))),
  list("Ignition covariates", "IgnitionCovariateFolder", "Ignition covariates (prebuilt)", "30 m · optional shortcut", list(
    u("P2", "Read in place of building the ten covariates for the fit domain", shortcut, TRUE))),
  list("Ignition covariates", "RoadsFile", "LANDFIRE roads", "LF 2023 · 30 m", list(u("P2", "Distance to the nearest road (dist_road)"))),
  list("Ignition covariates", "NlcdLandCoverFile", "Annual NLCD land cover", "Collection 1.2 · 2010 · 30 m", list(
    u("P2", "Distance to developed land and the developed share within 990 m (dist_dev, dev_frac)"))),
  list("Ignition covariates", "HousingDensityFile", "SILVIS housing density", "WUI v4 · HUDEN2010", list(u("P2", "Housing density covariate (huden)"))),
  list("Ignition covariates", "PrismPrecipFile", "PRISM precipitation normal", "1991-2020 · 4 km", list(u("P2", "Mean annual precipitation covariate (ppt)"))),
  list("Benchmark", "WrcBurnProbabilityFile", "WRC burn probability", "2nd edition · RDS-2020-0016-2 · 30 m", list(
    u("C7", "Benchmark burn probability, scored on the same cells as the model's", "Blank = C7 scores without the benchmark"),
    u("R", "WRC's burn probability beside the model's (Fig2) and the skill comparison (Fig7)"))))
names(D) <- vapply(D, `[[`, "", 2)
