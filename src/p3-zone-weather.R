# =============================================================================
# P3 - Extract Zone Weather
# =============================================================================
# The Daily Weather the calibration stage C2 builds its FWI stream from, for the weather zones present on this zone's grid (P1).
# Columns: weather zone, date, Temperature (C), RelativeHumidity (%), WindSpeed (km/h, 10 m), WindDirection (degrees from),
# Precipitation (mm). Two sources:
#   table     Data Sources: ZoneDailyWeatherFile - a zone-daily table built beforehand (the California table from development
#             script 07), read as is (UNROUNDED: rounding moved 3 borderline P26 days across FWI 19)
#   gridMET   otherwise built here, anywhere in CONUS, as script 07 built the table: gridMET daily files (Data Sources:
#             GridMetFolder, <var>/<var>_<year>.nc, every year present for all six variables), each zone's value the
#             FIRE-ACTIVITY-WEIGHTED mean of the gridMET cells assigned to it (cell centre in the zone, then any touched cell;
#             all NWS fire weather zones around are rasterized so a shared cell goes to the zone holding its centre).
#             Weight = MTBS perimeters (Data Sources: MtbsPerimeterFile, every perimeter) overlapping the cell / 8 + 0.05
#             (shared.R FIRE_COUNT_REF, WEIGHT_FLOOR), so a zone's stream reflects where fires burn, not its areal average.
#             Temperature = tmmx - 273.15; RelativeHumidity = rmin (1-100); WindSpeed = vs x 3.6; WindDirection = th as a
#             weighted CIRCULAR mean; Precipitation = pr.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE)
progressBar(type = "begin", totalSteps = 3)
src <- read_data_sources(c("ZoneDailyWeatherFile", "GridMetFolder", "WeatherZoneBoundaryFile", "MtbsPerimeterFile"), optional = TRUE)
has <- function(k) { v <- src[[k]]; length(v) && !is.na(v) && nzchar(v) }
wzr <- read_raster("LandscapeRasters", "WeatherZoneFileName"); if (is.null(wzr)) fail("Landscape Maps has no weather zone grid - run P1 first")
codes <- sort(unique(values(wzr, mat = FALSE))); codes <- codes[!is.na(codes) & codes != -9999]
wz <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE))
miss <- setdiff(codes, wz$ID); if (length(miss)) fail("weather zone codes on the grid with no Weather Zone in this project: %s", paste(miss, collapse = ", "))
progressBar()

if (has("ZoneDailyWeatherFile")) {
  if (!file.exists(src$ZoneDailyWeatherFile)) fail("Data Sources: ZoneDailyWeatherFile not found (%s)", src$ZoneDailyWeatherFile)
  dw <- fread(src$ZoneDailyWeatherFile)[zone %in% codes]
  if (!nrow(dw)) fail("no rows for this grid's weather zones in %s", src$ZoneDailyWeatherFile)
  dw <- dw[, .(WeatherZone = wz$Name[match(zone, wz$ID)], Date = as.character(date), Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)]
  say("daily weather from the table %s", basename(src$ZoneDailyWeatherFile))
} else {
  for (k in c("GridMetFolder", "WeatherZoneBoundaryFile", "MtbsPerimeterFile")) if (!has(k)) fail("Data Sources: no Zone-Daily Weather Table, so %s is needed to build the zone weather from gridMET", k)
  VARS <- c("tmmx", "rmin", "vs", "th", "pr")
  yrs <- Reduce(intersect, lapply(VARS, function(v) as.integer(sub(sprintf("^%s_([0-9]{4})[.]nc$", v), "\\1", list.files(file.path(src$GridMetFolder, v), sprintf("^%s_[0-9]{4}[.]nc$", v))))))
  if (!length(yrs)) fail("no gridMET year with all of %s under %s", paste(VARS, collapse = ", "), src$GridMetFolder); yrs <- sort(yrs)
  allz <- st_transform(weather_zone_polygons(src$WeatherZoneBoundaryFile), 4326)
  mine <- allz[allz$Name %in% wz$Name[match(codes, wz$ID)], ]
  if (nrow(mine) != length(codes)) fail("weather zones on the grid not found in %s: %s", basename(src$WeatherZoneBoundaryFile), paste(setdiff(wz$Name[match(codes, wz$ID)], mine$Name), collapse = "; "))
  gm <- rast(file.path(src$GridMetFolder, "vs", sprintf("vs_%d.nc", yrs[1])))[[1]]
  bb <- st_bbox(mine); gm <- crop(gm, ext(bb[["xmin"]] - 0.25, bb[["xmax"]] + 0.25, bb[["ymin"]] - 0.25, bb[["ymax"]] + 0.25), snap = "out")
  near <- allz[lengths(st_intersects(allz, st_as_sfc(st_bbox(gm)))) > 0, ]; near$ID <- seq_len(nrow(near))
  zid <- cover(rasterize(vect(near), gm, field = "ID"), rasterize(vect(near), gm, field = "ID", touches = TRUE))
  lyr <- st_layers(src$MtbsPerimeterFile)$name[1]; pc <- st_crs(read_sf(src$MtbsPerimeterFile, query = sprintf("SELECT * FROM \"%s\" LIMIT 0", lyr)))
  per <- read_sf(src$MtbsPerimeterFile, wkt_filter = st_as_text(st_transform(st_as_sfc(st_bbox(gm)), pc)))
  per <- st_make_valid(st_transform(per, 4326))
  cnt <- rasterize(vect(per), gm, fun = "count", background = 0); wt <- cnt / FIRE_COUNT_REF + WEIGHT_FLOOR
  key <- data.table(cell = seq_len(ncell(gm)), zone = near$Name[values(zid, mat = FALSE)], wt = values(wt, mat = FALSE))[zone %in% mine$Name & !is.na(wt)]
  say("gridMET cells assigned to this grid's %d weather zones: %s; MTBS perimeters around: %s (max %d on one cell)", nrow(mine), format(nrow(key), big.mark = ","),
      format(nrow(per), big.mark = ","), as.integer(global(cnt, "max", na.rm = TRUE)[[1]]))
  zmean <- function(v, y, circ = FALSE) { r <- crop(rast(file.path(src$GridMetFolder, v, sprintf("%s_%d.nc", v, y))), gm, snap = "near")
    m <- as.data.table(values(r)); m[, cell := .I]; m <- melt(m[key, on = "cell", nomatch = 0], id.vars = c("cell", "zone", "wt"), variable.name = "d", value.name = "x")[!is.na(x)]
    if (circ) m[, .(x = (atan2(sum(sin(x * pi / 180) * wt) / sum(wt), sum(cos(x * pi / 180) * wt) / sum(wt)) * 180 / pi) %% 360), by = .(zone, day = as.integer(d))]
    else m[, .(x = sum(x * wt) / sum(wt)), by = .(zone, day = as.integer(d))] }
  dw <- rbindlist(lapply(yrs, function(y) {
    d <- rbindlist(lapply(VARS, function(v) zmean(v, y, circ = v == "th")[, var := v])); d <- dcast(d, zone + day ~ var, value.var = "x")
    if (y %% 10 == 0) say("  gridMET %d done", y)
    d[, .(WeatherZone = zone, Date = as.character(as.Date(sprintf("%d-01-01", y)) + day - 1), Temperature = tmmx - 273.15,
          RelativeHumidity = pmin(100, pmax(1, rmin)), WindSpeed = vs * 3.6, WindDirection = th, Precipitation = pr)] }))
  if (anyNA(dw[, .(Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation)])) fail("the gridMET zone weather has NA values")   # checks must not return NA
  setorder(dw, WeatherZone, Date); say("daily weather built from gridMET %d-%d (fire-activity weighted)", min(yrs), max(yrs))
}
progressBar()
write_ds(dw, "DailyWeather")
say("daily weather: %s rows, %d weather zones, %s to %s", format(nrow(dw), big.mark = ","), uniqueN(dw$WeatherZone), min(dw$Date), max(dw$Date))
progressBar(); progressBar(type = "end")
