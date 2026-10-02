# =============================================================================
# P5 - Burning-Period Wind (RAWS)
# =============================================================================
# Builds the zone's Burning-Period Wind Ratio Table, which stages 6 and 8 use to turn each fire-hour's DAILY-MEAN 10 m wind
# (the weather stream) into the 20-ft wind the fire burned in. Ported from the development scripts 219 (FEMS download) and
# 220 (ratio table), with one change: the ratio's denominator is the zone's own Daily Weather (P3) for the station's weather
# zone - the same daily wind stage 6 multiplies - instead of the gridMET cell at the station.
#   stations  RAWS Stations if given; else every station of the FEMS catalogue (Data Sources: RawsCatalogueFile) inside the
#             zone boundary (P1) or within WindTableOption.BufferKm of it (default 10 km); written back to RAWS Stations
#   records   hourly FEMS observations, one CSV per station-year in Data Sources: RawsHourlyFolder
#             (<station id>_<year>.csv, as served by https://fems.fs2c.usda.gov/api/climatology/download-weather);
#             missing station-years are downloaded when WindTableOption.DownloadMissing is on (public, no key)
#   pairs     burning-period hours (FirstHour-LastHour in the station's local time - Time Zone option, else its NWS fire weather
#             zone's time zone; FirstMonth-LastMonth, FirstYear-LastYear), QC wind 0-150 km/h
#             and WSflag 0; each paired with the Daily Weather wind of the station's weather zone on that date (> 0)
#   table     ratio = RAWS hourly 20-ft wind / daily-mean 10 m wind, by daily-wind bin (0, 8, 12, 16, 20, 25, 30, Inf km/h),
#             201 quantiles per bin (0 to 1 by 0.005); a bin with fewer than 200 hours is left out (stage 6 then uses the
#             next lower bin for those days)
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE)
progressBar(type = "begin", totalSteps = 5)
wo <- read_ds("WindTableOption")
opt <- function(k, d) if (nrow(wo) && k %in% names(wo) && !is.na(wo[[k]][1])) wo[[k]][1] else d
Y0 <- opt("FirstYear", 2006L); Y1 <- opt("LastYear", 2024L); M0 <- opt("FirstMonth", 6L); M1 <- opt("LastMonth", 11L)
H0 <- opt("FirstHour", 12L); H1 <- opt("LastHour", 19L); BUF <- opt("BufferKm", 10); DL <- isTRUE(as.logical(opt("DownloadMissing", TRUE)))
dsrc <- read_data_sources(c("RawsHourlyFolder", "RawsCatalogueFile"), optional = TRUE)
RAWD <- dsrc$RawsHourlyFolder; if (is.na(RAWD) || !nzchar(RAWD)) fail("Data Sources: set RAWS Hourly Records Folder (where the station-year CSVs are kept)")
if (!dir.exists(RAWD)) { if (DL) dir.create(RAWD, recursive = TRUE) else fail("RAWS Hourly Records Folder not found (%s)", RAWD) }
zb <- need("OutputZoneBoundary", "FileName"); zone <- st_union(st_make_valid(read_sf(zb$FileName[1])))
wzr <- read_raster("LandscapeRasters", "WeatherZoneFileName"); if (is.null(wzr)) fail("Landscape Maps has no weather zone grid - run P1 first")
dw <- need("DailyWeather", c("WeatherZone", "Date", "WindSpeed"))
wz <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE))

# ---- stations -------------------------------------------------------------------------------------------------------------
st <- read_ds("RAWSStation")
if (nrow(st)) { say("RAWS Stations given: %d", nrow(st))
} else {
  cf <- dsrc$RawsCatalogueFile
  if (is.na(cf) || !nzchar(cf) || !file.exists(cf)) {   # no catalogue on disk: the national FEMS station list (stationMetaData, public),
    cf2 <- file.path(RAWD, "fems_raws_catalogue.json")   # cached beside the hourly records
    if (!file.exists(cf2)) { if (!DL) fail("RAWS Stations is empty, no RAWS Station Catalogue in Data Sources, and Download Missing is off")
      say("fetching the national RAWS station catalogue from FEMS")
      q <- '{"query":"{ stationMetaData { _metadata { total_count } data { station_id station_name latitude longitude elevation station_status } } }"}'
      h <- curl::new_handle(); curl::handle_setheaders(h, "Content-Type" = "application/json"); curl::handle_setopt(h, postfields = q)
      ok <- tryCatch({ curl::curl_download("https://fems.fs2c.usda.gov/api/climatology/graphql", cf2, handle = h); TRUE }, error = function(e) FALSE)
      if (!ok || !file.exists(cf2)) fail("could not fetch the FEMS station catalogue - set Data Sources: RAWS Station Catalogue") }
    cf <- cf2 }
  cat0 <- jsonlite::fromJSON(cf); cat0 <- as.data.table(if (!is.null(cat0$data$stationMetaData$data)) cat0$data$stationMetaData$data else cat0)
  say("RAWS catalogue %s: %d stations", basename(cf), nrow(cat0))
  p <- st_transform(st_as_sf(cat0, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE), st_crs(zone))
  d <- as.numeric(st_distance(p, zone)) / 1000; k <- d <= BUF
  if (!any(k)) fail("no RAWS station of the catalogue inside the zone or within %g km of it", BUF)
  st <- cat0[k, .(StationID = as.integer(station_id), Name = station_name, Latitude = latitude, Longitude = longitude, Elevation = round(elevation * 0.3048))]
  say("RAWS stations from the catalogue: %d inside the zone, %d more within %g km", sum(d == 0), sum(k & d > 0), BUF)
}
xy <- terra::project(vect(st, geom = c("Longitude", "Latitude"), crs = "EPSG:4326"), crs(wzr))
st[, zcode := terra::extract(wzr, xy, ID = FALSE)[[1]]]; st[, WeatherZone := wz$Name[match(zcode, wz$ID)]]
if (any(is.na(st$WeatherZone))) say("%d station(s) off the weather zone grid, left out: %s", sum(is.na(st$WeatherZone)), paste(st$Name[is.na(st$WeatherZone)], collapse = ", "))
write_ds(st[, .(StationID, Name, Latitude, Longitude, Elevation)], "RAWSStation")
st <- st[!is.na(WeatherZone)]; if (!nrow(st)) fail("no RAWS station on this zone's weather zone grid")
# local clock time for each station: Wind Table Options' Time Zone if set, else the TIME_ZONE of the station's NWS fire weather
# zone (Data Sources: WeatherZoneBoundaryFile; P = Pacific, M = Mountain, m = Arizona, C = Central, E = Eastern; for a split
# code such as CM the first letter), else Pacific with a warning (the package's California origin)
TZC <- c(P = "America/Los_Angeles", M = "America/Denver", m = "America/Phoenix", C = "America/Chicago", E = "America/New_York")
TZ <- opt("TimeZone", NA_character_)
if (!is.na(TZ) && nzchar(TZ)) { if (!TZ %in% OlsonNames()) fail("Wind Table Options: unknown time zone '%s' (use a name such as America/Denver)", TZ); st[, tz := TZ]
} else { wzb <- read_data_sources("WeatherZoneBoundaryFile", optional = TRUE)$WeatherZoneBoundaryFile
  if (length(wzb) && !is.na(wzb) && nzchar(wzb)) {
    fz <- read_sf(wzb); pt <- st_transform(st_as_sf(st, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE), st_crs(fz))
    code <- st_drop_geometry(fz)$TIME_ZONE[unlist(lapply(st_intersects(pt, fz), function(i) if (length(i)) i[1] else NA_integer_))]
    st[, tz := TZC[substr(gsub("[^A-Za-z]", "", code), 1, 1)]]
    if (anyNA(st$tz)) { say("time zone unknown for %d station(s) (outside the fire weather zones or code %s): Pacific used", sum(is.na(st$tz)),
                            paste(unique(code[is.na(st$tz)]), collapse = ", ")); st[is.na(tz), tz := TZC[["P"]]] }
  } else { say("WARNING: no Time Zone in Wind Table Options and no NWS fire weather zones in Data Sources: station clocks taken as Pacific time"); st[, tz := TZC[["P"]]] } }
say("station time zones: %s", paste(names(table(st$tz)), table(st$tz), sep = " ", collapse = "; "))
progressBar()

# ---- hourly records (download what is missing) -----------------------------------------------------------------------------
jobs <- CJ(id = st$StationID, yr = Y0:Y1); jobs[, f := file.path(RAWD, sprintf("%d_%d.csv", id, yr))]
todo <- jobs[!file.exists(f)]
if (nrow(todo) && DL) {
  say("downloading %d station-years from FEMS (%d already on disk)", nrow(todo), nrow(jobs) - nrow(todo)); nok <- 0L
  for (i in seq_len(nrow(todo))) { j <- todo[i]
    u <- sprintf("https://fems.fs2c.usda.gov/api/climatology/download-weather?stationIds=%d&startDate=%d-01-01T00:00:00Z&endDate=%d-12-31T23:00:00Z&dataset=observation&dataFormat=csv&dataIncrement=hourly", j$id, j$yr, j$yr)
    tmp <- paste0(j$f, ".part")
    for (k in 1:4) { ok <- tryCatch({ utils::download.file(u, tmp, quiet = TRUE, mode = "wb"); TRUE }, error = function(e) FALSE)
      if (ok && file.exists(tmp)) { file.rename(tmp, j$f); nok <- nok + 1L; break }; Sys.sleep(5 * k) }
    if (i %% 25 == 0) say("  %d of %d", i, nrow(todo)) }
  say("downloaded %d of %d station-years", nok, nrow(todo))
} else if (nrow(todo)) say("%d station-years missing and Download Missing is off: using what is on disk", nrow(todo))
fs <- jobs[file.exists(f) & file.size(f) > 1000]$f
if (!length(fs)) fail("no RAWS hourly records for this zone's stations in %s", RAWD)
progressBar()

# ---- burning-period hours, paired with the zone's daily wind ------------------------------------------------------------------
R <- rbindlist(lapply(fs, function(f) fread(f, select = c("DateTime", "WindSpeed(mph)", "GustSpeed(mph)", "WSflag", "StationId"), showProgress = FALSE)), fill = TRUE)
setnames(R, c("dt", "ws_mph", "gust_mph", "wsflag", "StationID")); R <- R[!is.na(ws_mph) & !is.na(dt)]
if (inherits(R$dt, "POSIXct")) R[, t := dt] else R[, t := as.POSIXct(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
R <- R[!is.na(t)]; R[st, on = "StationID", tz := i.tz]
R[, tl := format(t, tz = .BY$tz, usetz = FALSE), by = tz]
R[, `:=`(Date = substr(tl, 1, 10), hr = as.integer(substr(tl, 12, 13)), ws = ws_mph * 1.609344)]
R <- R[ws >= 0 & ws <= 150 & (is.na(wsflag) | wsflag == 0)]
R <- R[as.integer(substr(Date, 6, 7)) %between% c(M0, M1) & hr %between% c(H0, H1) & as.integer(substr(Date, 1, 4)) %between% c(Y0, Y1)]
R[st, on = "StationID", WeatherZone := i.WeatherZone]
dw[, Date := substr(as.character(Date), 1, 10)]
X <- dw[, .(WeatherZone, Date, vs = WindSpeed)][R, on = c("WeatherZone", "Date"), nomatch = 0][vs > 0]
if (nrow(X) < 1000) fail("only %d burning-period hours paired with Daily Weather - check the years, the stations and P3", nrow(X))
X[, ratio := ws / vs]
say("burning-period hours (%02d-%02d local, months %d-%d, %d-%d, QC'd): %s from %d stations; daily-mean 10 m wind median %.1f km/h, RAWS 20-ft median %.1f km/h",
    H0, H1, M0, M1, Y0, Y1, format(nrow(X), big.mark = ","), uniqueN(X$StationID), median(X$vs), median(X$ws))
progressBar()

# ---- the table ------------------------------------------------------------------------------------------------------------
BR <- c(0, 8, 12, 16, 20, 25, 30, Inf); X[, b := findInterval(vs, BR, left.open = FALSE)]
nb <- X[, .N, by = b]; keep <- nb[N >= 200]$b; if (!length(keep)) fail("no daily-wind bin has 200 paired hours")
if (length(setdiff(nb$b, keep))) say("bins with fewer than 200 hours left out: %s", paste(sprintf("%g+ km/h", BR[setdiff(nb$b, keep)]), collapse = ", "))
P <- seq(0, 1, by = 0.005)
Q <- X[b %in% keep, .(Quantile = P, Ratio = quantile(ratio, P, names = FALSE)), by = b][order(b, Quantile)]
Q[, `:=`(LowerWindSpeed = BR[b], UpperWindSpeed = ifelse(is.finite(BR[b + 1]), BR[b + 1], NA_real_))]
for (bb in sort(keep)) { r <- X[b == bb]$ratio
  say("  daily wind %2g-%s km/h: %8s hours, ratio median %.2f, p90 %.2f, p99 %.2f", BR[bb], ifelse(is.finite(BR[bb + 1]), BR[bb + 1], "  "), format(length(r), big.mark = ","),
      median(r), quantile(r, .9), quantile(r, .99)) }
S <- X[, .(hours = .N, ratio = median(ratio)), by = StationID][st, on = "StationID", nomatch = 0][order(-ratio)]
say("median ratio by station: %s", paste(sprintf("%s %.2f", S$Name, S$ratio), collapse = "; "))
if (anyNA(Q$Ratio)) fail("the wind table has NA ratios")                           # checks must not return NA
progressBar()
write_ds(Q[, .(LowerWindSpeed, UpperWindSpeed, Quantile, Ratio)], "WindRatio")
say("Burning-Period Wind Ratio Table: %d bins x %d quantiles; overall median ratio %.2f", length(keep), length(P), median(X$ratio))
progressBar(); progressBar(type = "end")
