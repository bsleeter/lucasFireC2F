# =============================================================================
# C2 - Build Weather Stream
# =============================================================================
# Builds the spread-day weather stream from the full daily record (validated P26 scripts 07b, 180 and 228):
#   delta change   optional: DeltaChange rows add a temperature change (all months, or one month per row); with
#                  HoldDewpoint the relative humidity falls as es(T) / es(T + dT) (Magnus, b 17.625, c 243.04 C)
#   fire weather   Canadian FWI System indices by weather zone (cffdrs), moisture codes carried day to day; the Drought
#                  Code runs through each fire-weather year (1 March - end of February) and is over-wintered each 1 March
#                  (Lawson & Armitage 2008, a = b = 0.75: DC on 30 November, December-February precipitation); the first
#                  year starts from the defaults (FFMC 85, DMC 6, DC 15) as spin-up. Latitude = the zone's (WeatherZone).
#   stream         days in CalibrationPeriod's weather years with FWI >= WeatherOption.MinimumFWI, one record per day,
#                  ordered by zone and date; season from the date's month (Season table)
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(cffdrs); library(parallel) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); source(file.path(env$PackageDirectory, "src", "calibration.R"))
progressBar(type = "begin", totalSteps = 3)
cp <- need("CalibrationPeriod", c("WeatherFirstYear", "WeatherLastYear"))
wo <- read_ds("WeatherOption"); MINF <- if (nrow(wo) && !is.na(wo$MinimumFWI)) wo$MinimumFWI else 19
dw <- need("DailyWeather", c("WeatherZone", "Date", "Temperature", "RelativeHumidity", "WindSpeed", "Precipitation"))
wz <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE)); se <- as.data.table(datasheet(prj, ds("Season"), lookupsAsFactors = FALSE))
miss <- setdiff(unique(dw$WeatherZone), wz[!is.na(Latitude), Name]); if (length(miss)) fail("weather zones with no latitude: %s", paste(miss, collapse = ", "))
dw[, date := as.IDate(Date)]; setorder(dw, WeatherZone, date)
dc <- read_ds("DeltaChange")
if (nrow(dc)) { es <- function(t) exp(17.625 * t / (243.04 + t)); mon <- month(dw$date)
  for (i in seq_len(nrow(dc))) { r <- dc[i]; k <- if (is.na(r$Month)) rep(TRUE, nrow(dw)) else mon == r$Month
    if (!isFALSE(r$HoldDewpoint)) dw[k, RelativeHumidity := pmin(100, RelativeHumidity * es(Temperature) / es(Temperature + r$TemperatureChange))]
    dw[k, Temperature := Temperature + r$TemperatureChange]
    say("delta change: %+.1f C%s%s", r$TemperatureChange, if (is.na(r$Month)) "" else sprintf(" in month %d", r$Month), if (!isFALSE(r$HoldDewpoint)) ", dewpoint held" else "") } }
progressBar()

fwi_zone <- function(d, la) {                                   # 07b's over-wintered FWI, unchanged
  suppressMessages({ library(cffdrs); library(data.table) })
  mon <- as.integer(format(d$date, "%m")); yr <- as.integer(format(d$date, "%Y")); fy <- ifelse(mon >= 3, yr, yr - 1L)
  inp <- data.frame(long = 0, lat = la, yr = yr, mon = mon, day = as.integer(format(d$date, "%d")),
                    temp = d$Temperature, rh = d$RelativeHumidity, ws = d$WindSpeed, prec = d$Precipitation)
  out <- vector("list", length(unique(fy))); init <- data.frame(ffmc = 85, dmc = 6, dc = 15, lat = la)
  for (k in seq_along(out)) {
    i <- which(fy == sort(unique(fy))[k])
    o <- suppressWarnings(cffdrs::fwi(inp[i, ], init = init, batch = TRUE)); out[[k]] <- o[, c("FFMC", "DMC", "DC", "ISI", "BUI", "FWI")]
    if (k < length(out)) { m <- mon[i]; dd <- inp$day[i]; nov30 <- which(m == 11 & dd == 30); winter <- m %in% c(12, 1, 2)
      dcf <- if (length(nov30)) o$DC[nov30] else o$DC[length(i)]
      dcs <- cffdrs::overwinter_drought_code(DCf = dcf, rw = sum(inp$prec[i][winter]), a = 0.75, b = 0.75)
      init <- data.frame(ffmc = o$FFMC[length(i)], dmc = o$DMC[length(i)], dc = dcs, lat = la) } }
  cbind(d, rbindlist(out))
}
zs <- unique(dw$WeatherZone)
cl <- makePSOCKcluster(min(length(zs), max(1, detectCores() - 1))); clusterExport(cl, "fwi_zone")
fw <- rbindlist(parLapply(cl, lapply(zs, function(z) list(d = dw[WeatherZone == z], la = wz[Name == z, Latitude])), function(j) fwi_zone(j$d, j$la)))
stopCluster(cl)
if (anyNA(fw$FWI)) fail("fire weather index could not be computed for %d days", sum(is.na(fw$FWI)))
progressBar()

fw[, `:=`(Temperature = round(Temperature, 2), RelativeHumidity = round(RelativeHumidity, 2), WindSpeed = round(WindSpeed, 2),
          WindDirection = round(WindDirection, 1), Precipitation = round(Precipitation, 2), FFMC = round(FFMC, 4), DMC = round(DMC, 4),
          DC = round(DC, 4), ISI = round(ISI, 4), BUI = round(BUI, 4), FWI = round(FWI, 4))]
S <- fw[year(date) >= cp$WeatherFirstYear & year(date) <= cp$WeatherLastYear & FWI >= MINF][order(WeatherZone, date)]
S[, `:=`(Order = .I, Season = season_of(date, se))]
write_ds(S[, .(Order, Date = format(date), Season, WeatherZone, Temperature, RelativeHumidity, WindSpeed, WindDirection, Precipitation,
               FineFuelMoistureCode = FFMC, DuffMoistureCode = DMC, DroughtCode = DC, InitialSpreadIndex = ISI, BuildupIndex = BUI,
               FireWeatherIndex = FWI)], "WeatherStream")
tb <- S[, .N, by = Season][order(-N)]
say("spread-day stream: %s days with FWI >= %g, %d-%d, %d weather zones (%s); summer median FWI %.1f, BUI %.1f",
    format(nrow(S), big.mark = ","), MINF, cp$WeatherFirstYear, cp$WeatherLastYear, length(zs), paste(tb$Season, tb$N, collapse = ", "),
    median(S[Season == "Summer", FWI]), median(S[Season == "Summer", BUI]))
progressBar(); progressBar(type = "end")
