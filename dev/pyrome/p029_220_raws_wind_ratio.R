# =============================================================================
# 220  Burning-period wind from RAWS: afternoon 20-ft hourly wind given gridMET's daily-mean 10 m wind
# =============================================================================
# The fire weather stream carries gridMET's DAILY MEAN 10 m wind. Fire effects need the wind the fire burned in:
# afternoon, 20 ft, hourly. This pairs every RAWS afternoon hour (12:00-19:00 local, Jun-Nov, 2006-2024) with
# gridMET's daily mean at the station's cell on the same day and tabulates
#   ratio = RAWS hourly 20-ft wind / gridMET daily-mean 10 m wind
# conditional on the day's gridMET wind (bins), so a windy day keeps its own, wider, ratio distribution.
# 217 then draws each burn hour's 20-ft wind as daily-mean x a ratio from the bin of that day
# (replacing the flat x1.5 placeholder and the fixed 10 m -> 20 ft /1.15).
# QC: wind 0-150 km/h, WSflag 0 only; gridMET vs > 0. Stations pooled (they span foothill to crest).
# Writes data/wind/p029_raws_ratio_quantiles.csv (bin x 201 quantiles) and prints the diagnostics.
# Usage: Rscript src/220_raws_wind_ratio.R
# =============================================================================
suppressMessages({library(data.table); library(terra)})
M <- "F:/lucas-master/Models/California 30m BurnP3"; G <- "F:/lucas-master/Data Sources/Climate/gridMET/vs"
say <- function(...) cat(sprintf(...), "\n", sep = "")
st <- fread(file.path(M, "data/wind/p029_raws_stations.csv"))
fs <- list.files(file.path(M, "data/wind/raws_hourly"), "^\\d+_\\d{4}\\.csv$", full.names = TRUE)
fs <- fs[file.size(fs) > 1000]
R <- rbindlist(lapply(fs, function(f) fread(f, select = c("DateTime", "WindSpeed(mph)", "GustSpeed(mph)", "WSflag", "StationId"), showProgress = FALSE)), fill = TRUE)
setnames(R, c("dt", "ws_mph", "gust_mph", "wsflag", "station_id"))
R <- R[!is.na(ws_mph) & !is.na(dt)]
if (inherits(R$dt, "POSIXct")) R[, t := dt] else R[, t := as.POSIXct(dt, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]   # fread may parse it already
R <- R[!is.na(t)]
R[, tl := format(t, tz = "America/Los_Angeles", usetz = FALSE)]
R[, `:=`(date = as.Date(substr(tl, 1, 10)), hr = as.integer(substr(tl, 12, 13)))]
R[, `:=`(ws = ws_mph * 1.609344, gust = gust_mph * 1.609344)]
R <- R[ws >= 0 & ws <= 150 & (is.na(wsflag) | wsflag == 0)]
R <- R[as.integer(format(date, "%m")) %in% 6:11 & hr >= 12 & hr <= 19]
say("RAWS afternoon hours (12-19 local, Jun-Nov, QC'd): %s from %d stations, %d-%d", format(nrow(R), big.mark = ","),
    uniqueN(R$station_id), min(year(R$date)), max(year(R$date)))

# gridMET daily-mean 10 m wind at each station (km/h), per year file
pts <- vect(st, geom = c("longitude", "latitude"), crs = "EPSG:4326")
yrs <- sort(unique(year(R$date)))
GM <- rbindlist(lapply(yrs, function(y) {
  r <- rast(file.path(G, sprintf("vs_%d.nc", y)))
  v <- terra::extract(r, project(pts, crs(r)), ID = FALSE)
  d <- as.Date(time(r)); if (anyNA(d)) d <- as.Date(sprintf("%d-01-01", y)) + seq_len(nlyr(r)) - 1
  data.table(station_id = rep(st$station_id, each = nlyr(r)), date = rep(d, nrow(st)), vs = as.vector(t(as.matrix(v))) * 3.6)
}))
X <- merge(R, GM, by = c("station_id", "date"))[vs > 0]
X[, ratio := ws / vs]
BR <- c(0, 8, 12, 16, 20, 25, 30, Inf); BL <- c("<8", "8-12", "12-16", "16-20", "20-25", "25-30", "30+")
X[, bin := cut(vs, BR, BL, right = FALSE)]
say("paired afternoon hours: %s; gridMET daily mean median %.1f km/h; RAWS afternoon 20-ft median %.1f km/h",
    format(nrow(X), big.mark = ","), median(X$vs), median(X$ws))

say("\n%-7s %9s | %6s %6s %6s %6s | %8s %8s %8s", "gridMET", "hours", "ratio", "p50", "p90", "p99", "ws>=30", "ws>=40", "gust>=40")
for (b in BL) { k <- X$bin == b; if (!any(k)) next
  say("%-7s %9s | %6.2f %6.2f %6.2f %6.2f | %7.1f%% %7.2f%% %7.1f%%", b, format(sum(k), big.mark = ","), mean(X$ratio[k]),
      median(X$ratio[k]), quantile(X$ratio[k], .9), quantile(X$ratio[k], .99),
      100 * mean(X$ws[k] >= 30), 100 * mean(X$ws[k] >= 40), 100 * mean(X$gust[k] >= 40, na.rm = TRUE)) }
say("ALL     %9s | %6.2f %6.2f %6.2f %6.2f | %7.1f%% %7.2f%% %7.1f%%", format(nrow(X), big.mark = ","), mean(X$ratio), median(X$ratio),
    quantile(X$ratio, .9), quantile(X$ratio, .99), 100 * mean(X$ws >= 30), 100 * mean(X$ws >= 40), 100 * mean(X$gust >= 40, na.rm = TRUE))
say("\nby station (median afternoon ratio; shows exposure spread):")
S <- X[, .(hours = .N, ratio = median(ratio), ws_p99 = quantile(ws, .99)), by = station_id][st, on = "station_id", nomatch = 0][order(-ratio)]
print(S[, .(station_id, station_name, elevation, hours, ratio = round(ratio, 2), ws_p99 = round(ws_p99, 1))], nrows = 60)

Q <- X[, .(p = seq(0, 1, by = 0.005), ratio = quantile(ratio, seq(0, 1, by = 0.005), names = FALSE)), by = bin]
Q[, `:=`(lo = BR[match(bin, BL)], hi = BR[match(bin, BL) + 1])]
fwrite(Q, file.path(M, "data/wind/p029_raws_ratio_quantiles.csv"))
say("\nwrote data/wind/p029_raws_ratio_quantiles.csv (%d bins x 201 quantiles)", uniqueN(Q$bin))
