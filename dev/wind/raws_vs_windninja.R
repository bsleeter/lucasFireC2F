# Is terrain wind worth modelling? Compares RAWS afternoon winds with the regional (gridMET zone) wind on the same fire-weather
# days, and asks whether WindNinja's terrain speed ratio and direction deviation at each station explain the differences
# better than the uniform wind the model uses now (ratio 1, deviation 0).
#   observed   station afternoon (13-17 PDT) mean speed and vector-mean direction, June-October, stream days only
#              (zone FWI >= 19, over-wintered codes), divided by / minus the zone's gridMET wind (10 m -> 20 ft / 1.15)
#   predicted  WindNinja (run_windninja.ps1) at the station's 150 m cell for the regional direction nearest the day's
#              gridMET direction: speed ratio = 20-ft output / (20 km/h / 1.15), deviation = output direction - input
#   skill      station x direction-sector cells (>= 10 days): speed 1 - MSE(log obs - log pred - offset) / MSE(log obs - mean);
#              direction mean |obs dev - pred dev| vs mean |obs dev| (the uniform-wind baseline)
# Also summarises the WindNinja fields over the zone (by elevation band and fuel group) and writes maps for the paper.
# Usage: Rscript raws_vs_windninja.R <windninja dir>
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf) })
WN <- commandArgs(TRUE)[1]; M <- "F:/lucas-master/Models/California 30m BurnP3"; OUT <- "F:/lucasFireC2F/dev/wind/out"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
X <- readRDS("F:/lucasFireC2F/dev/paper/result_8.rds"); dem <- rast(X$engine$ElevationFileName[1])
circ <- function(a, w = rep(1, length(a))) (atan2(sum(w * sin(a * pi / 180)), sum(w * cos(a * pi / 180))) * 180 / pi) %% 360
dang <- function(a, b) ((a - b + 180) %% 360) - 180   # signed a - b in (-180, 180]

# ---- WindNinja fields: speed ratio and direction deviation per regional direction ------------------------------------------
dd <- list.dirs(WN, recursive = FALSE); dd <- dd[grepl("dir_", basename(dd))]
D <- as.numeric(sub("dir_", "", basename(dd))); o <- order(D); D <- D[o]; dd <- dd[o]
rd <- function(p, pat) { f <- list.files(p, pat, full.names = TRUE); r <- rast(f[1]); crs(r) <- crs(dem); r }
REF <- 20 / 1.15
SR <- rast(lapply(dd, function(p) rd(p, "_vel\\.asc$") / REF)); names(SR) <- D
DV <- rast(lapply(seq_along(dd), function(i) { a <- rd(dd[i], "_ang\\.asc$"); app(a, function(x) dang(x, D[i])) })); names(DV) <- D
cat(sprintf("WindNinja: %d directions; grid matches the model grid: %s\n", length(D), compareGeom(SR, dem, stopOnError = FALSE)))
writeRaster(SR, file.path(OUT, "p026_speed_ratio.tif"), overwrite = TRUE); writeRaster(DV, file.path(OUT, "p026_direction_dev.tif"), overwrite = TRUE)

# ---- stations, their zone and cell -----------------------------------------------------------------------------------------
st <- fread(file.path(M, "data/wind/p026_raws_stations.csv"))
sp <- st_transform(st_as_sf(st, coords = c("longitude", "latitude"), crs = 4326), crs(dem))
wz <- st_transform(read_sf(file.path(M, "data/zones/weather_zones.gpkg")), crs(dem))
st$zone <- wz$ID[as.integer(st_intersects(sp, wz))]; st$cell <- cellFromXY(dem, st_coordinates(sp))
st$dem_m <- dem[st$cell][, 1]; st <- st[!is.na(cell) & !is.na(zone)]
cat(sprintf("stations on the grid with a weather zone: %d of 48\n", nrow(st)))

# ---- observed afternoon winds on stream days --------------------------------------------------------------------------------
zd <- fread(file.path(M, "data/weather/zone_daily_fwi_ow.csv"))[FWI >= 19, .(zone, date = as.IDate(date), gs = WindSpeed / 1.15, gd = WindDirection)]
rh <- rbindlist(lapply(st$station_id, function(id) { f <- list.files(file.path(M, "data/wind/raws_hourly"), sprintf("^%d_\\d+\\.csv$", id), full.names = TRUE)
  if (!length(f)) return(NULL); h <- rbindlist(lapply(f, fread, select = c("DateTime", "WindSpeed(mph)", "WindAzimuth(degrees)", "WSflag", "WAflag")), fill = TRUE)
  setnames(h, c("t", "ws", "wd", "wsf", "wdf")); h[, t := as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") - 7 * 3600]
  h <- h[!is.na(t) & !is.na(ws) & !is.na(wd) & (is.na(wsf) | wsf == 0) & (is.na(wdf) | wdf == 0) & as.integer(format(t, "%H")) %in% 13:17 &
         as.integer(format(t, "%m")) %in% 6:10 & ws < 150]
  h[, .(station_id = id, n = .N, ss = mean(ws) * 1.609344, sd = if (sum(ws > 0)) circ(wd[ws > 0], ws[ws > 0]) else NA_real_), by = .(date = as.IDate(t))][n >= 4] }))
ob <- merge(merge(rh, st[, .(station_id, zone, cell)], by = "station_id"), zd, by = c("zone", "date"))
ob[, `:=`(ratio = ss / gs, dev = dang(sd, gd), k = ((round(gd / (360 / length(D))) %% length(D)) + 1))]
cat(sprintf("observed station-days: %s from %d stations, %d-%d\n", format(nrow(ob), big.mark = ","), uniqueN(ob$station_id), year(min(ob$date)), year(max(ob$date))))

# ---- predicted at the station for the day's regional direction ---------------------------------------------------------------
srm <- as.matrix(SR[ob$cell]); dvm <- as.matrix(DV[ob$cell]); ix <- cbind(seq_len(nrow(ob)), ob$k)
ob[, `:=`(pr = srm[ix], pd = dvm[ix])]; saveRDS(ob, file.path(OUT, "station_days.rds"))
ss <- ob[is.finite(ratio) & ratio > 0 & !is.na(pr), .(n = .N, lo = median(log(ratio)), lp = log(median(pr)), od = circ(dev), pdv = median(pd)), by = .(station_id, k)][n >= 10]
ss[, od := dang(od, 0)]
off <- weighted.mean(ss$lo - ss$lp, ss$n)
spd <- ss[, .(mse_uniform = weighted.mean((lo - weighted.mean(lo, n))^2, n), mse_wn = weighted.mean((lo - lp - off)^2, n))]
dir <- ss[, .(mae_uniform = weighted.mean(abs(od), n), mae_wn = weighted.mean(abs(dang(od, pdv)), n))]
cat(sprintf("\nstation x direction cells (>= 10 days): %d from %d stations\n", nrow(ss), uniqueN(ss$station_id)))
cat(sprintf("SPEED  log-ratio spread (sd) across stations/directions %.2f; WindNinja explains %.0f%% (skill vs uniform); common offset x%.2f (afternoon + siting)\n",
            sqrt(spd$mse_uniform), 100 * (1 - spd$mse_wn / spd$mse_uniform), exp(off)))
cat(sprintf("        correlation log obs vs log WindNinja %.2f\n", ss[, cov.wt(cbind(lo, lp), wt = n / sum(n), cor = TRUE)$cor[1, 2]]))
cat(sprintf("DIRECTION mean |station - regional| %.0f deg (uniform wind); with WindNinja deviation %.0f deg\n", dir$mae_uniform, dir$mae_wn))
fwrite(ss, file.path(OUT, "station_direction_cells.csv"))
bys <- merge(ss[, .(n = sum(n), obs_ratio = exp(weighted.mean(lo, n)), wn_ratio = exp(weighted.mean(lp, n)), obs_dev = weighted.mean(abs(od), n),
                    wn_dev = weighted.mean(abs(pdv), n)), by = station_id], st[, .(station_id, station_name, elevation_ft = elevation, dem_m)], by = "station_id")
fwrite(bys[order(-obs_ratio)], file.path(OUT, "station_summary.csv")); print(bys[order(-obs_ratio)][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

# ---- the WindNinja fields over the zone, weighted by how often the stream blows from each direction -----------------------------
zone <- read_sf("F:/lucas-master/Models/California 30m BurnP3/data/zones/pyromes.gpkg"); zone <- st_transform(zone[zone$PYROME == 26 | zone$PYROME == "26", ], crs(dem))
inz <- !is.na(values(rasterize(vect(zone), dem), mat = FALSE))
sw <- fread(file.path(M, "data/weather/c2fw_stream_p026_dT+0.0.csv"))
wk <- tabulate(((round(sw$WindDirection / (360 / length(D))) %% length(D)) + 1), length(D)); wk <- wk / sum(wk)
cat("\nstream share by regional direction:", paste(sprintf("%g:%.0f%%", D, 100 * wk), collapse = " "), "\n")
set.seed(1); smp <- sample(which(inz), 200000); sv <- as.matrix(SR[smp]); dv <- as.matrix(DV[smp]); el <- dem[smp][, 1]
kk <- sample(length(D), length(smp), replace = TRUE, prob = wk); pick <- cbind(seq_along(smp), kk)
Z <- data.table(el = el, sr = sv[pick], dv = dv[pick])
qs <- function(x) round(quantile(x, c(.01, .1, .25, .5, .75, .9, .99), na.rm = TRUE), 2)
cat("zone, stream-weighted: speed ratio quantiles (1,10,25,50,75,90,99%):", qs(Z$sr), "\n")
cat("zone, stream-weighted: direction deviation quantiles:", round(qs(Z$dv), 0), "\n")
Z[, band := cut(el, c(-Inf, 500, 1000, 1500, 2000, 2500, Inf), c("<500", "500-1000", "1000-1500", "1500-2000", "2000-2500", ">2500"))]
eb <- Z[, .(share = .N / nrow(Z), median_ratio = median(sr), p10 = quantile(sr, .1), p90 = quantile(sr, .9), mean_abs_dev = mean(abs(dv))), by = band][order(band)]
print(eb[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]); fwrite(eb, file.path(OUT, "zone_by_elevation.csv"))
