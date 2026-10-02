# Observed vs simulated daily fire progression for Pyrome 26.
# OBSERVED  MTBS wildfires >= MINHA, 2012-2024 (VIIRS era), intersecting P26. Each 150 m cell inside the perimeter takes the
#           burning day of its nearest VIIRS 375 m detection (S-NPP 2012+, NOAA-20 2018+; nominal/high confidence; inside the
#           perimeter; ignition - 1 to + 150 days). Burning day = local date of (UTC - 7 h - 5 h): 05:00-05:00 PDT, so the
#           ~01:30 night pass counts toward the afternoon run before it. Cells > 1.5 km from any detection are left out.
# SIMULATED lucasFireC2F kept fires >= MINHA; burning day = spread day (Period %/% 120 + 1).
# SAME METRIC for both, day 2 onward: HEAD RUN = the largest distance from a cell burned that day to the nearest cell burned
#           on an earlier day (the day's advance beyond the previous perimeter); also its 95th percentile, and the day's
#           growth (ha). Active days = days with growth. Day 1 is left out (no observed ignition point).
# Usage: Rscript observed_vs_model_daily_runs.R <result id> [minimum fire size ha = 1000]
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf); library(arrow); library(dplyr); library(FNN) })
a <- commandArgs(TRUE); id <- as.integer(a[1]); MINHA <- if (length(a) > 1) as.numeric(a[2]) else 1000
OUT <- "F:/lucasFireC2F/dev/spread"; M <- "F:/lucas-master/Models/California 30m BurnP3"; VI <- "F:/lucas-master/Data Sources/VIIRS/archive"
Sys.setenv(ZONE_SYSTEM = "pyrome"); source(file.path(M, "src/00_config.R")); sf_use_s2(FALSE)
X <- readRDS(sprintf("F:/lucasFireC2F/dev/paper/result_%d.rds", id)); g <- rast(X$engine$ElevationFileName[1]); CS <- res(g)[1]
zone <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zone <- st_transform(zone[zone$PYROME == 26, ], crs(g))

runs <- function(x, y, day) {   # per day >= 2: max and p95 distance (km) from that day's cells to the nearest earlier cell
  ds <- sort(unique(day)); r <- list()
  for (d in ds[ds > min(ds)]) { A <- which(day == d); P <- which(day < d); if (!length(A) || !length(P)) next
    nn <- get.knnx(cbind(x[P], y[P]), cbind(x[A], y[A]), k = 1)$nn.dist[, 1] / 1000
    r[[length(r) + 1]] <- data.table(day = d - min(ds) + 1, run_km = max(nn), run95_km = unname(quantile(nn, .95)), growth_ha = length(A) * CS^2 / 1e4) }
  rbindlist(r) }

# ---- observed ----------------------------------------------------------------------------------------------------------------------
mt <- read_mtbs() |> filter(Incid_Type == "Wildfire", !is.na(Ig_Date)) |> mutate(yr = as.integer(format(as.Date(Ig_Date), "%Y")), ha = BurnBndAc * 0.404686) |>
  filter(yr >= 2012, yr <= 2024, ha >= MINHA) |> st_transform(crs(g))
mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
cat(sprintf("MTBS wildfires >= %g ha intersecting P26, 2012-2024: %d\n", MINHA, nrow(mt)))
vf <- c(list.files(file.path(VI, "VIIRS_SNPP_SP"), "csv$", full.names = TRUE), list.files(file.path(VI, "VIIRS_NOAA20_SP"), "csv$", full.names = TRUE))
v <- rbindlist(lapply(vf, fread, select = c("latitude", "longitude", "acq_date", "acq_time", "confidence", "satellite"), colClasses = list(character = c("confidence", "satellite", "acq_time"))), fill = TRUE)
v <- v[confidence != "l"]; v[, t := as.POSIXct(paste(acq_date, sprintf("%04d", as.integer(acq_time))), format = "%Y-%m-%d %H%M", tz = "UTC")]
v[, fday := as.IDate(t - 12 * 3600)]
xy <- st_coordinates(st_transform(st_as_sf(v, coords = c("longitude", "latitude"), crs = 4326), crs(g))); v[, `:=`(x = xy[, 1], y = xy[, 2])]
cat(sprintf("VIIRS detections (nominal/high): %s\n", format(nrow(v), big.mark = ",")))
ob <- list(); oa <- list()
for (i in seq_len(nrow(mt))) { p <- mt[i, ]; ig <- as.IDate(p$Ig_Date); b <- st_bbox(p)
  vi <- v[x >= b$xmin & x <= b$xmax & y >= b$ymin & y <= b$ymax & fday >= ig - 1 & fday <= ig + 150]
  if (nrow(vi) < 10) next
  vi <- vi[lengths(st_intersects(st_as_sf(vi, coords = c("x", "y"), crs = crs(g)), p)) > 0]; if (nrow(vi) < 10) next
  tpl <- rast(ext(align(ext(vect(p)), g, snap = "out")), resolution = CS, crs = crs(g)); cells <- which(!is.na(values(rasterize(vect(p), tpl), mat = FALSE)))
  cx <- xyFromCell(tpl, cells); nn <- get.knnx(cbind(vi$x, vi$y), cx, k = 1); ok <- nn$nn.dist[, 1] <= 1500
  day <- as.integer(vi$fday[nn$nn.index[ok, 1]])
  r <- runs(cx[ok, 1], cx[ok, 2], day); if (!nrow(r)) next
  r[, `:=`(fire = p$Incid_Name, year = p$yr, fire_ha = round(p$ha))]; ob[[length(ob) + 1]] <- r
  oa[[length(oa) + 1]] <- data.table(fire = p$Incid_Name, year = p$yr, fire_ha = round(p$ha), active_days = uniqueN(day), span_days = max(day) - min(day) + 1, detections = nrow(vi)) }
ob <- rbindlist(ob); oa <- rbindlist(oa)
fwrite(ob, file.path(OUT, "observed_daily_runs_p026.csv")); fwrite(oa, file.path(OUT, "observed_fires_p026.csv"))
cat(sprintf("observed: %d fires, %d fire-days (day 2+)\n", nrow(oa), nrow(ob))); print(oa[order(-fire_ha)][1:min(15, .N)])

# ---- simulated ---------------------------------------------------------------------------------------------------------------------
NC <- ncol(g)
k <- X$fire_stat[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINHA][, .(Iteration = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)),
       FireID = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)), fire_ha = Area, SpreadDays)]
b <- as.data.table(open_dataset(X$burn_table) |> select(Iteration, FireID, Cell, Period) |> collect())[k[, .(Iteration, FireID)], on = c("Iteration", "FireID"), nomatch = 0]
b[, `:=`(day = as.integer(Period %/% 120) + 1L, x = ((Cell - 1L) %% NC) * CS, y = -((Cell - 1L) %/% NC) * CS)]
sm <- b[, runs(x, y, day), by = .(Iteration, FireID)]; sm <- k[sm, on = c("Iteration", "FireID")]
fwrite(sm, file.path(OUT, sprintf("simulated_daily_runs_r%d.csv", id)))
cat(sprintf("simulated: %s fires, %s fire-days (day 2+)\n", format(nrow(k), big.mark = ","), format(nrow(sm), big.mark = ",")))

# ---- compare -----------------------------------------------------------------------------------------------------------------------
q <- function(x) round(quantile(x, c(.1, .25, .5, .75, .9, .99)), 2)
cmp <- rbind(data.table(set = "observed (VIIRS)", t(q(ob$run_km))), data.table(set = "simulated", t(q(sm$run_km))))
cat("\ndaily head run, km (day 2+):\n"); print(cmp)
cat("\nlargest daily run per fire, km:\n"); print(rbind(data.table(set = "observed", t(q(ob[, max(run_km), by = fire]$V1))), data.table(set = "simulated", t(q(sm[, max(run_km), by = .(Iteration, FireID)]$V1)))))
cat("\ndaily growth, ha (day 2+):\n"); print(rbind(data.table(set = "observed", t(q(ob$growth_ha))), data.table(set = "simulated", t(q(sm$growth_ha)))))
sa <- b[, .(active_days = uniqueN(day)), by = .(Iteration, FireID)][k, on = c("Iteration", "FireID")]
sz <- function(h) cut(h, c(MINHA, 3000, 10000, 30000, Inf), c("1k-3k", "3k-10k", "10k-30k", "30k+"), right = FALSE)
cat("\nactive (growth) days per fire, median by size:\n")
print(merge(oa[, .(obs_fires = .N, obs_days = median(active_days)), by = .(size = sz(fire_ha))], sa[, .(sim_fires = .N, sim_days = median(active_days)), by = .(size = sz(fire_ha))], by = "size", all = TRUE)[order(size)])

png(file.path(OUT, sprintf("Fig_observed_vs_model_runs_r%d.png", id)), 2600, 1100, res = 170); par(mfrow = c(1, 3), mar = c(4.5, 4.5, 3, 1))
ec <- function(x) { s <- sort(x); list(x = s, y = 1 - (seq_along(s) - 1) / length(s)) }
e1 <- ec(ob$run_km); e2 <- ec(sm$run_km)
plot(e2$x, e2$y, log = "xy", type = "l", lwd = 2, col = "#dd4b20", xlim = range(c(.1, e1$x, e2$x)), ylim = c(1e-4, 1), xlab = "Daily head run (km)", ylab = "Exceedance probability", main = "(a) Daily head runs, day 2+", las = 1)
lines(e1$x, e1$y, lwd = 2, col = "#20211f"); legend("bottomleft", c(sprintf("VIIRS, %d P26 fires 2012-24", nrow(oa)), sprintf("lucasFireC2F r%d", id)), col = c("#20211f", "#dd4b20"), lwd = 2, bty = "n")
e1 <- ec(ob$growth_ha); e2 <- ec(sm$growth_ha)
plot(e2$x, e2$y, log = "xy", type = "l", lwd = 2, col = "#dd4b20", xlim = range(c(e1$x, e2$x)), ylim = c(1e-4, 1), xlab = "Daily growth (ha)", ylab = "Exceedance probability", main = "(b) Daily growth, day 2+", las = 1); lines(e1$x, e1$y, lwd = 2, col = "#20211f")
boxplot(list(Observed = oa$active_days, Simulated = sa$active_days), log = "y", col = c("grey70", "#f4a37a"), ylab = "Active (growth) days per fire", main = sprintf("(c) Active days, fires >= %g ha", MINHA), las = 1)
dev.off(); cat("wrote figure\n")
