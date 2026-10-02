# Daily head-fire runs: how far each fire's front advances on each spread day. From the stage 4 burn table (spread network)
#   day          spread day of a burn = floor(Period / MinutesPerDay) + 1 (C2F-W Period = burning minutes since ignition)
#   anchor       for each burn, its ancestor along the spread network that burned before this burn's day began (day 1: the
#                ignition cell) - where the day's run that reached it started
#   run          straight-line distance anchor -> burn; the fire-day HEAD RUN = the longest of these that day, with its heading
# Writes a fire-day table (with the day's weather-stream wind), summaries, per-cell layers and a figure.
# Usage: Rscript head_fire_runs.R <result id> [minimum burns = 20] [burning minutes per spread day = 120]
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf); library(arrow); library(dplyr); library(rsyncrosim); library(Rcpp) })
a <- commandArgs(TRUE); id <- as.integer(a[1]); MINB <- if (length(a) > 1) as.integer(a[2]) else 20L; MPD <- if (length(a) > 2) as.numeric(a[3]) else 120
OUT <- "F:/lucasFireC2F/dev/spread"; M <- "F:/lucas-master/Models/California 30m BurnP3"
X <- readRDS(sprintf("F:/lucasFireC2F/dev/paper/result_%d.rds", id)); g <- rast(X$engine$ElevationFileName[1]); NC <- ncol(g); CS <- res(g)[1]
t0 <- Sys.time(); el <- function() sprintf("%.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

cppFunction('IntegerVector anchors(IntegerVector src, IntegerVector day) {
  int n = src.size(); IntegerVector a(n, NA_INTEGER); int left = n, pass = 0;
  while (left > 0 && pass < 1000) { left = 0; pass++;
    for (int i = 0; i < n; i++) { if (a[i] != NA_INTEGER) continue;
      int s = src[i];
      if (s == NA_INTEGER) { a[i] = i + 1; continue; }                 // ignition: its own anchor (1-based row)
      if (day[s - 1] < day[i]) { a[i] = s; continue; }                 // source burned on an earlier day: the run starts there
      if (a[s - 1] != NA_INTEGER) { a[i] = a[s - 1]; continue; }       // same day: inherit the source anchor
      left++; } }
  return a; }')

# ---- kept fires, their burns and spread network ----------------------------------------------------------------------------
st <- X$fire_stat[ResampleStatus %in% c("Kept", "Reassigned")]
k <- st[, .(Iteration = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), FireID = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)),
            Season = as.integer(Iteration), SeasonFireID = as.integer(FireID), SpreadDays, AreaHa = Area)]
b <- as.data.table(open_dataset(X$burn_table) |> select(Iteration, FireID, Cell, Source, Period) |> collect())[k[, .(Iteration, FireID)], on = c("Iteration", "FireID"), nomatch = 0]
setorder(b, Iteration, FireID, Period); b[, row := .I]
b[b[, .(Iteration, FireID, Source = Cell, srow = row)], on = c("Iteration", "FireID", "Source"), srow := i.srow]
if (b[!is.na(Source), anyNA(srow)]) stop("burns whose source cell is not a burn of the same fire")
b[, day := as.integer(Period %/% MPD) + 1L]
b[, anc := anchors(ifelse(is.na(srow), NA_integer_, srow), day)]
ac <- b$Cell[b$anc]
b[, `:=`(dr = (Cell - 1L) %/% NC - (ac - 1L) %/% NC, dc = (Cell - 1L) %% NC - (ac - 1L) %% NC)]
b[, `:=`(run = CS * sqrt(dr^2 + dc^2) / 1000, head = (atan2(dc, -dr) * 180 / pi) %% 360)]
cat(sprintf("%s burns of %s fires; anchors resolved (%s)\n", format(nrow(b), big.mark = ","), format(nrow(k), big.mark = ","), el()))

# ---- fire-day head runs, with the day's wind from the weather stream ---------------------------------------------------------
fd <- b[, { j <- which.max(run); .(run_km = run[j], heading = head[j], end_cell = Cell[j], cells = .N) }, by = .(Iteration, FireID, day)]
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
hw <- as.data.table(datasheet(rsyncrosim::scenario(L, id), "lucasFireC2F_OutputFireWeather", lookupsAsFactors = FALSE))
HPD <- MPD / 60; hw[, day := as.integer((Hour - 1) %/% HPD) + 1L]
wd <- hw[, .(wind20 = mean(WindSpeed), winddir = WindDirection[1]), by = .(Iteration, FireID, day)]
fd <- wd[fd, on = c("Iteration", "FireID", "day")]; fd <- k[fd, on = c("Iteration", "FireID")]
fu <- values(rast(X$engine$FuelFileName[1]), mat = FALSE); ft <- X$fuel_type; grp <- sub(" [A-Z]{2}[0-9]+$", "", ft$Description[match(fu, ft$ID)])
fd[, `:=`(end_fuel = grp[end_cell], angle_to_wind = abs(((heading - (winddir + 180) %% 360) + 180) %% 360 - 180))]
fwrite(fd[, .(Season, FireID = SeasonFireID, SpreadDays, AreaHa, day, run_km = round(run_km, 2), heading = round(heading), cells, wind20_kmh = round(wind20, 1),
              winddir = round(winddir), angle_to_wind = round(angle_to_wind), end_fuel)], file.path(OUT, sprintf("head_fire_runs_r%d.csv", id)))
qq <- function(x) paste(sprintf("%.2f", quantile(x, c(.1, .5, .9, .99), na.rm = TRUE)), collapse = " / ")
cat(sprintf("\nfire-days: %s from %s fires; daily head run (km per spread day) p10/p50/p90/p99: %s; max %.1f km\n",
            format(nrow(fd), big.mark = ","), format(uniqueN(fd[, .(Iteration, FireID)]), big.mark = ","), qq(fd$run_km), max(fd$run_km)))
cat(sprintf("fire-days with runs > 2 km %.1f%%, > 5 km %.1f%%, > 10 km %.2f%%; in km per burning hour, median %.2f\n",
            100 * mean(fd$run_km > 2), 100 * mean(fd$run_km > 5), 100 * mean(fd$run_km > 10), median(fd$run_km) / HPD))
cat(sprintf("head-run heading within 45 deg of the downwind direction on %.0f%% of fire-days (chance 25%%)\n", 100 * mean(fd$angle_to_wind <= 45, na.rm = TRUE)))
fd[, wbin := cut(wind20, c(0, 8, 12, 16, 20, 25, 100), right = FALSE)]
cat("\nby the day's 20-ft wind (km/h):\n"); print(fd[, .(fire_days = .N, p50_run = round(median(run_km), 2), p90_run = round(quantile(run_km, .9), 2), max_run = round(max(run_km), 1)), by = wbin][order(wbin)])
cat("\nby fuel where the head run ended:\n"); print(fd[, .(fire_days = .N, p50_run = round(median(run_km), 2), p90_run = round(quantile(run_km, .9), 2)), by = end_fuel][order(-fire_days)])
cat("\nby spread day of the fire:\n"); print(fd[, .(fire_days = .N, p50_run = round(median(run_km), 2), p90_run = round(quantile(run_km, .9), 2)), by = .(day = pmin(day, 10L))][order(day)])
big <- fd[, .(fire_max_run = max(run_km)), by = .(Iteration, FireID, AreaHa)]
cat("\nlargest daily run by fire size (ha):\n"); print(big[, .(fires = .N, p50_max_run = round(median(fire_max_run), 2), p90_max_run = round(quantile(fire_max_run, .9), 2)),
  by = .(size = cut(AreaHa, c(0, 405, 1000, 3000, 10000, 30000, Inf), c("<405", "405-1k", "1k-3k", "3k-10k", "10k-30k", "30k+")))][order(size)])

# ---- per cell: the head run of the fire-day that burned it, and which way the day's runs go ---------------------------------------
b[fd[, .(Iteration, FireID, day, dayrun = run_km)], on = c("Iteration", "FireID", "day"), dayrun := i.dayrun]
C <- b[run > 0, .(n = .N, dayrun = median(dayrun), S = sum(sin(head * pi / 180)), Cc = sum(cos(head * pi / 180))), by = Cell]
C[, `:=`(dir = (atan2(S, Cc) * 180 / pi) %% 360, R = sqrt(S^2 + Cc^2) / n)]
mk <- function(v) { r <- rast(g); v2 <- rep(NA_real_, ncell(g)); kk <- C$n >= MINB; v2[C$Cell[kk]] <- v[kk]; values(r) <- v2; r }
DR <- mk(C$dayrun); DIR <- mk(C$dir); R <- mk(C$R)
writeRaster(c(DR, DIR, R), file.path(OUT, sprintf("head_fire_runs_r%d.tif", id)), overwrite = TRUE, names = c("MedianDayHeadRunKm", "RunHeading", "Consistency"))
cat(sprintf("\nper cell (>= %d burns): median day head run %.2f km (p10 %.2f, p90 %.2f); run-heading consistency R median %.2f (%s)\n",
            MINB, median(C[n >= MINB]$dayrun), quantile(C[n >= MINB]$dayrun, .1), quantile(C[n >= MINB]$dayrun, .9), median(C[n >= MINB]$R), el()))

# ---- figure --------------------------------------------------------------------------------------------------------------------------
zone <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zone <- st_transform(zone[zone$PYROME == 26, ], crs(g))
hs <- shade(terrain(g, "slope", unit = "radians"), terrain(g, "aspect", unit = "radians"), 40, 315)
wheel <- function(dd, rr) { h <- (dd %% 360) / 360; s <- pmin(1, rr / 0.6); cc <- hsv(ifelse(is.na(h), 0, h), ifelse(is.na(s), 0, s), 0.95); cc[is.na(dd)] <- NA; cc }
ramp <- function(v, brk, pal) { i <- findInterval(v, brk, all.inside = TRUE); cc <- pal[i]; cc[is.na(v)] <- NA; cc }
torgb <- function(cols, e) { m <- col2rgb(ifelse(is.na(cols), "#FFFFFF", cols)) / 255; hv <- values(crop(hs, e), mat = FALSE); hv <- (hv - min(hv, na.rm = TRUE)) / diff(range(hv, na.rm = TRUE)); hv[is.na(hv)] <- .5
  sh <- 0.55 + 0.45 * hv; na <- is.na(cols); x <- rast(crop(rast(g), e), nlyrs = 3)
  values(x) <- cbind(ifelse(na, .86 * sh + .1, m[1, ] * sh), ifelse(na, .86 * sh + .1, m[2, ] * sh), ifelse(na, .86 * sh + .1, m[3, ] * sh)) * 255; x }
key <- function(E, pal, lab, title) { e <- as.vector(E); w <- (e[2] - e[1]) * .8 / length(pal); x0 <- e[1] + .1 * (e[2] - e[1]); y0 <- e[3] + .04 * (e[4] - e[3]); h <- .018 * (e[4] - e[3])
  op <- par(xpd = NA); on.exit(par(op)); clip(-1e9, 1e9, -1e9, 1e9)
  for (j in seq_along(pal)) rect(x0 + (j - 1) * w, y0, x0 + j * w, y0 + h, col = pal[j], border = NA)
  text(x0 + (seq_along(lab) - 0.5) * w, y0 - h * .9, lab, cex = .6); text(x0, y0 + 2.2 * h, title, adj = 0, cex = .65) }
png(file.path(OUT, sprintf("Fig_head_fire_runs_r%d.png", id)), 3300, 1650, res = 170)
layout(matrix(c(1, 2, 3, 4, 1, 2, 5, 6), 2, byrow = TRUE), widths = c(1, 1, 1.1, 1.1)); par(oma = c(0, 0, 3.2, 0))
E <- ext(g); vv <- function(r) values(r, mat = FALSE)
rb <- c(0, .5, .75, 1, 1.5, 2, 3, 4, 6, 1e9); rp <- colorRampPalette(c("#fff7ec", "#fdbb84", "#ef6548", "#990000", "#3b0000"))(9)
plotRGB(torgb(ramp(vv(DR), rb, rp), E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f")
title("(a) Head run of the fire-days that burn each cell", line = 1); key(E, rp, c("<.5", ".5", ".75", "1", "1.5", "2", "3", "4", "6+"), "km per spread day (median)")
plotRGB(torgb(wheel(vv(DIR), vv(R)), E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f"); title("(b) Direction of the daily runs", line = 1)
clip(-1e9, 1e9, -1e9, 1e9); kx <- E[1] + .15 * (E[2] - E[1]); ky <- E[3] + .08 * (E[4] - E[3]); kr <- .09 * (E[2] - E[1])
for (az in seq(0, 350, 10)) for (rr in c(.35, .7, 1)) { th <- az * pi / 180; polygon(kx + kr * rr * sin(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1), ky + kr * rr * cos(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1), col = wheel(az, rr * .6), border = NA) }
text(kx, ky + kr * 1.3, "N", cex = .8); text(kx, ky - kr * 1.4, "runs travel toward; pale = mixed", cex = .55)
par(mar = c(4.5, 4.5, 3, 1))
boxplot(run_km ~ wbin, fd, outline = FALSE, col = "#fdbb84", xlab = "Day's 20-ft wind (km/h)", ylab = "Daily head run (km)", main = "(c) Head run vs wind", names = c("<8", "8-12", "12-16", "16-20", "20-25", "25+"), las = 1)
fo <- fd[, .N, by = end_fuel][order(-N)][1:5]$end_fuel
boxplot(run_km ~ factor(end_fuel, fo), fd[end_fuel %in% fo], outline = FALSE, col = c(Grass = "#e6d77a", `Grass-Shrub` = "#d8b35a", Shrub = "#b0784a", `Timber-Understory` = "#5f8f4e", `Timber Litter` = "#2f6b3a", `Slash-Blowdown` = "#7a5a44")[fo], xlab = "", ylab = "Daily head run (km)",
        main = "(d) Head run by fuel where it ended", las = 2, cex.axis = .75)
bd <- fd[, .(p50 = median(run_km), p90 = quantile(run_km, .9)), by = .(day = pmin(day, 15L))][order(day)]
plot(bd$day, bd$p90, type = "b", pch = 19, col = "#990000", ylim = c(0, max(bd$p90) * 1.05), xlab = "Spread day of the fire (15 = 15+)", ylab = "Daily head run (km)", main = "(e) Head run by day of the fire", las = 1)
lines(bd$day, bd$p50, type = "b", pch = 19, col = "#ef6548"); legend("topright", c("90th percentile", "median"), col = c("#990000", "#ef6548"), pch = 19, lty = 1, bty = "n", cex = .8)
hist(pmin(fd$run_km, 12), breaks = seq(0, 12, .25), col = "#ef6548", border = "white", xlab = "Daily head run (km; 12 = 12+)", main = "(f) All fire-days", las = 1)
mtext(sprintf("Daily head-fire runs, lucasFireC2F result %d (P26, 2,500 seasons; %s fire-days; spread day = 2 burning h)", id, format(nrow(fd), big.mark = ",")), outer = TRUE, cex = .95, font = 2)
dev.off(); cat("wrote", sprintf("Fig_head_fire_runs_r%d.png", id), "(", el(), ")\n")
