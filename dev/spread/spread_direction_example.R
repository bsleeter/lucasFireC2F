# Example: per-cell mean spread direction from the stage 8 per-burn table (all kept fires of a result)
#   direction    circular mean of SpreadAzimuth (the heading from the igniting neighbour into the cell, degrees clockwise from
#                north; one of 8 neighbour directions per burn), over every burn of the cell
#   consistency  mean resultant length R = |mean unit vector| (0 = fire arrives from all sides, 1 = always the same way)
# Compared with the upslope direction (terrain aspect + 180) and the stream's most common wind heading.
# Usage: Rscript spread_direction_example.R <result id> [minimum burns = 20]
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf); library(arrow); library(dplyr) })
a <- commandArgs(TRUE); id <- as.integer(a[1]); MINB <- if (length(a) > 1) as.integer(a[2]) else 20L
OUT <- "F:/lucasFireC2F/dev/spread"; M <- "F:/lucas-master/Models/California 30m BurnP3"
X <- readRDS(sprintf("F:/lucasFireC2F/dev/paper/result_%d.rds", id)); g <- rast(X$engine$ElevationFileName[1])
t0 <- Sys.time()
agg <- open_dataset(X$burn_effects) |> filter(!is.na(SpreadAzimuth)) |>
  mutate(r = SpreadAzimuth * pi / 180) |> group_by(Cell) |> summarise(n = n(), S = sum(sin(r)), C = sum(cos(r))) |> collect() |> as.data.table()
cat(sprintf("aggregated %s burns over %s cells in %.1f min\n", format(sum(agg$n), big.mark = ","), format(nrow(agg), big.mark = ","), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
agg[, `:=`(dir = (atan2(S, C) * 180 / pi) %% 360, R = sqrt(S^2 + C^2) / n)]
mk <- function(v) { r <- rast(g); v2 <- rep(NA_real_, ncell(g)); k <- agg$n >= MINB; v2[agg$Cell[k]] <- v[k]; values(r) <- v2; r }
DIR <- mk(agg$dir); R <- mk(agg$R); N <- mk(as.numeric(agg$n))
writeRaster(c(DIR, R, N), file.path(OUT, sprintf("spread_direction_r%d.tif", id)), overwrite = TRUE, names = c("MeanSpreadDirection", "Consistency", "Burns"))

# ---- comparisons ------------------------------------------------------------------------------------------------------------------
dang <- function(a, b) abs(((a - b + 180) %% 360) - 180)
asp <- terrain(g, "aspect", unit = "degrees"); slp <- terrain(g, "slope", unit = "degrees"); up <- (asp + 180) %% 360
sw <- fread(file.path(M, "data/weather/c2fw_stream_p026_dT+0.0.csv"))
wh <- (atan2(sum(sin(sw$WindDirection * pi / 180)), sum(cos(sw$WindDirection * pi / 180))) * 180 / pi + 180) %% 360   # heading = from + 180
d <- data.table(dir = values(DIR, mat = FALSE), R = values(R, mat = FALSE), up = values(up, mat = FALSE), slope = values(slp, mat = FALSE))[!is.na(dir) & !is.na(up)]
cat(sprintf("cells with >= %d burns: %s; consistency R median %.2f (p10 %.2f, p90 %.2f)\n", MINB, format(nrow(d), big.mark = ","), median(d$R), quantile(d$R, .1), quantile(d$R, .9)))
cat(sprintf("stream mean wind heading %.0f deg (fires pushed toward)\n", wh))
cat(sprintf("mean spread direction within 45 deg of: upslope %.0f%% (slope > 10 deg: %.0f%%); the mean wind heading %.0f%%; chance 25%%\n",
            100 * mean(dang(d$dir, d$up) <= 45), 100 * mean(dang(d$dir[d$slope > 10], d$up[d$slope > 10]) <= 45), 100 * mean(dang(d$dir, wh) <= 45)))
d[, sb := cut(slope, c(0, 5, 10, 20, 30, 90), include.lowest = TRUE)]
print(d[, .(cells = .N, R = round(median(R), 2), within45_upslope = round(mean(dang(dir, up) <= 45), 2), within45_wind = round(mean(dang(dir, wh) <= 45), 2)), by = sb][order(sb)])

# ---- figure -------------------------------------------------------------------------------------------------------------------------
zone <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zone <- st_transform(zone[zone$PYROME == 26, ], crs(g))
hs <- shade(terrain(g, "slope", unit = "radians"), terrain(g, "aspect", unit = "radians"), 40, 315)
wheel <- function(dd, rr) { h <- (dd %% 360) / 360; s <- pmin(1, rr / 0.6); v <- ifelse(is.na(dd), NA, 0.95)
  cc <- hsv(ifelse(is.na(h), 0, h), ifelse(is.na(s), 0, s), ifelse(is.na(v), 1, v)); cc[is.na(dd)] <- NA; cc }
torgb <- function(cols, e) { m <- col2rgb(ifelse(is.na(cols), "#FFFFFF", cols)) / 255; hv <- values(crop(hs, e), mat = FALSE); hv <- (hv - min(hv, na.rm = TRUE)) / diff(range(hv, na.rm = TRUE)); hv[is.na(hv)] <- .5
  sh <- 0.55 + 0.45 * hv; r <- crop(rast(g), e); na <- is.na(cols)
  R1 <- ifelse(na, 0.86 * sh + .1, m[1, ] * sh); G1 <- ifelse(na, 0.86 * sh + .1, m[2, ] * sh); B1 <- ifelse(na, 0.86 * sh + .1, m[3, ] * sh)
  x <- rast(r, nlyrs = 3); values(x) <- cbind(R1, G1, B1) * 255; x }
png(file.path(OUT, sprintf("Fig_spread_direction_r%d.png", id)), 2700, 1650, res = 170)
layout(matrix(c(1, 2, 3, 3), 1), widths = c(1, 1, 1.05, 1.05)); par(oma = c(0, 0, 3.2, 0))
E <- ext(g); cols <- wheel(values(DIR, mat = FALSE), values(R, mat = FALSE))
par(mar = c(1, 1, 3, 1)); plotRGB(torgb(cols, E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f")
title("(a) Mean spread direction", line = 1)
# direction key
kx <- E[1] + 0.13 * (E[2] - E[1]); ky <- E[3] + 0.08 * (E[4] - E[3]); kr <- 0.08 * (E[2] - E[1])
for (az in seq(0, 350, 10)) for (rr in c(.35, .7, 1)) { th <- az * pi / 180; polygon(kx + kr * rr * sin(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1) * 1, ky + kr * rr * cos(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1), col = wheel(az, rr * .6), border = NA) }
text(kx, ky + kr * 1.25, "N", cex = .8); text(kx, ky - kr * 1.35, "fires travel toward", cex = .6)
Rv <- values(R, mat = FALSE); rc <- ifelse(is.na(Rv), NA, colorRampPalette(c("#f7f4ea", "#c9d9b6", "#6f9e6a", "#285c4d", "#0f2f2a"))(10)[pmin(10, 1 + floor(Rv * 10))])
plotRGB(torgb(rc, E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f"); title("(b) Consistency R", line = 1)
lg <- colorRampPalette(c("#f7f4ea", "#c9d9b6", "#6f9e6a", "#285c4d", "#0f2f2a"))(10)
for (j in 1:10) rect(E[1] + 0.1 * (E[2] - E[1]) + (j - 1) * 0.08 * (E[2] - E[1]), E[3] + 0.03 * (E[4] - E[3]), E[1] + 0.1 * (E[2] - E[1]) + j * 0.08 * (E[2] - E[1]), E[3] + 0.05 * (E[4] - E[3]), col = lg[j], border = NA)
text(E[1] + 0.1 * (E[2] - E[1]), E[3] + 0.015 * (E[4] - E[3]), "0", cex = .7); text(E[1] + 0.9 * (E[2] - E[1]), E[3] + 0.015 * (E[4] - E[3]), "1", cex = .7)
# zoom with arrows: the 36 x 45 km window with the most burned cells of high consistency
fz <- focal(!is.na(R) & R > .4, w = 241, fun = "sum", na.rm = TRUE); cz <- which.max(values(fz, mat = FALSE)); xy <- xyFromCell(g, cz)
Z <- ext(xy[1] - 18000, xy[1] + 18000, xy[2] - 22500, xy[2] + 22500)
cz_ <- wheel(values(crop(DIR, Z), mat = FALSE), values(crop(R, Z), mat = FALSE))
plotRGB(torgb(cz_, Z), mar = c(1, 1, 3, 1), axes = FALSE); title("(c) Zoom: arrows every 1.5 km, length = R", line = 1)
ag <- aggregate(c(sin(DIR * pi / 180) * R, cos(DIR * pi / 180) * R), 10, mean, na.rm = TRUE); ag <- crop(ag, Z); pts <- as.data.frame(ag, xy = TRUE, na.rm = TRUE)
L <- 1400 / max(1e-6, max(sqrt(pts[[3]]^2 + pts[[4]]^2)))
arrows(pts$x - L * pts[[3]] / 2, pts$y - L * pts[[4]] / 2, pts$x + L * pts[[3]] / 2, pts$y + L * pts[[4]] / 2, length = .04, lwd = 1.1, col = "#20211f")
plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1.5)
mtext(sprintf("Per-cell mean spread direction, lucasFireC2F result %d (P26, 2,500 seasons; cells with >= %d burns). Hue = direction fires travel toward, saturation = consistency.", id, MINB), outer = TRUE, cex = .95, font = 2)
dev.off(); cat("wrote", sprintf("Fig_spread_direction_r%d.png", id), "\n")
