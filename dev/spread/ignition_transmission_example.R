# Example: where does the fire that burns a cell come from, and how fast did it get there? From the stage 4 burn table
# (every burned cell of every kept fire, its igniting neighbour and arrival minute; the fire's origin = the cell with no Source).
#   per burn   distance and heading from the fire's ignition cell to the burned cell; arrival time = burning minutes since
#              ignition (C2F-W Period; the model burns BurningHours per spread day, so 120 burning min = 1 spread day at 2 h/day)
#              travel rate = distance / arrival time, reported as km per spread day (x 120 min / 1000) and m/min of burning
#   per cell   (cells with >= MINB burns, origin cells excluded) circular mean heading from ignition and its consistency R,
#              median distance from ignition, median travel rate
# Usage: Rscript ignition_transmission_example.R <result id> [minimum burns = 20] [burning minutes per spread day = 120]
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf); library(arrow); library(dplyr) })
a <- commandArgs(TRUE); id <- as.integer(a[1]); MINB <- if (length(a) > 1) as.integer(a[2]) else 20L; MPD <- if (length(a) > 2) as.numeric(a[3]) else 120
OUT <- "F:/lucasFireC2F/dev/spread"; M <- "F:/lucas-master/Models/California 30m BurnP3"
X <- readRDS(sprintf("F:/lucasFireC2F/dev/paper/result_%d.rds", id)); g <- rast(X$engine$ElevationFileName[1]); NC <- ncol(g); CS <- res(g)[1]
t0 <- Sys.time()
k <- X$fire_stat[ResampleStatus %in% c("Kept", "Reassigned")][, .(Iteration = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)),
       FireID = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
b <- as.data.table(open_dataset(X$burn_table) |> select(Iteration, FireID, Cell, Source, Period) |> collect())
b <- b[k, on = c("Iteration", "FireID"), nomatch = 0]
o <- b[is.na(Source), .(Iteration, FireID, oc = Cell)]; b <- b[o, on = c("Iteration", "FireID")][!is.na(Source)]
b[, `:=`(dr = (Cell - 1L) %/% NC - (oc - 1L) %/% NC, dc = (Cell - 1L) %% NC - (oc - 1L) %% NC)]
b[, `:=`(dist = CS * sqrt(dr^2 + dc^2) / 1000, head = (atan2(dc, -dr) * 180 / pi) %% 360)]
b[, rate := dist / ((Period + 1) / MPD)]                       # km per spread day
cat(sprintf("%s burns of %s kept fires (origins excluded) in %.1f min\n", format(nrow(b), big.mark = ","), format(nrow(o), big.mark = ","), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat(sprintf("per burn: distance from ignition median %.1f km (p90 %.1f); travel rate median %.2f km/spread day (p10 %.2f, p90 %.2f) = %.1f m/min of burning\n",
            median(b$dist), quantile(b$dist, .9), median(b$rate), quantile(b$rate, .1), quantile(b$rate, .9), 1000 * median(b$rate) / MPD))
A <- b[, .(n = .N, S = sum(sin(head * pi / 180)), C = sum(cos(head * pi / 180)), dist = median(dist), rate = median(rate)), by = Cell]
A[, `:=`(dir = (atan2(S, C) * 180 / pi) %% 360, R = sqrt(S^2 + C^2) / n)]
mk <- function(v) { r <- rast(g); v2 <- rep(NA_real_, ncell(g)); kk <- A$n >= MINB; v2[A$Cell[kk]] <- v[kk]; values(r) <- v2; r }
DIR <- mk(A$dir); R <- mk(A$R); DIST <- mk(A$dist); RATE <- mk(A$rate)
writeRaster(c(DIR, R, DIST, RATE, mk(as.numeric(A$n))), file.path(OUT, sprintf("ignition_transmission_r%d.tif", id)), overwrite = TRUE,
            names = c("HeadingFromIgnition", "Consistency", "MedianDistanceKm", "MedianRateKmPerSpreadDay", "Burns"))
cat(sprintf("cells with >= %d burns: %s; consistency R median %.2f (p10 %.2f, p90 %.2f); median distance %.1f km; median rate %.2f km/spread day\n",
            MINB, format(sum(A$n >= MINB), big.mark = ","), median(A[n >= MINB]$R), quantile(A[n >= MINB]$R, .1), quantile(A[n >= MINB]$R, .9),
            median(A[n >= MINB]$dist), median(A[n >= MINB]$rate)))
# travel rate by the fuel of the burned cell
fu <- values(rast(X$engine$FuelFileName[1]), mat = FALSE); ft <- X$fuel_type; grp <- sub(" [A-Z]{2}[0-9]+$", "", ft$Description[match(fu, ft$ID)])
b[, fg := grp[Cell]]; print(b[, .(share = round(.N / nrow(b), 3), median_rate = round(median(rate), 2), p90_rate = round(quantile(rate, .9), 2), median_dist_km = round(median(dist), 1)), by = fg][order(-share)])
# rate vs how long the fire had burned: early (first spread day) vs later
b[, day := ceiling((Period + 1) / MPD)]; print(b[, .(share = round(.N / nrow(b), 3), median_rate = round(median(rate), 2)), by = .(day = pmin(day, 10))][order(day)])

# ---- figure -----------------------------------------------------------------------------------------------------------------------------
zone <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zone <- st_transform(zone[zone$PYROME == 26, ], crs(g))
hs <- shade(terrain(g, "slope", unit = "radians"), terrain(g, "aspect", unit = "radians"), 40, 315)
wheel <- function(dd, rr) { h <- (dd %% 360) / 360; s <- pmin(1, rr / 0.6); cc <- hsv(ifelse(is.na(h), 0, h), ifelse(is.na(s), 0, s), 0.95); cc[is.na(dd)] <- NA; cc }
ramp <- function(v, brk, pal) { i <- findInterval(v, brk, all.inside = TRUE); cc <- pal[i]; cc[is.na(v)] <- NA; cc }
torgb <- function(cols, e) { m <- col2rgb(ifelse(is.na(cols), "#FFFFFF", cols)) / 255; hv <- values(crop(hs, e), mat = FALSE); hv <- (hv - min(hv, na.rm = TRUE)) / diff(range(hv, na.rm = TRUE)); hv[is.na(hv)] <- .5
  sh <- 0.55 + 0.45 * hv; na <- is.na(cols); x <- rast(crop(rast(g), e), nlyrs = 3)
  values(x) <- cbind(ifelse(na, .86 * sh + .1, m[1, ] * sh), ifelse(na, .86 * sh + .1, m[2, ] * sh), ifelse(na, .86 * sh + .1, m[3, ] * sh)) * 255; x }
key <- function(E, brk, pal, lab, title) { e <- as.vector(E); w <- (e[2] - e[1]) * .8 / length(pal); x0 <- e[1] + .1 * (e[2] - e[1]); y0 <- e[3] + .04 * (e[4] - e[3]); h <- .018 * (e[4] - e[3])
  op <- par(xpd = NA); on.exit(par(op)); clip(-1e9, 1e9, -1e9, 1e9)
  for (j in seq_along(pal)) rect(x0 + (j - 1) * w, y0, x0 + j * w, y0 + h, col = pal[j], border = NA)
  text(x0 + (seq_along(lab) - 0.5) * w, y0 - h * .9, lab, cex = .6); text(x0, y0 + 2.2 * h, title, adj = 0, cex = .65) }
png(file.path(OUT, sprintf("Fig_ignition_transmission_r%d.png", id)), 3300, 1650, res = 170)
layout(matrix(1:4, 1), widths = c(1, 1, 1, 1.25)); par(oma = c(0, 0, 3.2, 0))
E <- ext(g); vv <- function(r) values(r, mat = FALSE)
plotRGB(torgb(wheel(vv(DIR), vv(R)), E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f"); title("(a) Heading from ignition", line = 1)
kx <- E[1] + .15 * (E[2] - E[1]); ky <- E[3] + .08 * (E[4] - E[3]); kr <- .09 * (E[2] - E[1])
clip(-1e9, 1e9, -1e9, 1e9); for (az in seq(0, 350, 10)) for (rr in c(.35, .7, 1)) { th <- az * pi / 180; polygon(kx + kr * rr * sin(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1), ky + kr * rr * cos(th + c(-.09, .09, .09, -.09)) * c(.66, .66, 1, 1), col = wheel(az, rr * .6), border = NA) }
text(kx, ky + kr * 1.3, "N", cex = .8); text(kx, ky - kr * 1.4, "fire travels toward; pale = mixed", cex = .55)
db <- c(0, 1, 2, 4, 6, 8, 12, 16, 25, 1e9); dp <- colorRampPalette(c("#fbf3d5", "#f7c344", "#e0662b", "#8c1c13", "#3b0d10"))(9)
plotRGB(torgb(ramp(vv(DIST), db, dp), E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f"); title("(b) Median distance from ignition", line = 1)
key(E, db, dp, c("<1", "1", "2", "4", "6", "8", "12", "16", "25+"), "km")
rb <- c(0, .3, .45, .6, .75, .9, 1.1, 1.4, 2, 1e9); rp <- colorRampPalette(c("#edf8fb", "#9ebcda", "#8c6bb1", "#6e016b", "#2a0027"))(9)
plotRGB(torgb(ramp(vv(RATE), rb, rp), E), mar = c(1, 1, 3, 1), axes = FALSE); plot(st_geometry(zone), add = TRUE, border = "#20211f"); title("(c) Median travel rate from ignition", line = 1)
key(E, rb, rp, c("<.3", ".3", ".45", ".6", ".75", ".9", "1.1", "1.4", "2+"), "km per spread day (straight line from ignition / burning time)")
fz <- focal(!is.na(R) & R > .5, w = 241, fun = "sum", na.rm = TRUE); cz <- which.max(values(fz, mat = FALSE)); xy <- xyFromCell(g, cz)
Z <- ext(xy[1] - 18000, xy[1] + 18000, xy[2] - 22500, xy[2] + 22500)
plotRGB(torgb(ramp(values(crop(RATE, Z), mat = FALSE), rb, rp), Z), mar = c(1, 1, 3, 1), axes = FALSE)
title("(d) Zoom: rate (colour); arrows = heading from ignition, length = R", line = 1)
ag <- crop(aggregate(c(sin(DIR * pi / 180) * R, cos(DIR * pi / 180) * R), 10, mean, na.rm = TRUE), Z); pts <- as.data.frame(ag, xy = TRUE, na.rm = TRUE)
L <- 1400 / max(1e-6, max(sqrt(pts[[3]]^2 + pts[[4]]^2)))
arrows(pts$x - L * pts[[3]] / 2, pts$y - L * pts[[4]] / 2, pts$x + L * pts[[3]] / 2, pts$y + L * pts[[4]] / 2, length = .04, lwd = 1.1, col = "#20211f")
plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1.5)
mtext(sprintf("Where the fire that burns each cell comes from, and how fast it got there: lucasFireC2F result %d (P26, 2,500 seasons; cells with >= %d burns; 2 burning h per spread day)", id, MINB), outer = TRUE, cex = .95, font = 2)
dev.off(); cat("wrote", sprintf("Fig_ignition_transmission_r%d.png", id), "\n")
