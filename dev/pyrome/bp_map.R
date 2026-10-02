# Burn probability map of a pyrome result beside WRC V2, with MTBS fire outlines (2021-24 bold, 1984-2020 faint)
# Usage: Rscript bp_map.R <pyrome number> <result id> <out png>
suppressPackageStartupMessages({ library(terra); library(sf); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); RID <- as.integer(a[2]); OUTF <- a[3]; AUC1 <- if (length(a) > 3) a[4] else "?"; AUC2 <- if (length(a) > 4) a[5] else "?"; NOTE <- if (length(a) > 5) a[6] else ""
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, sprintf("spatial/pyromes_150m_LF2020/pyrome_%03d", PY))
L <- suppressWarnings(ssimLibrary(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P%02d.ssim", PY)))
bp <- rast(datasheet(rsyncrosim::scenario(L, RID), "lucasFireC2F_OutputBurnProbability")$FileName[1]) * 100
el <- rast(file.path(ZD, "Elevation.tif")); if (!compareGeom(bp, el, stopOnError = FALSE)) ext(bp) <- ext(el)
zone <- st_transform(read_sf(file.path(ZD, "zone.gpkg")), crs(el))
bb <- st_bbox(zone); E <- ext(bb$xmin - 5000, bb$xmax + 5000, bb$ymin - 5000, bb$ymax + 5000)
el <- crop(el, E); bp <- crop(bp, E)
wrc <- crop(terra::project(rast("F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"), el, method = "near"), E) * 100
fu <- crop(rast(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/P%02d/inputs/fuel_type.tif", PY)), E)
nb <- fu %in% c(91, 92, 93, 98, 99)
inz <- rasterize(vect(zone), el, field = 1)
source("F:/lucas-master/Models/California 30m BurnP3/src/00_config.R")
mt <- read_mtbs(); mt <- st_transform(mt[mt$Incid_Type == "Wildfire", ], crs(el)); mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y"))
mt <- suppressWarnings(st_crop(mt, st_bbox(el))); mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
hs <- shade(terrain(el, "slope", unit = "radians"), terrain(el, "aspect", unit = "radians"), 40, 315)
brk <- c(0, .1, .2, .35, .5, .7, 1, 1.5, 2, 3, 100); pal <- colorRampPalette(c("#f7f1e3", "#f8dc8a", "#f4a340", "#e0662b", "#b3261e", "#6b0f14"))(10)
lab <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
panel <- function(r, ttl, sub) {
  v <- mask(mask(r, inz), nb, maskvalues = TRUE)
  plot(hs, col = grey(seq(.55, .95, length.out = 50)), legend = FALSE, axes = FALSE, box = FALSE, mar = c(3, 1, 4, 1))
  plot(v, breaks = brk, col = adjustcolor(pal, .85), add = TRUE, legend = FALSE)
  plot(st_geometry(mt[mt$yr <= 2020, ]), add = TRUE, border = adjustcolor("#1d6fa5", .5), lwd = .5)
  plot(st_geometry(mt[mt$yr >= 2021, ]), add = TRUE, border = "#00aec4", lwd = 1.6)
  plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1.2)
  mtext(ttl, 3, line = 2, adj = 0, font = 2, cex = .95); mtext(sub, 3, line = .8, adj = 0, cex = .7, col = "#55534c") }
zm <- function(r) global(mask(mask(r, inz), nb, maskvalues = TRUE), "mean", na.rm = TRUE)[1, 1]
png(OUTF, 2600, 2200, res = 200); layout(matrix(c(1, 2, 3, 3), 2, byrow = TRUE), heights = c(1, .08)); par(oma = c(0, 0, 3, 0))
panel(bp, sprintf("(a) lucasFireC2F result %d (%%/yr)", RID), sprintf("zone mean %.2f%%/yr on burnable ground; %s; AUC vs 2021-24 MTBS %s", zm(bp), NOTE, AUC1))
panel(wrc, "(b) WRC V2 / FSim (%/yr)", sprintf("zone mean %.2f%%/yr on burnable ground; AUC vs 2021-24 MTBS %s", zm(wrc), AUC2))
par(mar = c(1, 4, 0, 4)); plot.new(); plot.window(c(0, 10), c(0, 1)); for (j in 1:10) rect(j - 1, .45, j, .95, col = pal[j], border = "white"); text(1:10 - .5, .2, lab, cex = .8)
mtext(sprintf("Pyrome %d %s: annual burn probability; outlines = MTBS wildfires (bright: 2021-24, faint: 1984-2020); grey = non-burnable", PY, sub("^P\\d+ ", "", zone$Name[1])), outer = TRUE, font = 2, cex = 1)
dev.off(); cat("wrote", OUTF, "\n")
