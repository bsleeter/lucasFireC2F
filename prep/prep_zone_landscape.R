# =============================================================================
# prep_zone_landscape.R - LANDFIRE and fire-history rasters for one fire zone, ready for lucasFireC2F
# =============================================================================
# Part of the prep toolkit (not the SyncroSim package): turns national 30 m LANDFIRE layers into the 150 m rasters the
# package's LandscapeRasters and CanopyRasters datasheets take. Same method as the validated P26 workflow (script 200):
#   Fuel type      FBFM40, modal 30 m -> 150 m, NA -> 99 (NB9). RAW codes: the package applies the C2F-W remaps
#                  (FuelType.EngineFuelModel) and the recent-fire rules (FireAgeRule) itself.
#   Canopy         CBH (m), CBD (kg/m3), canopy height (m): mean over the forested 30 m pixels; a 150 m cell is forest when
#                  at least half its pixels have CBD > 0, else NA (the package treats NA as non-forest: no crown fire).
#                  Canopy cover (%): plain mean.
#   Years since fire   from a fire-age class raster (521 = 1-2 yr, 522 = 3-5, 523 = 6-10; consumed ground only):
#                  written as 2 / 5 / 10 years so FireAgeRule ranges read naturally.
# The 150 m grid is a template raster on the LANDFIRE 30 m lattice (e.g. the zone's elevation); the aggregation is a pure
# 5 x 5 block, checked.
# Usage: Rscript prep/prep_zone_landscape.R <template.tif> <LANDFIRE root> <vintage> <out dir> [fire-age.tif] [canopy-height vintage]
# =============================================================================
suppressMessages(library(terra))
a <- commandArgs(TRUE)
if (length(a) < 4) stop("usage: prep_zone_landscape.R <template.tif> <LANDFIRE root> <vintage> <out dir> [fire-age.tif] [CH vintage]")
grid <- rast(a[1]); ROOT <- a[2]; V <- a[3]; OUT <- a[4]; AGE <- if (length(a) > 4 && nzchar(a[5])) a[5] else ""
CHV <- if (length(a) > 5) a[6] else V
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n", sep = "")

lf <- function(prod, v) { f <- Sys.glob(sprintf("%s/LF%s_%s_CONUS*/LF%s_%s_CONUS/Tif/LF%s_%s_CONUS.tif", ROOT, v, prod, v, prod, v, prod))
  if (length(f) != 1) stop("LANDFIRE ", prod, " LF", v, " not found under ", ROOT); f }
to_grid <- function(src, fun, zero_na = FALSE) {
  r <- crop(rast(src), ext(grid) + 300, snap = "out"); NAflag(r) <- -9999; levels(r) <- NULL
  if (zero_na) r <- classify(r, cbind(0, NA))
  g <- aggregate(r, 5, fun = fun, na.rm = TRUE)
  off <- (xmin(g) - xmin(grid)) %% res(grid)[1]
  if (abs(off) > 1e-6 && abs(off - res(grid)[1]) > 1e-6) stop("aggregated raster is off the grid lattice by ", off, " m")
  resample(g, grid, method = "near")
}
# Float64: C2F-W reads 3-decimal ASCII, and float32 rounding flipped ~12k P26 canopy cells (8.5% of fires grew differently)
w <- function(r, f, dt = "FLT8S") { writeRaster(r, file.path(OUT, f), overwrite = TRUE, datatype = dt, gdal = "COMPRESS=DEFLATE"); say("wrote %s", f) }

fb <- to_grid(lf("FBFM40", V), "modal"); fb[is.na(fb)] <- 99
say("fuel types: %s", paste(sort(unique(values(fb, mat = FALSE))), collapse = " "))
w(fb, "fuel_type.tif", "INT2S")
fsh <- to_grid(lf("CBD", V), function(x, ...) mean(x > 0, na.rm = TRUE))
nonfor <- is.na(fsh) | fsh < 0.5
cbh <- ifel(nonfor, NA, to_grid(lf("CBH", V), "mean", zero_na = TRUE) / 10)
cbd <- ifel(nonfor, NA, to_grid(lf("CBD", V), "mean", zero_na = TRUE) / 100)
cc  <- to_grid(lf("CC", V), "mean"); cc[is.na(cc)] <- 0
ch  <- ifel(nonfor, NA, to_grid(lf("CH", CHV), "mean", zero_na = TRUE) / 10)
w(cbh, "canopy_base_height_m.tif"); w(cbd, "canopy_bulk_density.tif"); w(cc, "canopy_cover_pct.tif"); w(ch, "canopy_height_m.tif")
say("forest cells %d of %d; CBH median %.1f m, CBD median %.3f, CH (LF%s) median %.1f m", sum(!values(nonfor, mat = FALSE)), ncell(grid),
    median(values(cbh, mat = FALSE), na.rm = TRUE), median(values(cbd, mat = FALSE), na.rm = TRUE), CHV, median(values(ch, mat = FALSE), na.rm = TRUE))
if (nzchar(AGE)) {
  ag <- crop(rast(AGE), grid); if (!compareGeom(ag, grid, stopOnError = FALSE)) ag <- resample(ag, grid, method = "near")
  yrs <- classify(ag, cbind(c(521, 522, 523), c(2, 5, 10)), others = NA)
  say("years since fire: %s", paste(names(table(values(yrs, mat = FALSE))), table(values(yrs, mat = FALSE)), sep = " yr: ", collapse = ", "))
  w(yrs, "years_since_fire.tif", "INT2S")
}
