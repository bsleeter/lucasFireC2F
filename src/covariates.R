# =============================================================================
# covariates.R - the ignition covariates P2 fits and predicts with, built for any area of CONUS (sourced by P2)
# =============================================================================
# The 10 layers of the California fit stack, built exactly as the development scripts 44 and 48 built it but for a window
# only (the P2 fit domain plus the zone grid), on the same 30 m lattice (shared.R LATTICE_X0/Y0, 5 x 5 blocks of the 150 m
# grid). Each layer is computed on the window plus a margin wide enough that every cell sees what it saw statewide - 32.1 km
# for the distances (capped at 32 km), 1.5 km for the 990 m windows - then cut back, so a California zone reproduces the
# statewide stack cell for cell (the climate layers can differ only where they are filled outward across the US border).
# Human-activity layers are fixed at 2010, mid-record for the 1992-2020 fire record.
#   dist_road   LANDFIRE Roads (classes 20-23: primary, secondary, tertiary, thinned), distance in m, capped at 32,000
#   dist_dev    Annual NLCD 2010 developed (21-24), distance in m, capped at 32,000
#   dev_frac    share of developed cells within 990 m (circle), per mille
#   huden       SILVIS 2010 housing density (field HUDEN2010; 2010 cp12 blocks or the 1990-2020 v4 blocks), round(log1p x 100)
#   elev, slope, tpi   the 30 m DEM; slope degrees x 10 (8 neighbours); elevation minus its 990 m circle mean
#   ppt         PRISM 1991-2020 annual precipitation normal (mm), filled outward at 4 km, bilinear to 30 m
#   wet_warm, warm_frac   gridMET pr 1991-2020: June-September days >= 2.5 mm (x 100); June-September share of the year (x 10,000)
# All are written INT2S, as the statewide stack was (integer casts truncate, as there).
# =============================================================================
fill_na_outward <- function(r, passes = 20, w = 5) {         # extrapolation across the US border at 4 km, as script 00_config
  for (i in seq_len(passes)) { n <- global(r, function(v) sum(is.na(v)))[[1]]; if (!is.na(n) && n == 0) break
    r <- terra::focal(r, w = w, fun = "mean", na.rm = TRUE, na.policy = "only") }
  r }

build_ignition_covariates <- function(win, src, outdir) {
  # win: SpatExtent in EPSG:5070 (snapped to the 150 m lattice); src: list(dem, roads, nlcd, wui, prism, gridmet)
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE); CRS <- crs(rast(src$dem))
  T <- rast(win, resolution = 30, crs = CRS)
  out <- function(r, n) { p <- file.path(outdir, paste0(n, ".tif")); writeRaster(crop(r, T, snap = "near"), p, overwrite = TRUE, datatype = "INT2S",
                                                                                    gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES")); p }
  grow <- function(m) ext(T) + m                               # margins are whole 30 m cells, so the edges stay on the lattice
  t0 <- Sys.time(); tm <- function() sprintf("%.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))
  # roads
  rd <- extend(crop(rast(src$roads), grow(32100), snap = "near"), grow(32100)); levels(rd) <- NULL
  d <- clamp(distance(ifel(rd %in% c(20, 21, 22, 23), 1L, NA)), 0, 32000, values = TRUE); out(d, "dist_road"); rm(rd, d)
  say("ignition covariates: dist_road (%s)", tm())
  # development
  nl <- rast(src$nlcd); crs(nl) <- CRS; nl <- extend(crop(nl, grow(32100), snap = "near"), grow(32100)); levels(nl) <- NULL
  dev <- nl %in% c(21L, 22L, 23L, 24L); rm(nl)
  out(clamp(distance(ifel(dev, 1L, NA)), 0, 32000, values = TRUE), "dist_dev")
  dv <- crop(dev, grow(1500), snap = "near"); w <- focalMat(dv, 495, "circle"); w[w > 0] <- 1 / sum(w > 0)
  out(round(focal(dv, w, "sum", na.rm = TRUE) * 1000), "dev_frac"); rm(dev, dv)
  say("ignition covariates: dist_dev, dev_frac (%s)", tm())
  # housing density
  lyr <- st_layers(src$wui)$name[1]; wc <- st_crs(read_sf(src$wui, query = sprintf("SELECT * FROM \"%s\" LIMIT 0", lyr)))
  wb <- read_sf(src$wui, layer = lyr, wkt_filter = st_as_text(st_transform(st_as_sfc(st_bbox(setNames(as.vector(win), c("xmin", "xmax", "ymin", "ymax")), crs = st_crs(CRS))), wc)))
  if (!"HUDEN2010" %in% names(wb)) fail("Data Sources: the housing density file has no HUDEN2010 field (%s)", basename(src$wui))
  wb <- st_make_valid(st_transform(wb, CRS)); wb$HUDEN2010 <- as.numeric(wb$HUDEN2010)
  out(round(log1p(rasterize(vect(wb), T, field = "HUDEN2010", background = 0)) * 100), "huden"); rm(wb)
  say("ignition covariates: huden (%s)", tm())
  # terrain
  el <- extend(crop(rast(src$dem), grow(1500), snap = "near"), grow(1500)); levels(el) <- NULL
  out(el, "elev"); out(round(terrain(el, "slope", unit = "degrees", neighbors = 8) * 10), "slope")
  w <- focalMat(el, 495, "circle"); w[w > 0] <- 1 / sum(w > 0); out(round(el - focal(el, w, "sum", na.rm = TRUE)), "tpi"); rm(el)
  say("ignition covariates: elev, slope, tpi (%s)", tm())
  # PRISM precipitation normal
  p <- rast(src$prism); bb <- project(as.polygons(ext(T), crs = CRS), crs(p))
  p <- fill_na_outward(crop(p, ext(bb) + 0.5)); out(round(project(p, T, method = "bilinear")), "ppt")
  # gridMET warm-season rain, 1991-2020
  acc <- NULL; bg <- ext(project(as.polygons(ext(T), crs = CRS), "EPSG:4326")) + 0.5
  for (y in 1991:2020) { f <- file.path(src$gridmet, "pr", sprintf("pr_%d.nc", y)); if (!file.exists(f)) fail("gridMET pr %d missing (%s)", y, f)
    r <- crop(rast(f), bg); mo <- as.integer(format(time(r), "%m")); wm <- which(mo %in% 6:9)
    a <- c(sum(r[[wm]], na.rm = TRUE), sum(r[[wm]] >= 2.5, na.rm = TRUE), sum(r, na.rm = TRUE)); acc <- if (is.null(acc)) a else acc + a }
  acc <- acc / 30; frac <- acc[[1]] / clamp(acc[[3]], 25, Inf, values = TRUE)
  out(round(project(fill_na_outward(acc[[2]]), T, method = "bilinear") * 100), "wet_warm")
  out(round(project(fill_na_outward(frac), T, method = "bilinear") * 10000), "warm_frac")
  say("ignition covariates: ppt, wet_warm, warm_frac (%s)", tm())
  file.path(outdir, paste0(c("dist_road", "dist_dev", "dev_frac", "huden", "elev", "slope", "tpi", "ppt", "wet_warm", "warm_frac"), ".tif"))
}
