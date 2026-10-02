# =============================================================================
# P1 - Prepare Zone Landscape
# =============================================================================
# Builds a fire zone's landscape from statewide and national data on disk (project Data Sources), the rasters stages 1-8
# read (LandscapeRasters, CanopyRasters) and the zone boundary the calibration and validation stages use. Ported from the
# validated development tools (src 158 for the grid; prep/prep_zone_landscape.R for the LANDFIRE layers); P26 and P29 are
# reproduced cell for cell.
#   zone         the Fire Zone's polygon(s) from the boundary file (field ID = the zone's Map Code, or the national pyrome
#                shapefile's PYROME, Map Code 1000 + PYROME), unioned
#   grid         zone + BufferKm (default 25 km: fires ignite only inside the zone, the buffer holds what spreads out),
#                snapped outward to the model's 150 m lattice: a 150 m elevation grid's own, or - from a 30 m DEM (LANDFIRE,
#                CONUS) - the fixed national lattice (shared.R LATTICE_X0/Y0), elevation = mean of each 5 x 5 block of 30 m pixels
#   fire zone    cropped from a 150 m fire-zone grid if Data Sources has one, else every zone polygon around the grid rasterized
#                (cell centre, then any touched cell for the gaps between polygons)
#   weather zone cropped from the 150 m weather-zone grid; all unmasked (INT2S, -9999 = no data)
#   fuel type    LANDFIRE FBFM40 of the vintage, modal 30 m -> 150 m (a pure 5 x 5 block), NA -> 99 (NB9); RAW codes
#                (stage 1 applies the C2F-W remaps and the recent-fire rules)
#   canopy       CBH (m), CBD (kg/m3), canopy height (m): mean of the forested 30 m pixels; a 150 m cell is forest when at
#                least half its pixels have CBD > 0, else NA (no crown fire). Canopy cover (%): plain mean. Float64 (C2F-W
#                reads 3-decimal ASCII; float32 rounding flipped ~12k P26 canopy cells).
#   years since fire   2 / 5 / 10: from a fire-age class raster (521 = 1-2 yr, 522 = 3-5, 523 = 6-10) if Data Sources has one,
#                else built from the MTBS severity mosaics and the interagency perimeters (see the section below)
#   vegetation   [optional, Data Sources BpsFile] LANDFIRE BpS vegetation groups mapped to Vegetation Classes (Conifer, Hardwood,
#                Shrub, Grass, Other), modal 30 m -> 150 m: the origin-vegetation strata C5 and stage 3 can fit spread days by
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE)
progressBar(type = "begin", totalSteps = 6)
dsrc <- read_data_sources(c("LandfireFolder", "StatewideElevationFile", "FireZoneBoundaryFile"))
opt_src <- read_data_sources(c("FireZoneGridFile", "WeatherZoneGridFile", "WeatherZoneBoundaryFile", "FireAgeFile", "MtbsSeverityFolder", "InteragencyPerimeterFile"), optional = TRUE)
has <- function(k) { v <- opt_src[[k]]; length(v) && !is.na(v) && nzchar(v) }
zo <- need("ZonePrepOption", "FireZone")
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); ZID <- fzt$ID[match(zo$FireZone, fzt$Name)]
if (is.na(ZID)) fail("Zone Prep Options: fire zone '%s' is not in this project's Fire Zones", zo$FireZone)
BUF <- if (!is.na(zo$BufferKm)) zo$BufferKm * 1000 else 25000
V <- if (!is.na(zo$LandfireVintage)) as.character(zo$LandfireVintage) else "2020"; CHV <- if (!is.na(zo$CanopyHeightVintage)) as.character(zo$CanopyHeightVintage) else V
TD <- env$TransferDirectory; RES <- 150
w <- function(r, f, dt) { p <- file.path(TD, f); writeRaster(r, p, overwrite = TRUE, datatype = dt, NAflag = -9999, gdal = c("TILED=YES", "COMPRESS=DEFLATE", "BIGTIFF=YES")); p }

# ---- zone and grid -----------------------------------------------------------------------------------------------------------
el0 <- rast(dsrc$StatewideElevationFile); levels(el0) <- NULL; CRS <- crs(el0)
NAT <- abs(res(el0)[1] - 30) < 1e-6                          # a 30 m DEM (LANDFIRE, CONUS): the national lattice; else a 150 m grid
if (!NAT && abs(res(el0)[1] - RES) > 1e-6) fail("Data Sources: the elevation must be a 30 m DEM or a %d m grid (got %g m)", RES, res(el0)[1])
bz <- st_transform(zone_polygons(dsrc$FireZoneBoundaryFile), CRS)
if (!nrow(bz[bz$ID == ZID, ])) fail("no polygon with ID %d (or PYROME %d) in %s", ZID, ZID - 1000L, dsrc$FireZoneBoundaryFile)
zone <- st_make_valid(st_sf(geometry = st_union(bz[bz$ID == ZID, ])))
run <- st_buffer(zone, BUF)
oX <- if (NAT) LATTICE_X0 else xmin(el0); oY <- if (NAT) LATTICE_Y0 else ymax(el0); b <- st_bbox(run)
e <- ext(oX + floor((b[["xmin"]] - oX) / RES) * RES, oX + ceiling((b[["xmax"]] - oX) / RES) * RES,
         oY - ceiling((oY - b[["ymin"]]) / RES) * RES, oY - floor((oY - b[["ymax"]]) / RES) * RES)
if (NAT) {                                                    # 150 m elevation = mean of each 5 x 5 block of 30 m pixels (as the
  off <- c((xmin(el0) - oX) %% 30, (ymax(el0) - oY) %% 30)    # statewide grid was built), truncated to whole metres
  if (any(abs(off) > 1e-6 & abs(off - 30) > 1e-6)) fail("the 30 m elevation is not on the 30 m lattice of the model grid (offset %s m)", paste(off, collapse = ", "))
  e30 <- extend(crop(el0, e, snap = "near"), e); ext(e30) <- e
  ref <- trunc(aggregate(e30, 5, fun = "mean", na.rm = TRUE))      # truncated, as the statewide grid was written
  say("elevation: 150 m means of the 30 m DEM %s", basename(dsrc$StatewideElevationFile))
} else { ref <- extend(crop(el0, e), e); coltab(ref) <- NULL }
ext(ref) <- ext(round(xmin(ref)), round(xmax(ref)), round(ymin(ref)), round(ymax(ref)))
say("%s (Map Code %d): %.0f km2, buffer %.0f km; grid %d x %d (%s cells)", zo$FireZone, ZID, as.numeric(st_area(zone)) / 1e6, BUF / 1000,
    nrow(ref), ncol(ref), format(ncell(ref), big.mark = ","))
cropg <- function(f) { r <- rast(f); levels(r) <- NULL; coltab(r) <- NULL; extend(crop(r, ext(ref)), ext(ref)) }
if (has("FireZoneGridFile")) fzr <- cropg(opt_src$FireZoneGridFile) else {   # national: rasterize every fire zone around the grid
  nb <- bz[lengths(st_intersects(bz, st_as_sfc(st_bbox(c(xmin = xmin(ref), ymin = ymin(ref), xmax = xmax(ref), ymax = ymax(ref)), crs = st_crs(CRS))))) > 0, ]
  fzr <- rasterize(vect(nb), rast(ref), field = "ID"); fzt2 <- rasterize(vect(nb), rast(ref), field = "ID", touches = TRUE)
  fzr <- cover(fzr, fzt2); say("fire zones rasterized from %s: %s", basename(dsrc$FireZoneBoundaryFile), paste(sort(unique(nb$ID)), collapse = ", ")) }
if (has("WeatherZoneGridFile")) wzr <- cropg(opt_src$WeatherZoneGridFile) else if (has("WeatherZoneBoundaryFile")) {
  # national: the NWS fire weather zones as the statewide grid was built (development scripts 04b, 24) - exact 30 m rasterization
  # (cell centre, then touched cells), remaining gaps from a 1 km nearest-zone surface grown by 5 x 5 modal passes, then the
  # modal zone of each 5 x 5 block; Map Codes from this project's Weather Zones (matched by name)
  wzt <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE))
  wv <- st_transform(weather_zone_polygons(opt_src$WeatherZoneBoundaryFile), CRS)
  wv <- wv[lengths(st_intersects(wv, st_as_sfc(st_bbox(c(xmin = xmin(ref) - 50000, ymin = ymin(ref) - 50000, xmax = xmax(ref) + 50000, ymax = ymax(ref) + 50000), crs = st_crs(CRS))))) > 0, ]
  wv$ID <- wzt$ID[match(wv$Name, wzt$Name)]
  if (anyNA(wv$ID)) fail("weather zones near the grid missing from this project's Weather Zones: %s", paste(head(wv$Name[is.na(wv$ID)], 10), collapse = "; "))
  v <- vect(wv); r30 <- rast(ext(ref), resolution = 30, crs = CRS); coarse <- rast(ext(ref), resolution = 1000, crs = CRS)
  z <- cover(rasterize(v, coarse, field = "ID"), rasterize(v, coarse, field = "ID", touches = TRUE)); it <- 0
  while (global(is.na(z), "sum")[[1]] > 0 && it < 400) { z <- focal(z, 5, fun = "modal", na.policy = "only", na.rm = TRUE); it <- it + 1 }
  fine <- cover(cover(rasterize(v, r30, field = "ID"), rasterize(v, r30, field = "ID", touches = TRUE)), resample(z, r30, method = "near"))
  wzr <- aggregate(fine, 5, fun = "modal", na.rm = TRUE); ext(wzr) <- ext(ref)
  say("weather zones rasterized from %s: %d zones on the grid", basename(opt_src$WeatherZoneBoundaryFile), length(unique(na.omit(values(wzr, mat = FALSE)))))
} else fail("Data Sources: set the Weather Zone Grid or the NWS Fire Weather Zones shapefile")
P <- list(Elevation = w(ref, "elevation.tif", "INT2S"), FireZone = w(fzr, "firezone.tif", "INT2S"), WeatherZone = w(wzr, "weatherzone.tif", "INT2S"))
grid <- rast(P$Elevation)
zp <- file.path(TD, "zone_boundary.gpkg"); st_write(transform(zone, Name = zo$FireZone, ID = ZID, buffer_m = BUF), zp, delete_dsn = TRUE, quiet = TRUE)
progressBar()

# ---- LANDFIRE fuel and canopy (5 x 5 block aggregation onto the grid) -----------------------------------------------------------
lf <- function(prod, v) landfire_file(dsrc$LandfireFolder, v, prod)
to_grid <- function(src, fun, zero_na = FALSE) {
  r <- crop(if (inherits(src, "SpatRaster")) src else rast(src), ext(grid) + 300, snap = "out"); NAflag(r) <- -9999; levels(r) <- NULL
  if (zero_na) r <- classify(r, cbind(0, NA))
  g <- aggregate(r, 5, fun = fun, na.rm = TRUE)
  off <- (xmin(g) - xmin(grid)) %% res(grid)[1]
  if (abs(off) > 1e-6 && abs(off - res(grid)[1]) > 1e-6) fail("aggregated LANDFIRE raster is off the grid lattice by %g m", off)
  resample(g, grid, method = "near") }
fb <- to_grid(lf("FBFM40", V), "modal"); fb[is.na(fb)] <- 99
say("fuel types (LF%s FBFM40): %s", V, paste(sort(unique(values(fb, mat = FALSE))), collapse = " "))
P$Fuel <- w(fb, "fuel_type.tif", "INT2S"); progressBar()
fsh <- to_grid(lf("CBD", V), function(x, ...) mean(x > 0, na.rm = TRUE)); nonfor <- is.na(fsh) | fsh < 0.5
cbh <- ifel(nonfor, NA, to_grid(lf("CBH", V), "mean", zero_na = TRUE) / 10)
cbd <- ifel(nonfor, NA, to_grid(lf("CBD", V), "mean", zero_na = TRUE) / 100)
cc  <- to_grid(lf("CC", V), "mean"); cc[is.na(cc)] <- 0
ch  <- ifel(nonfor, NA, to_grid(lf("CH", CHV), "mean", zero_na = TRUE) / 10)
C <- list(cbh = w(cbh, "canopy_base_height_m.tif", "FLT8S"), cbd = w(cbd, "canopy_bulk_density.tif", "FLT8S"),
          cc = w(cc, "canopy_cover_pct.tif", "FLT8S"), ch = w(ch, "canopy_height_m.tif", "FLT8S"))
say("forest cells %s of %s; CBH median %.1f m, CBD median %.3f, CH (LF%s) median %.1f m", format(sum(!values(nonfor, mat = FALSE)), big.mark = ","),
    format(ncell(grid), big.mark = ","), median(values(cbh, mat = FALSE), na.rm = TRUE), median(values(cbd, mat = FALSE), na.rm = TRUE), CHV, median(values(ch, mat = FALSE), na.rm = TRUE))
progressBar()

# ---- years since fire ------------------------------------------------------------------------------------------------------------
# Years since the last CONSUMING fire before the fuel vintage V, in classes 2 (fire in V-1..V), 5 (V-4..V-2), 10 (V-9..V-5).
# A statewide fire-age class raster (521 / 522 / 523) is cropped when Data Sources has one; otherwise it is built here as the
# development script 164 built it: a 150 m cell is consumed in year y when at least half its 30 m pixels are moderate or high
# severity (3, 4) on the MTBS annual mosaic; fires under 405 ha (no MTBS severity) count whole, from the interagency final
# wildfire perimeters (cell centre inside); the last consuming year wins. Unlike 164, each mosaic is cut to the zone grid before
# the 5 x 5 aggregation, so the blocks sit on the lattice (164 aggregated from the mosaic edge, 90 m off, then resampled: fire
# edges in the statewide grid can sit one cell off - P26 regression 2026-09-30: ~7% of fire-age cells differ, all at edges).
P$Age <- NA_character_
if (has("FireAgeFile")) {
  ag <- crop(rast(opt_src$FireAgeFile), grid); if (!compareGeom(ag, grid, stopOnError = FALSE)) ag <- resample(ag, grid, method = "near")
  yrs <- classify(ag, cbind(c(521, 522, 523), c(2, 5, 10)), others = NA); src_age <- basename(opt_src$FireAgeFile)
} else if (has("MtbsSeverityFolder")) {
  Vn <- as.integer(V); last <- setValues(rast(grid), NA_integer_)
  per <- if (has("InteragencyPerimeterFile")) { q <- st_as_text(st_as_sfc(st_bbox(st_transform(st_as_sfc(st_bbox(grid)), 3857))))
    x <- read_sf(opt_src$InteragencyPerimeterFile, wkt_filter = q)
    x$yr <- suppressWarnings(as.integer(x$FIRE_YEAR)); x$ha <- suppressWarnings(as.numeric(x$GIS_ACRES)) * 0.404686
    x <- x[x$FEATURE_CA %in% c("Wildfire", "Wildfire Final Fire Perimeter") & !is.na(x$yr) & !is.na(x$ha) & x$ha < 405, ]
    st_make_valid(st_transform(x, CRS)) } else NULL
  if (is.null(per)) say("fire age: no interagency perimeters in Data Sources - fires under 405 ha are left out")
  for (y in (Vn - 9):Vn) {
    sf_ <- file.path(opt_src$MtbsSeverityFolder, sprintf("mtbs_CONUS_%d.tif", y))
    con <- setValues(rast(grid), FALSE); nm <- 0
    if (file.exists(sf_)) { s <- rast(sf_); if (!same.crs(s, grid)) s <- project(s, CRS, method = "near")
      d <- (c(xmin(s), ymax(s)) - c(xmin(grid), ymax(grid))) %% 30; d <- ifelse(d > 15, d - 30, d)   # the 2019 mosaic sits 2 m off
      if (any(abs(d) > 3)) fail("MTBS mosaic %d is not on the 30 m lattice (offset %s m)", y, paste(round(d, 1), collapse = ", "))
      if (any(abs(d) > 1e-6)) s <- terra::shift(s, dx = -d[1], dy = -d[2])
      s <- extend(crop(s, ext(grid), snap = "near"), ext(grid)); ext(s) <- ext(grid)
      mh <- aggregate(s %in% c(3, 4), 5, mean, na.rm = TRUE); ext(mh) <- ext(grid); mh[is.na(mh)] <- 0
      con <- mh >= 0.5; nm <- global(con, "sum", na.rm = TRUE)[[1]]
    } else say("fire age %d: no MTBS mosaic %s", y, basename(sf_))
    ns <- 0; if (!is.null(per) && any(per$yr == y)) { r <- rasterize(vect(per[per$yr == y, ]), rast(grid), background = 0)
      ns <- global(r > 0 & !con, "sum", na.rm = TRUE)[[1]]; con <- con | (r > 0) }
    last <- ifel(con, y, last)
    if (nm + ns > 0) say("fire age %d: %s consumed cells from MTBS moderate/high, %s more from fires under 405 ha", y, format(nm, big.mark = ","), format(ns, big.mark = ","))
  }
  tsf <- Vn - last + 1; yrs <- ifel(tsf <= 2, 2L, ifel(tsf <= 5, 5L, ifel(tsf <= 10, 10L, NA))); src_age <- "MTBS mosaics + interagency perimeters"
} else yrs <- NULL
if (!is.null(yrs)) { t <- table(values(yrs, mat = FALSE)); say("years since fire (%s): %s", src_age, paste(names(t), t, sep = " yr: ", collapse = ", "))
  P$Age <- w(yrs, "years_since_fire.tif", "INT2S")
} else say("no fire-age raster and no MTBS severity folder in Data Sources: no recent-fire rules will apply")
progressBar()

# ---- vegetation (for spread-day strata): LANDFIRE BpS vegetation groups -> Vegetation Classes, modal 30 m -> 150 m ---------------
# BpS is potential vegetation, unchanged by fire, so recorded fires' origins are classed without the fuel map's memory of them.
P$Veg <- NA_character_
bps <- read_data_sources("BpsFile", optional = TRUE)$BpsFile
if (length(bps) && !is.na(bps) && nzchar(bps)) {
  if (!file.exists(bps)) fail("Data Sources: BpsFile not found (%s)", bps)
  cf <- list.files(file.path(dirname(dirname(bps)), "CSV_Data"), "[.]csv$", full.names = TRUE)[1]
  if (is.na(cf)) fail("BpS attribute table not found (a CSV_Data folder beside %s's Tif folder)", basename(bps))
  lut <- fread(cf); setnames(lut, 1, "VALUE"); if (!"GROUPVEG" %in% names(lut)) fail("BpS attribute table %s has no GROUPVEG column", basename(cf))
  vc <- as.data.table(datasheet(prj, ds("VegetationClass"), lookupsAsFactors = FALSE))
  cls <- fcase(lut$GROUPVEG %in% c("Conifer", "Conifer-Hardwood"), "Conifer", lut$GROUPVEG %in% c("Hardwood", "Hardwood-Conifer"), "Hardwood",
               lut$GROUPVEG == "Shrubland", "Shrub", lut$GROUPVEG %in% c("Grassland", "Savanna"), "Grass", default = "Other")
  lut[, id := vc$ID[match(cls, vc$Name)]]
  if (all(is.na(lut$id))) fail("none of the Vegetation Classes (Conifer, Hardwood, Shrub, Grass, Other) is in this project's Vegetation Classes")
  r <- crop(rast(bps), ext(grid) + 300, snap = "out"); levels(r) <- NULL; r <- classify(r, as.matrix(lut[!is.na(id), .(VALUE, id)]), others = NA)
  veg <- to_grid(r, "modal")
  t <- table(vc$Name[match(values(veg, mat = FALSE), vc$ID)]); say("vegetation (BpS): %s", paste(names(t), format(as.integer(t), big.mark = ","), sep = " ", collapse = ", "))
  P$Veg <- w(veg, "vegetation.tif", "INT2S")
} else say("no BpS file in Data Sources: no vegetation map (spread days cannot be fitted by vegetation)")

write_ds(data.frame(FuelFileName = P$Fuel, ElevationFileName = P$Elevation, FireZoneFileName = P$FireZone, WeatherZoneFileName = P$WeatherZone,
                    FireAgeFileName = P$Age, VegetationFileName = P$Veg), "LandscapeRasters")
write_ds(data.frame(CanopyBaseHeightFileName = C$cbh, CanopyBulkDensityFileName = C$cbd, CanopyCoverFileName = C$cc, CanopyHeightFileName = C$ch), "CanopyRasters")
write_ds(data.frame(FileName = zp), "OutputZoneBoundary")
progressBar(); progressBar(type = "end")
