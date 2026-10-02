# =============================================================================
# shared.R - helpers used by every lucasFireC2F transformer
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })

# SyncroSim Studio points PROJ at its own bundled database (C:\Program Files\SyncroSim\gdal\share), which terra's newer PROJ
# cannot read: every WGS84 -> Albers projection then fails ("Cannot find coordinate operations"). The console does not set it.
for (v in c("PROJ_LIB", "PROJ_DATA", "GDAL_DATA")) if (grepl("SyncroSim", Sys.getenv(v), fixed = TRUE)) Sys.unsetenv(v)

PKG <- "lucasFireC2F"
ds  <- function(name) paste0(PKG, "_", name)

# datasheet reads retry: the console occasionally fails a read with "Object reference not set to an instance of an object"
# (P18 Run 10,000, C6 reading Vegetation Classes after 50 minutes of run; the same read worked in the 2,500-season run).
# Defined here, after library(rsyncrosim), so every stage's unqualified datasheet() read goes through it.
datasheet <- function(...) { for (k in 1:3) { r <- tryCatch(rsyncrosim::datasheet(...), error = function(e) e)
    if (!inherits(r, "error")) return(r)
    if (k < 3) { message("datasheet read failed (", conditionMessage(r), "); retrying"); Sys.sleep(3 * k) } }
  stop(r) }

# the running scenario and environment (namespaced: terra and others also export project() / scenario-like names)
env      <- rsyncrosim::ssimEnvironment()
lib      <- suppressWarnings(rsyncrosim::ssimLibrary())
scn      <- rsyncrosim::scenario()
prj      <- rsyncrosim::project()

# SyncroSim multiprocessing runs a stage in job libraries (Job-1.ssim, ...). Not supported: stage 4 already runs WorkersPerJob
# C2F-W processes per job (100 jobs x 100 workers exhausted memory), and C3, C5 and stage 5 need every season in one place.
if (grepl("^Job-[0-9]+\\.ssim$", basename(env$LibraryFilePath)))
  stop("SyncroSim multiprocessing is not supported by lucasFireC2F: turn it off in the library properties. ",
       "Stage 4 runs fires in parallel itself (Growth Options: Workers per Job; 0 = all cores but one).", call. = FALSE)

# read a datasheet of this package as a data.table (empty table when the sheet has no rows); an empty option sheet comes back
# as one row of the package defaults (below), so a scenario the user never opened a sheet in still runs
read_ds <- function(name, ...) { x <- as.data.table(datasheet(scn, ds(name), optional = TRUE, lookupsAsFactors = FALSE, ...))
  if (!nrow(x) && name %in% DEFAULTED_SHEETS) { d <- sheet_defaults(name); if (!is.null(d)) x <- d }
  x }

# Option sheets that fall back to the defaultValue of each column in package.xml when empty (logged once per sheet). Not
# defaulted: sheets a fit writes (ResampleOption, SpreadDays, ...) and ZonePrepOption, whose fire zone has no sensible default.
# Workers per Job is left blank so stage 4 uses all cores but one (package.xml's default of 1 grows every fire on one process).
DEFAULTED_SHEETS <- c("EffectsOption", "RiskOption", "FireDetailOption", "OutputOption", "GrowthOption", "WeatherOption",
                      "CalibrationOption", "CalibrationPeriod", "ValidationOption", "WindTableOption", "EventOption", "BurningHours")
DEFAULT_OVERRIDES <- list(GrowthOption = list(WorkersPerJob = NA_integer_))
.defaults_said <- new.env()
sheet_defaults <- function(name) {
  xml <- paste(readLines(file.path(env$PackageDirectory, "package.xml"), warn = FALSE), collapse = "\n")
  blk <- regmatches(xml, regexpr(sprintf('(?s)<dataSheet name="%s"[^>]*(?<!/)>.*?</dataSheet>', name), xml, perl = TRUE))
  if (!length(blk)) return(NULL)
  cols <- regmatches(blk, gregexpr('<column [^>]*>', blk))[[1]]
  att <- function(tag, a) { m <- regmatches(tag, regexpr(sprintf('%s="[^"]*"', a), tag)); if (length(m)) sub('^[^"]*"(.*)"$', "\\1", m) else NA_character_ }
  out <- list()
  for (tg in cols) { k <- att(tg, "name"); v <- att(tg, "defaultValue"); ty <- att(tg, "dataType")
    out[[k]] <- switch(ty, Integer = as.integer(v), Double = as.numeric(v), Boolean = if (is.na(v)) NA else v != "0", v) }
  for (k in names(DEFAULT_OVERRIDES[[name]])) out[[k]] <- DEFAULT_OVERRIDES[[name]][[k]]
  if (all(vapply(out, is.na, TRUE))) return(NULL)
  if (is.null(.defaults_said[[name]])) { .defaults_said[[name]] <- TRUE
    shown <- out[!vapply(out, is.na, TRUE)]
    say("%s is empty: using the package defaults (%s)", name, paste(names(shown), unlist(lapply(shown, as.character)), sep = " = ", collapse = ", ")) }
  as.data.table(out) }
# a raster column of a single-row sheet as a SpatRaster (datasheet() returns the stored file's full path)
read_raster <- function(name, col) { p <- read_ds(name)[[col]][1]
  if (is.null(p) || is.na(p) || !nzchar(p)) return(NULL)
  if (!file.exists(p)) fail("%s: file for %s not found (%s)", name, col, p)
  terra::rast(p) }
# origin vegetation (Vegetation Classes names) of grid cells or of lon/lat points, from Landscape Maps' vegetation map (P1, from
# LANDFIRE BpS); NA everywhere when the scenario has no vegetation map
veg_map <- function() { p <- read_ds("LandscapeRasters")[["VegetationFileName"]]
  if (is.null(p) || !length(p) || is.na(p[1]) || !nzchar(p[1])) return(NULL); if (!file.exists(p[1])) fail("vegetation map not found (%s)", p[1]); terra::rast(p[1]) }
veg_of_cells <- function(cells, r = veg_map()) { if (is.null(r)) return(rep(NA_character_, length(cells)))
  vc <- as.data.table(datasheet(prj, ds("VegetationClass"), lookupsAsFactors = FALSE)); id <- terra::values(r, mat = FALSE)[cells]; vc$Name[match(id, vc$ID)] }
veg_of_lonlat <- function(lon, lat, r = veg_map()) { if (is.null(r)) return(rep(NA_character_, length(lon)))
  p <- terra::project(terra::vect(cbind(lon, lat), crs = "EPSG:4326"), terra::crs(r)); veg_of_cells(terra::cellFromXY(r, terra::crds(p)), r) }

# the project's Data Sources (paths to statewide and national data on disk); required columns must be set and exist
read_data_sources <- function(cols, optional = FALSE) {
  d <- as.data.table(datasheet(prj, ds("DataSource"), lookupsAsFactors = FALSE, optional = TRUE))
  if (!nrow(d)) { if (optional) return(as.list(setNames(rep(NA_character_, length(cols)), cols))); fail("Data Sources (project) is empty - point it at the data on disk") }
  out <- list()
  for (k in cols) { v <- if (k %in% names(d)) d[[k]][1] else NA_character_
    if (!optional && (is.na(v) || !nzchar(v))) fail("Data Sources: %s is not set", k)
    if (!optional && !file.exists(v)) fail("Data Sources: %s not found (%s)", k, v)
    out[[k]] <- v }
  out }

# a LANDFIRE product GeoTIFF under Data Sources' LANDFIRE Folder, as LANDFIRE ships it: LF<v>_<prod>_CONUS*/LF<v>_<prod>_CONUS/Tif/
# LF<v>_<prod>_CONUS.tif. Some re-released packages keep the old file name inside the new folder (LF2020 CH: Tif/LC22_CH_220.tif),
# so when the standard name is absent the product folder's single GeoTIFF is taken.
landfire_file <- function(folder, v, prod) {
  f <- Sys.glob(sprintf("%s/LF%s_%s_CONUS*/LF%s_%s_CONUS/Tif/LF%s_%s_CONUS.tif", folder, v, prod, v, prod, v, prod))
  if (!length(f)) { f <- Sys.glob(sprintf("%s/LF%s_%s_CONUS*/LF%s_%s_CONUS/Tif/*.tif", folder, v, prod, v, prod))
    if (length(f) == 1) say("LANDFIRE %s LF%s: using %s (non-standard file name)", prod, v, basename(f)) }
  if (length(f) != 1) fail("LANDFIRE %s LF%s not found under %s (expected an unzipped LF%s_%s_CONUS folder with one GeoTIFF in Tif; found %d)",
                           prod, v, folder, v, prod, length(f))
  f }

# The model's 150 m lattice (EPSG:5070 Albers): cell edges at LATTICE_X0 + k*150, LATTICE_Y0 + k*150. It is the lattice of the
# California 150 m grids the first zones were built on (30 m west of LANDFIRE's CONUS origin, so every 150 m cell is an exact
# 5 x 5 block of LANDFIRE 30 m pixels); zones built anywhere in CONUS from the national 30 m data land on it too, so any two
# zones can be merged (stage M).
LATTICE_X0 <- -2362425; LATTICE_Y0 <- 2585625

# Fire-zone polygons with the zones' Map Codes: a file with field ID (the package's California GeoPackage), or the national
# pyrome shapefile (RDS-2020-0020, field PYROME), whose Map Code is 1000 + PYROME and name "P<n> <NAME>" (the Fire Zones defaults).
# NWS fire weather zones (Data Sources: WeatherZoneBoundaryFile, e.g. fz03mr26.shp) named as the Weather Zones list names them,
# "<STATE><ZONE>: <NAME>"; a zone stored as several records is unioned into one.
weather_zone_polygons <- function(f) {
  z <- sf::read_sf(f); z$Name <- paste0(z$STATE, z$ZONE, ": ", z$NAME)
  d <- unique(z$Name[duplicated(z$Name)])
  if (length(d)) { u <- do.call(rbind, lapply(d, function(n) { k <- z[z$Name == n, ]; g <- suppressMessages(sf::st_union(sf::st_make_valid(k))); k <- k[1, ]; sf::st_geometry(k) <- g; k }))
    z <- rbind(z[!z$Name %in% d, ], u) }
  z }
# Fire-activity weights of the zone weather (development script 06): a gridMET cell weighs (number of MTBS perimeters overlapping
# it) / FIRE_COUNT_REF + WEIGHT_FLOOR. FIRE_COUNT_REF is the maximum count over California (8), held fixed so a zone's weights do
# not depend on where it lies; the floor keeps never-burned zones usable.
FIRE_COUNT_REF <- 8; WEIGHT_FLOOR <- 0.05

zone_polygons <- function(f) {
  z <- sf::read_sf(f)
  if (!"ID" %in% names(z)) { if (!"PYROME" %in% names(z)) fail("fire zone boundary file %s has neither an ID nor a PYROME field", basename(f))
    z$ID <- 1000L + as.integer(z$PYROME) }
  z }

# saveDatasheet goes through text: a whole number held as double (e.g. cell 700000) would be written "7e+05" and refused by an
# integer column, so whole-number doubles go as integers and nothing is written in scientific notation
options(scipen = 999)
write_ds <- function(x, name, ...) { x <- as.data.frame(x)
  for (k in names(x)) if (is.double(x[[k]]) && all(is.na(x[[k]]) | (x[[k]] == round(x[[k]]) & abs(x[[k]]) < .Machine$integer.max))) x[[k]] <- as.integer(x[[k]])
  saveDatasheet(scn, x, ds(name), ...) }

# metric lists (EffectsMetric, RiskMetric, SeverityMetric): code -> the display name users see; scripts work in codes
metric_names <- function(sheet) { x <- as.data.table(datasheet(prj, ds(sheet), lookupsAsFactors = FALSE))
  if (!nrow(x) || !"Code" %in% names(x)) fail("project sheet %s has no metric codes", sheet); setNames(x$Name, x$Code) }
metric_code <- function(sheet, name) { m <- metric_names(sheet); names(m)[match(name, m)] }

# stop with a message SyncroSim shows in the run log
fail <- function(...) stop(sprintf(...), call. = FALSE)
say  <- function(...) updateRunLog(sprintf(...))

# require a datasheet to have rows (and, optionally, named columns filled)
need <- function(name, cols = NULL) {
  x <- read_ds(name)
  if (!nrow(x)) fail("%s is empty - fill it in the scenario before running this stage", name)
  if (length(cols)) { miss <- cols[vapply(cols, function(k) all(is.na(x[[k]])), TRUE)]
    if (length(miss)) fail("%s: no value for %s", name, paste(miss, collapse = ", ")) }
  x
}

# a stage that is declared but not yet implemented (package version 1.0.0 dev = Phase A skeleton)
not_yet <- function(stage, inputs = character()) {
  for (n in inputs) say("%s: %s has %d rows", stage, n, nrow(read_ds(n)))
  fail("%s is not implemented in lucasFireC2F %s (Phase A skeleton); inputs were read and are listed above", stage, "1.0.0 (dev)")
}
