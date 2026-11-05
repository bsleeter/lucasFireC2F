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

# a file the package ships
pkg_file <- function(...) file.path(env$PackageDirectory, ...)

# The project's Data Sources (paths to statewide and national data on disk). A blank path reads its place under Data Sources Home
# (src/datahome.R; a Data Download scenario fills it): defaults/library/data-sources.csv gives each sheet column its Dataset, the
# manifest's name for it, and HomePath, where it reads inside that dataset. optional = TRUE: a column with nothing set or at home
# gives NA instead of stopping. A required path must exist.
source(file.path(env$PackageDirectory, "src", "datahome.R"))
source_columns <- function() fread(pkg_file("defaults", "library", "data-sources.csv"), sep = ",", colClasses = "character", na.strings = "", encoding = "UTF-8")
.home_cov <- new.env(); .home_cov$used <- character(); .home_cov$box <- NULL; .home_cov$what <- "the zone"; .home_cov$dirs <- list(); .home_cov$read <- list(); .home_cov$said <- list(); .home_cov$said_ed <- list()
data_sources_home <- function() { d <- as.data.table(datasheet(prj, ds("DataSource"), lookupsAsFactors = FALSE, optional = TRUE))
  h <- if (nrow(d) && "DataSourcesHome" %in% names(d)) d$DataSourcesHome[1] else NA_character_; if (is.na(h) || !nzchar(h)) NA_character_ else h }
read_data_sources <- function(cols, optional = FALSE) {
  d <- as.data.table(datasheet(prj, ds("DataSource"), lookupsAsFactors = FALSE, optional = TRUE))
  home <- data_sources_home(); hl <- if (!is.na(home)) source_columns() else NULL
  if (!nrow(d) && is.null(hl)) { if (optional) return(as.list(setNames(rep(NA_character_, length(cols)), cols))); fail("Data Sources (project) is empty - set Data Sources Home, or point it at the data on disk") }
  regions <- !is.null(hl) && length(home_regions(home)) > 0
  # the editions this library reads (Data Source Editions, set on the Data Sources page; no row = the first edition)
  ed <- tryCatch(as.data.table(datasheet(prj, ds("DataSourceEdition"), optional = TRUE, lookupsAsFactors = FALSE)), error = function(e) data.table())
  ed <- if (nrow(ed) && all(c("DataSource", "Edition") %in% names(ed))) setNames(as.character(ed$Edition), ed$DataSource) else character()
  out <- list()
  for (k in cols) { v <- if (k %in% names(d) && nrow(d)) d[[k]][1] else NA_character_; how <- "set"; dir <- NA_character_
    key <- if (!is.null(hl) && k %in% hl$Column) hl[Column == k, Dataset] else NA_character_
    if ((is.na(v) || !nzchar(v)) && !is.na(key)) {
      # with region folders, the folder is chosen by the zone: the smallest region whose manifest covers it (datahome.R)
      want_ed <- if (key %in% names(ed) && !is.na(ed[[key]]) && nzchar(ed[[key]])) ed[[key]] else NULL
      pk <- tryCatch(home_pick(home, key, hl[Column == k, HomePath], if (regions || !is.null(want_ed)) study_box() else NULL, want_ed), error = function(e) fail("%s", conditionMessage(e)))
      if (is.null(pk) && !is.null(want_ed)) fail("Data Sources: %s: edition %s, which this library reads (Data Sources page), is not in Data Sources Home %s", k, want_ed, home)
      if (!is.null(pk) && is.null(.home_cov$said_ed[[key]])) { .home_cov$said_ed[[key]] <- TRUE
        others <- tryCatch(home_editions(home, key), error = function(e) data.table())
        # (the other editions in the folder read from: another region's copy is not an edition)
        pd <- normalizePath(pk$dir, winslash = "/", mustWork = FALSE); base <- home_label(home, sub("/editions/[^/]+$", "", pd))
        others <- if (nrow(others)) others[Dir != pd & Folder == base] else others
        if (nrow(others)) say("Data Sources: %s: the home also holds %s; this library reads %s. A change of edition is made on the Data Sources page", k,
                              paste(sprintf("%s (%s, %s)", ifelse(nzchar(others$Edition), others$Edition, "the first edition"), others$Version, others$Folder), collapse = ", "),
                              if (is.null(want_ed)) "the first edition" else sprintf("edition %s", want_ed)) }
      if (!is.null(pk)) { v <- pk$path; how <- "home"; dir <- pk$dir; .home_cov$home <- home; .home_cov$dirs[[k]] <- pk$dir; .home_cov$used <- union(.home_cov$used, k)
        if ((regions || !is.null(want_ed)) && is.null(.home_cov$said[[k]])) { .home_cov$said[[k]] <- TRUE; say("Data Sources: %s from %s", k, home_label(home, pk$dir)) }
        if (!is.null(.home_cov$box)) home_covers(k, .home_cov$box, .home_cov$what) } }
    if (is.na(v) || !nzchar(v)) { if (optional) { out[k] <- list(NA_character_); next }
      fail("Data Sources: %s is not set and is not in Data Sources Home%s: run a Data Download scenario for this zone, or set its path on the Data Sources page", k, if (is.null(hl)) " (Data Sources Home is not set)" else "") }
    if (!optional && !file.exists(v)) fail("Data Sources: %s not found (%s)", k, v)
    .home_cov$read[[k]] <- list(DataSource = k, Dataset = key, Origin = how, Dir = dir, Path = v)
    out[[k]] <- v }
  out }

# a dataset's place in the top of the home whether or not it holds anything yet (P5 adds station-years to it); NA with no home
home_place <- function(col) { home <- data_sources_home(); if (is.na(home)) return(NA_character_)
  hp <- source_columns()[Column == col, HomePath]; if (!length(hp) || is.na(hp)) return(NA_character_); file.path(home, hp) }

# the box a zone needs (EPSG:5070, xmin, ymin, xmax, ymax), for choosing among region folders: the zone's bounding box plus its grid buffer and the
# reach of the ignition covariates (32.1 km past the grid), so P1 and P2 read one folder. NULL when the scenario names no zone: the top first
study_box <- function() {
  if (!is.null(.home_cov$study_box)) return(.home_cov$study_box)
  b <- tryCatch({ zo <- read_ds("ZonePrepOption"); zb <- read_ds("OutputZoneBoundary")
    buf <- if (nrow(zo) && !is.na(zo$BufferKm[1])) zo$BufferKm[1] else 25
    z <- if (nrow(zb) && file.exists(zb$FileName[1])) sf::read_sf(zb$FileName[1]) else if (nrow(zo) && !is.na(zo$FireZone[1])) {
        fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); id <- fzt$ID[match(zo$FireZone[1], fzt$Name)]
        d <- as.data.table(datasheet(prj, ds("DataSource"), lookupsAsFactors = FALSE, optional = TRUE)); f <- if (nrow(d)) d$FireZoneBoundaryFile[1] else NA_character_
        if (is.na(f) || !nzchar(f)) { hl <- source_columns()[Column == "FireZoneBoundaryFile"]; f <- home_pick(data_sources_home(), hl$Dataset, hl$HomePath)$path }
        y <- zone_polygons(f); y[y$ID == id, ] } else NULL
    if (is.null(z) || !nrow(z)) NULL else { x <- as.numeric(sf::st_bbox(sf::st_transform(z, 5070))); x + c(-1, -1, 1, 1) * (buf + 32.1 + 0.3) * 1000 } },
    error = function(e) { say("Data Sources Home: the zone is not known yet (%s); reading the top of the home first", conditionMessage(e)); NULL })
  .home_cov$study_box <- b; b }

# ---- Data Sources Home coverage -----------------------------------------------------------------------------------------------
# A dataset read from the home folder holds only the region its Data Download was run for (manifest.csv). A stage calls home_coverage(<what it
# needs>) once: every home dataset read so far, and every one read after, must cover it, or the stage stops naming the dataset, the region it holds
# and what to do. A home file with no manifest row (put there by hand) is not checked; a set path is never checked. home_coverage(x, cols, what) checks
# the named columns against a wider extent without changing the stage's own.
home_coverage <- function(x, cols = NULL, what = "the zone", warn_only = FALSE) {
  e <- if (inherits(x, "SpatRaster")) { r <- terra::res(x)[1]; terra::as.polygons(terra::ext(x) - r, crs = terra::crs(x)) } else terra::as.polygons(terra::ext(x), crs = terra::crs(x))
  box <- as.vector(terra::ext(terra::project(e, "EPSG:5070")))[c(1, 3, 2, 4)]
  if (is.null(cols)) { .home_cov$box <- box; .home_cov$what <- what; cols <- .home_cov$used } else cols <- intersect(cols, .home_cov$used)
  for (k in cols) if (warn_only) tryCatch(home_covers(k, box, what), error = function(e) say("Warning: %s", sub("^.*?Data Sources: ", "", conditionMessage(e)))) else home_covers(k, box, what)
  invisible(box) }
home_covers <- function(k, box, what) {
  key <- .home_cov$read[[k]]$Dataset; if (is.null(key)) key <- source_columns()[Column == k, Dataset]
  dir <- .home_cov$dirs[[k]]; if (is.null(dir)) dir <- .home_cov$home; at <- home_label(.home_cov$home, dir)
  ok <- manifest_covers(dir, key, box); if (!isFALSE(ok)) return(invisible(TRUE))
  m <- read_manifest(dir)[Column == key]
  if (!identical(m$Status, "done")) fail("Data Sources: %s in Data Sources Home (%s) is marked %s by its last Data Download (%s); run the Data Download again, or set its path",
                                         k, at, m$Status, if (is.na(m$Note)) "no note" else m$Note)
  fail("Data Sources: %s in Data Sources Home (%s, %s) was downloaded for %s%s and does not cover %s; run a Data Download for this zone with a larger buffer (or one holding it) into %s, or set its path",
       k, at, m$Dataset, m$Units, if (is.na(m$BufferKm) || !nzchar(m$BufferKm)) "" else sprintf(" + %s km", m$BufferKm), what, .home_cov$home) }

# the Data Sources this stage read, written to Data Sources Used: where each came from, and its files' sizes and checksum (from the home's files.csv:
# one file's sha256, or the sha256 of the sorted path and sha256 list of a folder). A path set by hand has no checksum. A home dataset whose files no
# longer match files.csv is noted: the write-once rule was broken
data_sources_used <- function() {
  if (!length(.home_cov$read)) return(NULL)
  stage <- tools::file_path_sans_ext(basename(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])))
  rbindlist(lapply(.home_cov$read, function(x) {
    f <- if (identical(x$Origin, "home")) read_files(x$Dir)[Column == x$Dataset] else data.table()
    if (nrow(f)) { fp <- file.path(x$Dir, f$Path); ok <- all(file.exists(fp)) && all(file.size(fp) == as.numeric(f$Bytes))
      sh <- if (nrow(f) == 1) f$Sha256 else digest::digest(paste(sort(paste(f$Path, f$Sha256)), collapse = "\n"), algo = "sha256", serialize = FALSE)
      return(data.table(Stage = stage, DataSource = x$DataSource, Origin = home_label(.home_cov$home, x$Dir), Path = x$Path,
                        Files = nrow(f), Bytes = sum(as.numeric(f$Bytes)), Sha256 = sh, Note = if (ok) NA_character_ else "files on disk differ from files.csv")) }
    fl <- if (dir.exists(x$Path)) list.files(x$Path, recursive = TRUE, full.names = TRUE) else x$Path
    data.table(Stage = stage, DataSource = x$DataSource, Origin = x$Origin, Path = x$Path, Files = length(fl), Bytes = sum(as.numeric(file.size(fl)), na.rm = TRUE), Sha256 = NA_character_,
               Note = if (identical(x$Origin, "home")) "no files.csv rows (laid out by hand)" else "not from Data Sources Home: no checksum") })) }
record_data_sources <- function() { u <- data_sources_used(); if (is.null(u) || !nrow(u)) return(invisible())
  tryCatch(write_ds(u, "OutputDataSources", append = TRUE), error = function(e) say("Data Sources Used not written (%s)", conditionMessage(e))) }

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
