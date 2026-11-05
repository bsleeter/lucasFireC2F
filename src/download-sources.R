# =============================================================================
# download-sources.R - the Data Download handlers: where each dataset comes from and how it lands at its place
# =============================================================================
# One handler per dataset of defaults/download/datasets.csv (src/download.R runs them). Those lucasFireC2F shares with lucasCore keep
# lucasCore's name, place and method (lucasCore src/download-sources.R; URLs checked there 2026-10-31), so a home folder can serve both
# packages and a dataset is fetched once. Those only the fire model reads were checked 2026-11-02. Each handler's plan lists its files:
# url, file (the name in the download folder) and fetch (FALSE: read remotely, not downloaded). finish writes the files the stages read,
# with the names they read, and returns the dataset's place (folder or file). Handlers marked national = TRUE always keep the whole files,
# at the top of the home: the fire records, gridMET (0.5 GB a year; a zone's weather zones reach well past its region), the zone polygons,
# PRISM and RAWS.
# =============================================================================

# GDAL reading remote archives: Box and some servers answer HEAD with 404, so GDAL asks with ranged GETs
gdal_remote <- function() Sys.setenv(CPL_VSIL_CURL_USE_HEAD = "NO", GDAL_HTTP_MAX_RETRY = "5", GDAL_HTTP_RETRY_DELAY = "10", GDAL_HTTP_TIMEOUT = "600",
                                     CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".zip,.tif,.dbf,.nc", GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR")
remote_member <- function(url, member) sprintf("/vsizip//vsicurl/%s/%s", url, member)

# the 48 conterminous states and DC by FIPS (TIGER STATEFP)
CONUS_STATES <- c(`01` = "AL", `04` = "AZ", `05` = "AR", `06` = "CA", `08` = "CO", `09` = "CT", `10` = "DE", `11` = "DC", `12` = "FL", `13` = "GA",
  `16` = "ID", `17` = "IL", `18` = "IN", `19` = "IA", `20` = "KS", `21` = "KY", `22` = "LA", `23` = "ME", `24` = "MD", `25` = "MA", `26` = "MI",
  `27` = "MN", `28` = "MS", `29` = "MO", `30` = "MT", `31` = "NE", `32` = "NV", `33` = "NH", `34` = "NJ", `35` = "NM", `36` = "NY", `37` = "NC",
  `38` = "ND", `39` = "OH", `40` = "OK", `41` = "OR", `42` = "PA", `44` = "RI", `45` = "SC", `46` = "SD", `47` = "TN", `48` = "TX", `49` = "UT",
  `50` = "VT", `51` = "VA", `53` = "WA", `54` = "WV", `55` = "WI", `56` = "WY")

# the states the region touches, from the national States file (fetched first by the stage); every state for CONUS
region_states <- function(ctx) {
  if (is.null(ctx$region$poly)) return(unname(CONUS_STATES))
  s <- home_path(ctx$home, "study-area/states/*.shp"); if (is.na(s)) { ctx$log("  no States file: taking every state"); return(unname(CONUS_STATES)) }
  sp <- sf::st_transform(sf::read_sf(s), 5070); hit <- sp[lengths(sf::st_intersects(sp, ctx$region$poly)) > 0, ]
  st <- unname(CONUS_STATES[hit$STATEFP]); st[!is.na(st)] }

# the years asked of an annual dataset, within what the source has
years_of <- function(ctx, have) { y1 <- if (is.na(ctx$years[2])) max(have) else ctx$years[2]; intersect(have, ctx$years[1]:y1) }

clipping <- function(ctx) !is.null(ctx$region$poly) && !ctx$keep
whole_conus <- function(ctx) is.null(ctx$region$poly)   # all of CONUS (Home layout National): the sources' national files

# the one GeoTIFF in a downloaded zip, unpacked to dest (sidecars left behind, as the region's files have none). R's unzip may
# refuse a member over 4 GB (LANDFIRE elevation): then Windows' tar, which reads zip64
zip_tif <- function(ctx, zip, dest) {
  m <- utils::unzip(zip, list = TRUE)$Name; t <- m[grepl("[.]tif$", m, ignore.case = TRUE)]
  if (length(t) != 1) stop(sprintf("%s holds %d .tif files (%s); expected one", basename(zip), length(t), paste(head(t, 4), collapse = ", ")), call. = FALSE)
  ex <- file.path(ctx$dl, "unzip"); dir.create(ex, recursive = TRUE, showWarnings = FALSE); x <- file.path(ex, t)
  ok <- tryCatch({ utils::unzip(zip, files = t, exdir = ex); file.exists(x) && file.size(x) > 0 }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!ok) { unlink(x); system2("tar", c("-xf", shQuote(zip), "-C", shQuote(ex), shQuote(t)))
    if (!file.exists(x)) stop(sprintf("could not unpack %s from %s", t, basename(zip)), call. = FALSE) }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!file.rename(x, dest)) stop(sprintf("could not move %s to its place", basename(t)), call. = FALSE)
  unlink(ex, recursive = TRUE); dest }

# a raster from a zip: the region read remotely, or (national) the whole zip fetched and the member unpacked. A remote read can
# break deep in a large zip (TreeMap's 5.4 GB zip64, 2026-10-31: a deflate error 12,000 rows in): then the whole zip is fetched
# (resumable) and clipped here
zip_raster <- function(ctx, items, i, member, dest) {
  if (clipping(ctx) && !file.exists(items$dest[i])) { gdal_remote()
    ok <- tryCatch({ clip_raster(remote_member(items$url[i], member), dest, ctx$region); TRUE }, error = function(e) { ctx$log(sprintf("  remote read of %s failed (%s): fetching the zip", basename(member), conditionMessage(e))); FALSE })
    if (ok) return(dest)
    gc(); unlink(paste0(dest, c("", ".aux.xml")))   # the broken attempt's partial file
    fetch_files(items$url[i], items$dest[i], ctx$connections, ctx$log)
    x <- unzip_to(items$dest[i], file.path(ctx$dl, "unzip"), member); clip_raster(x, dest, ctx$region); unlink(x); return(dest) }
  x <- unzip_to(items$dest[i], file.path(ctx$dl, "unzip"), member)
  if (clipping(ctx)) { clip_raster(x, dest, ctx$region); unlink(x); return(dest) }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE); file.rename(x, dest); dest }
# a dbf attribute table from a zip, read remotely (OGR) and written back, or unpacked from the fetched zip
zip_dbf <- function(ctx, items, i, member, dest) {
  if (clipping(ctx) && !file.exists(items$dest[i])) { gdal_remote(); x <- sf::st_drop_geometry(sf::read_sf(remote_member(items$url[i], member)))
    foreign::write.dbf(as.data.frame(x), dest); dest } else { x <- unzip_to(items$dest[i], file.path(ctx$dl, "unzip"), member); file.rename(x, dest); dest } }

# a shapefile (the .shp and every sidecar of its name) from a downloaded zip or .tar.gz, unpacked flat into place; returns the .shp
shp_from_archive <- function(ctx, archive, member, place) {
  tar <- grepl("[.]tar[.]gz$|[.]tgz$", archive)
  m <- if (tar) utils::untar(archive, list = TRUE) else utils::unzip(archive, list = TRUE)$Name
  stem <- sub("[.]shp$", "", member, ignore.case = TRUE); want <- m[startsWith(m, paste0(stem, "."))]
  if (!any(tolower(want) == tolower(member))) stop(sprintf("%s lacks %s", basename(archive), member), call. = FALSE)
  ex <- file.path(ctx$dl, "unpack"); unlink(ex, recursive = TRUE); dir.create(ex, recursive = TRUE)
  if (tar) utils::untar(archive, files = want, exdir = ex) else utils::unzip(archive, files = want, exdir = ex)
  dir.create(place, recursive = TRUE, showWarnings = FALSE)
  for (f in want) if (!file.rename(file.path(ex, f), file.path(place, basename(f)))) stop(sprintf("could not move %s to its place", basename(f)), call. = FALSE)
  unlink(ex, recursive = TRUE); file.path(place, basename(member)) }

# vector layers of a downloaded zip (read in place through /vsizip/) copied into one GeoPackage, with only the named fields. The first
# layer of the source when `layers` is NULL, written as `names` (default: the source layer names)
layers_to_gpkg <- function(archive, inner, layers, fields, dest, names = layers) {
  src <- sprintf("/vsizip/%s/%s", normalizePath(archive, winslash = "/"), inner)
  if (is.null(layers)) { layers <- sf::st_layers(src)$name[1]; if (is.null(names)) names <- layers }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE); unlink(dest)
  for (i in seq_along(layers)) sf::gdal_utils("vectortranslate", src, dest, options = c(if (i > 1) "-update", "-nln", names[i], "-select", paste(fields[[i]], collapse = ","),
                                                                                         "-nlt", "CONVERT_TO_LINEAR", "-nlt", "PROMOTE_TO_MULTI", layers[i]), quiet = TRUE)
  dest }

BOX <- "https://usfs-public.box.com/shared/static/%s.zip"
RDS <- "https://www.fs.usda.gov/rds/archive/products/%s/%s"

download_handlers <- function() {
  H <- list()

  # ---- MTBS burn severity mosaics: one ScienceBase item a year (no byte ranges: each zip is fetched whole, 2-10 MB) -------
  H$MtbsSeverityFolder <- list(column = "MtbsSeverityFolder", name = "MTBS burn severity", version = "ScienceBase 5e91dee782ce172707f02cdd",
    plan = function(ctx) { j <- jsonlite::fromJSON("https://www.sciencebase.gov/catalog/items?parentId=5e91dee782ce172707f02cdd&format=json&max=200&fields=title,files", simplifyVector = FALSE)$items
      x <- data.table::rbindlist(lapply(j, function(it) { y <- as.integer(regmatches(it$title, regexpr("(?<=for )[0-9]{4}", it$title, perl = TRUE)))
        if (!length(y)) return(NULL); f <- Filter(function(f) identical(f$name, sprintf("mtbs_CONUS_%d.zip", y)), it$files); if (!length(f)) return(NULL)
        data.table::data.table(year = y, url = sprintf("https://www.sciencebase.gov/catalog/file/get/%s?name=%s", it$id, f[[1]]$name), file = f[[1]]$name) }))
      x[year %in% years_of(ctx, x$year)][order(year)][, fetch := TRUE][] },
    finish = function(ctx, items) { dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE)
      for (i in seq_len(nrow(items))) { t <- sprintf("mtbs_CONUS_%d.tif", items$year[i]); x <- unzip_to(items$dest[i], file.path(ctx$dl, "unzip"), t)
        clip_raster(x, file.path(ctx$place, t), if (clipping(ctx)) ctx$region else list(poly = NULL)); unlink(x) }
      ctx$place })

  # ---- FPA-FOD: the GeoPackage (fetched: a database is slow to read through a remote zip), its Fires layer clipped -------
  H$FpaFodFile <- list(column = "FpaFodFile", name = "FPA-FOD fire occurrence", national = TRUE, version = "RDS-2013-0009.7",
    plan = function(ctx) data.table::data.table(url = sprintf(RDS, "RDS-2013-0009.7", "RDS-2013-0009.7_Data_Format3_GPKG.zip"), file = "fpafod.zip", fetch = TRUE),
    finish = function(ctx, items) { x <- unzip_to(items$dest[1], file.path(ctx$dl, "unzip"), "Data/FPA_FOD_20260615.gpkg"); dest <- file.path(ctx$place, "FPA_FOD_20260615.gpkg")
      if (clipping(ctx)) clip_vector(x, dest, ctx$region, layer = "Fires") else { dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE); file.rename(x, dest) }
      dest })

  # ---- PRISM: annual tmean and ppt (4 km, "us_25m") and the 1991-2020 normals, national (small) ------------------------
  # from PRISM's HTTP mirror (sizes and byte ranges; the web service blocks a file fetched twice in 24 h). The ignition fit reads the
  # ppt normal; the annual files are lucasCore's, kept so a shared home holds what both packages expect
  H$PrismFolder <- list(column = "PrismFolder", name = "PRISM annual climate and normals", national = TRUE, version = "PRISM AN (us_25m, 4 km)",
    plan = function(ctx) { last <- as.integer(format(Sys.Date(), "%Y")) - 1L; y <- sort(unique(c(years_of(ctx, 1895:last), 1991:2020, min(years_of(ctx, 1895:last)) - 1L)))
      a <- data.table::CJ(var = c("tmean", "ppt"), year = y)[, .(var, year, url = sprintf("https://data.prism.oregonstate.edu/time_series/us/an/4km/%s/monthly/%d/prism_%s_us_25m_%d.zip", var, year, var, year),
                                                                 file = sprintf("prism_%s_us_25m_%d.zip", var, year))]
      n <- data.table::data.table(var = c("tmean", "ppt"), year = NA_integer_, url = sprintf("https://data.prism.oregonstate.edu/normals/us/4km/%s/monthly/prism_%s_us_25m_2020_avg_30y.zip", c("tmean", "ppt"), c("tmean", "ppt")),
                                  file = sprintf("prism_%s_us_25m_2020_avg_30y.zip", c("tmean", "ppt")))
      rbind(a, n)[, fetch := TRUE][] },
    finish = function(ctx, items) {
      for (i in seq_len(nrow(items))) utils::unzip(items$dest[i], exdir = if (is.na(items$year[i])) file.path(ctx$place, "normals") else file.path(ctx$place, "annual-us25m", items$var[i]))
      ctx$place })

  # ---- SILVIS WUI: one geodatabase per state the region touches ---------------------------------------------------------
  H$WuiFolder <- list(column = "WuiFolder", name = "SILVIS WUI v4 (by state)", version = "v4, 1990-2020",
    plan = function(ctx) { st <- region_states(ctx)
      data.table::data.table(url = sprintf("https://geoserver.silvis.forest.wisc.edu/geodata/wui_change_2020_v4/zip/fgdb/%s_wui_block_1990_2020_change_v4_fgdb.zip", st),
                             file = sprintf("%s_wui_block_1990_2020_change_v4_fgdb.zip", st), fetch = TRUE) },
    finish = function(ctx, items) { for (i in seq_len(nrow(items))) utils::unzip(items$dest[i], exdir = ctx$place); ctx$place })

  # ---- the zone polygons: national, at the top of the home; the stage runs these before it works out the region --------------
  strata_shp <- function(column, name, version, url, file, member) list(column = column, name = name, national = TRUE, version = version,
    plan = function(ctx) data.table::data.table(url = url, file = file, fetch = TRUE),
    finish = function(ctx, items) shp_from_archive(ctx, items$dest[1], member, ctx$place))
  H$StatesFile <- strata_shp("StatesFile", "TIGER/Line states 2009", "TIGER/Line 2009 (tl_2009_us_state)",
                             "https://www2.census.gov/geo/tiger/TIGER2009/tl_2009_us_state.zip", "tl_2009_us_state.zip", "tl_2009_us_state.shp")
  H$PyromesFile <- strata_shp("PyromesFile", "Pyromes", "RDS-2020-0020", sprintf(RDS, "RDS-2020-0020", "RDS-2020-0020.zip"), "pyromes.zip",
                              "Data/Pyromes_CONUS_20200206.shp")

  # ==== clipped by the source for a region; for all of CONUS (Home layout National) the sources' national files =================

  # ---- Annual NLCD land cover (Collection 1.2) by WCS 1.0.0, one coverage a year, in tiles on the 30 m grid -----------------
  H$NlcdFolder <- list(column = "NlcdFolder", name = "Annual NLCD land cover", version = "Collection 1.2 (C1V2)", method = "wcs",
    note = "Clipped by MRLC's WCS; C1V2 for every year (the only collection published)",
    plan = function(ctx) {
      have <- as.integer(substr(regmatches(x <- rawToChar(curl::curl_fetch_memory(paste0(NLCD_WCS, "?service=WCS&version=1.0.0&request=DescribeCoverage&coverage=", NLCD_COV))$content),
        gregexpr("(?<=<gml:timePosition>)[0-9]{4}", x, perl = TRUE))[[1]], 1, 4))
      y <- years_of(ctx, sort(unique(have))); out <- sprintf("Annual_NLCD_LndCov_%d_CU_C1V2.tif", y)
      # all of CONUS: MRLC's national zip a year (1.4 GB each), on the national grid the 150 m blocks are anchored to
      if (whole_conus(ctx)) return(data.table::data.table(year = y, url = sprintf(NLCD_BUNDLE, y), file = sub("[.]tif$", ".zip", out), out = out, fetch = TRUE))
      data.table::data.table(year = y, url = NLCD_WCS, file = out, out = out, fetch = FALSE) },
    have = function(ctx, items) recorded(ctx, "NlcdFolder", file.path(ctx$place, items$out)),
    finish = function(ctx, items) {
      if (whole_conus(ctx)) { for (i in seq_len(nrow(items))) { d <- zip_tif(ctx, items$dest[i], file.path(ctx$place, items$out[i]))
          e <- as.vector(terra::ext(terra::rast(d)))
          if (any(abs(e - NLCD_NATIONAL_EXT) > 1)) stop(sprintf("NLCD %d: the national file's extent (%s) is not the national NLCD grid", items$year[i], paste(round(e), collapse = ", ")), call. = FALSE)
          unlink(items$dest[i]); ctx$log(sprintf("  NLCD %d (national)", items$year[i])) }
        return(ctx$place) }
      tl <- grid_tiles(ctx$region$box, c(-2415585, 3314805), 30, 4000)
      # under load the WCS now and then answers a full-size tile with no data (P26 2026-11-01: one tile each in 2001, 2003 and 2009).
      # NLCD's no-data cells are the same every year, so each tile's count of class cells must equal the most seen in any year;
      # a tile short of it is fetched again
      classed <- function(p) { v <- terra::values(terra::rast(p), mat = FALSE); sum(!is.na(v) & v >= 11 & v <= 95) }
      ref <- rep(0, nrow(tl)); got <- matrix(NA_real_, nrow(items), nrow(tl)); zero_ok <- rep(FALSE, nrow(tl))   # zero_ok: blank after 4 tries (outside CONUS)
      tile_urls <- function(y) sprintf("%s?service=WCS&version=1.0.0&request=GetCoverage&coverage=%s&crs=EPSG:5070&bbox=%.0f,%.0f,%.0f,%.0f&width=%d&height=%d&time=%d-01-01T00:00:00.000Z&format=GeoTIFF",
                                       NLCD_WCS, NLCD_COV, tl$xmin, tl$ymin, tl$xmax, tl$ymax, tl$nx, tl$ny, y)
      # one year asked (this package: 2010) has no other year to compare with, and a partly blank tile passed unseen (P26 2026-11-04: 14 million cells over two
      # tiles, 10.6% of the region, recorded done). So the neighbouring year's tiles are fetched for their counts only
      if (nrow(items) == 1) { ry <- if (items$year[1] > 1985L) items$year[1] - 1L else items$year[1] + 1L; rp <- file.path(ctx$dl, paste0("ref", ry), sprintf("tile%02d.tif", seq_len(nrow(tl))))
        fetch_files(tile_urls(ry), rp, 1, ctx$log, sizes = rep(NA_real_, nrow(tl))); ref <- unname(vapply(rp, classed, 0)); unlink(dirname(rp[1]), recursive = TRUE)
        ctx$log(sprintf("  NLCD: class cells per tile counted from %d, to check %d", ry, items$year[1])) }
      one_year <- function(i) { y <- items$year[i]
        u <- tile_urls(y)
        parts <- file.path(ctx$dl, y, sprintf("tile%02d.tif", seq_len(nrow(tl))))
        fetch_files(u, parts, 1, ctx$log, sizes = rep(NA_real_, length(u)))   # one tile at a time: the WCS answers 404 under parallel load (2026-11-01)
        n <- vapply(parts, classed, 0)
        for (try in 1:4) { bad <- which(n < ref | (n == 0 & !zero_ok)); if (!length(bad)) break
          ctx$log(sprintf("  NLCD %d: tile %s came back with %s class cells (expected %s); fetching again", y, paste(bad, collapse = ", "),
                          paste(n[bad], collapse = ", "), paste(ref[bad], collapse = ", ")))
          Sys.sleep(10 * try); file.remove(parts[bad]); fetch_files(u[bad], parts[bad], 1, ctx$log, sizes = rep(NA_real_, length(bad)))
          n[bad] <- vapply(parts[bad], classed, 0) }
        if (any(n < ref)) stop(sprintf("NLCD %d: tile %s still short of class cells after 4 tries", y, paste(which(n < ref), collapse = ", ")), call. = FALSE)
        ref <<- pmax(ref, n); got[i, ] <<- n; zero_ok <<- zero_ok | n == 0
        mosaic_to(parts, file.path(ctx$place, items$out[i]), "INT1U", c(min(tl$xmin), min(tl$ymin), max(tl$xmax), max(tl$ymax)), tiles = tl); unlink(dirname(parts[1]), recursive = TRUE)
        ctx$log(sprintf("  NLCD %d (%d tiles)", y, nrow(tl))) }
      for (i in seq_len(nrow(items))) one_year(i)
      # a year fetched before a later year raised a tile's count is fetched again
      for (i in which(apply(got, 1, function(n) any(n < ref)))) { ctx$log(sprintf("  NLCD %d: a tile has fewer class cells than in later years; fetching it again", items$year[i])); one_year(i) }
      ctx$place })

  # ---- LANDFIRE layers by LANDFIRE Product Service jobs (the job clips; output on LANDFIRE's own grid) -----------------
  # (all of CONUS: LANDFIRE's full-extent zip of the layer, `zip` under landfire.gov/data-downloads)
  lf_single <- function(column, name, layer, version, out, zip) list(column = column, name = name, version = version, method = "lfps",
    plan = function(ctx) { if (whole_conus(ctx)) return(data.table::data.table(url = sprintf("%s/%s", LF_DL, zip), file = basename(zip), fetch = TRUE))
      lfps_email(ctx); data.table::data.table(url = LFPS, file = out, layer = layer, fetch = FALSE) },
    finish = function(ctx, items) { dest <- file.path(ctx$place, out)
      if (whole_conus(ctx)) { zip_tif(ctx, items$dest[1], dest); unlink(items$dest[1]); return(dest) }
      b <- lfps_job(ctx, items$layer); write_band(b, 1, dest); dest })
  H$ElevationFile <- lf_single("ElevationFile", "LANDFIRE elevation", "LF2020_Elev", "LF 2020 (LF2020_Elev)", "LC20_Elev_220.tif", "CONUS_Topo_LF2020/LF2020_Elev_CONUS.zip")
  H$RoadsFile <- lf_single("RoadsFile", "LANDFIRE roads", "LF2023_Roads", "LF 2023 (LF2023_Roads)", "LF2023_Roads_CONUS.tif", "CONUS_LF2023/LF2023_Roads_CONUS.zip")

  # ---- NIFC Interagency Fire Perimeter History by ArcGIS REST, paged (2,000 features a page), to a GeoPackage -----------
  # from the six decade services: the All Years View holds only 342 perimeters from 2020 on (2026-11-01; the 2020s service has
  # 18,484), so it no longer matches the shapefile the build machine downloaded
  H$FirePerimeterHistoryFile <- list(column = "FirePerimeterHistoryFile", name = "Interagency fire perimeter history", version = "InterAgencyFirePerimeterHistory decade services (1979 and prior to 2020s)", method = "rest",
    plan = function(ctx) data.table::data.table(url = NIFC[1], file = "InterAgencyFirePerimeterHistory.gpkg", fetch = FALSE),
    finish = function(ctx, items) {
      geo <- if (is.null(ctx$region$poly)) "" else { b <- region_box_ll(ctx$region)
        sprintf("&geometry=%f,%f,%f,%f&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects", b[1], b[2], b[3], b[4]) }
      parts <- lapply(NIFC, function(s) {
        q <- function(extra) curl::curl_fetch_memory(sprintf("%s/query?where=1%%3D1%s%s", s, geo, extra))
        n <- jsonlite::fromJSON(rawToChar(q("&returnCountOnly=true&f=json")$content))$count
        if (!n) return(NULL)
        pages <- lapply(seq(0, n - 1, by = 2000), function(o) { for (k in 1:4) { r <- tryCatch(q(sprintf("&outFields=*&orderByFields=OBJECTID&resultOffset=%d&resultRecordCount=2000&outSR=4326&f=geojson", o)), error = function(e) NULL)
            if (!is.null(r) && r$status_code == 200) return(sf::read_sf(rawToChar(r$content), quiet = TRUE)); Sys.sleep(5 * k) }
          stop(sprintf("fire perimeters: %s page at %d failed", basename(dirname(dirname(s))), o), call. = FALSE) })
        x <- do.call(rbind, pages)
        if (nrow(x) != n) stop(sprintf("fire perimeters: %d of %d features came from %s", nrow(x), n, basename(dirname(dirname(s)))), call. = FALSE)
        ctx$log(sprintf("  %s: %d perimeters", sub("_Read_Only$", "", basename(dirname(dirname(s)))), n)); x })
      parts <- Filter(Negate(is.null), parts); keep <- Reduce(intersect, lapply(parts, names))
      x <- sf::st_transform(do.call(rbind, lapply(parts, function(p) p[, keep])), 5070)
      dest <- file.path(ctx$place, items$file[1]); dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE); if (file.exists(dest)) file.remove(dest)
      sf::write_sf(x, dest, layer = "perimeters"); dest })

  # the ignition covariates read one year of NLCD, 2010: the middle of the 1992-2020 fire record
  nlcd_plan <- H$NlcdFolder$plan; H$NlcdFolder$plan <- function(ctx) { ctx$years <- c(2010L, 2010L); nlcd_plan(ctx) }

  # ==== the datasets only the fire model reads ================================================================================

  # ---- LANDFIRE fuel and canopy layers of the vintages asked (Data Download: LANDFIRE Vintages, blank = 2020; Canopy Height Vintage), laid
  # out as landfire_file reads them: LF<v>_<prod>_CONUS/LF<v>_<prod>_CONUS/Tif/LF<v>_<prod>_CONUS.tif. LF2020 has left the Product Service
  # and landfire.gov (2026-11-02): its five layers are archived on ScienceBase, where a large file is handed out only past a captcha, so they cannot
  # be fetched by a script. They are a one-time step for the user: the five zips, as downloaded, in <home>/supplied (lucasCore's folder for files
  # only a person can get); the stage clips from there and leaves them for the next region. Other vintages: a Product Service job for a region, LANDFIRE's national zips for all of CONUS -------------------------------------------
  LF2020_SB <- c(FBFM40 = "6a1602c7b66b013d45990b82", CBD = "6a15f874b66b01e66c9c2e6e", CBH = "6a15f8e4b66b01e66c9c2ef7", CC = "6a15f914b66b01e66c9c2f39", CH = "6a16003fb66b013d45990b4b")
  H$LandfireFolder <- list(column = "LandfireFolder", name = "LANDFIRE fuels and canopy", version = "by vintage (LF2020 ScienceBase archive of 2026-05-22; others LANDFIRE)",
    note = "LF2020 from its ScienceBase archive (10.5066/P1LJEZ3G); other vintages by the LANDFIRE Product Service or its national zips",
    plan = function(ctx) { op <- function(k) { v <- if (nrow(ctx$opts) && k %in% names(ctx$opts)) ctx$opts[[k]][1] else NA; if (is.na(v) || !nzchar(as.character(v))) NA_character_ else as.character(v) }
      vs <- op("LandfireVintages"); vs <- if (is.na(vs)) "2020" else trimws(strsplit(vs, ",")[[1]]); ch <- op("CanopyHeightVintage")
      x <- data.table::rbindlist(lapply(vs, function(v) data.table::data.table(v = v, prod = c("FBFM40", "CBD", "CBH", "CC", if (is.na(ch)) "CH"))))
      if (!is.na(ch)) x <- rbind(x, data.table::data.table(v = ch, prod = "CH"))
      x[, out := sprintf("LF%s_%s_CONUS/LF%s_%s_CONUS/Tif/LF%s_%s_CONUS.tif", v, prod, v, prod, v, prod)]
      avail <- if (any(x$v != "2020") && !whole_conus(ctx)) jsonlite::fromJSON(paste0(LFPS, "/products"))$products$layerName else character()
      miss <- x[v != "2020" & !whole_conus(ctx) & !sprintf("LF%s_%s", v, prod) %in% avail]
      if (nrow(miss)) stop(sprintf("the LANDFIRE Product Service has no %s (LF2020 comes from its archive; it serves %s)", paste(sprintf("LF%s_%s", miss$v, miss$prod), collapse = ", "),
                                   paste(sort(unique(sub("_FBFM40$", "", grep("^LF[0-9]{4}_FBFM40$", avail, value = TRUE)))), collapse = ", ")), call. = FALSE)
      if (any(x$v != "2020") && !whole_conus(ctx)) lfps_email(ctx)
      x[, layer := sprintf("LF%s_%s", v, prod)]
      x[, archive := v == "2020"]; x[, zip := archive | whole_conus(ctx)]
      a0 <- file.path(ctx$home, "supplied"); x[, supplied := ifelse(archive, file.path(a0, sprintf("LF2020_%s_CONUS_20260522.zip", prod)), NA_character_)]
      gone <- x[archive == TRUE & !file.exists(supplied)]
      if (nrow(gone)) { dir.create(a0, recursive = TRUE, showWarnings = FALSE)
        stop(sprintf("needs a manual download: LANDFIRE keeps LF2020 only on ScienceBase, behind a captcha. Download %s (%s) into %s, then run this again; or choose another LANDFIRE vintage", paste(sprintf("LF2020_%s_CONUS_20260522.zip", gone$prod), collapse = ", "),
                     paste(sprintf("https://www.sciencebase.gov/catalog/item/%s", LF2020_SB[gone$prod]), collapse = " "), a0), call. = FALSE) }
      x[, url := ifelse(archive, sprintf("https://www.sciencebase.gov/catalog/item/%s", LF2020_SB[prod]),
                        if (whole_conus(ctx)) sprintf("%s/CONUS_LF%s/LF%s_%s_CONUS.zip", LF_DL, v, v, prod) else LFPS)]
      x[, file := ifelse(zip, sprintf("LF%s_%s_CONUS.zip", v, prod), out)]; x[, fetch := zip & !archive]; x[] },
    have = function(ctx, items) recorded(ctx, "LandfireFolder", file.path(ctx$place, items$out)),
    finish = function(ctx, items) {
      for (i in which(items$zip)) { dest <- file.path(ctx$place, items$out[i]); z <- if (items$archive[i]) items$supplied[i] else items$dest[i]
        if (clipping(ctx)) { t <- zip_tif(ctx, z, file.path(ctx$dl, "clip", basename(items$out[i]))); clip_raster(t, dest, ctx$region); unlink(t) } else zip_tif(ctx, z, dest)
        if (!items$archive[i]) unlink(z); ctx$log(sprintf("  %s", items$layer[i])) }
      job <- which(!items$zip)
      for (ch in split(job, ceiling(seq_along(job) / 15))) { b <- lfps_job(ctx, items$layer[ch])
        for (k in seq_along(ch)) write_band(b, k, file.path(ctx$place, items$out[ch[k]]))
        rm(b); gc(); unlink(file.path(ctx$dl, c("lfps.zip", "lfps")), recursive = TRUE) }
      ctx$place })

  # ---- LANDFIRE Biophysical Settings 2020 and its attribute table (P1 reads GROUPVEG from the CSV_Data folder beside Tif; the Product Service
  # gives the raster alone, so the table ships in defaults/landfire) ------------------------------------------------------------------------
  H$BpsFile <- list(column = "BpsFile", name = "LANDFIRE Biophysical Settings", version = "LF 2020 (LF2020_BPS)", method = "lfps",
    plan = function(ctx) { if (whole_conus(ctx)) return(data.table::data.table(url = sprintf("%s/CONUS_LF2020/LF2020_BPS_CONUS.zip", LF_DL), file = "LF2020_BPS_CONUS.zip", fetch = TRUE))
      lfps_email(ctx); data.table::data.table(url = LFPS, file = "LF2020_BPS_CONUS.tif", layer = "LF2020_BPS", fetch = FALSE) },
    finish = function(ctx, items) { d <- file.path(ctx$place, "LF2020_BPS_CONUS"); dest <- file.path(d, "Tif", "LF2020_BPS_CONUS.tif")
      if (whole_conus(ctx)) { zip_tif(ctx, items$dest[1], dest); unlink(items$dest[1]) } else { b <- lfps_job(ctx, "LF2020_BPS"); write_band(b, 1, dest) }
      dir.create(file.path(d, "CSV_Data"), recursive = TRUE, showWarnings = FALSE); file.copy(pkg_file("defaults", "landfire", "LF2020_BPS.csv"), file.path(d, "CSV_Data", "LF2020_BPS.csv"), overwrite = TRUE)
      ctx$place })

  # ---- gridMET: one national file a variable-year, 1979 (or Data Download: first year) to the last full year: P3 uses every year it finds, so a
  # year still being written would enter the weather stream part-filled. National: a zone's weather zones reach well past its region --------
  H$GridMetFolder <- list(column = "GridMetFolder", name = "gridMET daily weather", national = TRUE, version = "gridMET (Abatzoglou 2013)",
    note = "tmmx, rmin, vs, th, pr; the last year still being written is left out",
    plan = function(ctx) { last <- as.integer(format(Sys.Date(), "%Y")) - 1L; y <- years_of(ctx, 1979:last)
      data.table::CJ(var = c("tmmx", "rmin", "vs", "th", "pr"), year = y)[, `:=`(url = sprintf("https://www.northwestknowledge.net/metdata/data/%s_%d.nc", var, year), file = sprintf("%s/%s_%d.nc", var, var, year), fetch = TRUE)][] },
    have = function(ctx, items) recorded(ctx, "GridMetFolder", file.path(ctx$place, items$file)),
    finish = function(ctx, items) { for (i in seq_len(nrow(items))) { dest <- file.path(ctx$place, items$file[i]); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
        if (!file.rename(items$dest[i], dest)) stop(sprintf("could not move %s to its place", items$file[i]), call. = FALSE) }
      ctx$place })

  # ---- WRC burn probability (the benchmark; 2nd edition, 30 m). WRC publishes one zip a state (8-11 GB, every WRC layer) and one 32 GB CONUS zip per layer. A
  # region: the state zips it touches, fetched whole and BP clipped out of each, as a remote read broke deep in California's (2026-11-02, 15 min in) and Box
  # gave the whole 11 GB zip in 5 min. At 270 m it correlates 0.998 with the WRC raster of the build machine. States overlap at their edges with no-data around
  # each: the first value wins ------------------------------------------------------------------------------------------------------------------------------
  H$WrcBurnProbabilityFile <- list(column = "WrcBurnProbabilityFile", name = "WRC burn probability", version = "2nd edition (RDS-2020-0016-2)",
    plan = function(ctx) { p <- rawToChar(curl::curl_fetch_memory(WRC_PAGE)$content)
      if (whole_conus(ctx)) return(data.table::data.table(url = wrc_zip(p, "_BP_CONUS"), file = "BP_CONUS.zip", st = "CONUS", member = "BP_CONUS/BP_CONUS.tif", fetch = TRUE))
      st <- region_states(ctx); nm <- gsub(" ", "", c(state.name, "District of Columbia"))[match(st, c(state.abb, "DC"))]
      data.table::data.table(url = vapply(nm, function(n) wrc_zip(p, n), ""), file = paste0("BP_", st, ".zip"), st = st, member = sprintf("%s/BP_%s.tif", st, st), fetch = TRUE) },
    finish = function(ctx, items) { dest <- file.path(ctx$place, "wrc_bp.tif"); dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE); ex <- file.path(ctx$dl, "unzip")
      take <- function(i) { x <- file.path(ex, items$member[i])   # (R's unzip may refuse a large member: then Windows' tar, as zip_tif)
        ok <- tryCatch({ utils::unzip(items$dest[i], files = items$member[i], exdir = ex); file.exists(x) && file.size(x) > 0 }, error = function(e) FALSE, warning = function(w) FALSE)
        if (!ok) { unlink(x); dir.create(ex, recursive = TRUE, showWarnings = FALSE); system2("tar", c("-xf", shQuote(items$dest[i]), "-C", shQuote(ex), shQuote(items$member[i])))
          if (!file.exists(x)) stop(sprintf("could not unpack %s from %s", items$member[i], items$file[i]), call. = FALSE) }
        x }
      if (whole_conus(ctx)) { x <- take(1); if (!file.rename(x, dest)) stop("could not move BP_CONUS.tif to its place", call. = FALSE); unlink(items$dest[1]); return(dest) }
      parts <- file.path(ctx$dl, paste0("BP_", items$st, ".tif"))
      for (i in seq_len(nrow(items))) { x <- take(i); clip_raster(x, parts[i], ctx$region); unlink(x); unlink(items$dest[i]); ctx$log(sprintf("  WRC burn probability: %s", items$st[i])) }
      if (length(parts) == 1) file.rename(parts, dest) else terra::merge(terra::sprc(parts), filename = dest, overwrite = TRUE, datatype = terra::datatype(terra::rast(parts[1])), gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER"))
      unlink(parts); dest })

  # ---- MTBS burned-area boundaries (national; P3 and C7 read their surroundings from the whole file) --------------------------------------
  H$MtbsPerimeterFile <- list(column = "MtbsPerimeterFile", name = "MTBS perimeters", national = TRUE, version = "mtbs_perimeter_data (replaced in place)",
    note = "MTBS replaces the file in place; the home keeps the snapshot it fetched",
    plan = function(ctx) data.table::data.table(url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip", file = "mtbs_perimeter_data.zip", fetch = TRUE),
    finish = function(ctx, items) { m <- utils::unzip(items$dest[1], list = TRUE)$Name; s <- grep("mtbs_perims_DD[.]shp$", m, value = TRUE)[1]
      if (is.na(s)) stop("mtbs_perimeter_data.zip holds no mtbs_perims_DD.shp", call. = FALSE)
      shp_from_archive(ctx, items$dest[1], s, ctx$place) })

  # ---- NWS fire weather zones (national): the latest file the NWS page lists, as it keeps only its two latest; the Weather Zones list came from
  # fz03mr26, whose 3603 zone names fz16ap26 has unchanged (2026-11-02). P1 stops naming any zone a later file adds -----------------------------
  H$WeatherZoneBoundaryFile <- list(column = "WeatherZoneBoundaryFile", name = "NWS fire weather zones", national = TRUE, version = "the latest NWS lists (see Url)",
    plan = function(ctx) { p <- rawToChar(curl::curl_fetch_memory("https://www.weather.gov/gis/FireZones")$content)
      z <- unique(regmatches(p, gregexpr("/source/gis/Shapefiles/WSOM/fz[0-9]{2}[a-z]{2}[0-9]{2}[.]zip", p))[[1]])
      if (!length(z)) stop("the NWS page lists no fire weather zone file (https://www.weather.gov/gis/FireZones)", call. = FALSE)
      mon <- c("ja", "fe", "mr", "ap", "my", "jn", "jl", "au", "se", "oc", "no", "de"); b <- basename(z)
      u <- z[order(as.integer(substr(b, 7, 8)), match(substr(b, 5, 6), mon), as.integer(substr(b, 3, 4)), decreasing = TRUE)][1]
      data.table::data.table(url = paste0("https://www.weather.gov", u), file = basename(u), fetch = TRUE) },
    finish = function(ctx, items) shp_from_archive(ctx, items$dest[1], sub("[.]zip$", ".shp", items$file[1]), ctx$place))

  # ---- RAWS: the FEMS station catalogue, and the hourly station-years of the stations inside the region (P5's file names), 2006-2024. National:
  # one folder for every zone, growing as regions ask for more stations. All of CONUS: the catalogue alone (2,088 stations x 19 years), and P5
  # fetches each zone's stations when it runs. A station-year FEMS will not give is logged and left: P5 tries it again --------------------------
  H$RawsHourlyFolder <- list(column = "RawsHourlyFolder", name = "RAWS hourly records", national = TRUE, version = "FEMS climatology (replaced in place)", method = "rest",
    plan = function(ctx) { cf <- raws_catalogue(ctx); st <- data.table::as.data.table(jsonlite::fromJSON(cf)$data$stationMetaData$data)
      x <- data.table::data.table(url = RAWS_CATALOGUE, file = "fems_raws_catalogue.json", fetch = FALSE, id = NA_integer_, yr = NA_integer_)
      if (whole_conus(ctx)) return(x)
      p <- sf::st_transform(sf::st_as_sf(st[!is.na(longitude) & !is.na(latitude)], coords = c("longitude", "latitude"), crs = 4326), 5070)
      ids <- unique(as.integer(p$station_id[lengths(sf::st_intersects(p, ctx$region$poly)) > 0]))
      y <- 2006:2024; if (!is.na(ctx$years[2])) y <- y[y <= ctx$years[2]]
      j <- data.table::CJ(id = ids, yr = y)[, `:=`(url = sprintf("https://fems.fs2c.usda.gov/api/climatology/download-weather?stationIds=%d&startDate=%d-01-01T00:00:00Z&endDate=%d-12-31T23:00:00Z&dataset=observation&dataFormat=csv&dataIncrement=hourly", id, yr, yr), file = sprintf("%d_%d.csv", id, yr), fetch = FALSE)]
      ctx$log(sprintf("  RAWS: %d stations in the region of %d in the catalogue", length(ids), nrow(st)))
      rbind(x, j, use.names = TRUE) },
    # (on disk, recorded or not: P5 adds the station-years it lacks)
    have = function(ctx, items) file.exists(file.path(ctx$place, items$file)),
    finish = function(ctx, items) { dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE)
      if (any(is.na(items$id))) file.copy(file.path(ctx$dl, "fems_raws_catalogue.json"), file.path(ctx$place, "fems_raws_catalogue.json"), overwrite = TRUE)
      j <- items[!is.na(id)]; if (!nrow(j)) return(ctx$place)
      dest <- file.path(ctx$place, j$file); left <- seq_len(nrow(j)); ctx$log(sprintf("  RAWS: %d station-years from FEMS", nrow(j)))
      for (try in 1:3) { if (!length(left)) break; if (try > 1) Sys.sleep(20 * (try - 1))
        chs <- split(left, ceiling(seq_along(left) / max(1, ctx$connections)))
        for (n in seq_along(chs)) { ch <- chs[[n]]; r <- curl::multi_download(j$url[ch], dest[ch], progress = FALSE, multiplex = FALSE, connecttimeout = 60, low_speed_time = 300, low_speed_limit = 10, failonerror = TRUE)
          unlink(dest[ch][!r$success %in% TRUE]); if (n %% 50 == 0) ctx$log(sprintf("  RAWS: %d of %d done", sum(file.exists(dest)), nrow(j))) }
        left <- which(!file.exists(dest)) }
      if (length(left)) { ctx$log(sprintf("  RAWS: FEMS gave no answer for %d station-years (P5 tries them again): %s", length(left), paste(head(j$file[left], 8), collapse = ", ")))
        if (length(left) > 0.2 * nrow(j)) stop(sprintf("FEMS gave no answer for %d of %d station-years", length(left), nrow(j)), call. = FALSE) }
      ctx$place })
  H }

# ---- service helpers (phase 4) ----------------------------------------------------------------------------------------------
NLCD_WCS <- "https://dmsdata.cr.usgs.gov/geoserver/mrlc_Land-Cover-Native_conus_year_data/wcs"
NLCD_COV <- "mrlc_Land-Cover-Native_conus_year_data:Land-Cover-Native_conus_year_data"
LFPS <- "https://lfps.usgs.gov/api"
NIFC <- sprintf("https://services3.arcgis.com/T4QMspbfLg3qTGWY/arcgis/rest/services/%s/FeatureServer/0", c("InterAgencyFirePerimeterHistory_1979_And_Prior_Read_Only",
  "InteragencyFirePerimeterHistory_1980s_Read_Only", "InteragencyFirePerimeterHistory_1990s_Read_Only", "InteragencyFirePerimeterHistor_2000s_Read_Only",
  "InteragencyFirePerimeterHistory_2010s_Read_Only", "InterAgencyFirePerimeterHistory_2020s_Read_Only"))
CCAP <- "https://ocmgeodatastor1.blob.core.windows.net/ccap/bulk_download/C-CAP_Regional_30-meter_Data/C-CAP_Regional_Land_Cover_Classification/CONUS/conus_2001_ccap_landcover.tif"
PC_STAC <- "https://planetarycomputer.microsoft.com/api/stac/v1"
# national files (Home layout National; checked 2026-11-02)
NLCD_BUNDLE <- "https://www.mrlc.gov/downloads/sciweb1/shared/mrlc/data-bundles/Annual_NLCD_LndCov_%d_CU_C1V2.zip"
NLCD_NATIONAL_EXT <- c(-2415585, 2384415, 164805, 3314805)   # xmin, xmax, ymin, ymax: the national grid (shared.R, grid_blocks)
LF_DL <- "https://landfire.gov/data-downloads"
WRC_PAGE <- "https://www.fs.usda.gov/rds/archive/catalog/RDS-2020-0016-2"
# a zip's address on the archive's page (it names each zip beside its Box link)
wrc_zip <- function(page, name) { m <- regmatches(page, regexpr(sprintf('href="[^"]+"[^>]*>RDS-2020-0016-2_%s[.]zip<', name), page))
  if (!length(m)) stop(sprintf("the WRC archive page lists no RDS-2020-0016-2_%s.zip", name), call. = FALSE); sub('^href="([^"]+)".*$', "\\1", m) }

# the planned files already recorded in files.csv at their recorded size (a hand-placed, unrecorded file is fetched again)
recorded <- function(ctx, column, paths) { st <- if (is.null(ctx$store)) ctx$home else ctx$store; f <- read_files(st)[Column == column]
  rel <- substring(normalizePath(paths, winslash = "/", mustWork = FALSE), nchar(normalizePath(st, winslash = "/")) + 2)
  b <- f$Bytes[match(rel, f$Path)]; !is.na(b) & file.exists(paths) & file.size(paths) == as.numeric(b) }

# the region's box snapped outward to a grid (origin = the grid's top-left corner), cut into tiles of at most `px` cells a side
grid_tiles <- function(box, origin, cell, px) {
  x0 <- origin[1] + floor((box[1] - origin[1]) / cell) * cell; x1 <- origin[1] + ceiling((box[3] - origin[1]) / cell) * cell
  y1 <- origin[2] - floor((origin[2] - box[4]) / cell) * cell; y0 <- origin[2] - ceiling((origin[2] - box[2]) / cell) * cell
  xs <- seq(x0, x1, by = px * cell); if (tail(xs, 1) < x1) xs <- c(xs, x1); ys <- seq(y1, y0, by = -px * cell); if (tail(ys, 1) > y0) ys <- c(ys, y0)
  t <- data.table::CJ(i = seq_len(length(xs) - 1), j = seq_len(length(ys) - 1))
  t[, `:=`(xmin = xs[i], xmax = xs[i + 1], ymax = ys[j], ymin = ys[j + 1])][, `:=`(nx = as.integer(round((xmax - xmin) / cell)), ny = as.integer(round((ymax - ymin) / cell)))][] }

# tiles on one grid into one compressed GeoTIFF. tiles (grid_tiles rows, one per part): each part is given the corners it was asked
# with, after its size is checked, instead of the ones its server wrote: the NLCD WCS rounds them, and a thin last row of tiles (6 rows
# in P26) landed a row high, its first row over the row above (P26 2026-11-02: 508 cells of the region's row 12000 were national row 12001)
mosaic_to <- function(parts, dest, datatype, box = NULL, tiles = NULL) { dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  if (!is.null(tiles)) { stopifnot(nrow(tiles) == length(parts))
    parts <- vapply(seq_along(parts), function(i) { r <- terra::rast(parts[i])
      if (terra::nrow(r) != tiles$ny[i] || terra::ncol(r) != tiles$nx[i]) stop(sprintf("tile %d came back %d x %d cells, not the %d x %d asked for", i, terra::ncol(r), terra::nrow(r), tiles$nx[i], tiles$ny[i]), call. = FALSE)
      v1 <- tempfile(fileext = ".vrt"); sf::gdal_utils("translate", parts[i], v1, options = c("-of", "VRT", "-a_ullr", tiles$xmin[i], tiles$ymax[i], tiles$xmax[i], tiles$ymin[i])); v1 }, "") }
  v <- terra::vrt(parts, filename = tempfile(fileext = ".vrt"))
  # box (xmin, ymin, xmax, ymax): the grid the tiles were asked on. The NLCD WCS writes tile corners with too few digits, which
  # left the mosaic 0.2 m off the 30 m grid (res 30.000003) and moved 150 m blocks by a pixel (P26 2026-11-01: 6.6% of cells)
  if (!is.null(box)) { stopifnot(abs(terra::xmin(v) - box[1]) < 1, abs(terra::ymax(v) - box[4]) < 1); terra::set.ext(v, terra::ext(box[1], box[3], box[2], box[4])) }   # set.ext keeps the cell values (ext<- drops them)
  terra::writeRaster(v, dest, overwrite = TRUE, datatype = datatype, gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")); dest }

# a band of a multi-band job output written as its own file
write_band <- function(b, i, dest) { dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(b[[i]], dest, overwrite = TRUE, datatype = terra::datatype(b)[i], gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")); dest }

# the LFPS email: the Data Download page keeps it in the home folder's settings.csv
# the FEMS station list (public), fetched into the download folder
RAWS_CATALOGUE <- "https://fems.fs2c.usda.gov/api/climatology/graphql"
raws_catalogue <- function(ctx) { f <- file.path(ctx$dl, "fems_raws_catalogue.json"); dir.create(ctx$dl, recursive = TRUE, showWarnings = FALSE); if (file.exists(f)) return(f)
  h <- curl::new_handle(); curl::handle_setheaders(h, "Content-Type" = "application/json")
  curl::handle_setopt(h, postfields = '{"query":"{ stationMetaData { _metadata { total_count } data { station_id station_name latitude longitude elevation station_status } } }"}')
  curl::curl_download(RAWS_CATALOGUE, f, handle = h); f }

lfps_email <- function(ctx) { e <- home_setting(ctx$home, "LfpsEmail")
  if (is.na(e) || !grepl("@", e)) stop("LANDFIRE: enter an email address on the Data Download page (LANDFIRE Product Service jobs need one)", call. = FALSE)
  e }

# one LANDFIRE Product Service job: the layers over the region's lon/lat box in EPSG:5070 at 30 m, which is LANDFIRE's own CONUS grid
# (cells on the national files' 15 m offset, no resampling; without it LFPS reprojects to an Albers centred on the box);
# waits for it (polling every 15 s, up to 3 hours), fetches its zip and returns the multi-band raster, one band per layer in order
lfps_job <- function(ctx, layers) {
  b <- region_box_ll(ctx$region); j <- function(u) jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(u)$content), simplifyVector = TRUE)
  s <- j(sprintf("%s/job/submit?Email=%s&Layer_List=%s&Area_of_Interest=%s&Output_Projection=5070&Resample_Resolution=30", LFPS, curl::curl_escape(lfps_email(ctx)), curl::curl_escape(paste(layers, collapse = ";")),
                 curl::curl_escape(sprintf("%.5f %.5f %.5f %.5f", b[1], b[2], b[3], b[4]))))
  if (is.null(s$jobId)) stop(sprintf("LFPS refused the job: %s", paste(unlist(s$message), collapse = "; ")), call. = FALSE)
  ctx$log(sprintf("  LFPS job %s: %d layers", s$jobId, length(layers))); t0 <- Sys.time(); last <- ""
  repeat { Sys.sleep(15); st <- tryCatch(j(sprintf("%s/job/status?JobId=%s", LFPS, s$jobId)), error = function(e) list(status = "unreachable"))
    if (!identical(st$status, last)) { ctx$log(sprintf("  LFPS job: %s", st$status)); last <- st$status }
    if (grepl("Succeeded", st$status)) break
    if (grepl("Failed|Cancel", st$status)) stop(sprintf("LFPS job %s failed: %s", s$jobId, paste(grep("ERROR", unlist(st$messages), value = TRUE), collapse = "; ")), call. = FALSE)
    if (difftime(Sys.time(), t0, units = "hours") > 3) stop(sprintf("LFPS job %s still %s after 3 hours", s$jobId, st$status), call. = FALSE) }
  z <- file.path(ctx$dl, "lfps.zip"); fetch_files(st$outputFile, z, 1, ctx$log, sizes = NA_real_)
  x <- unzip_to(z, file.path(ctx$dl, "lfps")); r <- terra::rast(grep("[.]tif$", x, value = TRUE)[1])
  if (terra::nlyr(r) != length(layers)) stop(sprintf("LFPS job returned %d bands for %d layers", terra::nlyr(r), length(layers)), call. = FALSE)
  r }
