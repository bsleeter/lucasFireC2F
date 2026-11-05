# =============================================================================
# download.R - the Data Download engine (stage D; kept in step with lucasCore src/download.R): plan, check, fetch with resume, unpack, clip, record
# =============================================================================
# One dataset at a time, the same six steps (implementation plan, "How a download runs"):
#   1. up to date?  the manifest says done, its region holds this one (or the copy is national), and every file files.csv lists is
#                   on disk at its recorded size: skipped, nothing fetched
#   2. plan         the dataset's handler (src/download-sources.R) lists its files: url, the name to save it under, and how to
#                   finish (unzip, pick the files the stages read, clip)
#   3. check        HEAD each url for its size (Check only stops here and reports what would download)
#   4. fetch        curl::multi_download into <home>/.downloads/<column>/, resuming partial files (a stopped run picks up where it
#                   was); each file's size is checked against the server's
#   5. finish       the handler unpacks and clips into the dataset's place under the home folder (src/datahome.R); national
#                   copies are clipped to the region unless Keep national copies is ticked
#   6. record       files.csv gets each file's size and sha256, the manifest one row (status, region, source, version, DOI);
#                   the downloaded archives are deleted
# A dataset that fails is recorded failed with its error, and the others go on. No rsyncrosim here: the stage passes `log`.
# =============================================================================

# ---- the region ----------------------------------------------------------------------------------------------------------
# region: list(box = c(xmin, ymin, xmax, ymax) in EPSG:5070, poly = sf polygon (EPSG:5070) or NULL for CONUS, units, buffer_km)
region_box_ll <- function(region) {   # the box in lon/lat (EPSG:4326), for services and archives indexed that way
  b <- sf::st_bbox(sf::st_transform(sf::st_segmentize(sf::st_as_sfc(sf::st_bbox(setNames(region$box, c("xmin", "ymin", "xmax", "ymax")), crs = sf::st_crs(5070))), 5000), 4326))
  as.numeric(b) }

# ---- clipping ------------------------------------------------------------------------------------------------------------
# a raster cut to the region (its own grid and values, compressed); a categorical raster's .vat.dbf / .vat.cpg go with it
clip_raster <- function(src, dest, region) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(src)
  if (is.null(region$poly)) { if (normalizePath(src, mustWork = FALSE) != normalizePath(dest, mustWork = FALSE)) file.copy(src, dest, overwrite = TRUE) } else {
    b <- terra::project(terra::vect(sf::st_as_sfc(sf::st_bbox(setNames(region$box, c("xmin", "ymin", "xmax", "ymax")), crs = sf::st_crs(5070)))), terra::crs(r))
    e <- terra::intersect(terra::ext(b), terra::ext(r)); if (is.null(e)) stop(sprintf("%s does not reach the region", basename(src)), call. = FALSE)
    terra::crop(r, e, snap = "out", filename = dest, overwrite = TRUE, datatype = terra::datatype(r)[1], gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_SAFER")) }
  for (s in c(".vat.dbf", ".vat.cpg")) if (file.exists(paste0(src, s))) file.copy(paste0(src, s), paste0(dest, s), overwrite = TRUE)
  dest }

# a vector layer cut to the region (features touching the box), written as a GeoPackage
clip_vector <- function(src, dest, region, layer = NULL) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  args <- list(dsn = src, quiet = TRUE); if (!is.null(layer)) args$layer <- layer
  if (!is.null(region$poly)) { crs0 <- sf::st_crs(do.call(sf::read_sf, c(args[c("dsn", "quiet")], list(query = sprintf('SELECT * FROM "%s" LIMIT 0', if (is.null(layer)) sf::st_layers(src)$name[1] else layer)))))
    args$wkt_filter <- sf::st_as_text(sf::st_transform(sf::st_segmentize(sf::st_as_sfc(sf::st_bbox(setNames(region$box, c("xmin", "ymin", "xmax", "ymax")), crs = sf::st_crs(5070))), 5000), crs0)) }
  x <- do.call(sf::read_sf, args)
  if (file.exists(dest)) file.remove(dest)
  sf::write_sf(x, dest, layer = if (is.null(layer)) tools::file_path_sans_ext(basename(dest)) else layer)
  dest }

# unzip, all or the named members (paths inside the archive), into a folder; returns the extracted paths
unzip_to <- function(zip, exdir, files = NULL) {
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(files)) { have <- utils::unzip(zip, list = TRUE)$Name
    miss <- setdiff(files, have); if (length(miss)) stop(sprintf("%s lacks %s", basename(zip), paste(miss, collapse = ", ")), call. = FALSE) }
  utils::unzip(zip, files = files, exdir = exdir, junkpaths = FALSE)
  file.path(exdir, if (is.null(files)) utils::unzip(zip, list = TRUE)$Name else files) }

# ---- fetching ------------------------------------------------------------------------------------------------------------
# the size of each url: HEAD, else (servers that refuse HEAD, such as Box) the total of a one-byte ranged GET; NA when neither says
url_sizes <- function(urls) vapply(urls, function(u) {
  h <- curl::new_handle(nobody = TRUE, followlocation = TRUE, timeout = 60)
  r <- tryCatch(curl::curl_fetch_memory(u, handle = h), error = function(e) NULL)
  if (!is.null(r) && r$status_code < 400) { cl <- curl::parse_headers_list(r$headers)[["content-length"]]; if (!is.null(cl) && as.numeric(cl) > 0) return(as.numeric(cl)) }
  h <- curl::new_handle(followlocation = TRUE, timeout = 60, range = "0-0")
  r <- tryCatch(curl::curl_fetch_memory(u, handle = h), error = function(e) NULL)
  cr <- if (is.null(r) || r$status_code != 206) NULL else curl::parse_headers_list(r$headers)[["content-range"]]
  if (!is.null(cr) && grepl("/[0-9]+$", cr)) return(as.numeric(sub(".*/", "", cr)))
  # ScienceBase's large files say neither (the LF2020 zips, 2026-11-02): the item's own record gives each file's size
  if (!grepl("sciencebase[.]gov/catalog/file/get/", u)) return(NA_real_)
  tryCatch({ f <- jsonlite::fromJSON(sprintf("https://www.sciencebase.gov/catalog/item/%s?format=json&fields=files", sub("^.*/get/([^?]+).*$", "\\1", u)))$files
    s <- f$size[f$name == sub("^.*[?]name=", "", u)]; if (length(s) == 1) as.numeric(s) else NA_real_ }, error = function(e) NA_real_) }, 0, USE.NAMES = FALSE)

# fetch urls to files, `connections` at a time. A file already at the size the server reports is kept; a shorter one resumes
# (HTTP 206); a server that ignores the range (200) or a file that ends up the wrong size starts again. Stops naming what failed.
fetch_files <- function(urls, dest, connections = 4, log = message, sizes = url_sizes(urls)) {
  for (d in unique(dirname(dest))) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  good <- function(i) file.exists(dest[i]) && (if (is.na(sizes[i])) isTRUE(attr(dest, "done")[i]) else file.size(dest[i]) == sizes[i])
  attr(dest, "done") <- rep(FALSE, length(dest))
  for (try in 1:4) {
    todo <- which(!vapply(seq_along(urls), good, TRUE)); if (!length(todo)) return(invisible(TRUE))
    if (try > 1) { log(sprintf("  retrying %d of %d files (%d)", length(todo), length(urls), try - 1)); Sys.sleep(10 * (try - 1)) }   # a busy server gets a moment
    # a partial file bigger than the server's size is wrong: start it again
    for (i in todo) if (file.exists(dest[i]) && !is.na(sizes[i]) && file.size(dest[i]) > sizes[i]) file.remove(dest[i])
    # a file of unknown size cannot be resumed or checked (a WCS answer has no ranges): one left by an earlier run starts again
    for (i in todo) if (file.exists(dest[i]) && is.na(sizes[i])) file.remove(dest[i])
    for (ch in split(todo, ceiling(seq_along(todo) / max(1, connections)))) {
      r <- curl::multi_download(urls[ch], dest[ch], resume = TRUE, progress = FALSE, multiplex = FALSE,
                                connecttimeout = 60, low_speed_time = 300, low_speed_limit = 100, failonerror = TRUE)
      for (k in seq_along(ch)) { i <- ch[k]; st <- r$status_code[k]
        if (isTRUE(r$success[k]) && (st == 206 || (st == 200 && r$resumefrom[k] == 0))) attr(dest, "done")[i] <- TRUE
        else if (isTRUE(r$success[k]) && st == 200 && r$resumefrom[k] > 0) { file.remove(dest[i]); log(sprintf("  %s: the server ignored the resume; starting it again", basename(dest[i]))) }
        else if (!isTRUE(r$success[k]) && !is.na(r$error[k])) log(sprintf("  %s: %s", basename(dest[i]), r$error[k])) } } }
  bad <- which(!vapply(seq_along(urls), good, TRUE))
  stop(sprintf("could not fetch %s", paste(basename(dest[bad]), collapse = ", ")), call. = FALSE) }
# ---- one dataset ---------------------------------------------------------------------------------------------------------
# a handler (src/download-sources.R) is list(column, name, plan = function(ctx) -> data.table(url, file), finish =
# function(ctx, files) -> the place's path; optional version, note). ctx: home, region, years, place (the dataset's folder),
# dl (its download folder), keep (keep national copies), connections, opts, log
dataset_up_to_date <- function(home, column, region) {
  if (!isTRUE(manifest_covers(home, column, region$box))) return(FALSE)
  # a CONUS download needs a national copy
  if (is.null(region$poly) && !identical(read_manifest(home)[Column == column, Copy], "national")) return(FALSE)
  f <- read_files(home)[Column == column]
  nrow(f) > 0 && all(file.exists(file.path(home, f$Path))) && all(file.size(file.path(home, f$Path)) == as.numeric(f$Bytes)) }

# "in regions/P26", "at the top of the home": where a dataset is, for Download Results
held_in <- function(where) if (identical(where, "the top of the home")) paste("at", where) else paste("in", where)

# the folder a dataset is written to: the top of the home for national copies, else the run's region folder (regions/<name>;
# the top when the run is national or the home has no regions)
store_for <- function(h, ctx) if (isTRUE(h$national) || is.null(ctx$region_home)) ctx$home else ctx$region_home

# write-once (plan, "Second study areas"): a handler's finish writes into a staging folder, and its files then move to the
# place. A file already there is kept when identical; when it differs it is replaced only if no library can have read it (not in
# files.csv, and the dataset has no done manifest row), else the dataset stops and nothing moves. Returns `path` at the place
commit_staged <- function(stage, place, path, store, column, log = message) {
  f <- list.files(stage, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  rel <- substring(f, nchar(stage) + 2); dest <- file.path(place, rel)
  to_store <- function(p) substring(normalizePath(p, winslash = "/", mustWork = FALSE), nchar(normalizePath(store, winslash = "/")) + 2)
  done <- identical(read_manifest(store)[Column == column, Status], "done"); rec <- read_files(store)$Path
  sha <- function(x) digest::digest(file = x, algo = "sha256")
  have <- file.exists(dest); same <- rep(FALSE, length(f)); bad <- character()
  for (i in which(have)) { same[i] <- file.size(f[i]) == file.size(dest[i]) && sha(f[i]) == sha(dest[i])
    if (!same[i] && (done || to_store(dest[i]) %in% rec)) bad <- c(bad, to_store(dest[i])) }
  if (length(bad)) { unlink(stage, recursive = TRUE)
    stop(sprintf("write-once: %d files would replace different files a library may have read (%s%s); nothing was changed. Download this region under another region name",
                 length(bad), paste(head(bad, 3), collapse = ", "), if (length(bad) > 3) ", ..." else ""), call. = FALSE) }
  if (any(have & !same)) log(sprintf("  replacing %d unrecorded files left by an unfinished download", sum(have & !same)))
  file.remove(f[have & same])
  for (i in which(!(have & same))) { dir.create(dirname(dest[i]), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest[i])) file.remove(dest[i])
    if (!file.rename(f[i], dest[i]) && !(file.copy(f[i], dest[i]) && file.remove(f[i]))) stop(sprintf("could not move %s into %s", rel[i], place), call. = FALSE) }
  unlink(stage, recursive = TRUE)
  if (sum(have & same)) log(sprintf("  %d files already in place, identical: kept", sum(have & same)))
  sp <- normalizePath(path, winslash = "/", mustWork = FALSE); st <- normalizePath(stage, winslash = "/", mustWork = FALSE)
  if (identical(sp, st)) place else if (startsWith(sp, paste0(st, "/"))) file.path(place, substring(sp, nchar(st) + 2)) else path }

run_dataset <- function(h, ctx, layout, check_only = FALSE) {
  t0 <- Sys.time(); col <- h$column; log <- ctx$log
  out <- function(status, files = NA_integer_, mb = NA_real_, nxt = NA_character_)
    data.table::data.table(DataSource = col, Dataset = h$name, Status = status, Files = as.integer(files), SizeMb = round(mb, 1),
                           Minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2), Next = nxt)
  # where it goes: national copies (FIA, PRISM, LOCA2, FPA-FOD) whole at the top of the home, the rest in the region folder
  ctx$store <- store_for(h, ctx); where <- home_label(ctx$home, ctx$store); if (isTRUE(h$national)) ctx$keep <- TRUE
  # a handler with have(ctx, items) (one TRUE per planned item already in place) can grow: a rerun asking for more (LOCA2 models,
  # scenarios, members) fetches only what is missing; without it, a covering manifest means the dataset is done
  up <- dataset_up_to_date(ctx$store, col, ctx$region)
  # a new edition (the folder holds this dataset for the region, at another version): into <folder>/editions/<tag>/ when Fetch new
  # editions is ticked, else skipped; the first edition is never touched, and a library reads it until its Data Sources page says
  m0 <- read_manifest(ctx$store)[Column == col]
  newer <- up && !is.null(h$version) && nrow(m0) == 1 && !is.na(m0$Version) && !identical(m0$Version, h$version)
  if (newer) { ed <- file.path(ctx$store, "editions", edition_tag(h$version))
    if (!isTRUE(ctx$new_editions) && !dataset_up_to_date(ed, col, ctx$region)) {
      log(sprintf("%s: %s holds %s; this version fetches %s, a new edition (tick Fetch new editions to download it)", h$name, where, m0$Version, h$version))
      return(out("skipped", nrow(read_files(ctx$store)[Column == col]), nxt = sprintf("%s; new edition %s available: tick Fetch new editions to add it beside the first", held_in(where), h$version))) }
    ctx$store <- ed; where <- home_label(ctx$home, ed); up <- dataset_up_to_date(ed, col, ctx$region)
    log(sprintf("%s: new edition %s, into %s", h$name, h$version, where)) }
  if (up && is.null(h$have)) { log(sprintf("%s: up to date in %s (manifest and files.csv)", h$name, where))
    return(out("skipped", nrow(read_files(ctx$store)[Column == col]), nxt = held_in(where))) }
  # a copy held elsewhere for this region (a clipped copy in the region folder of a dataset now kept whole, or a national copy)
  if (!up && !newer && is.null(h$have)) for (d in setdiff(unique(c(ctx$region_home, ctx$home)), ctx$store))
    if (dataset_up_to_date(d, col, ctx$region)) { log(sprintf("%s: up to date in %s", h$name, home_label(ctx$home, d)))
      return(out("skipped", nrow(read_files(d)[Column == col]), nxt = held_in(home_label(ctx$home, d)))) }
  # write-once: a folder that holds this dataset for another region is never refetched over
  m <- read_manifest(ctx$store)[Column == col]
  if (nrow(m) && identical(m$Status, "done") && !isTRUE(manifest_covers(ctx$store, col, ctx$region$box)))
    stop(sprintf("write-once: %s already holds %s for %s%s, which does not cover this region, and a library may have read it; download this region under another region name",
                 where, h$name, m$Units, if (is.na(m$BufferKm) || !nzchar(m$BufferKm)) "" else sprintf(" + %s km", m$BufferKm)), call. = FALSE)
  hp <- layout[Column == col, HomePath]; first <- strsplit(hp, "|", fixed = TRUE)[[1]][1]
  ctx$place <- file.path(ctx$store, if (grepl("*", first, fixed = TRUE)) dirname(first) else first)
  ctx$dl <- file.path(ctx$store, ".downloads", col)
  items <- h$plan(ctx)
  if (!nrow(items)) return(out("failed", 0, 0, "the source lists no files for these years or this region"))
  # (only within a covered region: a file in place for a smaller region is fetched again)
  if (!is.null(h$have) && up) { got <- h$have(ctx, items)
    if (all(got)) { log(sprintf("%s: up to date in %s (all %d asked files in place)", h$name, where, nrow(items)))
      return(out("skipped", nrow(read_files(ctx$store)[Column == col]), nxt = held_in(where))) }
    if (any(got)) log(sprintf("%s: %d of %d asked files already in place; fetching the other %d", h$name, sum(got), nrow(items), sum(!got)))
    items <- items[!got] }
  items[, dest := file.path(ctx$dl, file)]; if (!"fetch" %in% names(items)) items[, fetch := TRUE]
  # supplied: files a person put in <home>/supplied (a captcha, say); NA on any row that comes from its source as usual
  sup <- if ("supplied" %in% names(items)) items$supplied[!is.na(items$supplied)] else character()
  get <- items[fetch == TRUE]; remote <- if ("supplied" %in% names(items)) items[fetch == FALSE & is.na(supplied)] else items[fetch == FALSE]
  if (check_only && length(sup) && length(sup) == nrow(items)) { log(sprintf("%s: %d files in %s, supplied by hand", h$name, length(sup), dirname(sup[1])))
    return(out("would download", nrow(items), 0, sprintf("from %s, supplied by hand", dirname(sup[1])))) }
  if (check_only) { s <- url_sizes(get$url); miss <- sum(is.na(s))
    log(sprintf("%s: %d files to fetch into %s, %.0f MB%s%s", h$name, nrow(get), where, sum(s, na.rm = TRUE) / 1e6, if (miss) sprintf(" (%d sizes unknown)", miss) else "",
                if (nrow(remote)) sprintf("; %d read from its service%s", nrow(remote), if (is.null(ctx$region$poly)) " whole" else " for the region only") else ""))
    return(out("would download", nrow(items), sum(s, na.rm = TRUE) / 1e6,
               paste(c(sub("^in ", "into ", held_in(where)), if (miss) sprintf("%d files did not report a size", miss), if (nrow(remote)) (if (is.null(ctx$region$poly)) "read from its service whole" else "read remotely: only the region is written")), collapse = "; "))) }
  mb <- 0
  if (nrow(get)) { sizes <- url_sizes(get$url)
    log(sprintf("%s: fetching %d files (%.0f MB%s)", h$name, nrow(get), sum(sizes, na.rm = TRUE) / 1e6, if (anyNA(sizes)) ", some sizes unknown" else ""))
    fetch_files(get$url, get$dest, ctx$connections, log, sizes); mb <- sum(file.size(get$dest), na.rm = TRUE) / 1e6 }
  if (length(sup)) log(sprintf("%s: reading %d files in %s, supplied by hand", h$name, length(sup), dirname(sup[1])))
  if (nrow(remote)) log(if (!is.null(h$method)) sprintf("%s: %s by its source (%s)", h$name, if (is.null(ctx$region$poly)) "read whole" else "clipped", h$method) else sprintf("%s: reading %d archives remotely, writing the region only", h$name, nrow(remote)))
  log(sprintf("%s: %.0f MB fetched; unpacking%s", h$name, mb, if (is.null(ctx$region$poly) || ctx$keep) "" else " and clipping to the region"))
  # finish writes into a staging folder; commit_staged moves its files into place without replacing any a library may have read
  place <- ctx$place; stage <- file.path(ctx$store, ".staging", col); unlink(stage, recursive = TRUE); dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  ctx$place <- stage
  path <- h$finish(ctx, items); path <- commit_staged(stage, place, path, ctx$store, col, log)
  # record: every file under the place, with checksums; the manifest row
  fr <- files_rows(ctx$store, col, path, sha = TRUE); write_files(ctx$store, fr)
  # a region's download is a national copy only when whole files were kept: a source that clips (NLCD, LANDFIRE, perimeters,
  # gNATSGO, stand age tiles) gives the region alone, Keep national copies or not
  by_source <- isTRUE(h$method %in% c("wcs", "lfps", "rest", "stac", "login"))
  copy <- if (is.null(ctx$region$poly) || isTRUE(h$national) || (ctx$keep && !by_source)) "national" else "clipped"
  src <- layout[Column == col]
  write_manifest(ctx$store, data.table::data.table(Column = col, Dataset = h$name, Source = src$Source, Version = if (!is.null(h$version)) h$version else src$Version,
    Url = paste(unique(dirname(items$url)), collapse = " "), Doi = src$Doi, Method = if (length(sup)) "by hand" else if (!is.null(h$method)) h$method else "direct", Copy = copy,
    Units = ctx$region$units, BufferKm = ctx$region$buffer_km, Xmin = ctx$region$box[1], Ymin = ctx$region$box[2], Xmax = ctx$region$box[3], Ymax = ctx$region$box[4],
    Files = nrow(fr), Bytes = sprintf("%.0f", sum(as.numeric(fr$Bytes))), Downloaded = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), Status = "done",
    Note = if (!is.null(h$note)) h$note else NA_character_))
  unlink(ctx$dl, recursive = TRUE)
  log(sprintf("%s: done, %d files, %.0f MB at %s", h$name, nrow(fr), sum(as.numeric(fr$Bytes)) / 1e6, substring(path, nchar(ctx$home) + 2)))
  out("done", nrow(fr), sum(as.numeric(fr$Bytes)) / 1e6, held_in(where)) }

# a failed dataset: its manifest row says so (kept from an earlier done run otherwise untouched files would be lost: only a
# dataset with no done row gets a failed row)
record_failure <- function(home, h, msg) {
  m <- read_manifest(home)[Column == h$column]
  if (!nrow(m) || m$Status != "done") write_manifest(home, data.table::data.table(Column = h$column, Dataset = h$name, Status = "failed", Note = substr(msg, 1, 500))) }
