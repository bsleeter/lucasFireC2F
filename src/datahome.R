# =============================================================================
# datahome.R - the Data Sources Home folder: each dataset's standard place in it, and the manifest
# =============================================================================
# Kept in step with lucasCore src/datahome.R: one home folder can serve both packages. Data Sources: Data Sources Home names one
# folder. defaults/download/datasets.csv (HomePath) gives every dataset (a Column, here and in the manifest) its
# place under it: a folder column's subfolder ("land-cover/nlcd-annual"), or a file column's subfolder and file pattern, alternatives
# split by "|" ("disturbance/fire-perimeters/*.shp|*.gpkg"). A pattern must match exactly one file. A blank Data Sources path reads
# its home place (src/shared.R, read_data_sources). The Data Download stage writes this layout, and two tables at the top:
#   manifest.csv  one row per dataset: what it is, where it came from, the region it covers (EPSG:5070) and its status
#   files.csv     one row per file: path under the home folder, bytes, sha256, modified time (a rerun skips finished files)
# defaults/library/data-sources.csv says where each Data Sources sheet column reads inside its dataset. The stages' coverage checks read the manifest. No rsyncrosim here: tools and tests source it.
# =============================================================================

MANIFEST_COLS <- c("Column", "Dataset", "Source", "Version", "Url", "Doi", "Method", "Copy", "Units", "BufferKm",
                   "Xmin", "Ymin", "Xmax", "Ymax", "Files", "Bytes", "Downloaded", "Status", "Note")
FILES_COLS <- c("Column", "Path", "Bytes", "Sha256", "Modified")
MANIFEST_STATUS <- c("done", "partial", "manual", "failed")

# the package's data sources table: Column, HomePath and the descriptive columns
home_layout <- function(pkgdir) data.table::fread(file.path(pkgdir, "defaults", "download", "datasets.csv"), sep = ",",
                                                   colClasses = "character", na.strings = "", encoding = "UTF-8")

# the home place of one HomePath: the folder or the one matching file, else NA with attribute "found" (files matched, 0 or 2+)
home_path <- function(home, homepath) {
  if (is.null(home) || is.na(home) || !nzchar(home) || is.na(homepath)) return(structure(NA_character_, found = 0L))
  parts <- strsplit(homepath, "|", fixed = TRUE)[[1]]
  # a folder counts only when it holds something: Make folders / home_create leave every place empty
  if (!grepl("*", parts[1], fixed = TRUE)) { p <- file.path(home, parts[1]); return(if (dir.exists(p) && length(list.files(p))) p else structure(NA_character_, found = 0L)) }
  dir <- dirname(file.path(home, parts[1])); pats <- c(basename(parts[1]), parts[-1])
  f <- unique(unlist(lapply(pats, function(p) Sys.glob(file.path(dir, p))))); f <- f[!dir.exists(f)]
  if (length(f) == 1) f else structure(NA_character_, found = length(f)) }

# every dataset's home place, and whether it is there: Column, Place (the HomePath), Path (NA if missing), Found (files matched)
home_status <- function(home, pkgdir) {
  d <- home_layout(pkgdir)[!is.na(HomePath)]
  r <- lapply(d$HomePath, function(h) home_path(home, h))
  data.table::data.table(Column = d$Column, Place = d$HomePath, Path = vapply(r, function(x) x[1], ""),
                         Found = vapply(r, function(x) if (is.na(x[1])) attr(x, "found") else 1L, 1L)) }

# make the empty layout (folders only; nothing is moved or deleted). Returns the folders it made
home_create <- function(home, pkgdir) {
  d <- home_layout(pkgdir)[!is.na(HomePath)]
  dirs <- file.path(home, unique(c(vapply(d$HomePath, function(h) { p <- strsplit(h, "|", fixed = TRUE)[[1]][1]
    if (grepl("*", p, fixed = TRUE)) dirname(p) else p }, ""), "logs")))
  made <- dirs[!dir.exists(dirs)]; for (x in made) dir.create(x, recursive = TRUE)
  invisible(made) }

# ---- manifest.csv and files.csv ------------------------------------------------------------------------------------------
.home_table <- function(home, name, cols) {
  f <- file.path(home, name)
  if (!file.exists(f)) return(data.table::as.data.table(setNames(rep(list(character(0)), length(cols)), cols)))
  x <- data.table::fread(f, sep = ",", colClasses = "character", na.strings = "", encoding = "UTF-8")
  for (k in setdiff(cols, names(x))) x[, (k) := rep(NA_character_, nrow(x))]
  x[, cols, with = FALSE] }
read_manifest <- function(home) .home_table(home, "manifest.csv", MANIFEST_COLS)
read_files <- function(home) .home_table(home, "files.csv", FILES_COLS)

# settings.csv: Key,Value pairs that belong to the person filling the home folder rather than to a scenario (LfpsEmail: the address
# LANDFIRE Product Service jobs are submitted with; the Data Download page writes it)
home_setting <- function(home, key) { f <- file.path(home, "settings.csv"); if (!file.exists(f)) return(NA_character_)
  x <- data.table::fread(f, colClasses = "character", na.strings = ""); v <- x$Value[x$Key == key]; if (length(v)) v[1] else NA_character_ }

# replace the rows of the given columns (manifest: one row per Column; files: every row of those Columns), keep the rest. Written
# through a temporary file and renamed, so a stopped run never leaves half a table
.write_home_table <- function(home, name, cols, old, rows) {
  rows <- data.table::as.data.table(rows); for (k in setdiff(cols, names(rows))) rows[, (k) := rep(NA_character_, nrow(rows))]
  rows <- rows[, cols, with = FALSE][, (cols) := lapply(.SD, as.character)]
  out <- rbind(old[!Column %in% rows$Column], rows); data.table::setorder(out, Column)
  f <- file.path(home, name); tmp <- paste0(f, ".tmp"); data.table::fwrite(out, tmp); file.rename(tmp, f)
  invisible(out) }
write_manifest <- function(home, rows) {
  rows <- data.table::as.data.table(rows)
  if (anyDuplicated(rows$Column)) stop("manifest: one row per dataset (", paste(unique(rows$Column[duplicated(rows$Column)]), collapse = ", "), ")", call. = FALSE)
  bad <- setdiff(rows$Status, MANIFEST_STATUS); if (length(bad)) stop("manifest: unknown status ", paste(bad, collapse = ", "), call. = FALSE)
  .write_home_table(home, "manifest.csv", MANIFEST_COLS, read_manifest(home), rows) }
write_files <- function(home, rows) .write_home_table(home, "files.csv", FILES_COLS, read_files(home), rows)

# the file rows of one dataset as on disk now: every file under its home place, path relative to the home folder. sha256 only
# when asked (it reads every byte)
files_rows <- function(home, column, path, sha = FALSE) {
  # a file column: the file and its sidecars (a shapefile's .dbf, .shx, .prj; a raster's .aux.xml)
  f <- if (dir.exists(path)) list.files(path, recursive = TRUE, full.names = TRUE) else {
    a <- list.files(dirname(path), full.names = TRUE); a[startsWith(basename(a), paste0(tools::file_path_sans_ext(basename(path)), "."))] }
  if (!length(f)) return(data.table::data.table())
  i <- file.info(f, extra_cols = FALSE); rel <- substring(normalizePath(f, winslash = "/"), nchar(normalizePath(home, winslash = "/")) + 2)
  data.table::data.table(Column = column, Path = rel, Bytes = sprintf("%.0f", i$size),
                         Sha256 = if (sha) vapply(f, function(x) digest::digest(file = x, algo = "sha256"), "") else NA_character_,
                         Modified = format(i$mtime, "%Y-%m-%dT%H:%M:%S")) }

# does the manifest show a dataset done for a box (EPSG:5070: xmin, ymin, xmax, ymax)? TRUE / FALSE, or NA when the manifest has no
# row for it (a path set by hand, or a home folder laid out by hand: nothing to check against). National copies cover everything
manifest_covers <- function(home, column, box) {
  m <- read_manifest(home)[Column == column]
  if (!nrow(m)) return(NA)
  if (m$Status != "done") return(FALSE)
  if (identical(m$Copy, "national")) return(TRUE)
  e <- suppressWarnings(as.numeric(unlist(m[, .(Xmin, Ymin, Xmax, Ymax)])))
  if (anyNA(e)) return(FALSE)
  # within a metre: the box goes through text in manifest.csv (15 significant digits)
  box[1] >= e[1] - 1 && box[2] >= e[2] - 1 && box[3] <= e[3] + 1 && box[4] <= e[4] + 1 }

# ---- regions (Home layout: By region; plan, "Second study areas: home layout") ---------------------------------------------
# A home folder keeps national copies at its top and one folder per region under regions/<name>/, each laid out like a home
# (its own manifest.csv and files.csv). A Data Download writes only into its own region folder (or the top, for national
# copies), and never replaces a file already there (src/download.R, write-once), so one region's download cannot change the
# data another library read. A Build takes each blank dataset from the smallest folder whose manifest covers its study area.
home_regions <- function(home) {
  d <- file.path(home, "regions"); if (is.null(home) || is.na(home) || !dir.exists(d)) return(character())
  normalizePath(list.dirs(d, recursive = FALSE), winslash = "/") }

# a region's folder name from its Study Area unit names (or the name typed on the page), and an edition's from its version:
# letters, digits, - and _ only
region_name <- function(x) {
  n <- gsub("^-+|-+$", "", gsub("[^A-Za-z0-9_]+", "-", paste(x, collapse = "+")))
  if (!nzchar(n)) stop("the region needs a name of letters or digits", call. = FALSE)
  if (nchar(n) > 60) n <- paste0(substr(n, 1, 51), "-", substr(digest::digest(n, algo = "md5"), 1, 8))
  n }

edition_tag <- function(version) region_name(version)

# a folder's label in messages: "regions/P26", "regions/P26/editions/LF2025-final", or "the top of the home"
home_label <- function(home, dir) { d <- normalizePath(dir, winslash = "/", mustWork = FALSE); h <- normalizePath(home, winslash = "/", mustWork = FALSE)
  if (identical(d, h)) "the top of the home" else if (startsWith(d, paste0(h, "/"))) substring(d, nchar(h) + 2) else d }

# ---- editions (plan, "Design: editions") ------------------------------------------------------------------------------------
# A new edition of a dataset a folder already holds goes to <folder>/editions/<tag>/, laid out like a home (own manifest.csv and
# files.csv); the dataset's first place stays as it is. A library reads the first edition until its Data Source Editions sheet
# (set on the Data Sources page) names another. Every edition of a dataset in the home: Folder (the top or a region), Edition
# (blank = the first), Dir, Version, Downloaded
home_editions <- function(home, column) {
  base <- c(normalizePath(home, winslash = "/", mustWork = FALSE), home_regions(home))
  data.table::rbindlist(lapply(base, function(b) { m <- read_manifest(b)[Column == column & Status == "done"]
    eds <- if (dir.exists(file.path(b, "editions"))) list.dirs(file.path(b, "editions"), recursive = FALSE) else character()
    x <- data.table::rbindlist(lapply(eds, function(e) { r <- read_manifest(e)[Column == column & Status == "done"]
      if (nrow(r)) data.table::data.table(Folder = home_label(home, b), Edition = basename(e), Dir = normalizePath(e, winslash = "/"), Version = r$Version, Downloaded = r$Downloaded) }))
    rbind(if (nrow(m)) data.table::data.table(Folder = home_label(home, b), Edition = "", Dir = b, Version = m$Version, Downloaded = m$Downloaded), x) })) }

# where a blank dataset is read from: the folders holding its place (the top and every region), best first. A folder whose
# manifest covers the box comes first, smallest box first (a national copy last); then folders with no manifest row (laid out
# by hand: the top before regions); folders whose manifest does not cover the box last, so the coverage check names what to do.
# box NULL (no study area known): the top first. Returns list(dir, path), or NULL when no folder has it; stops when a folder
# matches 2+ files
# edition: the library's chosen edition (blank = the first: each folder itself; else each folder's editions/<edition>)
home_pick <- function(home, column, homepath, box = NULL, edition = NULL) {
  dirs <- c(normalizePath(home, winslash = "/", mustWork = FALSE), home_regions(home)); best <- NULL; rank <- Inf
  if (!is.null(edition) && !is.na(edition) && nzchar(edition)) { dirs <- file.path(dirs, "editions", edition); dirs <- dirs[dir.exists(dirs)] }
  for (i in seq_along(dirs)) { p <- home_path(dirs[i], homepath)
    if (is.na(p)) { if (attr(p, "found") > 1) stop(sprintf("Data Sources: %s is blank and %d files match its place in %s (%s); keep one, or set the path",
                                                           column, attr(p, "found"), home_label(home, dirs[i]), file.path(dirs[i], homepath)), call. = FALSE)
                    next }
    cov <- if (is.null(box)) NA else manifest_covers(dirs[i], column, box)
    r <- if (isTRUE(cov)) { m <- read_manifest(dirs[i])[Column == column]; e <- as.numeric(unlist(m[, .(Xmin, Ymin, Xmax, Ymax)]))
           if (identical(m$Copy, "national")) 1e30 else (e[3] - e[1]) * (e[4] - e[2]) }
         else if (is.na(cov)) 1e31 else 1e32   # ties keep the earlier folder: the top first
    if (r < rank) { rank <- r; best <- list(dir = dirs[i], path = p) } }
  best }

# move a home laid out before regions (one clip per dataset at the top) into regions/<name>: every dataset whose manifest says
# clipped moves with its files.csv and manifest rows; national copies, settings.csv and logs stay at the top. A folder place moves
# whole; a file place moves its recorded files and their sidecars. Nothing is rewritten: each file is renamed, and its size checked
# against files.csv after. dry = TRUE lists what would move. Returns the moved columns
home_to_region <- function(home, name, pkgdir, dry = FALSE, log = message) {
  name <- region_name(name); dest <- file.path(home, "regions", name)
  m <- read_manifest(home); f <- read_files(home); lay <- home_layout(pkgdir)
  cols <- m[Copy %in% "clipped", Column]
  if (!length(cols)) { log("nothing to move: no clipped datasets at the top of the home"); return(invisible(character())) }
  dm <- read_manifest(dest); clash <- intersect(cols, dm$Column)
  if (length(clash)) stop(sprintf("regions/%s already holds %s", name, paste(clash, collapse = ", ")), call. = FALSE)
  moves <- rbindlist(lapply(cols, function(k) { hp <- strsplit(lay[Column == k, HomePath], "|", fixed = TRUE)[[1]][1]
    if (!grepl("*", hp, fixed = TRUE) && dir.exists(file.path(home, hp))) return(data.table(Column = k, Rel = hp, Folder = TRUE))
    rel <- f[Column == k, Path]
    side <- unlist(lapply(unique(rel), function(r) { a <- list.files(file.path(home, dirname(r)))
      file.path(dirname(r), a[startsWith(a, paste0(tools::file_path_sans_ext(basename(r)), "."))]) }))
    data.table(Column = k, Rel = unique(c(rel, side)), Folder = FALSE) }))
  for (i in seq_len(nrow(moves))) log(sprintf("%s %s: %s%s", if (dry) "would move" else "moving", moves$Column[i], moves$Rel[i], if (moves$Folder[i]) "/ (folder)" else ""))
  if (dry) return(invisible(cols))
  for (i in seq_len(nrow(moves))) { from <- file.path(home, moves$Rel[i]); to <- file.path(dest, moves$Rel[i])
    if (file.exists(to)) stop(sprintf("regions/%s/%s already exists; nothing more was moved", name, moves$Rel[i]), call. = FALSE)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    if (!file.rename(from, to)) stop(sprintf("could not move %s (is it open?); nothing more was moved", moves$Rel[i]), call. = FALSE) }
  # the tables: rows move with their files (written region first, so a stop leaves the rows where the files are)
  .write_home_table(dest, "files.csv", FILES_COLS, read_files(dest), f[Column %in% cols])
  .write_home_table(dest, "manifest.csv", MANIFEST_COLS, read_manifest(dest), m[Column %in% cols])
  for (nm in c("files.csv", "manifest.csv")) { x <- if (nm == "files.csv") f[!Column %in% cols] else m[!Column %in% cols]
    tmp <- file.path(home, paste0(nm, ".tmp")); data.table::fwrite(x, tmp); file.rename(tmp, file.path(home, nm)) }
  fr <- read_files(dest)[Column %in% cols]; p <- file.path(dest, fr$Path)
  badf <- fr$Path[!file.exists(p) | file.size(p) != as.numeric(fr$Bytes)]
  if (length(badf)) stop(sprintf("after the move %d files differ from files.csv: %s", length(badf), paste(head(badf, 5), collapse = ", ")), call. = FALSE)
  log(sprintf("moved %d datasets (%d files) into regions/%s; every file at its files.csv size", length(cols), nrow(fr), name))
  invisible(cols) }
