# =============================================================================
# D - Data Download: fetch the Data Sources into Data Sources Home for the fire zones asked
# =============================================================================
# Its own scenario, run before Prep. The region is this scenario's Fire Zones to Download (their pyrome polygons) plus Data Download:
# Buffer, in EPSG:5070. One download serves every zone it holds. Each dataset goes through src/download.R's steps (up to date? plan,
# check, fetch with resume, unpack and clip, record in manifest.csv and files.csv) into its place under the home folder
# (src/datahome.R), where a blank Data Sources path finds it. Datasets: Data Download: Datasets (names from
# defaults/download/datasets.csv), blank = every one. Check only lists what would download, and how big. Download Results gets one row
# per dataset; a failed dataset does not stop the others.
# Home layout: By region (the default) writes the region's clips into <home>/regions/<Region name>/ (blank = from the fire zones, as
# Fire-P26-Northern-Sierra-Nevada) and the datasets kept whole at the top, shared by every region; National ignores the zones and
# keeps every dataset whole at the top. Write-once (src/download.R): no file in the home is ever replaced, so a second region never
# changes what another library read. The folder layout and manifest are lucasCore's, so one home can serve both packages: what they
# share (elevation, roads, NLCD, FPA-FOD, MTBS, PRISM, perimeters, pyromes) is fetched once.
# The default buffer of 60 km holds the zone grid (P1, zone + 25 km) and the reach of the ignition covariates' capped distances (P2,
# 32.1 km past that grid).
# =============================================================================
source(file.path(rsyncrosim::ssimEnvironment()$PackageDirectory, "src", "shared.R"))
source(pkg_file("src", "download.R")); source(pkg_file("src", "download-sources.R"))
suppressPackageStartupMessages({ library(terra); library(sf) }); sf_use_s2(FALSE)

opt <- as.data.table(datasheet(scn, ds("DataDownloadOptions"), optional = TRUE, lookupsAsFactors = FALSE))
o <- function(k, d = NULL) { v <- if (nrow(opt) && k %in% names(opt)) opt[[k]][1] else NA; if (is.null(v) || is.na(v) || (is.character(v) && !nzchar(v))) d else v }
dsrc <- as.data.table(datasheet(prj, ds("DataSource"), lookupsAsFactors = FALSE, optional = TRUE))
home <- if (nrow(dsrc) && "DataSourcesHome" %in% names(dsrc)) dsrc$DataSourcesHome[1] else NA_character_
if (is.na(home) || !nzchar(home)) fail("Data Sources: set Data Sources Home, the folder the download fills")
home <- normalizePath(home, winslash = "/", mustWork = FALSE); dir.create(home, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(home)) fail("Data Sources Home %s cannot be created", home)
home_create(home, env$PackageDirectory)
layout <- home_layout(env$PackageDirectory)

# a log per run in <home>/logs, as well as the run log
logf <- file.path(home, "logs", sprintf("download-%s.log", format(Sys.time(), "%Y%m%d-%H%M%S")))
log <- function(x) { say("%s", x); cat(format(Sys.time(), "%H:%M:%S "), x, "\n", sep = "", file = logf, append = TRUE) }

# ---- the datasets asked ----------------------------------------------------------------------------------------------------
H <- download_handlers()
asked <- trimws(strsplit(o("Datasets", ""), ",")[[1]]); asked <- asked[nzchar(asked)]
if (length(asked)) { bad <- setdiff(asked, names(H)); if (length(bad)) fail("Data Download: Datasets: %s are not datasets this package fetches (%s)", paste(bad, collapse = ", "), paste(names(H), collapse = ", ")) }
todo <- if (length(asked)) asked else names(H)
check_only <- isTRUE(as.logical(o("CheckOnly", FALSE)))
y_to <- o("YearTo", NA_integer_)
make_ctx <- function(region, region_home) list(home = home, region_home = region_home, region = region,
  years = c(as.integer(o("YearFrom", 1979L)), if (is.na(y_to)) NA_integer_ else as.integer(y_to)),
  keep = isTRUE(as.logical(o("KeepNational", FALSE))), new_editions = isTRUE(as.logical(o("FetchNewEditions", FALSE))), connections = as.integer(o("Connections", 4L)),
  opts = opt, log = log, prj = prj)
CONUS <- list(box = c(-2400000, 200000, 2300000, 3200000), poly = NULL, units = "CONUS", buffer_km = NA_real_)
fetch_one <- function(k, ctx, check_only) { h <- H[[k]]
  tryCatch(run_dataset(h, ctx, layout, check_only), error = function(e) {
    # a step only the user can take (a captcha) is not a failure: the run goes on and the manifest is left as it was
    if (grepl("needs a manual download", conditionMessage(e))) { log(sprintf("%s: %s", h$name, conditionMessage(e)))
      return(data.table(DataSource = k, Dataset = h$name, Status = "manual", Files = NA_integer_, SizeMb = NA_real_, Minutes = NA_real_, Next = conditionMessage(e))) }
    log(sprintf("%s: FAILED: %s", h$name, conditionMessage(e))); if (!check_only) record_failure(store_for(h, ctx), h, conditionMessage(e))
    data.table(DataSource = k, Dataset = h$name, Status = "failed", Files = NA_integer_, SizeMb = NA_real_, Minutes = NA_real_,
               Next = if (grepl("^write-once", conditionMessage(e))) substr(conditionMessage(e), 1, 300) else sprintf("%s (rerun to resume)", substr(conditionMessage(e), 1, 300))) }) }

# ---- the polygons first: the region is the fire zones' polygons, and the SILVIS files go by the states it touches -----------
hl <- o("HomeLayout", 2L)   # a list column reads as its label ("National (...)") or its number
national <- if (is.numeric(hl)) identical(as.integer(hl), 1L) else grepl("^National", as.character(hl))
zs <- as.data.table(datasheet(scn, ds("DataDownloadZone"), optional = TRUE, lookupsAsFactors = FALSE))
want <- if (nrow(zs) && !national) unique(as.character(zs$FireZone)) else character()
if (!national && !length(want)) fail("Fire Zones to Download is empty: list the zones this download is for, or choose the National home layout (every dataset whole, about 100 GB)")
first <- intersect(c("PyromesFile", "StatesFile"), names(H))
say("Polygons first (national, at the top of the home): %s", paste(first, collapse = ", "))
res0 <- rbindlist(lapply(first, function(k) fetch_one(k, make_ctx(CONUS, NULL), FALSE)), fill = TRUE)
todo <- setdiff(todo, first)
if (any(res0$Status == "failed")) fail("Data Download: the zone polygons could not be fetched: %s", paste(res0[Status == "failed", sprintf("%s: %s", DataSource, Next)], collapse = "; "))

# ---- the region ------------------------------------------------------------------------------------------------------------
buffer_km <- as.numeric(o("BufferKm", 60))
region_home <- NULL
if (length(want)) {
  fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); ids <- fzt$ID[match(want, fzt$Name)]
  if (anyNA(ids)) fail("Fire Zones to Download: not in this project's Fire Zones: %s", paste(want[is.na(ids)], collapse = ", "))
  z <- zone_polygons(home_path(home, layout[Column == "PyromesFile", HomePath]))
  miss <- setdiff(ids, z$ID); if (length(miss)) fail("Fire Zones to Download: no polygon for Map Code %s in the pyromes; a zone that is not a pyrome needs its files set by hand", paste(miss, collapse = ", "))
  poly <- st_buffer(st_union(st_make_valid(st_transform(z[z$ID %in% ids, ], 5070))), buffer_km * 1000)
  box <- as.numeric(st_bbox(poly))
  region <- list(box = box, poly = poly, units = paste(want, collapse = "; "), buffer_km = buffer_km)
  say("Region: %s + %g km (%.0f x %.0f km)", region$units, buffer_km, (box[3] - box[1]) / 1000, (box[4] - box[2]) / 1000)
  # its own folder: the clips of one region never share a folder with another region's. "Fire-": a fire region is wider than a
  # lucasCore study area of the same name, and write-once keeps them apart in a shared home
  rn <- o("RegionName", ""); rn <- tryCatch(region_name(if (nzchar(trimws(rn))) trimws(rn) else c("Fire", want)), error = function(e) fail("Data Download: Region name: %s", conditionMessage(e)))
  region_home <- file.path(home, "regions", rn); home_create(region_home, env$PackageDirectory)
  say("Region folder: regions/%s (national copies at the top of the home)", rn)
} else {
  region <- CONUS
  say("Region: all of CONUS (Home layout National; national copies at the top of the home)") }

# ---- the datasets ----------------------------------------------------------------------------------------------------------
ctx <- make_ctx(region, region_home)
say("%s %d datasets into %s%s", if (check_only) "Checking" else "Downloading", length(todo), home, if (ctx$keep) " (national copies kept)" else "")
progressBar(type = "begin", totalSteps = length(todo))
res <- rbind(res0, rbindlist(lapply(todo, function(k) { r <- fetch_one(k, ctx, check_only); progressBar(); r }), fill = TRUE), fill = TRUE)
write_ds(res, "OutputDataDownload")
progressBar(type = "end")

say("%s", paste(sprintf("%s %d", names(table(res$Status)), as.integer(table(res$Status))), collapse = ", "))
if (check_only) say("Check only: %.1f GB would download", sum(res$SizeMb, na.rm = TRUE) / 1000)
# (the run ends normally, or a failed run would lose Download Results: its rows say what failed, and a rerun resumes)
for (k in which(res$Status %in% c("failed", "manual"))) updateRunLog(sprintf("%s %s: %s", if (res$Status[k] == "failed") "FAILED:" else "TO DO:", res$DataSource[k], res$Next[k]), type = "warning")
