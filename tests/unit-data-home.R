# =============================================================================
# unit-data-home.R - plain-R tests of Data Sources Home without SyncroSim or the network
# =============================================================================
# Rscript tests/unit-data-home.R (from the package root). The tables, the engine against a fake dataset, and the handlers' plans.
suppressPackageStartupMessages({ library(data.table); library(sf); library(terra) })
root <- normalizePath(if (dir.exists("src")) "." else ".."); pkg_file <- function(...) file.path(root, ...)
env <- list(PackageDirectory = root)
source(pkg_file("src", "datahome.R")); source(pkg_file("src", "download.R")); source(pkg_file("src", "download-sources.R"))
ok <- 0L; expect <- function(what, x) { if (!isTRUE(x)) stop("FAILED: ", what, call. = FALSE); ok <<- ok + 1L; cat("ok ", what, "\n") }

# ---- the tables agree ----------------------------------------------------------------------------------------------------------
cols <- fread(pkg_file("defaults", "library", "data-sources.csv"), colClasses = "character", na.strings = "")
sets <- home_layout(root); ep <- fread(pkg_file("defaults", "download", "endpoints.csv"), colClasses = "character", na.strings = "")
H <- download_handlers()
xml <- paste(readLines(pkg_file("package.xml"), warn = FALSE), collapse = "\n")
blk <- regmatches(xml, regexpr('(?s)<dataSheet name="DataSource" .*?</dataSheet>', xml, perl = TRUE))
sheet <- setdiff(sub('.*name="([^"]+)".*', "\\1", regmatches(blk, gregexpr('<column name="[^"]+"', blk))[[1]]), "DataSourcesHome")
expect("every Data Sources column is in data-sources.csv", setequal(sheet, cols$Column))
expect("every column with a place names a known dataset", all(cols[!is.na(HomePath), Dataset] %in% sets$Column))
expect("every dataset has a handler", setequal(sets$Column, names(H)))
expect("every dataset has an endpoints row", setequal(sets$Column, ep$Column))
expect("each handler's column is its name", all(vapply(names(H), function(k) identical(H[[k]]$column, k), TRUE)))
# what lucasCore also holds keeps lucasCore's place, or a shared home would hold two copies
core <- "F:/lucasCore/defaults/library/data-sources.csv"
if (file.exists(core)) { cc <- fread(core, colClasses = "character", na.strings = ""); both <- merge(sets[, .(Column, HomePath)], cc[, .(Column, Core = HomePath)], by = "Column")
  expect(sprintf("the %d datasets shared with lucasCore have its places", nrow(both)), nrow(both) >= 9 && all(both$HomePath == both$Core)) }

# ---- the home: reading a column's place inside its dataset --------------------------------------------------------------------
h <- file.path(tempdir(), "home"); dir.create(h); home_create(h, root)
expect("home_create makes each dataset's place", all(dir.exists(file.path(h, c("fuels/landfire", "climate/gridmet", "disturbance/mtbs-perimeters")))))
expect("an empty folder place is not there", is.na(home_path(h, "climate/gridmet")))
nl <- file.path(h, "land-cover/nlcd-annual"); file.create(file.path(nl, c("Annual_NLCD_LndCov_2010_CU_C1V2.tif", "Annual_NLCD_LndCov_2011_CU_C1V2.tif")))
expect("NLCD: the 2010 file among the years", grepl("2010_CU", home_path(h, cols[Column == "NlcdLandCoverFile", HomePath])))
p <- file.path(h, "climate/prism/normals"); dir.create(p, recursive = TRUE); file.create(file.path(p, c("prism_ppt_us_25m_2020_avg_30y.tif", "prism_tmean_us_25m_2020_avg_30y.tif", "prism_ppt_us_25m_2020_avg_30y.tif.aux.xml")))
expect("PRISM: the ppt normal alone", grepl("prism_ppt", home_path(h, cols[Column == "PrismPrecipFile", HomePath])))

# ---- regions pick by coverage, keyed on the dataset ---------------------------------------------------------------------------
mk <- function(dir, box, copy = "clipped") { dir.create(dir, recursive = TRUE, showWarnings = FALSE); d <- file.path(dir, "fuels/landfire"); dir.create(d, recursive = TRUE); file.create(file.path(d, "x.tif"))
  write_manifest(dir, data.table(Column = "LandfireFolder", Dataset = "LF", Copy = copy, Xmin = box[1], Ymin = box[2], Xmax = box[3], Ymax = box[4], Status = "done")) }
mk(file.path(h, "regions/Big"), c(0, 0, 100, 100)); mk(file.path(h, "regions/Small"), c(40, 40, 60, 60))
pick <- function(b) basename(home_pick(h, "LandfireFolder", "fuels/landfire", b)$dir)
expect("the smallest region that covers", pick(c(45, 45, 55, 55)) == "Small")
expect("the larger when the smaller does not cover", pick(c(10, 10, 55, 55)) == "Big")
expect("manifest_covers refuses a wider box", isFALSE(manifest_covers(file.path(h, "regions/Small"), "LandfireFolder", c(0, 0, 55, 55))))

# ---- write-once ------------------------------------------------------------------------------------------------------------------
st <- file.path(tempdir(), "w1"); pl <- file.path(tempdir(), "w1pl"); dir.create(st); dir.create(pl); s0 <- file.path(tempdir(), "w1s"); dir.create(s0)
writeLines("a", file.path(st, "f.txt")); writeLines("b", file.path(pl, "f.txt"))
write_manifest(s0, data.table(Column = "K", Dataset = "K", Status = "done"))
expect("a differing file a library may have read stops the commit", inherits(tryCatch(commit_staged(st, pl, st, s0, "K", function(x) NULL), error = function(e) e), "error") && readLines(file.path(pl, "f.txt")) == "b")

# ---- the engine end to end on a fake dataset; rerun skips -------------------------------------------------------------------------
src <- file.path(tempdir(), "src.txt"); writeLines(rep("data", 100), src)
fake <- list(column = "StatesFile", name = "fake", national = TRUE, version = "v1",
  plan = function(ctx) data.table(url = paste0("file:///", normalizePath(src, winslash = "/")), file = "src.txt", fetch = FALSE),
  finish = function(ctx, items) { dir.create(ctx$place, recursive = TRUE, showWarnings = FALSE); file.copy(src, file.path(ctx$place, "tl.shp")); file.path(ctx$place, "tl.shp") })
region <- list(box = c(-2400000, 200000, 2300000, 3200000), poly = NULL, units = "CONUS", buffer_km = NA_real_)
ctx <- list(home = h, region_home = NULL, region = region, years = c(1979L, NA), keep = FALSE, new_editions = FALSE, connections = 2L, opts = data.table(), log = function(x) NULL)
r1 <- run_dataset(fake, ctx, sets); r2 <- run_dataset(fake, ctx, sets)
expect("first run done, recorded with a checksum", r1$Status == "done" && nzchar(read_files(h)[Column == "StatesFile", Sha256][1]))
expect("second run skipped", r2$Status == "skipped")

# ---- the plans, offline ---------------------------------------------------------------------------------------------------------
op <- function(...) as.data.table(list(...))
reg <- list(box = c(-2100000, 1900000, -2000000, 2200000), poly = st_sfc(st_polygon(list(rbind(c(-2100000, 1900000), c(-2000000, 1900000), c(-2000000, 2200000), c(-2100000, 2200000), c(-2100000, 1900000)))), crs = 5070), units = "t", buffer_km = 60)
cx <- modifyList(ctx, list(region = reg, opts = data.table(), dl = tempdir(), place = tempdir()))
m <- tryCatch(H$LandfireFolder$plan(cx), error = function(e) conditionMessage(e))
expect("LF2020 without its zips: the manual step, naming where they go", is.character(m) && grepl("needs a manual download", m) && grepl(file.path(h, "supplied"), m, fixed = TRUE))
for (p in c("FBFM40", "CBD", "CBH", "CC", "CH")) file.create(file.path(h, "supplied", sprintf("LF2020_%s_CONUS_20260522.zip", p)))
lf <- H$LandfireFolder$plan(cx)
expect("LF2020 is the default: five supplied zips, nothing fetched", nrow(lf) == 5 && !any(lf$fetch) && all(lf$archive) && all(file.exists(lf$supplied)))
chk <- run_dataset(H$LandfireFolder, cx, sets, TRUE)
expect("check only names the supplied folder and fetches nothing", chk$Status == "would download" && grepl("supplied by hand", chk$Next) && chk$SizeMb == 0)
expect("named as landfire_file reads them", all(grepl("^LF2020_([A-Z0-9]+)_CONUS/LF2020_\\1_CONUS/Tif/LF2020_\\1_CONUS[.]tif$", lf$out)))
cx$opts <- data.table(LandfireVintages = "2020", CanopyHeightVintage = 2020L); expect("a canopy height vintage replaces the CH row", sum(H$LandfireFolder$plan(cx)$prod == "CH") == 1)
gm <- H$GridMetFolder$plan(modifyList(ctx, list(years = c(2019L, 2020L))))
expect("gridMET: var/var_year.nc for each year asked", nrow(gm) == 10 && "vs/vs_2020.nc" %in% gm$file)
last <- as.integer(format(Sys.Date(), "%Y")); g2 <- H$GridMetFolder$plan(ctx)
expect("gridMET leaves out the year still being written, from 1979", !any(grepl(as.character(last), g2$file)) && min(g2$year) == 1979L)
n <- H$NlcdFolder$plan
cat(sprintf("%d checks passed\n", ok))
