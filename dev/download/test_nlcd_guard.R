# =============================================================================
# test_nlcd_guard.R - the one-year NLCD download refetches a partly blank tile, against the real WCS
# =============================================================================
# Rscript dev/download/test_nlcd_guard.R <good NLCD tif of the region> <empty scratch folder>
# Blanks half of one tile the first time it arrives (as the WCS did on 2026-11-04), runs the handler for P26 + 60 km into the scratch folder, and
# compares the mosaic with a good copy. No library and no home manifest are touched. About 3 minutes.
# =============================================================================
args <- commandArgs(TRUE); GOOD <- args[1]; OUT <- args[2]; stopifnot(file.exists(GOOD), length(args) == 2, !length(list.files(OUT)))
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf) }); terraOptions(progress = 0)
P <- "F:/lucasFireC2F-data-download"; for (f in c("datahome.R", "download.R", "download-sources.R")) source(file.path(P, "src", f))
g <- rast(GOOD); b <- as.vector(ext(g))[c(1, 3, 2, 4)]
ctx <- list(home = OUT, region_home = OUT, store = OUT, region = list(box = b, poly = st_as_sfc(st_bbox(setNames(b, c("xmin", "ymin", "xmax", "ymax")), crs = st_crs(5070))), units = "P26", buffer_km = 60),
            years = c(1979L, NA), keep = FALSE, connections = 1L, opts = data.table(), place = file.path(OUT, "place"), dl = file.path(OUT, "dl"), log = function(x) { LOG <<- c(LOG, x); cat(x, "\n") })
LOG <- character(); dir.create(ctx$place, recursive = TRUE); h <- download_handlers()$NlcdFolder; items <- h$plan(ctx); stopifnot(nrow(items) == 1, items$year == 2010L)
real <- fetch_files; spoiled <- FALSE
assign("fetch_files", function(urls, dest, ...) { real(urls, dest, ...)
  i <- which(grepl("[/]2010[/]tile06[.]tif$", dest)); if (length(i) && !spoiled) { spoiled <<- TRUE; r <- rast(dest[i]); v <- values(r, mat = FALSE); v[seq_len(length(v) %/% 2)] <- NA; x <- setValues(rast(r), v); tmp <- paste0(dest[i], ".t.tif"); writeRaster(x, tmp, datatype = "INT1U", NAflag = 255, overwrite = TRUE); rm(r, x); gc(); file.remove(dest[i]); file.rename(tmp, dest[i]); cat("  (test: blanked half of tile 6)\n") }
  invisible(TRUE) }, envir = globalenv())
h$finish(ctx, items)
stopifnot("the test spoiled no tile" = spoiled, "the guard did not refetch" = any(grepl("tile 6 came back", LOG)), "no counts from the neighbouring year" = any(grepl("counted from 2009", LOG)))
n <- rast(file.path(ctx$place, items$out)); stopifnot(compareGeom(g, n, stopOnError = FALSE)); va <- values(g, mat = FALSE); vn <- values(n, mat = FALSE)
cat("cells", length(va), " NA in the new mosaic", sum(is.na(vn)), " identical to the good copy", identical(as.numeric(va), as.numeric(vn)), "\n")
stopifnot("the mosaic is not the good copy" = identical(as.numeric(va), as.numeric(vn))); cat("PASSED\n")
