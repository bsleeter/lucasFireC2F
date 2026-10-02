# Stage 1 output (a result scenario) vs the validated own-draw run's C2F-W landscape (script 200 + scar rules)
suppressPackageStartupMessages({ library(rsyncrosim); library(terra) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
x <- datasheet(rsyncrosim::scenario(L, id), "lucasFireC2F_OutputEngineLandscape")
O <- "F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own/landscape"
pairs <- c(FuelFileName = "fuels.tif", ElevationFileName = "elevation.tif", CanopyBaseHeightFileName = "cbh.tif",
           CanopyBulkDensityFileName = "cbd.tif", CanopyCoverFileName = "ccf.tif", CanopyHeightFileName = "hm.tif")
ok <- TRUE
for (k in names(pairs)) { a <- rast(x[[k]][1]); b <- rast(file.path(O, pairs[[k]])); ext(b) <- ext(a)
  va <- values(a, mat = FALSE); vb <- values(b, mat = FALSE)
  nam <- sum(is.na(va) != is.na(vb)); d <- if (nam) NA else max(abs(va - vb), na.rm = TRUE); nd <- sum(abs(va - vb) > 1e-4, na.rm = TRUE)
  if (is.na(d)) { ok <- FALSE; cat(sprintf("%-26s NA cells in different places: %d\n", k, nam)); next }
  if (nd) ok <- FALSE
  cat(sprintf("%-26s max |diff| %.2e, cells differing %d  %s\n", k, d, nd, basename(x[[k]][1]))) }
cat(if (ok) "STAGE 1 REPRODUCES THE VALIDATED LANDSCAPE\n" else "STAGE 1 DIFFERS\n")
