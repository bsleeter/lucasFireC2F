# Do stage 1's canopy grids and the validated run's print differently at C2F-W's 3-decimal ASCII precision?
suppressPackageStartupMessages({ library(rsyncrosim); library(terra) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
x <- datasheet(rsyncrosim::scenario(L, id), "lucasFireC2F_OutputEngineLandscape")
O <- "F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own/landscape"
for (p in list(c("CanopyBaseHeightFileName", "cbh.tif"), c("CanopyBulkDensityFileName", "cbd.tif"), c("CanopyHeightFileName", "hm.tif"))) {
  a <- values(rast(x[[p[1]]][1]), mat = FALSE); b <- values(rast(file.path(O, p[2])), mat = FALSE)
  k <- !is.na(a) & !is.na(b)
  fa <- formatC(a[k], format = "f", digits = 3, drop0trailing = TRUE); fb <- formatC(b[k], format = "f", digits = 3, drop0trailing = TRUE)
  cat(sprintf("%-26s cells printing differently: %d of %d; own-run file type %s\n", p[1], sum(fa != fb), sum(k), datatype(rast(file.path(O, p[2])))))
}
