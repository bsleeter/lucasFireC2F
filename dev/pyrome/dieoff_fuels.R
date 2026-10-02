# Are LANDFIRE fuels in insect die-off stands different from comparable stands without mortality? (P27, 150 m model grid)
# Die-off = LANDFIRE insect/disease mortality share >= 0.5 of the cell (beetle_kill_test.R raster); control = forest cells with no
# mortality. Both restricted to cells not burned since 2014 (fire age), and the control reweighted to the die-off cells' 100 m
# elevation mix. Compares: LF2020 FBFM40 (the model's fuel, before the recent-fire rules), FBFM13 in LF2016 vs LF2020 (did LANDFIRE
# change the class after the mortality?), LF2022 FBFM40, canopy cover / bulk density / base height, and the Rothermel rate of the
# mapped models (D1L1, 8.5 km/h taken as midflame, C2F-W's convention).
# Usage: Rscript dieoff_fuels.R "<library>" <prep result id> <mortality tif> <out dir>
suppressPackageStartupMessages({ library(terra); library(data.table); library(rsyncrosim) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::scenario(L, as.integer(a[2])); MT <- a[3]; OUT <- a[4]
g <- function(n) as.data.table(datasheet(P, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
lr <- g("LandscapeRasters"); cr <- g("CanopyRasters"); PR <- rsyncrosim::project(L, 1)
fm <- as.data.table(datasheet(PR, "lucasFireC2F_FuelModel", lookupsAsFactors = FALSE))
f20 <- rast(lr$FuelFileName[1]); el <- rast(lr$ElevationFileName[1]); age <- rast(lr$FireAgeFileName[1]); mort <- rast(MT)
LF <- "F:/lucas-master/Data Sources/FBFM"
one <- function(dir) { f <- list.files(file.path(LF, dir), "[.]tif$", recursive = TRUE, full.names = TRUE); f[!grepl("ovr|aux", f)][1] }
to150 <- function(dir) { r <- rast(one(dir)); r <- crop(r, terra::project(as.polygons(ext(f20), crs = crs(f20)), crs(r)), snap = "out"); terra::project(r, f20, method = "mode") }
f13_16 <- to150("LF2016_FBFM13_CONUS"); f13_20 <- to150("LF2020_FBFM13_CONUS"); f40_22 <- to150("LF2022_FBFM40_CONUS")
v <- function(r) values(r, mat = FALSE)
D <- data.table(f40 = v(f20), elev = v(el), age = v(age), mort = v(mort), f13a = v(f13_16), f13b = v(f13_20), f22 = v(f40_22),
                cc = v(rast(cr$CanopyCoverFileName[1])), cbd = v(rast(cr$CanopyBulkDensityFileName[1])), cbh = v(rast(cr$CanopyBaseHeightFileName[1])))
D[, grp := fm$FuelGroup[match(f40, fm$ID)]]; D[is.na(mort), mort := 0]
D <- D[!is.na(f40) & !is.na(elev) & (is.na(age) | age > 6) & !is.na(f13a) & !is.na(f13b)]   # fire age in years before 2020: > 6 = not burned since 2014
D[, set := fifelse(mort >= 0.5, "die-off", fifelse(mort == 0 & cc > 0, "control", NA_character_))]; D <- D[!is.na(set)]
D[, eb := floor(elev / 100)]; w <- D[set == "die-off", .N, by = eb][, p := N / sum(N)]
D[set == "control", wt := w$p[match(eb, w$eb)] / (.N / sum(D$set == "control")), by = eb]; D[set == "die-off", wt := 1]; D <- D[!is.na(wt)]
cat(sprintf("cells: die-off %s, control %s (elevation-weighted); die-off elevation median %.0f m\n",
            format(sum(D$set == "die-off"), big.mark = ","), format(sum(D$set == "control"), big.mark = ","), D[set == "die-off", median(elev)]))
share <- function(col) { x <- D[, .(w = sum(wt)), by = c("set", col)][, share := round(w / sum(w), 3), by = set]
  dcast(x, as.formula(paste(col, "~ set")), value.var = "share", fill = 0)[order(-`die-off`)] }
nm40 <- function(x) fm$Name[match(x, fm$ID)]
cat("\n== LF2020 FBFM40 (the model's fuel) - share of cells\n"); s40 <- share("f40"); s40[, model := nm40(f40)]; print(head(s40[, .(model, `die-off`, control)], 12))
cat("\n== FBFM13 LF2016 -> LF2020: share of cells whose class CHANGED, and the main transitions\n")
print(D[, .(changed = round(sum(wt * (f13a != f13b)) / sum(wt), 3)), by = set])
tr <- D[f13a != f13b, .(w = sum(wt)), by = .(set, from = f13a, to = f13b)][, share_of_set := round(w / D[, sum(wt), by = set]$V1[match(set, D[, unique(set)])], 3)]
print(tr[order(set, -w)][, head(.SD, 6), by = set][, .(set, from, to, share_of_set)])
cat("\n== FBFM13 class shares, LF2016 and LF2020\n"); print(merge(share("f13a"), share("f13b"), by.x = "f13a", by.y = "f13b", suffixes = c("_2016", "_2020"))[order(-`die-off_2020`)])
cat("\n== canopy (LF2020): weighted means\n"); print(D[, .(cc = round(weighted.mean(cc, wt, na.rm = TRUE), 1), cbd = round(weighted.mean(cbd, wt, na.rm = TRUE), 3),
                                                     cbh = round(weighted.mean(cbh, wt, na.rm = TRUE), 2)), by = set])
cat("\n== LF2022 FBFM40 in the same unburned cells\n"); s22 <- share("f22"); s22[, model := nm40(f22)]; print(head(s22[, .(model, `die-off`, control)], 10))
source("F:/lucasFireC2F/src/rothermel.R")
rate <- sapply(fm$ID, function(k) { p <- fm[ID == k]; mm <- c(3, 4, 5, 30, 60); tryCatch(rothermel(p, mm, c(8.5, 8.5))$R[1], error = function(e) NA_real_) }); names(rate) <- fm$ID
D[, ros := rate[as.character(f40)]]; D[, ros22 := rate[as.character(f22)]]
cat("\n== Rothermel rate of the mapped model (m/min, D1L1, 8.5 km/h as midflame): weighted mean / median\n")
print(D[!is.na(ros), .(lf2020_mean = round(weighted.mean(ros, wt), 2), lf2020_median = round(median(ros), 2),
                       lf2022_mean = round(weighted.mean(ros22, wt, na.rm = TRUE), 2)), by = set])
fwrite(s40, file.path(OUT, "H_dieoff_fbfm40.csv")); fwrite(tr, file.path(OUT, "H_dieoff_fbfm13_transitions.csv"))
cat("\n== timber stands only (LF2020 group Timber-Understory or Timber Litter)\n")
T <- D[grp %in% c("Timber-Understory", "Timber Litter")]
print(T[, .(cells = .N, ros_mean = round(weighted.mean(ros, wt), 2), ros_median = round(median(ros), 2), TU_share = round(sum(wt * (grp == "Timber-Understory")) / sum(wt), 2),
            cbd = round(weighted.mean(cbd, wt, na.rm = TRUE), 3), cc = round(weighted.mean(cc, wt, na.rm = TRUE), 1), cbh = round(weighted.mean(cbh, wt, na.rm = TRUE), 2)), by = set])
cat("slash-blowdown (SB1-SB4, heavy dead and down) share of cells: LF2020\n"); print(D[, .(SB_2020 = round(sum(wt * (f40 %in% 201:204)) / sum(wt), 4), SB_2022 = round(sum(wt * (f22 %in% 201:204), na.rm = TRUE) / sum(wt), 4)), by = set])
sbr <- sapply(c("SB1","SB2","SB3","SB4","TU5","TL3","TL8"), function(n) { p <- fm[Name == n]; rothermel(p, c(3,4,5,30,60), c(8.5,8.5))$R[1] }); cat("\nRothermel reference rates:\n"); print(round(sbr, 2))
