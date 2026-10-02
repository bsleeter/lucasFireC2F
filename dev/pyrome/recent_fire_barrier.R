# Are the recent-fire rules (years since fire <= 2 -> NB9, 3-5 -> TL1, 6-10 -> TL8) walling off a zone's timber? Read-only.
#   (1) which MTBS fires make up each years-since-fire class of the prep's fire-age map, and the area / BpS vegetation it covers
#   (2) observed reburn: share of each class burned by MTBS wildfires >= 405 ha in 2021-2024, vs unburned-since-1984 ground
#   (3) the model: mean BP per class and the rule's fuel; (4) map
# Usage: Rscript recent_fire_barrier.R "<library>" <result id> <prep result id> <out dir>
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); R <- rsyncrosim::scenario(L, as.integer(a[2])); P <- rsyncrosim::scenario(L, as.integer(a[3])); OUT <- a[4]
dir.create(OUT, FALSE, TRUE); DS <- "F:/lucas-master/Data Sources"
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
lr <- g(P, "LandscapeRasters"); age <- rast(lr$FireAgeFileName[1]); bp <- rast(g(R, "OutputBurnProbability")$FileName[1]) * 100
zone <- st_transform(st_union(st_make_valid(read_sf(g(P, "OutputZoneBoundary")$FileName[1]))), crs(age)); inz <- !is.na(values(rasterize(vect(zone), age), mat = FALSE))
bl <- fread(file.path(DS, "BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/CSV_Data/LF2020_BPS.csv")); setnames(bl, 1, "VALUE")
CLS <- c("Conifer", "Hardwood", "Shrub", "Grass", "Other")
bl[, k := match(fcase(GROUPVEG %in% c("Conifer", "Conifer-Hardwood"), "Conifer", GROUPVEG %in% c("Hardwood", "Hardwood-Conifer"), "Hardwood",
                      GROUPVEG == "Shrubland", "Shrub", GROUPVEG %in% c("Grassland", "Savanna"), "Grass", default = "Other"), CLS)]
br <- rast(file.path(DS, "BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/Tif/LF2020_BPS_CONUS.tif")); br <- crop(br, terra::project(as.polygons(ext(age), crs = crs(age)), crs(br)), snap = "out")
bk <- values(terra::project(classify(br, as.matrix(bl[, .(VALUE, k)]), others = NA), age, method = "mode"), mat = FALSE)
av <- values(age, mat = FALSE); bv <- values(bp, mat = FALSE); bv[is.na(bv)] <- 0
mt <- read_sf(file.path(DS, "MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp")); names(mt) <- tolower(names(mt))
mt <- mt[mt$incid_type == "Wildfire" & mt$burnbndac * 0.404686 >= 405, ]; mt$yr <- as.integer(substr(as.character(mt$ig_date), 1, 4))
mt <- st_transform(mt, crs(age)); mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
cellsof <- function(p) { w <- which(!is.na(values(rasterize(vect(p), age, field = 1), mat = FALSE))); w[inz[w]] }
# (1) composition of each age class
cls <- c(`2` = "0-2 yr -> NB9 (non-burnable)", `5` = "3-5 yr -> TL1", `10` = "6-10 yr -> TL8")
cat("== years-since-fire classes inside the zone (the fire-age map P1 read)\n")
own <- mt[mt$yr >= 2005 & mt$yr <= 2020, ]; oc <- lapply(seq_len(nrow(own)), function(i) cellsof(own[i, ]))
for (y in c(2, 5, 10)) { w <- which(inz & av %in% y); t <- table(factor(CLS[bk[w]], CLS))
  hit <- vapply(oc, function(c) sum(c %in% w), 0)
  top <- data.table(fire = own$incid_name, yr = own$yr, cells = hit)[cells > 500][order(-cells)][seq_len(min(8, sum(hit > 500)))]
  cat(sprintf("\n%s: %s cells = %s ha; BpS %s\n  main fires: %s\n", cls[as.character(y)], format(length(w), big.mark = ","), format(round(length(w) * 2.25), big.mark = ","),
              paste(sprintf("%s %.0f%%", names(t), 100 * t / sum(t)), collapse = ", "), paste(sprintf("%s %d (%s ha)", top$fire, top$yr, format(round(top$cells * 2.25), big.mark = ",")), collapse = "; "))) }
# (2)+(3) observed reburn in 2021-24 and model BP by class
m24 <- mt[mt$yr >= 2021, ]; p24 <- unique(unlist(lapply(seq_len(nrow(m24)), function(i) cellsof(m24[i, ]))))
m84 <- mt[mt$yr >= 1984 & mt$yr <= 2020, ]; since84 <- unique(unlist(lapply(seq_len(nrow(m84)), function(i) cellsof(m84[i, ]))))
Dt <- data.table(cell = which(inz), age = av[inz], veg = CLS[bk[inz]], bp = bv[inz])
Dt[, grp := fifelse(age %in% 2, "0-2 yr (NB9)", fifelse(age %in% 5, "3-5 yr (TL1)", fifelse(age %in% 10, "6-10 yr (TL8)", fifelse(cell %in% since84, "burned 1984-2009", "unburned since 1984"))))]
Dt[, burned24 := cell %in% p24]
S <- Dt[veg %in% c("Conifer", "Hardwood"), .(cells = .N, burned_2021_24_pct = round(100 * mean(burned24), 1), model_bp = round(mean(bp), 3)), by = grp]
S[, obs_rel := round(burned_2021_24_pct / S[grp == "unburned since 1984", burned_2021_24_pct], 2)]; S[, model_rel := round(model_bp / S[grp == "unburned since 1984", model_bp], 2)]
cat("\n== forest (BpS Conifer + Hardwood) by years since fire: 2021-24 burned share (record) vs model BP, each relative to ground unburned since 1984\n"); print(S[order(grp)])
fwrite(S, file.path(OUT, "K_recent_fire_reburn.csv"))
# (4) map
png(file.path(OUT, "K_recent_fire_classes.png"), 1500, 1900, res = 150); par(mar = c(1, 1, 4, 1))
zv <- vect(zone); m <- classify(mask(crop(age, zv), zv), cbind(c(2, 5, 10), 1:3), others = NA)
plot(crop(mask(bp, zv), zv) > -1, col = "grey93", legend = FALSE, axes = FALSE, main = "Recent-fire fuel rules in the model, with 2021-2024 fires")
plot(m, col = c("#3b0f70", "#b5367a", "#fb8761"), add = TRUE, legend = FALSE)
lines(zv, lwd = .6); lines(vect(m24), col = "#1a9850", lwd = 1.6)
legend("bottomleft", c("0-2 yr -> NB9 non-burnable", "3-5 yr -> TL1", "6-10 yr -> TL8", "MTBS fires 2021-2024"), fill = c("#3b0f70", "#b5367a", "#fb8761", NA),
       border = c(1, 1, 1, NA), col = c(NA, NA, NA, "#1a9850"), lwd = c(NA, NA, NA, 2), bty = "n", cex = .9)
dev.off(); cat("wrote", file.path(OUT, "K_recent_fire_classes.png"), "\n")
