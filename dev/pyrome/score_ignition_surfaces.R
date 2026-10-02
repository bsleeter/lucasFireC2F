# Scores candidate ignition surfaces for a pyrome against the recorded origins of LARGE fires (FPA-FOD >= MINHA, 1992-2020):
# expected share of starts per stratum (cause-mixed: human x h + lightning x (1 - h), masked to zone x burnable as 158 does)
# vs the observed share; burn-weighted mean |log ratio| per dimension, and a rank AUC of the surface at the observed origins.
# Usage: Rscript score_ignition_surfaces.R <pyrome> <human share> <min ha> <tag1> [tag2 ...]   (tag = ign_*_150m_LF2020<tag>.tif)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); HS <- as.numeric(a[2]); MINHA <- as.numeric(a[3]); TAGS <- a[-(1:3)]
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, sprintf("spatial/pyromes_150m_LF2020/pyrome_%03d", PY))
fu <- rast(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/P%02d/inputs/fuel_type.tif", PY)); el <- rast(file.path(ZD, "Elevation.tif"))
zone <- st_transform(read_sf(file.path(ZD, "zone.gpkg")), crs(fu)); inz <- !is.na(values(rasterize(vect(zone), fu), mat = FALSE))
fv <- values(fu, mat = FALSE); ok <- inz & !(fv %in% c(91, 92, 93, 98, 99)) & !is.na(fv)
ev <- values(el, mat = FALSE); sl <- values(terrain(el, "slope", unit = "degrees"), mat = FALSE); yy <- yFromCell(fu, seq_len(ncell(fu))); zy <- range(yy[inz])
S <- data.table(cell = seq_len(ncell(fu)), ok = ok, elev = cut(ev, c(-Inf, 150, 300, 500, 750, 1000, Inf)), third = cut(yy, zy[1] + diff(zy) * c(-1e-6, 1/3, 2/3, 1 + 1e-6), c("south", "central", "north")),
                slope = cut(sl, c(-Inf, 5, 10, 20, 30, Inf)))
fpa <- st_transform(read_sf("F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg", query = sprintf("SELECT FIRE_SIZE, Shape FROM Fires WHERE FIRE_SIZE >= %f AND FIRE_YEAR BETWEEN 1992 AND 2020", MINHA / 0.404686)), crs(fu))
oc <- cellFromXY(fu, st_coordinates(fpa)); oc <- oc[!is.na(oc) & ok[oc]]; S[data.table(cell = oc)[, .N, by = cell], on = "cell", obs := i.N]; S[is.na(obs), obs := 0]; S <- S[ok == TRUE]
cat(sprintf("observed origins >= %g ha on burnable ground in P%d: %d\n", MINHA, PY, sum(S$obs)))
res <- list()
for (tg in TAGS) {
  rd <- function(cause) { r <- rast(file.path(M, sprintf("data/ignition/ign_%s_150m_LF2020%s.tif", cause, tg))); if (!compareGeom(r, fu, stopOnError = FALSE)) r <- resample(r, fu, "near")
    v <- values(r, mat = FALSE)[S$cell]; v[is.na(v) | v < 0] <- 0; v / sum(v) }
  S[, p := HS * rd("human") + (1 - HS) * rd("lightning")]
  auc <- { r <- rank(S$p); n1 <- sum(S$obs); r1 <- sum(r * S$obs); n0 <- nrow(S) - sum(S$obs > 0); (r1 - n1 * (n1 + 1) / 2) / (n1 * n0) }
  row <- data.table(surface = tg, AUC_at_origins = round(auc, 3))
  for (d in c("elev", "third", "slope")) { x <- S[, .(e = sum(p), o = sum(obs) / sum(S$obs)), by = d][o > 0]
    row[, (d) := round(x[, sum(o * abs(log(e / o)))], 3)]
    if (d == "third") row[, north_share := round(S[third == "north", sum(p)], 3)]
    if (d == "elev") row[, above500_share := round(S[as.integer(elev) >= 4, sum(p)], 3)] }
  res[[tg]] <- row }
obsrow <- data.table(surface = "OBSERVED", AUC_at_origins = NA, elev = 0, above500_share = round(S[as.integer(elev) >= 4, sum(obs)] / sum(S$obs), 3), third = 0,
                     north_share = round(S[third == "north", sum(obs)] / sum(S$obs), 3), slope = 0)
print(rbind(rbindlist(res), obsrow, fill = TRUE))
cat("(elev / third / slope = observed-weighted mean |log(expected / observed share)|: 0 = perfect placement)\n")
