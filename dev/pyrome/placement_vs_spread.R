# Where does a pyrome result put its fire wrong - ignition PLACEMENT or SPREAD? (the P26 190 method, generalised)
# For large fires (>= MINHA, default 405 ha: MTBS maps nothing smaller), 1992-2020, per stratum s of the zone's burnable ground:
#   ignition share   I_s   model: origins of kept fires >= MINHA (fire list + fire statistics of the result)
#                          observed: FPA-FOD origins of fires >= MINHA in the zone
#   burned share     B_s   model: burns of kept fires >= MINHA (burn table) per cell, summed
#                          observed: MTBS wildfire perimeters (years 1992-2020), cell x years burned
#   spread factor    A_s = B_s / I_s  (ground a stratum ends up with per fire that starts in it)
#   error            B_model / B_obs = (I_model / I_obs) x (A_model / A_obs): placement and spread multiply, so their logs add;
#                    each one's share of |log error| is reported. Strata: elevation band, north-south third, slope class.
# Usage: Rscript placement_vs_spread.R <pyrome> <result id> [min ha = 405] [out png]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(arrow); library(dplyr); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); RID <- as.integer(a[2]); MINHA <- if (length(a) > 2) as.numeric(a[3]) else 405
PNG <- if (length(a) > 3) a[4] else sprintf("F:/lucasFireC2F/dev/pyrome/Fig_P%02d_placement_spread_r%d.png", PY, RID)
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, sprintf("spatial/pyromes_150m_LF2020/pyrome_%03d", PY))
L <- suppressWarnings(ssimLibrary(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P%02d.ssim", PY))); R <- rsyncrosim::scenario(L, RID)
g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
fu <- rast(g("OutputEngineLandscape")$FuelFileName[1]); el <- rast(file.path(ZD, "Elevation.tif")); if (!compareGeom(el, fu, stopOnError = FALSE)) ext(el) <- ext(fu)
zone <- st_transform(read_sf(file.path(ZD, "zone.gpkg")), crs(fu)); inz <- !is.na(values(rasterize(vect(zone), fu), mat = FALSE))
fv <- values(fu, mat = FALSE); burnable <- inz & !(fv %in% c(91, 92, 93, 98, 99)) & !is.na(fv)
ev <- values(el, mat = FALSE); sl <- values(terrain(el, "slope", unit = "degrees"), mat = FALSE); yy <- yFromCell(fu, seq_len(ncell(fu)))
zy <- range(yy[inz]); third <- cut(yy, zy[1] + diff(zy) * c(-1e-6, 1/3, 2/3, 1 + 1e-6), c("south", "central", "north"))
ebr <- c(-Inf, 150, 300, 500, 750, 1000, Inf); elab <- c("<150 m", "150-300", "300-500", "500-750", "750-1000", ">1000")
S <- data.table(cell = seq_len(ncell(fu)), ok = burnable, elev = cut(ev, ebr, elab), third = third, slope = cut(sl, c(-Inf, 5, 10, 20, 30, Inf), c("<5 deg", "5-10", "10-20", "20-30", ">30")))

# ---- model: kept fires >= MINHA, their origins and their burns -------------------------------------------------------------
st <- g("OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINHA]
st[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
fl <- g("OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell)]
st <- fl[st, on = c("bI", "bF")]; NS <- g("RunControl")$MaximumIteration
bt <- as.data.table(open_dataset(g("OutputBurnTable")$FileName[1]) |> select(Iteration, FireID, Cell) |> collect())[st[, .(Iteration = bI, FireID = bF)], on = c("Iteration", "FireID"), nomatch = 0]
mb <- bt[, .N, by = Cell]; mi <- st[, .N, by = .(Cell)]
cat(sprintf("model: %s kept fires >= %g ha in %d seasons (%.2f/yr), %s burns\n", format(nrow(st), big.mark = ","), MINHA, NS, nrow(st) / NS, format(nrow(bt), big.mark = ",")))

# ---- observed: FPA-FOD origins and MTBS burned cells, 1992-2020 --------------------------------------------------------------
fpa <- read_sf("F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg", query = sprintf("SELECT FIRE_SIZE, Shape FROM Fires WHERE FIRE_SIZE >= %f AND FIRE_YEAR BETWEEN 1992 AND 2020", MINHA / 0.404686))
fpa <- st_transform(fpa, crs(fu)); oc <- cellFromXY(fu, st_coordinates(fpa)); oc <- oc[!is.na(oc) & inz[oc]]; oi <- data.table(Cell = oc)[, .N, by = Cell]
source(file.path(M, "src/00_config.R")); mt <- read_mtbs(); mt <- mt[mt$Incid_Type == "Wildfire" & !is.na(mt$Ig_Date), ]; mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y"))
mt <- st_transform(mt[mt$yr >= 1992 & mt$yr <= 2020 & mt$BurnBndAc * 0.404686 >= MINHA, ], crs(fu)); mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
ob <- rbindlist(lapply(split(seq_len(nrow(mt)), mt$yr), function(k) { r <- rasterize(vect(mt[k, ]), fu, field = 1); data.table(Cell = which(!is.na(values(r, mat = FALSE)))) }))[, .N, by = Cell]
cat(sprintf("observed: %d FPA-FOD origins >= %g ha in the zone (%.2f/yr); %d MTBS perimeters\n", length(oc), MINHA, length(oc) / 29, nrow(mt)))

S[mi, on = c(cell = "Cell"), mI := i.N]; S[mb, on = c(cell = "Cell"), mB := i.N]; S[oi, on = c(cell = "Cell"), oI := i.N]; S[ob, on = c(cell = "Cell"), oB := i.N]
for (v in c("mI", "mB", "oI", "oB")) set(S, which(is.na(S[[v]])), v, 0)
S <- S[ok == TRUE]
dec <- function(by) { d <- S[, .(ground = .N, mI = sum(mI), mB = sum(mB), oI = sum(oI), oB = sum(oB)), by = by][order(get(by))]
  d[, `:=`(ground = ground / sum(ground), mI = mI / sum(mI), mB = mB / sum(mB), oI = oI / sum(oI), oB = oB / sum(oB))]
  d[, `:=`(burned_ratio = mB / oB, placement = mI / oI, spread = (mB / mI) / (oB / oI))]
  d[, place_share := abs(log(placement)) / (abs(log(placement)) + abs(log(spread)))]; d }
out <- list()
for (by in c("elev", "third", "slope")) { d <- dec(by); setnames(d, by, "stratum"); d[, dimension := by]; out[[by]] <- d
  cat(sprintf("\n== %s (shares of zone burnable ground; ratios model / observed)\n", toupper(by)))
  print(d[, .(stratum, ground = round(ground, 3), ign_obs = round(oI, 3), ign_model = round(mI, 3), burn_obs = round(oB, 3), burn_model = round(mB, 3),
              burned_ratio = round(burned_ratio, 2), placement = round(placement, 2), spread = round(spread, 2), placement_share = round(place_share, 2))]) }
out <- rbindlist(out); fwrite(out, sub("\\.png$", ".csv", PNG))
w <- out[is.finite(placement) & is.finite(spread) & placement > 0 & spread > 0, .(err = sum(oB * abs(log(burned_ratio)), na.rm = TRUE), pl = sum(oB * abs(log(placement)), na.rm = TRUE), sp = sum(oB * abs(log(spread)), na.rm = TRUE)), by = dimension]
cat("\nburn-weighted |log error| split:\n"); print(w[, .(dimension, placement_share = round(pl / (pl + sp), 2), spread_share = round(sp / (pl + sp), 2))])

png(PNG, 2700, 1000, res = 170); par(mfrow = c(1, 3), mar = c(7, 4.5, 3.5, 1))
for (by in c("elev", "third", "slope")) { d <- out[dimension == by]
  m <- rbind(Placement = d$placement, Spread = d$spread, Burned = d$burned_ratio); m[!is.finite(m)] <- NA
  b <- barplot(log2(m), beside = TRUE, names.arg = d$stratum, las = 2, col = c("#4c78a8", "#f58518", "#54a24b"), ylim = c(-3, 3), yaxt = "n",
               main = c(elev = "(a) Elevation", third = "(b) North-south third", slope = "(c) Slope")[by], ylab = "model / observed")
  axis(2, at = -3:3, labels = c("1/8", "1/4", "1/2", "1", "2", "4", "8"), las = 1); abline(h = 0)
  if (by == "elev") legend("topright", c("ignition placement", "spread per fire", "burned area (product)"), fill = c("#4c78a8", "#f58518", "#54a24b"), bty = "n", cex = .85) }
mtext(sprintf("P%d result %d: large fires >= %g ha, 1992-2020; model vs FPA-FOD origins and MTBS burned area", PY, RID, MINHA), outer = TRUE, line = -1.5, font = 2)
dev.off(); cat("wrote", PNG, "\n")
