# Does 2012-2019 insect/disease mortality explain where a zone result misses the fire that burned after the fuel vintage?
# Mortality: LANDFIRE annual disturbance (30 m), DIST_TYPE Insects, Disease or Insects/Disease (not Insecticide), any severity,
# years FIRST-LAST; aggregated to the model's 150 m grid as the share of 30 m cells ever disturbed (and the share at High severity).
# Tests on the zone's burnable ground: (1) 2021-24 burned share by mortality class, timber only and all; (2) model and benchmark BP
# by mortality class; (3) logistic regression burned ~ logit(BP percentile) [+ mortality], AUC with and without, per map.
# Usage: Rscript beetle_kill_test.R "<library>" <result id> <prep result id> <out dir> [first = 2012] [last = 2019]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); LIBF <- a[1]; RID <- as.integer(a[2]); PID <- as.integer(a[3]); OUT <- a[4]; dir.create(OUT, FALSE, TRUE)
Y1 <- if (length(a) > 4) as.integer(a[5]) else 2012L; Y2 <- if (length(a) > 5) as.integer(a[6]) else 2019L
DD <- "F:/lucas-master/Data Sources/Landfire Disturbance"; TYPES <- c("Insects", "Disease", "Insects/Disease")
L <- suppressWarnings(ssimLibrary(LIBF)); R <- rsyncrosim::scenario(L, RID); P <- rsyncrosim::scenario(L, PID); PR <- rsyncrosim::project(L, 1)
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
dsrc <- as.data.table(datasheet(PR, "lucasFireC2F_DataSource", optional = TRUE))
bp <- rast(g(R, "OutputBurnProbability")$FileName[1]) * 100; fu <- rast(g(R, "OutputEngineLandscape")$LandscapeFuelFileName[1])
wrc <- terra::project(rast(dsrc$WrcBurnProbabilityFile[1]), bp, method = "near") * 100
zone <- st_transform(st_union(st_make_valid(read_sf(g(P, "OutputZoneBoundary")$FileName[1]))), crs(bp)); zv <- vect(zone)
fm <- g(PR, "FuelModel"); grp <- fm$FuelGroup[match(values(fu, mat = FALSE), fm$ID)]
inz <- !is.na(values(rasterize(zv, bp), mat = FALSE)); ok <- inz & !is.na(grp)
timber <- grp %in% c("Timber-Understory", "Timber Litter")

# ---- mortality share per 150 m cell ------------------------------------------------------------------------------------------
any30 <- NULL; hi30 <- NULL
for (y in Y1:Y2) { tf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "\\.tif$", recursive = TRUE, full.names = TRUE); tf <- tf[!grepl("ovr|aux", tf)][1]
  cf <- list.files(file.path(DD, sprintf("US_DIST%d", y)), "\\.csv$", recursive = TRUE, full.names = TRUE); cf <- cf[grepl("CSV_Data", cf)][1]
  lut <- fread(cf); setnames(lut, toupper(names(lut)))
  codes <- lut[DIST_TYPE %in% TYPES]$VALUE; hic <- lut[DIST_TYPE %in% TYPES & SEVERITY == "High"]$VALUE
  r <- rast(tf); r <- crop(r, terra::project(zv, crs(r)), snap = "out")
  a1 <- r %in% codes; h1 <- r %in% hic
  any30 <- if (is.null(any30)) a1 else (any30 | a1); hi30 <- if (is.null(hi30)) h1 else (hi30 | h1)
  cat(sprintf("%d: %d insect/disease codes; %s disturbed 30 m cells in the zone window\n", y, length(codes), format(global(a1, "sum", na.rm = TRUE)[1, 1], big.mark = ","))) }
mort <- terra::project(as.numeric(any30), bp, method = "average"); mhi <- terra::project(as.numeric(hi30), bp, method = "average")
mv <- values(mort, mat = FALSE); mv[is.na(mv)] <- 0; hv <- values(mhi, mat = FALSE); hv[is.na(hv)] <- 0
writeRaster(mort, file.path(OUT, sprintf("mortality_share_%d_%d.tif", Y1, Y2)), overwrite = TRUE)

# ---- 2021-24 burned cells ----------------------------------------------------------------------------------------------------
mt <- read_sf(dsrc$MtbsPerimeterFile[1]); names(mt) <- tolower(names(mt))
mt <- mt[mt$incid_type == "Wildfire" & mt$burnbndac * 0.404686 >= 405, ]; mt$yr <- as.integer(substr(as.character(mt$ig_date), 1, 4))
mt <- st_transform(mt[mt$yr >= 2021 & mt$yr <= 2024, ], crs(bp)); mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
burn <- !is.na(values(rasterize(vect(mt), bp, field = 1), mat = FALSE))
bv <- values(bp, mat = FALSE); bv[is.na(bv)] <- 0; wv <- values(wrc, mat = FALSE)
pct <- function(v) { e <- ecdf(v[ok & !is.na(v)]); r <- rep(NA_real_, length(v)); r[ok] <- e(v[ok]); r }
D <- data.table(burn, mort = mv, mhi = hv, timber, grp, bpp = pct(bv), wpp = pct(wv), bp = bv, wrc = wv)[ok]
D[, mclass := cut(mort, c(-Inf, 0, 0.1, 0.3, 0.6, Inf), c("none", "0-10%", "10-30%", "30-60%", ">60%"))]

cat(sprintf("\nzone burnable cells %s; timber %s; timber with any mortality %.1f%%, >30%% of the cell %.1f%%\n", format(nrow(D), big.mark = ","),
            format(sum(D$timber), big.mark = ","), 100 * D[timber == TRUE, mean(mort > 0)], 100 * D[timber == TRUE, mean(mort > 0.3)]))
tab <- function(d) d[, .(cells = .N, burned_2021_24 = round(100 * mean(burn), 1), model_bp = round(mean(bp), 2), wrc_bp = round(mean(wrc, na.rm = TRUE), 2),
                        model_pct = round(100 * mean(bpp), 0), wrc_pct = round(100 * mean(wpp, na.rm = TRUE), 0)), by = mclass][order(mclass)]
cat("\n== (1,2) timber: 2021-24 burned % and each map's mean BP (%/yr) and mean percentile by mortality class\n"); T1 <- tab(D[timber == TRUE]); print(T1)
cat("\n== all burnable ground\n"); T2 <- tab(D); print(T2)
cat("\nfuel group mix by mortality class (timber cells are the LF2020 fuels after the recent-fire rules):\n")
print(dcast(D[, .N, by = .(mclass, grp)][, share := round(N / sum(N), 2), by = mclass], mclass ~ grp, value.var = "share", fill = 0))

# ---- (3) does mortality add information beyond each map? ---------------------------------------------------------------------
auc <- function(s, y) { r <- rank(s); n1 <- as.numeric(sum(y)); n0 <- as.numeric(length(y)) - n1; (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
lg <- function(p) qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))
res <- rbindlist(lapply(list(c("model", "bpp"), c("WRC", "wpp")), function(m) { d <- D[!is.na(get(m[2]))]
  f0 <- glm(burn ~ lg(get(m[2])), binomial, d); f1 <- glm(burn ~ lg(get(m[2])) + mort, binomial, d); f2 <- glm(burn ~ lg(get(m[2])) + mort + mhi, binomial, d)
  data.table(map = m[1], auc_map = auc(d[[m[2]]], d$burn), auc_plus_mort = auc(predict(f1), d$burn), auc_plus_mort_hi = auc(predict(f2), d$burn),
             mort_coef = coef(f1)[["mort"]], mort_odds_x_per_10pct = exp(0.1 * coef(f1)[["mort"]]), dAIC = AIC(f0) - AIC(f1)) }))
res[, c("auc_map", "auc_plus_mort", "auc_plus_mort_hi", "mort_coef", "mort_odds_x_per_10pct", "dAIC") := lapply(.SD, round, 3), .SDcols = c("auc_map", "auc_plus_mort", "auc_plus_mort_hi", "mort_coef", "mort_odds_x_per_10pct", "dAIC")]
cat("\n== (3) 2021-24 burned ~ map percentile [+ mortality share], 150 m burnable cells (AUC here is on 150 m cells, not C7's 270 m)\n"); print(res)
cat("mortality alone, AUC:", round(auc(D$mort + 1e-9 * runif(nrow(D)), D$burn), 3), "\n")
fwrite(T1, file.path(OUT, "G_mortality_timber.csv")); fwrite(res, file.path(OUT, "G_mortality_regression.csv"))

png(file.path(OUT, "G_mortality_vs_bp.png"), 2400, 1500, res = 150); par(mfrow = c(1, 3), mar = c(1, 1, 4, 1), oma = c(3, 0, 1, 0))
br <- c(0, 0.1, 0.2, 0.35, 0.5, 0.7, 1, 1.5, 2, 3, 100); pal <- hcl.colors(10, "YlOrRd", rev = TRUE); pv <- vect(mt)
plot(crop(mask(mort, zv), zv), breaks = c(0, 0.001, 0.1, 0.3, 0.6, 1), col = c("grey92", hcl.colors(4, "Greens", rev = TRUE)), axes = FALSE, legend = "bottomleft",
     main = sprintf("(a) Insect/disease mortality %d-%d (share of cell)", Y1, Y2)); lines(zv, lwd = .6); lines(pv, col = "#c0282d", lwd = 1.2)
plot(crop(mask(bp, zv), zv), breaks = br, col = pal, axes = FALSE, legend = FALSE, main = "(b) lucasFireC2F BP (result)"); lines(zv, lwd = .6); lines(pv, col = "#1f5fbf", lwd = 1.2)
plot(crop(mask(wrc, zv), zv), breaks = br, col = pal, axes = FALSE, legend = FALSE, main = "(c) WRC V2 BP"); lines(zv, lwd = .6); lines(pv, col = "#1f5fbf", lwd = 1.2)
mtext("Outlines: MTBS wildfires >= 405 ha, 2021-2024", side = 1, outer = TRUE, line = 1); dev.off(); cat("wrote", OUT, "\n")
