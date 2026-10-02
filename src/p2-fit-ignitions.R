# =============================================================================
# P2 - Fit Ignition Surfaces
# =============================================================================
# Where large fires start, per cause (Human, Lightning): a binomial GAM of FPA-FOD ignition points against random background
# points - an inhomogeneous Poisson process (Warton & Shepherd 2010) - fitted to the origins of fires of at least
# ZonePrepOption.IgnitionMinimumFireSize (default 405 ha: the fires that decide burn probability, not every fire above the
# FSim threshold), IgnitionFirstYear-IgnitionLastYear, inside the zone plus IgnitionBufferKm (default 20 km: the zone's own
# regime; a 50 km ring supplied three-quarters of P26's points with the opposite slope response). Ported from the development
# script 185, with the fuel term on the LANDFIRE FBFM40 fuel groups (the package's own) instead of the legacy BurnP3 classes.
#   covariates   a prebuilt 30 m stack (Data Sources: IgnitionCovariateFolder, the California fit stack), or - anywhere in CONUS - built
#                for this zone's fit domain and grid from LANDFIRE roads, Annual NLCD 2010, SILVIS 2010 housing density, the 30 m
#                DEM, PRISM and gridMET (src/covariates.R; reproduces the California stack cell for cell):
#                distance to roads and development, developed fraction, housing density, elevation, slope, topographic position,
#                precipitation, and two lightning-season climate terms; winsorised on the background's 1st/99th percentiles
#   human        s(log dist road) + s(log dist development) + s(developed fraction) + s(housing density) + s(elevation) + s(slope)
#                + s(TPI) + s(log precipitation) + fuel group
#   lightning    s(elevation) + s(slope) + s(TPI) + s(log precipitation) + s(wet-warm) + s(warm fraction) + fuel group
#   check        spatially cross-validated AUC (10 km blocks, 5 folds) on held-out points
#   surface      the covariates averaged 5 x 5 onto the zone's 150 m grid (P1), fuel group from the zone's fuel map, predicted
#                intensity up to a constant, x burnable, zero outside the zone (by the Fire Zone grid)
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf); library(mgcv) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE); terraOptions(memfrac = 0.45, progress = 0)
progressBar(type = "begin", totalSteps = 6)
dsrc <- read_data_sources(c("FpaFodFile", "FireZoneBoundaryFile", "LandfireFolder")); zo <- need("ZonePrepOption", "FireZone")
csrc <- read_data_sources(c("IgnitionCovariateFolder", "StatewideElevationFile", "RoadsFile", "NlcdLandCoverFile", "HousingDensityFile", "PrismPrecipFile", "GridMetFolder"), optional = TRUE)
chas <- function(k) { v <- csrc[[k]]; length(v) && !is.na(v) && nzchar(v) }
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); ZID <- fzt$ID[match(zo$FireZone, fzt$Name)]
ESC <- if (!is.na(zo$IgnitionMinimumFireSize)) zo$IgnitionMinimumFireSize else 405; BUF <- 1000 * (if (!is.na(zo$IgnitionBufferKm)) zo$IgnitionBufferKm else 20)
Y1 <- if (!is.na(zo$IgnitionFirstYear)) zo$IgnitionFirstYear else 1992L; Y2 <- if (!is.na(zo$IgnitionLastYear)) zo$IgnitionLastYear else 2020L
V <- if (!is.na(zo$LandfireVintage)) as.character(zo$LandfireVintage) else "2020"
lr <- need("LandscapeRasters", c("FuelFileName", "FireZoneFileName")); gf <- rast(lr$FuelFileName[1]); fzg <- rast(lr$FireZoneFileName[1])
fm <- as.data.table(datasheet(prj, ds("FuelModel"), lookupsAsFactors = FALSE)); grp_of <- function(code) fm$FuelGroup[match(code, fm$ID)]
N_BG <- 120000L; COVS <- c("dist_road", "dist_dev", "dev_frac", "huden", "elev", "slope", "tpi", "ppt", "wet_warm", "warm_frac")
if (chas("IgnitionCovariateFolder")) {                           # a prebuilt covariate stack (the California fit stack)
  cf <- file.path(csrc$IgnitionCovariateFolder, paste0(COVS, ".tif")); if (any(!file.exists(cf))) fail("ignition covariates missing: %s", paste(basename(cf[!file.exists(cf)]), collapse = ", "))
  CRS <- crs(rast(cf[1])) } else { cf <- NULL; CRS <- crs(gf) }
z <- st_transform(zone_polygons(dsrc$FireZoneBoundaryFile), CRS); z <- z[z$ID == ZID, ]; if (!nrow(z)) fail("no boundary polygon with ID %d", ZID)
dom <- st_buffer(st_union(z), BUF)
if (is.null(cf)) {                                                # anywhere in CONUS: built for the fit domain + the zone grid (src/covariates.R)
  for (k in c("StatewideElevationFile", "RoadsFile", "NlcdLandCoverFile", "HousingDensityFile", "PrismPrecipFile", "GridMetFolder"))
    if (!chas(k) || !file.exists(csrc[[k]])) fail("Data Sources: no Ignition Covariate Folder, so %s is needed to build the covariates (%s)", k, csrc[[k]])
  if (abs(res(rast(csrc$StatewideElevationFile))[1] - 30) > 1e-6) fail("Data Sources: building the ignition covariates needs the 30 m DEM as the elevation")
  source(file.path(env$PackageDirectory, "src", "covariates.R"))
  b <- st_bbox(dom); g <- as.vector(ext(gf)); RES <- 150
  win <- ext(LATTICE_X0 + floor((min(b[["xmin"]], g[1]) - LATTICE_X0) / RES) * RES, LATTICE_X0 + ceiling((max(b[["xmax"]], g[2]) - LATTICE_X0) / RES) * RES,
             LATTICE_Y0 - ceiling((LATTICE_Y0 - min(b[["ymin"]], g[3])) / RES) * RES, LATTICE_Y0 - floor((LATTICE_Y0 - max(b[["ymax"]], g[4])) / RES) * RES)
  say("building the ignition covariates for %.0f x %.0f km (fit domain + zone grid)", (win[2] - win[1]) / 1000, (win[4] - win[3]) / 1000)
  cf <- build_ignition_covariates(win, list(dem = csrc$StatewideElevationFile, roads = csrc$RoadsFile, nlcd = csrc$NlcdLandCoverFile, wui = csrc$HousingDensityFile,
                                            prism = csrc$PrismPrecipFile, gridmet = csrc$GridMetFolder), file.path(tempdir(), "ignition_covariates"))
}
say("%s: fit domain = zone + %.0f km, fires >= %g ha, %d-%d; fuel term = LANDFIRE %s FBFM40 groups", zo$FireZone, BUF / 1000, ESC, Y1, Y2, V)
lff <- landfire_file(dsrc$LandfireFolder, V, "FBFM40")
e <- ext(vect(dom)); cov_d <- crop(rast(cf), e); names(cov_d) <- COVS
tmpl_d <- crop(rast(lff), project(e, from = CRS, to = crs(rast(lff))), snap = "out"); levels(tmpl_d) <- NULL
progressBar()

# ---- presences and background --------------------------------------------------------------------------------------------------
ign <- read_sf(dsrc$FpaFodFile, query = sprintf("SELECT FIRE_YEAR, FIRE_SIZE, NWCG_CAUSE_CLASSIFICATION AS CAUSE, Shape FROM Fires WHERE FIRE_YEAR >= %d AND FIRE_YEAR <= %d AND FIRE_SIZE >= %f",
                                                  Y1, Y2, ESC / 0.404686))
ign <- st_transform(ign, CRS); ign <- ign[st_within(ign, dom, sparse = FALSE)[, 1], ]
ign$Cause <- ifelse(ign$CAUSE == "Natural", "Lightning", ifelse(ign$CAUSE == "Human", "Human", NA)); ign <- ign[!is.na(ign$Cause), ]
say("presences in the domain: %d (%d human, %d lightning); in the zone itself: %d", nrow(ign), sum(ign$Cause == "Human"), sum(ign$Cause == "Lightning"),
    sum(st_within(ign, st_union(z), sparse = FALSE)[, 1]))
set.seed(1)
bgx <- spatSample(cov_d[["elev"]], N_BG * 3, "random", xy = TRUE, na.rm = TRUE, values = FALSE)
bgx <- bgx[st_within(st_as_sf(as.data.frame(bgx), coords = c("x", "y"), crs = CRS), dom, sparse = FALSE)[, 1], ]
xy <- st_coordinates(ign)
pts <- rbind(data.frame(x = xy[, 1], y = xy[, 2], y_ = 1, Cause = ign$Cause), data.frame(x = bgx[, 1], y = bgx[, 2], y_ = 0, Cause = NA))
fuel_at <- function(p) terra::extract(tmpl_d, project(vect(p, geom = c("x", "y"), crs = CRS), crs(tmpl_d)))[, 2]
d <- cbind(pts, terra::extract(cov_d, as.matrix(pts[, c("x", "y")]))); d$fuel <- fuel_at(pts[, c("x", "y")])
d <- d[!(d$fuel %in% 91:99) & !is.na(d$fuel), ]
d <- rbind(d[d$y_ == 1, ], head(d[d$y_ == 0, ], N_BG))
say("background cells: %s (burnable)", format(sum(d$y_ == 0), big.mark = ","))
decode <- function(df) { df$dev_frac <- df$dev_frac / 1000; df$huden <- df$huden / 100; df$slope <- df$slope / 10
  df$wet_warm <- df$wet_warm / 100; df$warm_frac <- df$warm_frac / 10000
  df$ldist_road <- log1p(df$dist_road); df$ldist_dev <- log1p(df$dist_dev); df$lppt <- log1p(df$ppt); df }
d <- decode(d); d <- d[complete.cases(d[, c(COVS, "fuel")]), ]; d$fgrp <- factor(grp_of(d$fuel)); d <- d[!is.na(d$fgrp), ]
CONT <- c("ldist_road", "ldist_dev", "dev_frac", "huden", "elev", "slope", "tpi", "lppt", "wet_warm", "warm_frac")
qs <- lapply(setNames(CONT, CONT), function(v) quantile(d[[v]][d$y_ == 0], c(0.01, 0.99), na.rm = TRUE))
for (v in CONT) d[[v]] <- pmin(pmax(d[[v]], qs[[v]][1]), qs[[v]][2])
d$blk <- paste(floor(d$x / 10000), floor(d$y / 10000)); ub <- unique(d$blk); set.seed(2); d$fold <- setNames(sample(rep_len(1:5, length(ub))), ub)[d$blk]
progressBar()

# ---- fits, with spatial cross-validation -----------------------------------------------------------------------------------------
F <- list(Human = y_ ~ s(ldist_road, k = 5) + s(ldist_dev, k = 5) + s(dev_frac, k = 4) + s(huden, k = 5) + s(elev, k = 6) + s(slope, k = 4) + s(tpi, k = 4) + s(lppt, k = 4) + fgrp,
          Lightning = y_ ~ s(elev, k = 6) + s(slope, k = 4) + s(tpi, k = 4) + s(lppt, k = 4) + s(wet_warm, k = 4) + s(warm_frac, k = 4) + fgrp)
auc <- function(p, y) { r <- rank(p); n1 <- as.numeric(sum(y == 1)); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
fits <- list(); fitrow <- list()
for (cz in names(F)) { sub <- d[d$y_ == 0 | d$Cause %in% cz, ]; sub$fgrp <- droplevels(sub$fgrp); av <- c()
  if (sum(sub$y_) < 20) fail("%s: only %d presences - lower Ignition Fit: Minimum Fire Size or widen the buffer", cz, sum(sub$y_))
  for (k in 1:5) { tr <- sub[sub$fold != k, ]; te <- sub[sub$fold == k, ]; if (!sum(te$y_)) next
    m <- try(bam(F[[cz]], data = tr, family = binomial, discrete = TRUE, select = TRUE), silent = TRUE); if (inherits(m, "try-error")) next
    te$fgrp <- factor(as.character(te$fgrp), levels = levels(tr$fgrp)); te <- te[!is.na(te$fgrp), ]; av <- c(av, auc(as.numeric(predict(m, te, type = "link")), te$y_)) }
  fits[[cz]] <- bam(F[[cz]], data = sub, family = binomial, discrete = TRUE, select = TRUE)
  say("%s: %d presences; spatially cross-validated AUC %.3f (folds %s)", cz, sum(sub$y_), mean(av), paste(sprintf("%.2f", av), collapse = " "))
  fitrow[[cz]] <- data.frame(Cause = cz, Presences = sum(sub$y_), Background = sum(sub$y_ == 0), MinimumFireSize = ESC, BufferKm = BUF / 1000,
                             FirstYear = Y1, LastYear = Y2, CrossValidatedAUC = mean(av))
  progressBar() }

# ---- predict onto the zone's 150 m grid, mask, write ----------------------------------------------------------------------------
c150 <- resample(aggregate(crop(rast(cf), ext(gf)), 5, mean, na.rm = TRUE), gf, method = "near"); names(c150) <- COVS
nd <- decode(as.data.frame(c150, cells = TRUE, na.rm = FALSE)); gv <- values(gf, mat = FALSE); nd$fgrp <- grp_of(gv[nd$cell])
for (v in CONT) nd[[v]] <- pmin(pmax(nd[[v]], qs[[v]][1]), qs[[v]][2])
mask <- !(gv %in% 91:99) & !is.na(gv) & values(fzg, mat = FALSE) %in% ZID
TD <- env$TransferDirectory; out <- c()
for (cz in names(F)) { m <- fits[[cz]]; n2 <- nd; n2$fgrp <- factor(as.character(n2$fgrp), levels = levels(m$model$fgrp))
  ok <- complete.cases(n2[, intersect(all.vars(delete.response(terms(m))), names(n2))]); eta <- rep(NA_real_, nrow(n2)); eta[ok] <- as.numeric(predict(m, n2[ok, ], type = "link"))
  v <- exp(eta - max(eta, na.rm = TRUE)); v[is.na(v) | !mask[n2$cell]] <- 0; r <- rast(gf); values(r) <- v
  out[cz] <- file.path(TD, sprintf("ignition_%s.tif", tolower(cz))); writeRaster(r, out[cz], overwrite = TRUE, datatype = "FLT8S", gdal = "COMPRESS=DEFLATE")
  say("%s surface: %s nonzero cells", cz, format(sum(v > 0), big.mark = ",")) }
write_ds(data.frame(Cause = names(out), FileName = unname(out)), "IgnitionGrid")
write_ds(do.call(rbind, fitrow), "OutputIgnitionFit")
progressBar(); progressBar(type = "end")
