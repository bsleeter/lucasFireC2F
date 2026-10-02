# Historical target: how recorded fires burned by VEGETATION, from the record alone (no WRC), and the same measures for model results.
# Vegetation = LANDFIRE Biophysical Settings (BpS, potential vegetation: fire does not change it, so no fuel memory), grouped to
# Conifer (Conifer, Conifer-Hardwood), Hardwood (Hardwood, Hardwood-Conifer), Shrub, Grass (Grassland, Savanna), Other.
# EVT 2001 (LANDFIRE 1.0.5, pre-fire for 2002+) is reported beside BpS for the origin shares as a check.
#   T1  fire size by ORIGIN vegetation: FPA-FOD fires >= the zone's threshold, 1992-2020 - share of fires, share reaching 405 ha, mean
#       size of those >= 405 ha, share of the large-fire area. Model: kept fires, origin cell of the fire list.
#   T2  burned area by vegetation: MTBS wildfires >= 405 ha (1992-2020 and 2021-2024), cell x years; model: BP summed. Selection =
#       share of burned / share of ground.
#   T3  duration and spread: FPA-FOD discovery-to-containment days by origin vegetation and cause (record only); VIIRS arrival rates
#       (147's viirs_spread_arrivals, 2017-2024) by vegetation, relative to Grass; model: realized same-vegetation step rates.
#   T4  severity: MTBS high share (of classes 1-4) by vegetation, 1992-2024; model: stage 8 per-burn classes (if the result has them).
# Usage: Rscript historical_target.R <out dir>   (zones and results are set below)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(arrow); library(dplyr); library(rsyncrosim) }); sf_use_s2(FALSE)
OUT <- commandArgs(TRUE)[1]; dir.create(OUT, FALSE, TRUE)
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; DS <- "F:/lucas-master/Data Sources"
ZONES <- list(
  P26 = list(lib = file.path(D, "lucasFireC2F P26.ssim"), results = 17L,
             zone = "F:/lucas-master/Models/California 30m BurnP3/spatial/pyromes_150m_LF2020/pyrome_026/zone.gpkg"),
  P27 = list(lib = file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim"), results = c(11L, 15L, 19L), prep = 2L))
CLS <- c("Conifer", "Hardwood", "Shrub", "Grass", "Other")
bps_class <- function(g) fcase(g %in% c("Conifer", "Conifer-Hardwood"), "Conifer", g %in% c("Hardwood", "Hardwood-Conifer"), "Hardwood",
                               g == "Shrubland", "Shrub", g %in% c("Grassland", "Savanna"), "Grass", default = "Other")
evt_class <- function(g) fcase(g %in% c("Conifer", "Conifer-Hardwood"), "Conifer", g %in% c("Hardwood", "Hardwood-Conifer"), "Hardwood",
                               g == "Shrubland", "Shrub", g %in% c("Grassland", "Exotic Herbaceous"), "Grass", default = "Other")
bl <- fread(file.path(DS, "BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/CSV_Data/LF2020_BPS.csv")); setnames(bl, 1, "VALUE"); bl[, k := match(bps_class(GROUPVEG), CLS)]
el <- fread(file.path(DS, "EVT/US_105_EVT/US_105_EVT/CSV_Data/us_105evt.csv")); setnames(el, 1, "VALUE"); el[, k := match(evt_class(SystmGrpPhys), CLS)]
veg150 <- function(tif, lut, tmpl) { r <- rast(tif); r <- crop(r, terra::project(as.polygons(ext(tmpl), crs = crs(tmpl)), crs(r)), snap = "out")
  r <- classify(r, as.matrix(lut[, .(VALUE, k)]), others = NA); terra::project(r, tmpl, method = "mode") }
mt_all <- read_sf(file.path(DS, "MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp")); names(mt_all) <- tolower(names(mt_all))
mt_all <- mt_all[mt_all$incid_type == "Wildfire" & mt_all$burnbndac * 0.404686 >= 405, ]; mt_all$yr <- as.integer(substr(as.character(mt_all$ig_date), 1, 4))
va <- fread(cmd = 'gzip -dc "F:/lucas-master/Models/California 30m BurnP3/outputs/crosswalk_review/viirs_spread_arrivals.csv.gz"')
pd <- function(s) { s <- as.character(s); o <- as.Date(s, "%m/%d/%Y"); o[is.na(o)] <- as.Date(substr(s[is.na(o)], 1, 10)); o }
sh <- function(x, w = NULL) { if (is.null(w)) w <- rep(1, length(x)); t <- tapply(w, factor(x, 1:5), sum); t[is.na(t)] <- 0; setNames(as.numeric(t / sum(t)), CLS) }
ALL <- list()

for (z in names(ZONES)) { Z <- ZONES[[z]]; L <- suppressWarnings(ssimLibrary(Z$lib)); PR <- rsyncrosim::project(L, 1)
  g <- function(id, n) as.data.table(datasheet(rsyncrosim::scenario(L, id), paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
  tmpl <- rast(g(Z$results[1], "OutputBurnProbability")$FileName[1])
  zf <- if (!is.null(Z$prep)) g(Z$prep, "OutputZoneBoundary")$FileName[1] else Z$zone
  zone <- st_transform(st_union(st_make_valid(read_sf(zf))), crs(tmpl)); inz <- !is.na(values(rasterize(vect(zone), tmpl), mat = FALSE))
  bk <- values(veg150(file.path(DS, "BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/Tif/LF2020_BPS_CONUS.tif"), bl, tmpl), mat = FALSE)
  ek <- values(veg150(file.path(DS, "EVT/US_105_EVT/US_105_EVT/Tif/us_105evt.tif"), el, tmpl), mat = FALSE)
  MINA <- g(Z$results[1], "ResampleOption")$MinimumFireSize[1]
  ground <- sh(bk[inz & !is.na(bk)])
  cat(sprintf("\n################ %s: threshold %.1f ha; ground by BpS: %s\n", z, MINA, paste(sprintf("%s %.0f%%", CLS, 100 * ground), collapse = ", ")))

  # ---- record: FPA origins, sizes, durations --------------------------------------------------------------------------------
  bb <- st_bbox(st_transform(zone, 4326))
  f <- read_sf(file.path(DS, "FPA-FOD/Data/FPA_FOD_20221014.gpkg"), query = sprintf(
    "SELECT FIRE_SIZE, FIRE_YEAR, DISCOVERY_DATE, CONT_DATE, NWCG_CAUSE_CLASSIFICATION AS C, Shape FROM Fires WHERE FIRE_SIZE >= %f AND FIRE_YEAR BETWEEN 1992 AND 2020 AND LONGITUDE BETWEEN %f AND %f AND LATITUDE BETWEEN %f AND %f",
    MINA / 0.404686, bb$xmin, bb$xmax, bb$ymin, bb$ymax))
  f <- st_transform(f, crs(tmpl)); cc <- cellFromXY(tmpl, st_coordinates(f)); keep <- !is.na(cc) & inz[cc]; f <- f[keep, ]; cc <- cc[keep]
  H <- data.table(ha = f$FIRE_SIZE * 0.404686, yr = f$FIRE_YEAR, cause = f$C, k = bk[cc], ke = ek[cc], days = as.numeric(pd(f$CONT_DATE) - pd(f$DISCOVERY_DATE)) + 1)
  t1 <- function(d, who) { d <- d[!is.na(k)]; rbindlist(lapply(1:5, function(i) { x <- d[k == i]
    data.table(zone = z, source = who, veg = CLS[i], fires_share = nrow(x) / nrow(d), p405 = if (nrow(x)) mean(x$ha >= 405) else NA_real_,
               mean_ha_405 = if (any(x$ha >= 405)) mean(x$ha[x$ha >= 405]) else NA_real_, area_share_405 = sum(x$ha[x$ha >= 405]) / sum(d$ha[d$ha >= 405]), n = nrow(x)) })) }
  T1 <- rbind(t1(H, "record (BpS)"), t1(H[, .(ha, k = ke)], "record (EVT 2001)")[, source := "record (EVT 2001)"])
  T3d <- H[!is.na(k) & !is.na(days) & ha >= 405, .(zone = z, fires = .N, median_days = median(days), p75_days = quantile(days, .75)), by = .(veg = CLS[k], cause)][order(veg, cause)]
  # ---- record: MTBS burned area by vegetation ---------------------------------------------------------------------------------
  mt <- st_transform(mt_all, crs(tmpl)); mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
  burned <- function(yrs) { m <- mt[mt$yr %in% yrs, ]; if (!nrow(m)) return(NULL)
    cells <- unlist(lapply(split(seq_len(nrow(m)), m$yr), function(i) { w <- which(!is.na(values(rasterize(vect(m[i, ]), tmpl, field = 1), mat = FALSE))); w[inz[w]] })); bk[cells] }
  T2 <- rbindlist(list(data.table(zone = z, source = "record 1992-2020", veg = CLS, share = sh(burned(1992:2020)[!is.na(burned(1992:2020))])),
                       data.table(zone = z, source = "record 2021-2024", veg = CLS, share = sh(burned(2021:2024)[!is.na(burned(2021:2024))]))))
  # ---- record: VIIRS arrival rates -----------------------------------------------------------------------------------------
  vc <- cellFromXY(tmpl, as.matrix(va[, .(x, y)])); vk <- ifelse(!is.na(vc) & inz[pmax(vc, 1)], bk[pmax(vc, 1)], NA); V <- va[!is.na(vk)][, k := vk[!is.na(vk)]]
  T3v <- V[, .(n = .N, fires = uniqueN(Event_ID), median_mh = median(rate_mh)), by = .(veg = CLS[k])]; T3v[, rel_grass := median_mh / T3v[veg == "Grass", median_mh]]; T3v[, `:=`(zone = z, source = "VIIRS 2017-2024")]
  # ---- record: severity ---------------------------------------------------------------------------------------------------------
  svf <- file.path(DS, "MTBS/Burn Severity")   # MTBS thematic severity mosaics, taken to the 150 m grid (modal class) like everything else
  sev <- rbindlist(lapply(1992:2024, function(y) { p <- file.path(svf, sprintf("mtbs_CONUS_%d.tif", y)); if (!file.exists(p)) return(NULL)
    r <- rast(p); r <- crop(r, terra::project(vect(zone), crs(r)), snap = "out"); r <- classify(r, cbind(c(0, 5, 6), NA))
    s150 <- values(terra::project(r, tmpl, method = "mode"), mat = FALSE)
    d <- data.table(s = s150, k = bk)[inz & s %in% 1:4 & !is.na(k)]; d[, .N, by = .(k, s)] }))
  T4 <- if (nrow(sev)) sev[, .(N = sum(N)), by = .(k, s)][, .(zone = z, source = "record MTBS 1992-2024", veg = CLS[k], high_share = sum(N[s == 4]) / sum(N), n = sum(N)), by = k][, k := NULL] else NULL

  # ---- model results ---------------------------------------------------------------------------------------------------------------
  for (rid in Z$results) { who <- sprintf("model r%d", rid)
    st <- g(rid, "OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA]
    st[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
    fl <- g(rid, "OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell)]; st <- fl[st, on = c("bI", "bF")]
    T1 <- rbind(T1, t1(data.table(ha = st$Area, k = bk[st$Cell]), who))
    bpv <- values(rast(g(rid, "OutputBurnProbability")$FileName[1]), mat = FALSE); ok <- inz & !is.na(bk) & !is.na(bpv)
    T2 <- rbind(T2, data.table(zone = z, source = who, veg = CLS, share = sh(bk[ok], bpv[ok])))
    set.seed(1); pick <- st[sample(.N, min(.N, 2000)), .(Iteration = bI, FireID = bF)]
    bt <- as.data.table(open_dataset(g(rid, "OutputBurnTable")$FileName[1]) |> select(Iteration, FireID, Cell, Source, Period) |> collect())[pick, on = c("Iteration", "FireID"), nomatch = 0]
    bt <- bt[, .(Iteration, FireID, Source = Cell, Psrc = Period)][bt, on = c("Iteration", "FireID", "Source")][!is.na(Psrc) & Period > Psrc]
    NC <- ncol(tmpl); bt[, `:=`(k1 = bk[Cell], k0 = bk[Source])]
    bt <- bt[k1 == k0 & !is.na(k1)][, rate := 150 * sqrt(((Cell - 1) %/% NC - (Source - 1) %/% NC)^2 + ((Cell - 1) %% NC - (Source - 1) %% NC)^2) / (Period - Psrc)]
    mv <- bt[, .(n = .N, fires = uniqueN(paste(Iteration, FireID)), median_mh = 60 * median(rate)), by = .(veg = CLS[k1])]; mv[, rel_grass := median_mh / mv[veg == "Grass", median_mh]]
    T3v <- rbind(T3v, mv[, `:=`(zone = z, source = who)], fill = TRUE)
    be <- g(rid, "OutputBurnEffects"); if (nrow(be)) { e <- as.data.table(open_dataset(be$FileName[1]) |> select(Cell, SeverityClass) |> collect())
      e[, hi := as.character(SeverityClass) %in% c("4", "High")]; e[, k := bk[Cell]]
      T4 <- rbind(T4, e[!is.na(k), .(zone = z, source = who, veg = CLS[k], high_share = mean(hi), n = .N), by = k][, k := NULL]) } }
  T2 <- rbind(T2, data.table(zone = z, source = "ground", veg = CLS, share = ground))
  ALL[[z]] <- list(T1 = T1, T2 = T2, T3d = T3d, T3v = T3v, T4 = T4)

  r3 <- function(x) { x <- copy(x); num <- names(x)[sapply(x, is.numeric)]; x[, (num) := lapply(.SD, function(v) round(v, 3)), .SDcols = num]; x }
  cat("\n== T1 fire size by ORIGIN vegetation (fires >= threshold)\n"); print(r3(T1[, .(source, veg, fires_share, p405, mean_ha_405 = round(mean_ha_405), area_share_405, n)]))
  cat("\n== T2 burned area share by vegetation (selection = share / ground)\n")
  w <- dcast(T2, veg ~ source, value.var = "share"); gcol <- w$ground; for (s in setdiff(names(w), c("veg", "ground"))) w[, (paste0(s, " sel")) := round(get(s) / gcol, 2)]
  print(r3(w[, c("veg", "ground", grep(" sel$", names(w), value = TRUE)), with = FALSE]))
  cat("\n== T3 duration of fires >= 405 ha by origin vegetation and cause (FPA-FOD days, discovery to containment)\n"); print(T3d)
  cat("\n== T3 spread: VIIRS arrival rate (m/h) vs model same-vegetation step rate (m/h during burning), each relative to Grass\n"); print(r3(T3v[, .(source, veg, n, median_mh, rel_grass)])[order(source, veg)])
  if (!is.null(T4)) { cat("\n== T4 high-severity share by vegetation\n"); print(r3(dcast(T4, veg ~ source, value.var = "high_share"))) }
  for (k in names(ALL[[z]])) if (!is.null(ALL[[z]][[k]])) fwrite(ALL[[z]][[k]], file.path(OUT, sprintf("%s_%s.csv", z, k))) }
