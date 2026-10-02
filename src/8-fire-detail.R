# =============================================================================
# 8 - Fire Detail (optional)
# =============================================================================
# Per-burn and per-fire detail from a finished run (stages 1-6 outputs), without regrowing fires. The per-event physics is
# src/effects.R - the same code and random seed as stage 6 - so every value here aggregates exactly to stage 6's maps.
# Each output is switched in FireDetailOption:
#   per-burn effects table   one row per burned cell of every kept fire: season, fire, cell, fuel code, the hour it burned,
#                            spread azimuth, angle to the wind, slope along the path (%), 20-ft / midflame wind and wind
#                            adjustment factor, head / directional / final rate of spread, surface and total fireline
#                            intensity, flame length, crown fraction burned, fire type (surface / passive / active), the
#                            severity index and MTBS-style class, and C2F-W's own rate of spread (parquet)
#   fire perimeters          one polygon per kept fire with its season, cause, size, spread days, crown shares, flame
#                            lengths and MTBS-style class shares (GeoPackage)
#   severity maps            for the first N seasons: MTBS-style class, fire type, and the ID of the fire behind each cell's
#                            most severe burn that season (what a LUCAS realization-mode time step consumes)
# MTBS-style severity. MTBS classes measure vegetation change, not fire behaviour, so they are mapped, not derived: an ordered
# SEVERITY INDEX per burn (surface fire by flame length 0-1, passive crown 1 + crown fraction burned, active crown 3) is cut
# at per-fuel-group thresholds (SeverityClass). With FitSeverityClasses the thresholds are fitted so the burn-weighted class
# shares match SeverityTarget (MTBS shares by fuel group) and written back; fit ONCE on a calibrated baseline and hold the
# thresholds fixed for scenarios, or the classes would absorb the scenario's change in fire behaviour.
# The fire-level columns of Fire Statistics (crown shares, flame lengths, high-severity share) are filled here.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(arrow); library(sf); library(parallel) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
source(file.path(env$PackageDirectory, "src", "rothermel.R")); source(file.path(env$PackageDirectory, "src", "effects.R"))
sf_use_s2(FALSE); progressBar(type = "begin", totalSteps = 6)
fd <- read_ds("FireDetailOption")
opt <- function(k, d) if (nrow(fd) && !is.na(fd[[k]][1])) fd[[k]][1] else d
DO_TAB <- isTRUE(opt("BurnEffectsTable", TRUE)); DO_PER <- isTRUE(opt("Perimeters", TRUE)); NSEV <- as.integer(opt("SeverityMapSeasons", 10)); FIT <- isTRUE(opt("FitSeverityClasses", FALSE))
E <- burn_event_effects(); ev <- E$ev; g <- E$g; fm <- E$fm
progressBar()

# ---- severity index and MTBS-style classes ---------------------------------------------------------------------------------
ev[, `:=`(ctype = 1L + as.integer(crown) + as.integer(active), grp = fm$FuelGroup[match(fb, fm$ID)])]
ev[, sidx := ifelse(active, 3, ifelse(crown, 1 + cfb, pmin(ft, 999) / 1000))]
tg <- need("SeverityTarget", c("FuelGroup", "UnburnedLow", "Low", "Moderate", "High")); CLS <- c("Unburned-Low", "Low", "Moderate", "High")
# no thresholds yet (a zone's first run, i.e. its baseline): fit them rather than stop - later scenarios then hold them fixed
if (!FIT && !nrow(read_ds("SeverityClass"))) { FIT <- TRUE; say("Severity Class Thresholds are empty: fitting them on this run (the zone's baseline)") }
if (FIT) {
  th <- rbindlist(lapply(unique(ev$grp), function(k) { t <- tg[FuelGroup == k]
    if (!nrow(t)) fail("Severity Target has no row for fuel group %s", k)
    s <- c(t$UnburnedLow, t$Low, t$Moderate, t$High); s <- s / sum(s); q <- quantile(ev[grp == k, sidx], cumsum(s)[1:3], type = 1, names = FALSE)
    data.table(FuelGroup = k, UnburnedLowMax = q[1], LowMax = q[2], ModerateMax = q[3]) }))
  write_ds(th, "SeverityClass"); say("severity thresholds FITTED to the target for %d fuel groups (hold them fixed for scenario runs)", nrow(th))
} else {
  th <- need("SeverityClass", c("FuelGroup", "UnburnedLowMax", "LowMax", "ModerateMax"))
  miss <- setdiff(unique(ev$grp), th$FuelGroup); if (length(miss)) fail("Severity Class Thresholds have no row for: %s - fit them once (Fit Severity Class Thresholds)", paste(miss, collapse = ", "))
}
ev[th, on = c(grp = "FuelGroup"), `:=`(t1 = i.UnburnedLowMax, t2 = i.LowMax, t3 = i.ModerateMax)]
ev[, sev := 1L + (sidx > t1) + (sidx > t2) + (sidx > t3)]
fit <- ev[, .(Model = as.numeric(table(factor(sev, 1:4))) / .N, SeverityClass = CLS), by = .(FuelGroup = grp)]
fit[melt(tg, id.vars = "FuelGroup", variable.name = "cl", value.name = "Target")[, SeverityClass := CLS[match(cl, c("UnburnedLow", "Low", "Moderate", "High"))]],
    on = c("FuelGroup", "SeverityClass"), Target := i.Target]
write_ds(fit[, .(FuelGroup, SeverityClass, Target, Model)], "OutputSeverityFit")
for (k in unique(fit$FuelGroup)) { x <- fit[FuelGroup == k]; say("  %-18s model %s  (target %s)", k, paste(sprintf("%.2f", x$Model), collapse = "/"), paste(sprintf("%.2f", x$Target), collapse = "/")) }
progressBar()

# ---- fire-level columns of Fire Statistics --------------------------------------------------------------------------------
fs <- ev[, .(CrownShare = mean(crown), ActiveCrownShare = mean(active), MeanFlameLength = mean(ft), MaxFlameLength = max(ft), HighSeverityShare = mean(sev == 4L),
             UnburnedLowShare = mean(sev == 1L), LowShare = mean(sev == 2L), ModerateShare = mean(sev == 3L)), by = .(Iteration = Season, FireID = SeasonFireID)]
st <- read_ds("OutputFireStatistic")
st[fs, on = c("Iteration", "FireID"), `:=`(CrownShare = i.CrownShare, ActiveCrownShare = i.ActiveCrownShare, MeanFlameLength = i.MeanFlameLength,
                                            MaxFlameLength = i.MaxFlameLength, HighSeverityShare = i.HighSeverityShare)]
write_ds(st, "OutputFireStatistic")
progressBar()
TD <- env$TransferDirectory

# ---- per-burn effects table -------------------------------------------------------------------------------------------------
if (DO_TAB) {
  r2 <- function(x, d = 2) round(x, d)
  T <- ev[, .(Season, FireID = SeasonFireID, Cell, FuelCode = as.integer(fb), Hour = as.integer(h), SpreadAzimuth = r2(az, 1), AngleToWind = r2(th * 180 / pi, 1),
              SlopePct = r2(100 * tanp, 1), Wind20ft = r2(ws20), WAF = r2(waf, 3), MidflameWind = r2(u), HeadROS = r2(Rh, 3), ROS = r2(R, 3), FinalROS = r2(Rfin, 3),
              SurfaceIntensity = r2(Is, 1), FireIntensity = r2(Ic, 1), FlameLengthFt = r2(ft), CFB = r2(cfb, 3), FireType = ctype,
              SeverityIndex = r2(sidx, 4), SeverityClass = sev, ROSC2FW = if ("ROSC2FW" %in% names(ev)) ROSC2FW else NA_real_)]
  bf <- file.path(TD, "burn_effects.parquet"); arrow::write_parquet(T, bf); rm(T)
  write_ds(data.frame(FileName = bf, Description = sprintf("%s burn events: timing, direction, wind, spread, intensity, flame length, crown, severity", format(nrow(ev), big.mark = ","))), "OutputBurnEffects")
  say("per-burn effects table: %s rows (%.0f MB)", format(nrow(ev), big.mark = ","), file.size(bf) / 1e6)
}
progressBar()

# ---- fire perimeters ------------------------------------------------------------------------------------------------------------
if (DO_PER) {
  E0 <- as.vector(ext(g)); RS <- res(g); NC <- ncol(g); CRS <- crs(g)
  cells <- split(ev$Cell, list(ev$Season, ev$SeasonFireID), drop = TRUE)
  poly_of <- function(cl) { r <- (cl - 1L) %/% NC + 1L; c <- (cl - 1L) %% NC + 1L; r0 <- min(r); c0 <- min(c)
    w <- terra::rast(nrows = max(r) - r0 + 1L, ncols = max(c) - c0 + 1L, xmin = E0[1] + (c0 - 1) * RS[1], xmax = E0[1] + max(c) * RS[1],
                     ymax = E0[4] - (r0 - 1) * RS[2], ymin = E0[4] - max(r) * RS[2], crs = CRS)
    v <- rep(NA_integer_, ncell(w)); v[(r - r0) * ncol(w) + (c - c0) + 1L] <- 1L; terra::values(w) <- v
    terra::geom(terra::as.polygons(w, dissolve = TRUE), wkt = TRUE) }
  cl <- makePSOCKcluster(min(32, max(1, detectCores() - 1))); clusterExport(cl, c("E0", "RS", "NC", "CRS"), envir = environment())
  clusterEvalQ(cl, suppressMessages(library(terra)))
  wkt <- unlist(parLapply(cl, cells, poly_of), use.names = FALSE); stopCluster(cl)
  key <- data.table(k = names(cells))[, c("Season", "FireID") := tstrsplit(k, ".", fixed = TRUE, type.convert = TRUE)]
  P <- merge(key[, .(Season, FireID)], st[, .(Season = Iteration, FireID, SeasonName = Season, Cause, FireZone, SpreadDays, HoursBurning, AreaHa = Area,
                                              ResampleStatus, CrownShare, ActiveCrownShare, MeanFlameLength, MaxFlameLength)], by = c("Season", "FireID"), all.x = TRUE, sort = FALSE)
  P <- merge(P, fs[, .(Season = Iteration, FireID, UnburnedLowShare, LowShare, ModerateShare, HighSeverityShare)], by = c("Season", "FireID"), all.x = TRUE, sort = FALSE)
  Fl <- read_ds("OutputFireList"); P[st[, .(Season = Iteration, FireID, bI = ifelse(ResampleStatus == "Reassigned", 0L, Iteration),
                                            bF = ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID))], on = c("Season", "FireID"), `:=`(bI = i.bI, bF = i.bF)]
  P[Fl, on = c(bI = "Iteration", bF = "FireID"), `:=`(IgnitionLatitude = i.Latitude, IgnitionLongitude = i.Longitude)]; P[, c("bI", "bF") := NULL]
  pf <- file.path(TD, "fire_perimeters.gpkg"); unlink(pf)
  st_write(st_sf(P, geometry = st_as_sfc(wkt, crs = st_crs(CRS))), pf, layer = "fire_perimeters", quiet = TRUE)
  write_ds(data.frame(FileName = pf, Description = sprintf("%s fire perimeters with season, cause, size, crown and MTBS-style severity shares", format(nrow(P), big.mark = ","))), "OutputFirePerimeter")
  say("fire perimeters: %s polygons (%.0f MB)", format(nrow(P), big.mark = ","), file.size(pf) / 1e6)
}
progressBar()

# ---- severity maps for the first N seasons ------------------------------------------------------------------------------------
if (NSEV > 0) {
  SMN <- metric_names("SeverityMetric"); NCELL <- ncell(g); out <- list(); ss <- sort(unique(ev$Season)); ss <- ss[ss <= NSEV]
  for (s in ss) { x <- ev[Season == s][order(Cell, -sev, -ctype)][!duplicated(Cell)]
    for (m in c("MTBS", "BEHAVIOUR", "FIREID")) { v <- integer(NCELL)
      v[x$Cell] <- switch(m, MTBS = x$sev, BEHAVIOUR = x$ctype, FIREID = x$SeasonFireID)
      p <- file.path(TD, sprintf("severity_%s_s%05d.tif", tolower(m), s))
      writeRaster(setValues(rast(g), v), p, overwrite = TRUE, datatype = if (m == "FIREID") "INT2U" else "INT1U", gdal = "COMPRESS=DEFLATE")
      out[[length(out) + 1]] <- data.table(Iteration = s, Timestep = 0L, Metric = SMN[[m]], FileName = p) } }
  write_ds(rbindlist(out), "OutputSeverityMap"); say("severity maps: %d seasons x 3 (MTBS class, fire type, fire ID)", length(ss))
}
progressBar(); progressBar(type = "end")
