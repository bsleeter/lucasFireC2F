# =============================================================================
# effects.R - per-burn-event fire effects (stage 6's physics), shared by stages 6 (maps) and 8 (per-burn detail)
# =============================================================================
# One function, so the two stages cannot drift: same inputs, same random seed (RunControl.RandomSeed - 1, as script 217),
# same order of events, same numbers. See stage 6's header for the science. Needs shared.R and rothermel.R sourced.
# Returns list(ev = one row per burn event of the kept fires (burnable cells only), g = the engine grid, fm = Fuel Models).
# =============================================================================
burn_event_effects <- function() {
  rs <- read_ds("RunControl")$RandomSeed[1]                         # a blank seed (e.g. a Merge scenario) takes the package default
  if (is.null(rs) || is.na(rs)) { rs <- 20260925L; say("Random Seed is blank: using the package default %d", rs) }
  set.seed(rs - 1L)
  eo <- need("EffectsOption"); WF <- eo$WindFactor
  # list columns come back as their labels ("Observed RAWS ratio", "Cruz et al. 2005") or as codes - accept both
  WMODE <- if (is.character(eo$WindMode)) as.integer(!grepl("^Observed", eo$WindMode)) else as.integer(eo$WindMode)
  CROWNM <- if (is.character(eo$CrownModel)) as.integer(!grepl("^Cruz", eo$CrownModel)) else as.integer(eo$CrownModel)
  if (is.na(WMODE) || is.na(CROWNM)) fail("Fire Effects Options: choose a wind mode and a crown model")
  CBHK <- eo$CanopyBaseHeightMultiplier; FMC <- eo$FoliarMoisture
  MINCCA <- if (!is.null(eo$MinimumCanopyCoverActive) && !is.na(eo$MinimumCanopyCoverActive)) eo$MinimumCanopyCoverActive else 40   # % (1.8.0)
  wo <- read_ds("WeatherOption"); W20 <- if (nrow(wo) && !is.na(wo$WindSpeedTo20ft)) wo$WindSpeedTo20ft else 1.15
  el <- need("OutputEngineLandscape"); g <- terra::rast(el$FuelFileName[1]); NC <- ncell(g); CS <- res(g)[1]
  V <- function(col) values(terra::rast(el[[col]][1]), mat = FALSE)
  f40 <- V("FuelFileName"); cbh <- V("CanopyBaseHeightFileName"); cbd <- V("CanopyBulkDensityFileName"); ccf <- V("CanopyCoverFileName")
  hm <- if (!is.na(el$CanopyHeightFileName[1])) V("CanopyHeightFileName") else rep(-9999, NC); elev <- V("ElevationFileName")
  forest <- !is.na(cbd) & cbd > 0 & !is.na(cbh) & cbh < 99
  if (CBHK != 1) cbh[forest] <- cbh[forest] * CBHK
  ch <- ifelse(forest & !is.na(hm) & hm > 0, hm, ifelse(forest, 2 * cbh, NA)); ch <- ifelse(forest, pmax(ch, cbh + 1), ch)
  fm <- as.data.table(datasheet(prj, ds("FuelModel"), lookupsAsFactors = FALSE)); ms <- as.data.table(datasheet(prj, ds("MoistureScenario"), lookupsAsFactors = FALSE))

  # ---- burn events of the kept fires ------------------------------------------------------------------------------------
  st <- need("OutputFireStatistic", c("ResampleStatus"))
  kept <- st[ResampleStatus %in% c("Kept", "Reassigned")]
  kept[, `:=`(bIter = ifelse(ResampleStatus == "Reassigned", 0L, Iteration), bFire = ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID),
              ord2 = ResampleStatus == "Reassigned")]
  setorder(kept, Iteration, ord2, bFire); kept[, kseq := .I]                         # 203's order: season, originals, then replacements
  Fl <- need("OutputFireList"); kept[Fl, on = c(bIter = "Iteration", bFire = "FireID"), scen := ms$ID[match(i.MoistureScenario, ms$Name)]]
  BT <- as.data.table(arrow::read_parquet(need("OutputBurnTable", "FileName")$FileName[1]))
  ev <- BT[kept[, .(bIter, bFire, kseq, scen, Season = Iteration, SeasonFireID = FireID)], on = c(Iteration = "bIter", FireID = "bFire"), nomatch = 0]
  setorder(ev, kseq)                                                                 # stable: cells keep C2F-W's order within a fire
  NET <- "Source" %in% names(ev) && any(!is.na(ev$Source))
  hw <- need("OutputFireWeather"); nh <- hw[, .N, by = .(Iteration, FireID)]
  ev[nh, on = c("Iteration", "FireID"), nh := i.N]
  ev[, h := if (NET) pmin(nh, 1L + as.integer(Period %/% 60)) else 1L + floor(runif(.N) * nh)]
  ev[hw, on = .(Iteration, FireID, h = Hour), `:=`(ws20 = i.WindSpeed, wd = i.WindDirection, evw = i.EventWindSpeed)]
  if (anyNA(ev$ws20) || anyNA(ev$scen)) fail("burn events with no hourly wind (%d) or moisture scenario (%d)", sum(is.na(ev$ws20)), sum(is.na(ev$scen)))
  say("%s burn events from %s kept fires; spread network %s", format(nrow(ev), big.mark = ","), format(nrow(kept), big.mark = ","), if (NET) "used" else "absent")

  # ---- burning-period wind -----------------------------------------------------------------------------------------------
  if (WMODE == 0) {
    Q <- need("WindRatio", c("LowerWindSpeed", "Quantile", "Ratio"))
    Q[, zq := if ("FireZone" %in% names(Q)) as.character(FireZone) else NA_character_]   # a table per fire zone (merged runs), or one for all
    ev[, d10 := ws20 * W20]
    fh <- unique(ev[, .(Iteration, FireID, h, d10)])
    fh[, p := runif(.N)]                                                             # same draw order as the single-table version
    fh[, zq := NA_character_]
    if (any(!is.na(Q$zq))) { fz <- unique(kept[, .(Iteration = bIter, FireID = bFire, zq = as.character(FireZone))])
      fh[fz, on = c("Iteration", "FireID"), zq := i.zq]; fh[!(zq %in% Q$zq), zq := NA_character_]
      if (fh[is.na(zq), .N] && !any(is.na(Q$zq))) fail("Burning-Period Wind Ratio Table has no rows for fire zone(s) %s and no all-zone rows", paste(unique(kept$FireZone[!kept$FireZone %in% Q$zq]), collapse = ", ")) }
    fh[, c("lo", "ratio") := { qz <- if (is.na(.BY$zq)) Q[is.na(zq)] else Q[zq == .BY$zq]; los <- sort(unique(qz$LowerWindSpeed))
      lo <- los[pmax(1, findInterval(d10, los))]
      r <- numeric(.N); for (b in unique(lo)) { q <- qz[LowerWindSpeed == b][order(Quantile)]; k <- lo == b; r[k] <- approx(q$Quantile, q$Ratio, xout = p[k], rule = 2)$y }
      list(lo, r) }, by = zq]
    ev[fh, on = .(Iteration, FireID, h), ws20 := i.d10 * i.ratio]; ev[, d10 := NULL]
    say("wind: observed burning-period ratio per fire-hour (median %.2f); 20-ft wind median %.1f, p99 %.1f km/h", median(fh$ratio), median(ev$ws20), quantile(ev$ws20, .99))
  } else { ev[, ws20 := ws20 * WF]; say("wind: fixed factor %.2f", WF) }
  if (any(!is.na(ev$evw))) { ev[!is.na(evw), ws20 := evw]; say("observed offshore-event wind on %.1f%% of burn events", 100 * mean(!is.na(ev$evw))) }
  ev[, fb := f40[Cell]]; ev <- ev[!(fb %in% 91:99)]

  # ---- Rothermel lookups: fuel model x moisture scenario x midflame wind (src/rothermel.R, the project's sheets) -----------
  codes <- sort(unique(ev$fb)); fmi <- match(codes, fm$ID)
  if (anyNA(fmi)) fail("no Fuel Model for fuel code(s) %s", paste(codes[is.na(fmi)], collapse = ", "))
  ms <- ms[order(ID)]; if (!identical(as.integer(ms$ID), seq_len(nrow(ms)))) fail("Fuel Moisture Scenario IDs must run 1..%d (C2F-W's scenario numbers)", nrow(ms))
  MOIST <- as.matrix(ms[, .(Moisture1h, Moisture10h, Moisture100h, MoistureLiveHerb, MoistureLiveWoody)])
  UG <- seq(0, 80, by = 0.25)
  roth <- function(pars, s, u) { o <- rothermel(pars, MOIST[s, ], u)
    data.table(u = u, R = o$R, HA = o$IR * 384 / (o$SAV / 3.281), beta = o$beta) }          # HA = reaction intensity x residence time
  LU <- rbindlist(lapply(seq_along(codes), function(i) rbindlist(lapply(seq_len(nrow(ms)), function(s)
    roth(as.list(fm[fmi[i]]), s, UG)[, `:=`(fb = codes[i], scen = s)]))))
  depth_ft <- setNames(fm$DepthCm[fmi] * 0.0328084, codes)

  # ---- per-event physics (217) ---------------------------------------------------------------------------------------------
  ev[, `:=`(fo = forest[Cell], cbh = cbh[Cell], cbd = cbd[Cell], ch = ch[Cell], cc = ccf[Cell])]
  ev[, f := ifelse(fo, cc * pmax(0, (ch - cbh) / ch) / 3, 0)]; ev[, H := ch * 3.28084]
  ev[, waf := ifelse(fo & f > 0.05, 0.555 / (sqrt(f * H) * log((20 + 0.36 * H) / (0.13 * H))),
                     { d <- depth_ft[as.character(fb)]; 1.83 / log((20 + 0.36 * d) / (0.13 * d)) })]
  ev[, u := pmin(80, ws20 * waf)]; ev[, ui := round(u * 4) / 4]
  ev[LU, on = .(fb, scen, ui = u), `:=`(Rh = i.R, HA = i.HA)]
  ev[, lb := pmin(8, 1 + 0.25 * (u / 1.609))]; ev[, e := sqrt(1 - 1 / lb^2)]
  if (NET) {
    ncol_g <- ncol(g)
    ev[, `:=`(dr = (Cell - 1L) %/% ncol_g - (Source - 1L) %/% ncol_g, dc = (Cell - 1L) %% ncol_g - (Source - 1L) %% ncol_g)]
    ev[, az := (atan2(dc, -dr) * 180 / pi) %% 360]
    ev[, th := { d <- abs(((az - (wd + 180) %% 360) + 180) %% 360 - 180); d * pi / 180 }]; ev[is.na(Source), th := 0]
    ev[, tanp := ifelse(is.na(Source), 0, pmax(0, (elev[Cell] - elev[Source]) / (CS * sqrt(dr^2 + dc^2))))]
    ev[LU[u == 0], on = .(fb, scen), `:=`(Rnw = i.R, beta = i.beta)]; ev[, phis := 5.275 * beta^(-0.3) * tanp^2]
  } else { ev[, th := runif(.N) * pi]; ev[, `:=`(Rnw = 0, phis = 0, az = NA_real_, tanp = NA_real_)] }
  ev[, R := Rh * (1 - e) / (1 - e * cos(th)) + Rnw * phis]; ev[, Is := HA * R]; ev[, fl_s := 0.0775 * pmax(Is, 0)^0.46]
  dm1 <- ms$Moisture1h
  if (CROWNM == 0) { ev[, Ract := ifelse(fo, 11.02 * (ws20 * W20)^0.90 * pmax(cbd, 1e-6)^0.19 * exp(-0.17 * dm1[scen]), 0)]
  } else { FM10 <- rbindlist(lapply(seq_len(nrow(ms)), function(s) roth(ANDERSON_FM10, s, UG)[, scen := s]))
    ev[, u20 := round(pmin(80, 0.4 * ws20) * 4) / 4]; ev[FM10, on = .(scen, u20 = u), R10 := i.R]; ev[, Ract := 3.34 * R10] }
  ev[, `:=`(I0 = ifelse(fo, (0.01 * cbh * (460 + 25.9 * FMC))^1.5, Inf), RAC = ifelse(fo, 3 / cbd, Inf))]
  ev[, R0 := I0 / pmax(HA, 1e-6)]; ev[, crown := fo & Is >= I0]
  ev[, cfb := ifelse(crown, pmin(1, pmax(0, 1 - exp(-(-log(0.1) / (0.9 * pmax(RAC - R0, 1e-3))) * (R - R0)))), 0)]
  # active crown fire needs a closed canopy: below EffectsOption.MinimumCanopyCoverActive (default 40%) a crown fire can torch
  # (passive, Scott & Reinhardt fraction burned) but not run from crown to crown. Without it, LANDFIRE's open woodland over shrub
  # models (P26-P29 1.7.0: cover ~30%, CBD ~0.07, CBH ~1 m) made 7% of shrub-model burns active crown fire.
  ev[, active := crown & Ract >= RAC & !is.na(cc) & cc >= MINCCA / 100]
  say("active crown fire needs canopy cover >= %.0f%%: %s burn events that met the spread criterion stay passive", MINCCA,
      format(ev[, sum(crown & Ract >= RAC & cc < MINCCA / 100, na.rm = TRUE)], big.mark = ","))
  ev[active == TRUE, cfb := 1]
  ev[, Rfin := ifelse(crown, R + cfb * pmax(Ract - R, 0), R)]
  ev[, Ic := HA * Rfin + cfb * cbd * pmax(ch - cbh, 0) * 18000 * Rfin / 60]
  ev[, ft := ifelse(crown, pmax(fl_s, 0.0266 * Ic^(2 / 3)), fl_s) * 3.28084]
  say("burn events: flame length median %.1f ft; > 4 ft %.0f%%, > 8 ft %.0f%%; crown %.0f%%, active %.0f%%; median WAF %.2f forest / %.2f open",
      median(ev$ft), 100 * mean(ev$ft > 4), 100 * mean(ev$ft > 8), 100 * mean(ev$crown), 100 * mean(ev$active), median(ev$waf[ev$fo]), median(ev$waf[!ev$fo]))
  list(ev = ev, g = g, fm = fm)
}
