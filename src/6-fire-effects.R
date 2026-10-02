# =============================================================================
# 6 - Fire Effects
# =============================================================================
# How hard each cell burns, recomputed per burn event from standard physics (ported from the validated P26 script 217).
# C2F-W decides WHERE fire goes; its Scott & Burgan surrogate reads its input wind as midflame wind, so its own
# intensities run high under canopy. Effects are therefore recomputed for every burned cell of every kept fire:
#   wind      the hour the cell burned (spread network arrival minute; else a random hour of the fire); the 20-ft wind is
#             the fire-hour's daily-mean wind x a burning-period ratio drawn from WindRatio for that day's wind bin
#             (EffectsOption.WindMode 0), or x a fixed factor (mode 1); observed offshore-event hours use the event wind
#   midflame  Albini & Baughman 1979 wind adjustment (sheltered under > 5% crown fill, else by fuel-bed depth)
#   surface   Rothermel for the cell's fuel model at the fire's moisture scenario; direction to the wind from the spread
#             network (else uniform), elliptical spread (Anderson 1983 length-to-breadth), Rothermel slope factor along
#             the path; Byram intensity; flame length 0.0775 I^0.46
#   crown     Van Wagner initiation (foliar moisture, canopy base height x EffectsOption multiplier), Scott & Reinhardt
#             crown fraction burned; active when the active crown spread rate reaches 3 / CBD: Cruz et al. 2005
#             (11.02 U10^0.9 CBD^0.19 e^-0.17 EFFM, CrownModel 0) or Rothermel 1991 (3.34 x FM10, CrownModel 1);
#             crown flame length Thomas 1963; active only where canopy cover >= EffectsOption.MinimumCanopyCoverActive (40%)
# Maps (cells with at least EffectsOption.MinimumBurns burn events): conditional flame length, FLEP4, FLEP8, the six FSim
# flame-length class shares, P(crown), P(active crown); crown-fire probability per year = BP x P(crown); burn events.
# The per-event physics is src/effects.R, shared with stage 8 (per-burn detail), so both give the same numbers.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(arrow) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
source(file.path(env$PackageDirectory, "src", "rothermel.R")); source(file.path(env$PackageDirectory, "src", "effects.R"))
progressBar(type = "begin", totalSteps = 2)
MINB <- need("EffectsOption")$MinimumBurns
E <- burn_event_effects(); ev <- E$ev; g <- E$g; fm <- E$fm; NC <- ncell(g)                    # src/effects.R (shared with stage 8)
progressBar()

# ---- per-cell maps and the fuel-group summary ---------------------------------------------------------------------------------
cellsum <- function(vals) { s <- numeric(NC); t <- data.table(cell = ev$Cell, x = vals)[, .(x = sum(x)), by = cell]; s[t$cell] <- t$x; s }
n <- cellsum(rep(1, nrow(ev))); cls <- cut(ev$ft, c(-Inf, 2, 4, 6, 8, 12, Inf), labels = FALSE)
M <- list(CFL = cellsum(ev$ft) / n, FLEP4 = cellsum(as.numeric(ev$ft > 4)) / n, FLEP8 = cellsum(as.numeric(ev$ft > 8)) / n)
for (k in 1:6) M[[paste0("FLP", k)]] <- cellsum(as.numeric(cls == k)) / n
M$PCROWN <- cellsum(as.numeric(ev$crown)) / n; M$PACTIVE <- cellsum(as.numeric(ev$active)) / n
lo <- n < MINB; for (k in names(M)) M[[k]][lo] <- NA
bp <- values(terra::rast(read_ds("OutputBurnProbability")$FileName[1]), mat = FALSE); bp[is.na(bp)] <- 0
M$CROWNBP <- bp * ifelse(is.na(M$PCROWN), 0, M$PCROWN); M$BURNS <- n
TD <- env$TransferDirectory
MN <- metric_names("EffectsMetric"); miss <- setdiff(names(M), names(MN)); if (length(miss)) fail("Fire Effects Metrics has no code(s) %s", paste(miss, collapse = ", "))
out <- rbindlist(lapply(names(M), function(k) { p <- file.path(TD, sprintf("effects_%s.tif", k))
  writeRaster(setValues(rast(g), M[[k]]), p, overwrite = TRUE, datatype = "FLT4S", gdal = "COMPRESS=DEFLATE")
  data.table(Iteration = 1L, Timestep = 0L, Metric = MN[[k]], FileName = p) }))
write_ds(out, "OutputFireEffects")
grp <- fm$FuelGroup[match(ev$fb, fm$ID)]
S <- ev[, .(BurnShare = .N / nrow(ev), CrownShare = mean(crown), ActiveCrownShare = mean(active), FLEP4 = mean(ft > 4), FLEP8 = mean(ft > 8),
            ConditionalFlameLength = mean(ft)), by = .(FuelGroup = grp)][order(-BurnShare)]
S <- rbind(S, ev[, .(FuelGroup = "All", BurnShare = 1, CrownShare = mean(crown), ActiveCrownShare = mean(active), FLEP4 = mean(ft > 4),
                     FLEP8 = mean(ft > 8), ConditionalFlameLength = mean(ft))])
write_ds(S, "OutputEffectsSummary")
for (i in seq_len(nrow(S))) say("%-20s burns %4.1f%%  crown %3.0f%%  active %3.0f%%  FLEP4 %.2f  FLEP8 %.2f  CFL %.1f ft", S$FuelGroup[i], 100 * S$BurnShare[i],
                                100 * S$CrownShare[i], 100 * S$ActiveCrownShare[i], S$FLEP4[i], S$FLEP8[i], S$ConditionalFlameLength[i])
progressBar(); progressBar(type = "end")
