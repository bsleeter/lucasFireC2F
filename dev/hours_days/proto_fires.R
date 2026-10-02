# Prototype (outside the package): a SEED fire list for joint calibration of burning hours and spread days.
# Fires = the season draws of seasons 1..N of the own-draw population (data/c2fw/p026_LF2020_own), spread days uniform 1..DMAX.
# Each fire takes D consecutive records of its zone x season weather stream (as stage 3), and per spread day d:
#   hours     h_d = clamp(round(A * (FWI_d / FWIREF)^B), 1, HMAX)      (B = 0: constant A h/day)
#   wind      WIND = daily: the stream's daily-mean wind / 1.15 (as now)
#             WIND = raws : x a burning-period ratio drawn per day from the RAWS quantiles of that day's wind bin
#                           (data/wind/p026_raws_ratio_quantiles.csv, the table stage 6 uses)
#   moisture  scenario from day 1 EMC and season (as stage 3)
# Writes data/c2fw/<tag>/fires_r4.rds for 202 (landscape = a junction to the own tag's) plus the per-fire hour schedule.
# Usage: Rscript proto_fires.R <tag> <A> <B> <wind daily|raws> [seasons = 300] [DMAX = 20] [HMAX = 12] [FWIREF = 46]
suppressPackageStartupMessages(library(data.table))
a <- commandArgs(TRUE); TAG <- a[1]; A <- as.numeric(a[2]); B <- as.numeric(a[3]); WIND <- a[4]
NS <- if (length(a) > 4) as.integer(a[5]) else 300L; DMAX <- if (length(a) > 5) as.integer(a[6]) else 20L
HMAX <- if (length(a) > 6) as.numeric(a[7]) else 12; FREF <- if (length(a) > 7) as.numeric(a[8]) else 46
M <- "F:/lucas-master/Models/California 30m BurnP3"; BASE <- file.path(M, "data/c2fw/p026_LF2020_own"); OUT <- file.path(M, "data/c2fw", TAG)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(file.path(OUT, "landscape"))) system2("cmd", c("/c", "mklink", "/J", shQuote(gsub("/", "\\\\", file.path(OUT, "landscape"))), shQuote(gsub("/", "\\\\", file.path(BASE, "landscape")))), stdout = FALSE)
x <- readRDS(file.path(BASE, "fires_r4.rds"))
CURVE <- Sys.getenv("PROTO_CURVE", "")   # a fitted days curve (Days, RelativeFrequency): FULL run - every fire incl. the pool, D from the curve
f <- if (nzchar(CURVE)) as.data.table(x$fires) else as.data.table(x$fires)[Iteration >= 1 & Iteration <= NS]
wx <- fread(file.path(M, "data/weather/c2fw_stream_p026_dT+0.0.csv")); setorder(wx, WeatherZone, Season, ord)
q <- fread(file.path(M, "data/wind/p026_raws_ratio_quantiles.csv")); qb <- split(q[order(p)], q[order(p)]$lo); los <- sort(unique(q$lo))
emc <- function(t_c, rh) { t <- t_c * 9 / 5 + 32
  ifelse(rh < 10, 0.03229 + 0.281073 * rh - 0.000578 * rh * t, ifelse(rh < 50, 2.22749 + 0.160107 * rh - 0.01478 * t, 21.0606 + 0.005565 * rh^2 - 0.00035 * rh * t - 0.483199 * rh)) }
wx[, dcls := cut(emc(Temperature, RelativeHumidity), c(-Inf, 4.5, 7.5, 10.5, Inf), labels = FALSE)]
live <- c(Summer = 1, Autumn = 1, Spring = 2, Winter = 3); blocks <- split(seq_len(nrow(wx)), paste(wx$WeatherZone, wx$Season))
set.seed(20260926); f[, `:=`(FireDuration = sample.int(DMAX, .N, replace = TRUE), wseed = sample.int(.Machine$integer.max, .N))]
if (nzchar(CURVE)) { cv <- fread(CURVE)[RelativeFrequency > 0]; set.seed(20260927); f[, FireDuration := as.integer(sample(cv$Days, .N, replace = TRUE, prob = cv$RelativeFrequency))]
  cat(sprintf("FULL run: %s fires (seasons %d, pool %s), spread days from %s, mean %.2f\n", format(nrow(f), big.mark = ","), max(f$Iteration), format(sum(f$Iteration == 0), big.mark = ","), CURVE, mean(f$FireDuration))) }
hourly <- vector("list", nrow(f)); sched <- vector("list", nrow(f)); scen <- integer(nrow(f)); fwi1 <- numeric(nrow(f))
for (i in seq_len(nrow(f))) {
  b <- blocks[[paste(f$WeatherZone[i], f$Season[i])]]; D <- f$FireDuration[i]; set.seed(f$wseed[i]); s <- sample.int(length(b), 1)
  d <- wx[b[((s - 1 + 0:(D - 1)) %% length(b)) + 1]]
  h <- pmin(HMAX, pmax(1, round(A * (d$FireWeatherIndex / FREF)^B)))
  ws <- d$WindSpeed / 1.15
  if (WIND == "raws") { lo <- los[findInterval(ws, los)]; r <- mapply(function(l, u) { t <- qb[[as.character(l)]]; approx(t$p, t$ratio, xout = u, ties = "ordered")$y }, lo, runif(D)); ws <- ws * r }
  scen[i] <- max(d$dcls[1], live[[f$season_name[i]]]); fwi1[i] <- d$FireWeatherIndex[1]
  k <- rep(seq_len(D), times = h)
  hourly[[i]] <- data.frame(key = f$key[i], hour = seq_along(k), WS = round(ws[k], 2), WD = d$WindDirection[k])
  sched[[i]] <- h
}
f[, `:=`(scenario = scen, fwi0 = fwi1, HoursBurning = vapply(sched, sum, 0))]
names(sched) <- f$key
saveRDS(list(fires = as.data.frame(f), hourly = do.call(rbind, hourly), grid_file = file.path(OUT, "landscape/fuels.tif"), library = x$library, result = 4,
             schedule = sched, params = list(A = A, B = B, WIND = WIND, NS = NS, DMAX = DMAX, HMAX = HMAX, FWIREF = FREF)), file.path(OUT, "fires_r4.rds"))
hh <- unlist(sched)
cat(sprintf("%s: %d seed fires (seasons 1-%d), spread days uniform 1-%d; hours/day mean %.2f (p10 %g, p50 %g, p90 %g, max %g); total hours mean %.1f; wind %s\n",
            TAG, nrow(f), NS, DMAX, mean(hh), quantile(hh, .1), quantile(hh, .5), quantile(hh, .9), max(hh), mean(f$HoursBurning), WIND))
