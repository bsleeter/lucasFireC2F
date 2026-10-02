# Prototype scoring of one hours-function candidate (a seed run from proto_fires.R grown by 202 with the spread network):
#  1. SIZE     fit the spread-day distribution to FPA-FOD 1992-2020 fires >= 36.8 ha with the package's own C5 fit
#              (src/calibration.R fit_spread_days: size CDF + 5 x log mean-size error)
#  2. WEIGHTS  each seed fire (spread days D) gets weight w_D / n_D, the fitted draw weight over the seed count at D, so
#              the weighted seed fires >= 36.8 ha stand for a model run with the fitted curve (no second run needed)
#  3. PROGRESSION  for weighted fires >= 1,000 ha, day 2+: the head run (max distance from the day's cells to the nearest
#              earlier cell), daily growth, active days - the SAME metrics as the VIIRS set (observed_vs_model_daily_runs.R)
#  4. SCORE    KS distance (weighted) for daily run, largest run per fire, daily growth; size-fit KS and area ratio
# Appends one row to dev/hours_days/scores.csv.
# Usage: Rscript proto_score.R <tag>
suppressPackageStartupMessages({ library(data.table); library(FNN); library(rsyncrosim) })
TAG <- commandArgs(TRUE)[1]; M <- "F:/lucas-master/Models/California 30m BurnP3"; B <- file.path(M, "data/c2fw", TAG); OUT <- "F:/lucasFireC2F/dev/hours_days"
source("F:/lucasFireC2F/src/calibration.R"); MINA <- 36.8; BIG <- 1000; NC <- 1260L; CS <- 150
fl <- readRDS(file.path(B, "fires_r4.rds")); f <- as.data.table(fl$fires); P <- fl$params
ch <- list.files(file.path(B, "runs_r4"), "^chunk_\\d+\\.rds$", full.names = TRUE)
res <- list(); bt <- list()
for (p in ch) { z <- readRDS(p); res[[p]] <- z$res
  for (j in seq_along(z$burnt)) { k <- z$res$key[j]; if (!length(z$burnt[[j]]) || z$res$area_ha[j] < BIG) next
    bt[[k]] <- data.table(Cell = z$burnt[[j]], period = z$msg[[j]][, "period"]) } }
res <- rbindlist(res); f <- res[f, on = "key"]
cat(sprintf("%s: %d/%d fires grown; A %g B %g wind %s\n", TAG, sum(!is.na(f$area_ha)), nrow(f), P$A, P$B, P$WIND))
f <- f[!is.na(area_ha)]

# ---- 1 size fit ---------------------------------------------------------------------------------------------------------------
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
H <- as.data.table(datasheet(rsyncrosim::scenario(L, 1), "lucasFireC2F_HistoricalFire", lookupsAsFactors = FALSE))[as.integer(substr(Date, 1, 4)) %in% 1992:2020]
fit <- fit_spread_days(f$area_ha, f$FireDuration, H$AreaHa, MINA, 5)
w <- setNames(fit$curve$RelativeFrequency, fit$curve$Days); nD <- f[, .N, by = FireDuration]
f[nD, on = "FireDuration", nd := i.N]; f[, wt := w[as.character(FireDuration)] / nd]; f[is.na(wt), wt := 0]
cat(sprintf("size fit: KS %.3f, area ratio %.3f, mean spread days %.2f\n", fit$summary$KS, fit$summary$AreaRatio, fit$summary$MeanSpreadDays))

# ---- 3 progression of the weighted fires >= 1,000 ha ----------------------------------------------------------------------------
sched <- fl$schedule
runs <- function(x, y, day) { ds <- sort(unique(day)); r <- list()
  for (d in ds[ds > min(ds)]) { A <- which(day == d); Pp <- which(day < d); if (!length(A) || !length(Pp)) next
    nn <- get.knnx(cbind(x[Pp], y[Pp]), cbind(x[A], y[A]), k = 1)$nn.dist[, 1] / 1000
    r[[length(r) + 1]] <- data.table(day = d - min(ds) + 1, run_km = max(nn), growth_ha = length(A) * CS^2 / 1e4) }
  rbindlist(r) }
big <- f[area_ha >= BIG & wt > 0]
pr <- rbindlist(lapply(big$key, function(k) { b <- bt[[k]]; if (is.null(b)) return(NULL); h <- sched[[k]]
  hr <- b$period %/% 60 + 1; day <- findInterval(hr - 1, c(0, cumsum(h)), rightmost.closed = TRUE)   # burning hour -> spread day
  r <- runs(((b$Cell - 1L) %% NC) * CS, -((b$Cell - 1L) %/% NC) * CS, day); if (!nrow(r)) return(NULL); r[, key := k]; r[, active := uniqueN(day)]; r }))
pr <- big[, .(key, wt, area_ha)][pr, on = "key"]
cat(sprintf("progression: %d weighted fires >= %d ha with %d fire-days (day 2+)\n", uniqueN(pr$key), BIG, nrow(pr)))

# ---- 4 score against VIIRS -----------------------------------------------------------------------------------------------------
ob <- fread("F:/lucasFireC2F/dev/spread/observed_daily_runs_p026.csv"); oa <- fread("F:/lucasFireC2F/dev/spread/observed_fires_p026.csv")
wks <- function(xo, xs, ws) { g <- sort(unique(c(xo, xs))); Fo <- ecdf(xo)(g); o <- order(xs); cs <- cumsum(ws[o]) / sum(ws); Fs <- approx(xs[o], cs, g, method = "constant", yleft = 0, yright = 1, ties = max)$y; max(abs(Fo - Fs)) }
wq <- function(x, w, p) { o <- order(x); cs <- cumsum(w[o]) / sum(w); sapply(p, function(q) x[o][which(cs >= q)[1]]) }
mx <- pr[, .(mx = max(run_km), wt = wt[1]), by = key]; ac <- pr[, .(active = active[1], wt = wt[1]), by = key]
row <- data.table(tag = TAG, A = P$A, B = P$B, wind = P$WIND, seasons = P$NS, dmax = P$DMAX,
  size_KS = fit$summary$KS, area_ratio = fit$summary$AreaRatio, mean_days = fit$summary$MeanSpreadDays,
  run_KS = wks(ob$run_km, pr$run_km, pr$wt), maxrun_KS = wks(ob[, max(run_km), by = fire]$V1, mx$mx, mx$wt), growth_KS = wks(ob$growth_ha, pr$growth_ha, pr$wt),
  run_p50 = wq(pr$run_km, pr$wt, .5), run_p90 = wq(pr$run_km, pr$wt, .9), run_p99 = wq(pr$run_km, pr$wt, .99),
  maxrun_p50 = wq(mx$mx, mx$wt, .5), maxrun_p90 = wq(mx$mx, mx$wt, .9), active_p50 = wq(ac$active, ac$wt, .5),
  obs_run_p50 = median(ob$run_km), obs_run_p90 = quantile(ob$run_km, .9), obs_run_p99 = quantile(ob$run_km, .99),
  obs_maxrun_p50 = median(ob[, max(run_km), by = fire]$V1), obs_maxrun_p90 = quantile(ob[, max(run_km), by = fire]$V1, .9), obs_active_p50 = median(oa$active_days))
row[, score := size_KS + run_KS + maxrun_KS + growth_KS]
print(row[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
fwrite(row, file.path(OUT, "scores.csv"), append = file.exists(file.path(OUT, "scores.csv")))
fwrite(fit$curve, file.path(B, "days_curve_proto.csv")); saveRDS(list(fit = fit, pr = pr), file.path(B, "proto_score.rds"))
