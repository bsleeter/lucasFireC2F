# Daily progression of a FULL prototype run (kept fires >= 1,000 ha) against VIIRS, same metric as observed_vs_model_daily_runs.R
# Usage: Rscript full_check.R <tag>
suppressPackageStartupMessages({ library(data.table); library(FNN) })
TAG <- commandArgs(TRUE)[1]; B <- file.path("F:/lucas-master/Models/California 30m BurnP3/data/c2fw", TAG); NC <- 1260L; CS <- 150; BIG <- 1000
fl <- readRDS(file.path(B, "fires_r4.rds")); sched <- fl$schedule
kept <- fread(file.path(B, "kept_r4.csv")); big <- kept[c2fw_ha >= BIG]$key
runs <- function(x, y, day) { ds <- sort(unique(day)); r <- list()
  for (d in ds[ds > min(ds)]) { A <- which(day == d); P <- which(day < d); if (!length(A) || !length(P)) next
    nn <- get.knnx(cbind(x[P], y[P]), cbind(x[A], y[A]), k = 1)$nn.dist[, 1] / 1000
    r[[length(r) + 1]] <- data.table(day = d - min(ds) + 1, run_km = max(nn), growth_ha = length(A) * CS^2 / 1e4) }
  rbindlist(r) }
pr <- list(); ac <- list()
for (p in list.files(file.path(B, "runs_r4"), "^chunk_\\d+\\.rds$", full.names = TRUE)) { z <- readRDS(p)
  for (j in which(z$res$key %in% big)) { k <- z$res$key[j]; h <- sched[[k]]; hr <- z$msg[[j]][, "period"] %/% 60 + 1
    day <- findInterval(hr - 1, c(0, cumsum(h)), rightmost.closed = TRUE); cl <- z$burnt[[j]]
    r <- runs(((cl - 1L) %% NC) * CS, -((cl - 1L) %/% NC) * CS, day); ac[[k]] <- data.table(key = k, active = uniqueN(day))
    if (nrow(r)) { r[, fire := k]; pr[[k]] <- r } } }
pr <- rbindlist(pr); ac <- rbindlist(ac)
ob <- fread("F:/lucasFireC2F/dev/spread/observed_daily_runs_p026.csv"); oa <- fread("F:/lucasFireC2F/dev/spread/observed_fires_p026.csv")
q <- function(x) round(quantile(x, c(.1, .25, .5, .75, .9, .99)), 2)
ks <- function(a, b) max(abs(ecdf(a)(sort(c(a, b))) - ecdf(b)(sort(c(a, b)))))
cat(sprintf("%s: %d kept fires >= %d ha, %d fire-days (day 2+)\n", TAG, length(big), BIG, nrow(pr)))
cat("daily head run km:\n"); print(rbind(data.table(set = "observed", t(q(ob$run_km))), data.table(set = TAG, t(q(pr$run_km)))))
mo <- ob[, max(run_km), by = fire]$V1; ms <- pr[, max(run_km), by = fire]$V1
cat("largest run per fire km:\n"); print(rbind(data.table(set = "observed", t(q(mo))), data.table(set = TAG, t(q(ms)))))
cat("daily growth ha:\n"); print(rbind(data.table(set = "observed", t(q(ob$growth_ha))), data.table(set = TAG, t(q(pr$growth_ha)))))
cat(sprintf("KS: run %.3f, largest run %.3f, growth %.3f; active days median obs %.0f / sim %.0f\n", ks(ob$run_km, pr$run_km), ks(mo, ms), ks(ob$growth_ha, pr$growth_ha), median(oa$active_days), median(ac$active)))
fwrite(pr, file.path(B, "daily_runs_full.csv"))
