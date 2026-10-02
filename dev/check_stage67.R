# Stages 6-7 (a result scenario) vs the validated own-draw run: 217's effects raster and 224's risk rasters
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); S <- rsyncrosim::scenario(L, id)
B <- "F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own"
fx <- as.data.table(datasheet(S, "lucasFireC2F_OutputFireEffects", lookupsAsFactors = FALSE)); em <- as.data.table(datasheet(rsyncrosim::project(L, 1), "lucasFireC2F_EffectsMetric")); fx[, Metric := em$Code[match(Metric, em$Name)]]
ref <- rast(file.path(B, "effects_roth_raws_cbh15_net_cruz_r4.tif"))
n <- values(ref[["burn_events"]], mat = FALSE); ok <- n >= 10
cmp <- function(a, b, k) { k <- k & !is.na(a) & !is.na(b); c(n = sum(k), maxdiff = max(abs(a[k] - b[k])), meanA = mean(a[k]), meanB = mean(b[k]), cor = cor(a[k], b[k])) }
for (m in c("CFL", "FLEP4", "FLEP8", "FLP6", "PCROWN", "PACTIVE")) {
  a <- values(rast(fx[Metric == m]$FileName[1]), mat = FALSE); b <- values(ref[[if (m == "CFL") "CFL_ft" else m]], mat = FALSE)
  r <- cmp(a, b, ok); cat(sprintf("%-8s cells %d  mean pkg %.4f vs 217 %.4f  max |diff| %.2e  cor %.5f\n", m, r["n"], r["meanA"], r["meanB"], r["maxdiff"], r["cor"])) }
a <- values(rast(fx[Metric == "BURNS"]$FileName[1]), mat = FALSE); cat(sprintf("BURNS    identical cells %.2f%%\n", 100 * mean(a == n)))
rk <- as.data.table(datasheet(S, "lucasFireC2F_OutputRisk", lookupsAsFactors = FALSE)); rm_ <- as.data.table(datasheet(rsyncrosim::project(L, 1), "lucasFireC2F_RiskMetric")); rk[, Metric := rm_$Code[match(Metric, rm_$Name)]]; RD <- file.path(B, "risk")
for (p in list(c("HIBP", "hibp.tif"), c("CRPS", "crps_oozed.tif"), c("RPS", "rps.tif"), c("RPSCLASS", "rps_class.tif"))) {
  a <- values(rast(rk[Metric == p[1]]$FileName[1]), mat = FALSE); b <- values(rast(file.path(RD, p[2])), mat = FALSE)
  r <- cmp(a, b, rep(TRUE, length(a))); cat(sprintf("%-8s cells %d  mean pkg %.4f vs 224 %.4f  max |diff| %.2e  cor %.5f\n", p[1], r["n"], r["meanA"], r["meanB"], r["maxdiff"], r["cor"])) }
