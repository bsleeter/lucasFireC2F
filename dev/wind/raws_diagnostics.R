# Diagnostics on the RAWS vs WindNinja comparison: strong regional wind only, and within-station (siting removed)
suppressPackageStartupMessages(library(data.table))
OUT <- "F:/lucasFireC2F/dev/wind/out"; ob <- readRDS(file.path(OUT, "station_days.rds"))
dang <- function(a, b) ((a - b + 180) %% 360) - 180
circ <- function(a) (atan2(sum(sin(a * pi / 180)), sum(cos(a * pi / 180))) * 180 / pi) %% 360
ob <- ob[is.finite(ratio) & ratio > 0 & !is.na(pr)]
cat(sprintf("regional 20-ft wind on stream days: median %.1f, p75 %.1f, p90 %.1f km/h\n", median(ob$gs), quantile(ob$gs, .75), quantile(ob$gs, .9)))
test <- function(d, lab) {
  ss <- d[, .(n = .N, lo = median(log(ratio)), lp = log(median(pr)), od = dang(circ(dev), 0), pdv = median(pd)), by = .(station_id, k)][n >= 10]
  off <- weighted.mean(ss$lo - ss$lp, ss$n)
  a <- ss[, 1 - weighted.mean((lo - lp - off)^2, n) / weighted.mean((lo - weighted.mean(lo, n))^2, n)]
  ss[, `:=`(lo_c = lo - weighted.mean(lo, n), lp_c = lp - weighted.mean(lp, n)), by = station_id]
  w <- ss[, 1 - weighted.mean((lo_c - lp_c)^2, n) / weighted.mean(lo_c^2, n)]
  wr <- ss[, cov.wt(cbind(lo_c, lp_c), wt = n / sum(n), cor = TRUE)$cor[1, 2]]
  # direction, within station: does the station's turning change with regional direction as WindNinja says?
  ss[, `:=`(od_c = dang(od, circ(od)), pd_c = pdv - mean(pdv)), by = station_id]
  cat(sprintf("%-34s cells %4d  SPEED skill all %+4.0f%%  within-station skill %+4.0f%% (r %.2f)  |  DIR mean|dev| %2.0f vs WN %2.0f; within-station %2.0f vs WN %2.0f\n",
              lab, nrow(ss), 100 * a, 100 * w, wr, weighted.mean(abs(ss$od), ss$n), weighted.mean(abs(dang(ss$od, ss$pdv)), ss$n),
              weighted.mean(abs(ss$od_c), ss$n), weighted.mean(abs(ss$od_c - ss$pd_c), ss$n)))
}
test(ob, "all stream days")
for (q in c(.5, .75, .9)) test(ob[gs >= quantile(ob$gs, q)], sprintf("regional wind >= p%d (%.1f km/h)", 100 * q, quantile(ob$gs, q)))
# afternoon vs daily-mean direction mismatch: how far is the station's afternoon from the regional daily direction on strong days
cat(sprintf("\nstation-day |direction dev|: median %.0f deg all days; %.0f deg on days >= p90 regional wind\n",
            median(abs(ob$dev), na.rm = TRUE), median(abs(ob[gs >= quantile(gs, .9)]$dev), na.rm = TRUE)))
