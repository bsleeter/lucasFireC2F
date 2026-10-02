# fit_spread_days() on the validated h2 seed run must reproduce 204's P26 curves (1992-2020 and 2006-2020 targets);
# fsim_threshold / count_fit / cause_mix must reproduce 184's threshold and count and the library's season x cause shares.
suppressPackageStartupMessages({ library(sf); library(data.table) })
source("F:/lucasFireC2F/src/calibration.R")
M <- "F:/lucas-master/Models/California 30m BurnP3"; sf_use_s2(FALSE)
f <- readRDS(file.path(M, "data/c2fw/p026_LF2020_h2_seed/fires_r4.rds"))$fires
ch <- do.call(rbind, lapply(list.files(file.path(M, "data/c2fw/p026_LF2020_h2_seed/runs_r4"), "^chunk_\\d+\\.rds$", full.names = TRUE), function(x) readRDS(x)$res))
f$ha <- ch$area_ha[match(f$key, ch$key)]; stopifnot(!anyNA(f$ha))
zone <- read_sf(file.path(M, "spatial/pyromes_150m_LF2020/pyrome_026/zone.gpkg"))
fpa <- read_sf("F:/lucas-master/Data Sources/FPA-FOD/Data/FPA_FOD_20221014.gpkg",
  query = "SELECT FIRE_YEAR, FIRE_SIZE, DISCOVERY_DATE, NWCG_CAUSE_CLASSIFICATION AS CAUSE, Shape FROM Fires WHERE STATE IN ('CA','NV','OR','AZ') AND FIRE_YEAR >= 1992")
fpa <- fpa[st_within(st_transform(fpa, st_crs(zone)), st_union(zone), sparse = FALSE)[, 1], ] |> st_drop_geometry()
fpa$ha <- fpa$FIRE_SIZE * 0.404686; ok <- TRUE
for (w in list(c(1992, 2020, "p026_LF2020_h2_seed/days_curve.csv"), c(2006, 2020, "p026_LF2020_h2_seed/days_curve_2006_2020.csv"))) {
  y <- fpa$FIRE_YEAR >= as.integer(w[1]) & fpa$FIRE_YEAR <= as.integer(w[2])
  r <- fit_spread_days(f$ha, f$FireDuration, fpa$ha[y], 36.8)
  ref <- fread(file.path(M, "data/c2fw", w[3])); d <- max(abs(r$curve$RelativeFrequency - ref$w[match(r$curve$Days, ref$D)]))
  cat(sprintf("%s-%s: KS %.3f, area ratio %.2f, mean days %.2f; max |w - 204| %.1e\n", w[1], w[2], r$summary$KS, r$summary$AreaRatio, r$summary$MeanSpreadDays, d))
  if (d > 1e-5) ok <- FALSE
}
y <- fpa$FIRE_YEAR <= 2020; thr <- fsim_threshold(fpa$ha[y]); cf <- count_fit(fpa$FIRE_YEAR[y], fpa$ha[y], thr, 1992:2020)
cat(sprintf("FSim threshold %.1f ha (184: 36.8); count mean %.3f sd %.3f (184: 10.931 / 7.644)\n", thr, cf$mean, cf$sd))
if (abs(thr - 36.8) > 0.1 || abs(cf$mean - 10.931) > 1e-3 || abs(cf$sd - 7.644) > 1e-3) ok <- FALSE
se <- data.frame(Name = c("Spring", "Summer", "Autumn", "Winter"), FirstMonth = c(3, 6, 10, 12), LastMonth = c(5, 9, 11, 2))
mix <- cause_mix(season_of(as.Date(fpa$DISCOVERY_DATE, format = "%m/%d/%Y")[y], se),
                 ifelse(fpa$CAUSE[y] == "Human", "Human", ifelse(fpa$CAUSE[y] == "Natural", "Lightning", NA)), fpa$ha[y], thr)   # the same large-fire population as the count
lib <- data.frame(Season = c("Autumn", "Autumn", "Spring", "Spring", "Summer", "Summer", "Winter"), Cause = rep(c("Human", "Lightning"), 4)[1:7],
                  RL = c(0.11788421, 0.01733591, 0.04920459, 0.01054384, 0.44486904, 0.3538731, 0.00628931))
m <- merge(mix, lib, by = c("Season", "Cause")); cat(sprintf("season x cause: %d rows, max |diff| from the library %.1e\n", nrow(m), max(abs(m$RelativeLikelihood - m$RL))))
# the development library took the shares at 36.8 ha but the count at the unrounded threshold (36.83): one fire apart;
# the package uses the unrounded threshold for both, so the shares may differ by up to one fire's weight
if (nrow(m) != 7 || max(abs(m$RelativeLikelihood - m$RL)) > 1 / 300) ok <- FALSE
cat(if (ok) "CALIBRATION FUNCTIONS REPRODUCE 184 / 158 / 204\n" else "CALIBRATION FUNCTIONS DIFFER\n")
