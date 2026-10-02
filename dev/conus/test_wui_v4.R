# Option A check: SILVIS v4 (HUDEN2010 on 2020 blocks) vs the 2010 cp12 release, on P26 - the huden layer, then P2's fit and surfaces
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra); library(sf) }); sf_use_s2(FALSE)
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; V4 <- "F:/lucas-master/Data Sources/WUI/v4_CONUS/Data/CONUS_WUI_block_1990_2020_change_v4.gdb"
ref <- rast("F:/lucasFireC2F/Data/ignition_covariates/huden.tif"); a <- rast("F:/lucasFireC2F/dev/conus/cov_test/huden.tif"); ref <- crop(ref, a, snap = "near")
lyr <- st_layers(V4)$name[1]; wb <- read_sf(V4, layer = lyr, wkt_filter = st_as_text(st_transform(st_as_sfc(st_bbox(a)), st_crs(read_sf(V4, query = sprintf("SELECT * FROM \"%s\" LIMIT 0", lyr))))))
wb <- st_make_valid(st_transform(wb, crs(a))); h4 <- round(log1p(rasterize(vect(wb), rast(a), field = "HUDEN2010", background = 0)) * 100)
x <- values(h4, mat = FALSE); y <- values(ref, mat = FALSE); k <- !is.na(x) & !is.na(y)
cat(sprintf("huden v4 vs cp12 on the P26 window (%s cells): identical %.1f%%, |d| > 10 (~10%% density) %.2f%%, correlation %.4f; cells > 0: %.2f%% vs %.2f%%\n",
  format(sum(k), big.mark = ","), 100 * mean(x[k] == y[k]), 100 * mean(abs(x[k] - y[k]) > 10), cor(x[k], y[k]), 100 * mean(x[k] > 0), 100 * mean(y[k] > 0)))
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F CONUS test.ssim"))); P <- rsyncrosim::project(L, "National inputs")
d <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); d$HousingDensityFile <- V4; d$DataSourceId <- NULL; d$ProjectId <- NULL
saveDatasheet(P, d, "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
S <- rsyncrosim::scenario(P, "P26 P2 national SILVIS v4", sourceScenario = rsyncrosim::scenario(P, "P26 P2 national"), overwrite = TRUE); dependency(S) <- 6L
description(S) <- "CONUS Phase 3 option A test: P2 on the national P26 prep with SILVIS v4 (RDS-2015-0012-4, HUDEN2010 on 2020 blocks) instead of the 2010 cp12 release."
tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"])); g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), optional = TRUE, lookupsAsFactors = FALSE))
R0 <- rsyncrosim::scenario(L, 10L); cat("\nfits (v4 vs cp12 result 10):\n"); print(rbind(cbind(src = "v4", g(R, "OutputIgnitionFit")[, .(Cause, Presences, CrossValidatedAUC)]), cbind(src = "cp12", g(R0, "OutputIgnitionFit")[, .(Cause, Presences, CrossValidatedAUC)])))
a1 <- g(R, "IgnitionGrid"); b1 <- g(R0, "IgnitionGrid")
for (cz in a1$Cause) { u <- values(rast(a1[Cause == cz]$FileName[1]), mat = FALSE); v <- values(rast(b1[Cause == cz]$FileName[1]), mat = FALSE); n <- v > 0
  su <- u / sum(u); sv <- v / sum(v)   # as ignition probabilities
  cat(sprintf("  %-9s surface correlation %.4f; share of ignitions moved %.2f%%; top-decile cells shared %.1f%%\n", cz, cor(u[n], v[n]), 50 * sum(abs(su - sv)),
              100 * mean(rank(-u[n]) <= 0.1 * sum(n) & rank(-v[n]) <= 0.1 * sum(n)) / 0.1)) }
