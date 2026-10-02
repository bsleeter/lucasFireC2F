# Phase 2 CONUS test: in "National inputs", drop the weather zone grid, the zone-daily table and the RAWS catalogue (P1, P3, P5 build
# them from the NWS zones, gridMET + MTBS weights, and the FEMS national catalogue); run P26's prep (P1, P3, P5) and compare with
# SN&F result 32 (weather zone grid, Daily Weather) and 81 (wind table).
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; DS <- "F:/lucas-master/Data Sources"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F CONUS test.ssim"))); P <- rsyncrosim::project(L, "National inputs")
d <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE); d$WeatherZoneGridFile <- NA; d$ZoneDailyWeatherFile <- NA; d$RawsCatalogueFile <- NA
d$GridMetFolder <- file.path(DS, "Climate/gridMET"); d$DataSourceId <- NULL; d$ProjectId <- NULL
saveDatasheet(P, d, "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
S <- rsyncrosim::scenario(P, "P26 Prep national weather", sourceScenario = rsyncrosim::scenario(P, "P26 Prep test"), overwrite = TRUE)
description(S) <- "CONUS Phase 2 test (2.1.0): P26 prep (P1, P3, P5) with national weather inputs only - weather zones rasterized from NWS fz03mr26, zone daily weather from gridMET with MTBS fire-activity weights, RAWS stations from the FEMS national catalogue."
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- sub(" +$", "", strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]])
cat(sprintf("\n==== result %d (%.1f min)\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
cat(unique(grep("weather zones|gridMET|daily weather|catalogue|stations|time zone|burning-period|daily wind|Table|WARNING|Failure|Error|error", lg, value = TRUE)), sep = "\n")
SNF <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim"))); g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), optional = TRUE, lookupsAsFactors = FALSE))
a <- rast(g(R, "LandscapeRasters")$WeatherZoneFileName[1]); b <- rast(g(rsyncrosim::scenario(SNF, 32), "LandscapeRasters")$WeatherZoneFileName[1])
va <- values(a, mat = FALSE); vb <- values(b, mat = FALSE); cat(sprintf("\nweather zone grid vs result 32: %s cells, %s differ (%.3f%%), NA new/ref %d/%d\n", format(length(va), big.mark = ","),
  format(sum(va != vb, na.rm = TRUE), big.mark = ","), 100 * mean(va != vb, na.rm = TRUE), sum(is.na(va)), sum(is.na(vb))))
n <- g(R, "DailyWeather"); o <- g(rsyncrosim::scenario(SNF, 32), "DailyWeather"); for (x in list(n, o)) x[, Date := substr(as.character(Date), 1, 10)]
m <- merge(n, o, by = c("WeatherZone", "Date"), suffixes = c(".n", ".o")); cat(sprintf("daily weather: %s rows new, %s ref, %s matched; zones %d / %d\n", format(nrow(n), big.mark = ","), format(nrow(o), big.mark = ","), format(nrow(m), big.mark = ","), uniqueN(n$WeatherZone), uniqueN(o$WeatherZone)))
for (v in c("Temperature", "RelativeHumidity", "WindSpeed", "WindDirection", "Precipitation")) { x <- m[[paste0(v, ".n")]]; y <- m[[paste0(v, ".o")]]
  dd <- if (v == "WindDirection") abs(((x - y) + 180) %% 360 - 180) else abs(x - y); cat(sprintf("  %-17s mean |d| %.4f  p99 |d| %.3f  identical %.1f%%\n", v, mean(dd), quantile(dd, .99), 100 * mean(dd < 1e-6))) }
w1 <- g(R, "WindRatio"); w0 <- g(rsyncrosim::scenario(SNF, 81), "WindRatio"); f <- function(q, p) approx(q$Quantile, q$Ratio, p, ties = "ordered")$y
cat("wind table vs result 81 (median / p90 / p99):\n"); for (b in sort(unique(w1$LowerWindSpeed))) { q1 <- w1[LowerWindSpeed == b]; q0 <- w0[LowerWindSpeed == b]
  cat(sprintf("  %4g km/h: %.2f / %.2f / %.2f  vs  %.2f / %.2f / %.2f\n", b, f(q1, .5), f(q1, .9), f(q1, .99), f(q0, .5), f(q0, .9), f(q0, .99))) }
