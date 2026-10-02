# Runs the P26 calibration scenario and checks C2 / C3 / C5 against the development record.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(DBI) })
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
P <- rsyncrosim::project(L, "P26 Northern Sierra Nevada"); C <- rsyncrosim::scenario(P, "P26 Calibration 1992-2020")
t0 <- Sys.time(); tryCatch(run(C), error = function(e) NULL)
ids <- rsyncrosim::scenario(P)[, "ScenarioId"]; R <- rsyncrosim::scenario(L, max(ids))
lg <- strsplit(paste(runLog(R), collapse = "\n"), "\n")[[1]]
cat(unique(trimws(grep("imported|threshold|ignition count|season x cause|delta|spread-day|target|fitted|distribution|observed|fires/yr|vs the record|seasons:|SEED|done in|Failure|Error|error|TRANSFORMER: C", lg, value = TRUE))), sep = "\n")
cat(sprintf("\n%.1f min; RESULT_SCENARIO %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), max(ids)))
g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
# C2 vs the library stream (scenario 3 of the development library) for the same zones, block by block
ws <- g("WeatherStream")
con <- dbConnect(RSQLite::SQLite(), "F:/lucas-master/Models/California 30m BurnP3/California BurnP3 Pyrome026 LF2020.ssim", flags = RSQLite::SQLITE_RO)
ref <- as.data.table(dbGetQuery(con, "select s.[Order] ord, se.Name Season, wz.Name WeatherZone, s.FireWeatherIndex FWI from burnP3Plus_WeatherStream s
  join burnP3Plus_Season se on se.SeasonId = s.Season join burnP3Plus_WeatherZone wz on wz.WeatherZoneId = s.WeatherZone where s.ScenarioId = 3"))
dbDisconnect(con); ref <- ref[WeatherZone %in% unique(ws$WeatherZone)]
setorder(ref, WeatherZone, Season, ord); setorder(ws, WeatherZone, Season, Order)
cat(sprintf("C2 stream: %d rows vs library %d; FWI identical in order within zone-season: %s\n", nrow(ws), nrow(ref),
            nrow(ws) == nrow(ref) && isTRUE(all.equal(ws$FireWeatherIndex, ref$FWI, tolerance = 1e-6))))
ic <- g("IgnitionCount"); cat(sprintf("C3 count: Gamma %.3f / %.3f (library 10.931 / 7.644)\n", ic$Mean, ic$DistributionSD))
sd <- g("SpreadDays"); cat(sprintf("C5 curve: mean %.2f spread days over %d day counts (204 on the FireSTARR-fire seed: 2.88)\n",
                                   sum(sd$Days * sd$RelativeFrequency) / sum(sd$RelativeFrequency), nrow(sd)))
