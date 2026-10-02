# Stage 3 (a result scenario) vs 201's fires_r4.rds for p026_LF2020_own: spread days, hours, moisture scenario, hourly wind
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); S <- rsyncrosim::scenario(L, id)
a <- as.data.table(datasheet(S, "lucasFireC2F_OutputFireList", lookupsAsFactors = FALSE))
h <- as.data.table(datasheet(S, "lucasFireC2F_OutputFireWeather", lookupsAsFactors = FALSE))
r <- readRDS("F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own/fires_r4.rds")
b <- as.data.table(r$fires); hb <- as.data.table(r$hourly)
m <- merge(a, b[, .(Iteration, FireID, D = FireDuration, H = HoursBurning, sc = scenario, key)], by = c("Iteration", "FireID"))
cat(sprintf("fires matched %d of %d / %d\n", nrow(m), nrow(a), nrow(b)))
cat(sprintf("spread days equal %d, hours equal %d, moisture scenario equal %d (of %d)\n", sum(m$SpreadDays == m$D), sum(m$HoursBurning == m$H),
            sum(m$MoistureScenario == c("D1L1", "D2L2", "D3L3", "D4L4")[m$sc]), nrow(m)))
h[, key := sprintf("i%05d_f%03d", Iteration, FireID)]
x <- merge(h, hb, by.x = c("key", "Hour"), by.y = c("key", "hour"))
cat(sprintf("hourly rows: package %d, 201 %d, matched %d; max |dWS| %.3g, max |dWD| %.3g\n", nrow(h), nrow(hb), nrow(x),
            max(abs(x$WindSpeed - x$WS)), max(abs(x$WindDirection - x$WD))))
ok <- nrow(m) == nrow(b) && all(m$SpreadDays == m$D) && nrow(x) == nrow(hb) && max(abs(x$WindSpeed - x$WS)) < 1e-6
cat(if (ok) "STAGE 3 REPRODUCES 201\n" else "STAGE 3 DIFFERS\n")
