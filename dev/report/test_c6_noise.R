# Tests C6's noise test and Fig6 (src/report.R record_tests, model_noise, fire_size_figure) on a Demo library Run result, read
# straight from the .ssim with SQLite (read-only, no SyncroSim): Rscript test_c6_noise.R [result id, default 6]
# Vegetation tests are left out (they need the vegetation map); everything else is scored as C6 scores it.
suppressPackageStartupMessages({ library(DBI); library(data.table) })
source("F:/lucasFireC2F/src/report.R")
RID <- as.integer(commandArgs(TRUE)[1] %||% 6L); if (is.na(RID)) RID <- 6L
con <- dbConnect(RSQLite::SQLite(), "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F-Demo.ssim", flags = RSQLite::SQLITE_RO)
q <- function(s) as.data.table(dbGetQuery(con, sprintf(s, RID)))
cz <- as.data.table(dbGetQuery(con, "SELECT CauseId, Name FROM lucasFireC2F_Cause"))
st <- q("SELECT Iteration, Cause, Area, ResampleStatus FROM lucasFireC2F_OutputFireStatistic WHERE ScenarioId = %d")
H <- q("SELECT Date, AreaHa, Cause FROM lucasFireC2F_HistoricalFire WHERE ScenarioId = %d")
cp <- q("SELECT FireFirstYear, FireLastYear FROM lucasFireC2F_CalibrationPeriod WHERE ScenarioId = %d")
NS <- q("SELECT MaximumIteration FROM lucasFireC2F_RunControl WHERE ScenarioId = %d")$MaximumIteration
MINA <- q("SELECT MinimumFireSize FROM lucasFireC2F_ResampleOption WHERE ScenarioId = %d")$MinimumFireSize
sd <- q("SELECT Days, RelativeFrequency FROM lucasFireC2F_SpreadDays WHERE ScenarioId = %d"); dbDisconnect(con)
st[, Cause := cz$Name[match(Cause, cz$CauseId)]]; H[, Cause := cz$Name[match(Cause, cz$CauseId)]]
Y <- cp$FireFirstYear:cp$FireLastYear; NY <- length(Y)
Hc <- H[as.integer(substr(Date, 1, 4)) %in% Y & AreaHa >= MINA]; sc <- st[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA]
cat(sprintf("result %d: %d seasons, record %d-%d (%d fires >= %.1f ha), %d simulated fires; season range %d-%d\n",
            RID, NS, min(Y), max(Y), nrow(Hc), MINA, nrow(sc), min(sc$Iteration), max(sc$Iteration)))
t0 <- Sys.time()
obs <- record_tests(Hc$AreaHa, Hc$Cause, NA, NY, MINA); sim <- record_tests(sc$Area, sc$Cause, NA, NS, MINA)
nz <- model_noise(sc$Area, sc$Cause, rep(NA_character_, nrow(sc)), sc$Iteration, NS, NY, MINA)
R <- obs[, .(Test, Group, Observed = V, Simulated = sim$V, ModelLow = nz$Q05, ModelHigh = nz$Q95)][, Ratio := Simulated / Observed]
R[, Verdict := noise_verdict(Observed, nz)]
cat("noise test", format(Sys.time() - t0), "\n"); print(R, digits = 3)
f <- sprintf("F:/lucasFireC2F/dev/report/out/Fig6_fire_sizes_result%d.png", RID)
t0 <- Sys.time(); fire_size_figure(f, R, Hc$AreaHa, sc$Area, sc$Iteration, NS, NY, sd, commandArgs(TRUE)[2] %||% sprintf("Demo result %d", RID)); cat("figure", format(Sys.time() - t0), f, "\n")
