# =============================================================================
# C6 - Score Against Fire Records
# =============================================================================
# Scores a model run's kept fires against the historical record (validated P26 script 176): for the fires at or above
# ResampleOption.MinimumFireSize in CalibrationPeriod's fire-record years, fires per year and area per year by size class,
# simulated / observed, plus the size quantiles, by cause and by origin vegetation. (Spatial skill against burned-area holdouts -
# AUC - is C7.) A seed run is not scored: its sizes are not a calibrated population. Score the check run (a Run that uses C5's
# fitted spread days) instead.
# 2.4.0: each test is judged against the noise of a record as short as the real one (report.R model_noise): the range a record of
# that many years would fall in if the model were right, and a verdict (within noise / check / poor). C6 also draws Fig6 (fire sizes
# against the record with those ranges, and the spread days), so every scored run carries its calibration figure without stage R.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); source(file.path(env$PackageDirectory, "src", "report.R"))
go <- read_ds("GrowthOption")
if (nrow(go) && isTRUE(go$SeedRun)) { say("seed run (spread days uniform 1-25): sizes are not a calibrated population, so no score - score a model run that uses the fitted spread days"); quit(save = "no") }
cp <- need("CalibrationPeriod", c("FireFirstYear", "FireLastYear")); NS <- need("RunControl", "MaximumIteration")$MaximumIteration
MINA <- need("ResampleOption", "MinimumFireSize")$MinimumFireSize
st <- need("OutputFireStatistic", c("Area", "ResampleStatus"))
H <- need("HistoricalFire", c("Date", "AreaHa")); fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))
if (any(!is.na(H$FireZone)) && nrow(fzt)) H <- H[FireZone %in% fzt$Name]
Y <- cp$FireFirstYear:cp$FireLastYear; NY <- length(Y)
# by cause (C5 fits a spread-day curve per cause): the record's unknown causes count in the totals but not by cause, so simulated
# counts by cause, which include the unknowns C3 split between causes, run a little above the observed
Hc <- H[as.integer(substr(Date, 1, 4)) %in% Y & AreaHa >= MINA]; sc <- st[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA]
# by origin vegetation (when the scenario has a vegetation map): recorded fires placed by their location, simulated fires by their
# origin cell (a reassigned pool fire keeps the origin it was grown from)
vn <- character(); vm <- veg_map(); Hc[, Veg := NA_character_]; sc[, Veg := NA_character_]
if (!is.null(vm) && all(c("Longitude", "Latitude") %in% names(Hc))) {
  Fl <- read_ds("OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell)]
  sc[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
  sc <- Fl[sc, on = c("bI", "bF")]; sc[, Veg := veg_of_cells(Cell, vm)]; Hc[, Veg := veg_of_lonlat(Longitude, Latitude, vm)]
  vn <- as.data.table(datasheet(prj, ds("VegetationClass"), lookupsAsFactors = FALSE))$Name; vn <- vn[vn %in% c(Hc$Veg, sc$Veg)] }
if (!"Cause" %in% names(Hc)) Hc[, Cause := NA_character_]

obs <- record_tests(Hc$AreaHa, Hc$Cause, Hc$Veg, NY, MINA, vn)
sim <- record_tests(sc$Area, sc$Cause, sc$Veg, NS, MINA, vn)
q <- model_noise(sc$Area, sc$Cause, sc$Veg, sc$Iteration, NS, NY, MINA, vn)
R <- obs[, .(Test, Group, Observed = V, Simulated = sim$V, ModelLow = q$Q05, ModelHigh = q$Q95)][, Ratio := Simulated / Observed]
R[, Verdict := noise_verdict(Observed, q)]
for (k in c("Observed", "Simulated", "Ratio", "ModelLow", "ModelHigh")) R[!is.finite(get(k)), (k) := NA_real_]   # a group with no recorded (or simulated) fires: blank, not Inf/NaN (SyncroSim refuses those)
write_ds(R, "OutputScore")

say("vs the record >= %.1f ha, %d-%d (%d fires, %.1f/yr, %.0f ha/yr); model range = where a %d-year record would fall (90%%) if the model were right:",
    MINA, min(Y), max(Y), nrow(Hc), nrow(Hc) / NY, sum(Hc$AreaHa) / NY, NY)
row <- function(t, g) R[Test == t & Group == g]
for (l in c(SIZE_LABELS(MINA), "TOTAL")) { a <- row("Fires per year", l); b <- row("Area per year (ha)", l)
  say("  %-14s fires/yr %6.2f obs %6.2f sim [%5.2f-%5.2f] %-12s | ha/yr %8.0f obs %8.0f sim [%7.0f-%7.0f] %s", l, a$Observed, a$Simulated, a$ModelLow, a$ModelHigh,
      a$Verdict, b$Observed, b$Simulated, b$ModelLow, b$ModelHigh, b$Verdict) }
for (g in c("p50", "p90", "p99")) { a <- row("Size quantile (ha)", g)
  say("  size %s %8.0f obs %8.0f sim [%7.0f-%7.0f] %s", g, a$Observed, a$Simulated, a$ModelLow, a$ModelHigh, a$Verdict) }
grp <- function(prefix, groups) for (g in groups) { a <- R[startsWith(Test, prefix) & Group == g]
  say("  %-9s fires/yr %5.2f obs %5.2f sim %s | ha/yr %7.0f obs %7.0f sim %s | mean %5.0f obs %5.0f sim ha %s", g,
      a$Observed[1], a$Simulated[1], a$Verdict[1], a$Observed[2], a$Simulated[2], a$Verdict[2], a$Observed[3], a$Simulated[3], a$Verdict[3]) }
say("by cause (the record without its unknown causes):"); grp("By cause: ", c("Human", "Lightning"))
if (length(vn)) { say("by origin vegetation (BpS; the record placed by its FPA-FOD location):"); grp("By origin vegetation: ", vn) }
v <- R$Verdict[!is.na(R$Verdict)]
say("noise test: %d of %d tests within noise, %d check, %d poor (by chance alone about 1 test in 10 lands outside the 90%% range)",
    sum(v == "within noise"), length(v), sum(v == "check"), sum(v == "poor"))

# Fig6, the calibration figure (stage R draws the same figure when it runs)
zl <- if ("FireZone" %in% names(st) && any(!is.na(st$FireZone))) paste(unique(na.omit(st$FireZone)), collapse = " + ") else rsyncrosim::name(scn)
f6 <- file.path(env$TransferDirectory, "Fig6_fire_sizes.png"); dir.create(dirname(f6), showWarnings = FALSE, recursive = TRUE)
fire_size_figure(f6, R, Hc$AreaHa, sc$Area, sc$Iteration, NS, NY, read_ds("SpreadDays"), zl)
write_ds(data.table(Figure = "Fire sizes and calibration", FileName = f6), "OutputReportFigure"); say("figure: %s", basename(f6))
