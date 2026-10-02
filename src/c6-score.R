# =============================================================================
# C6 - Score Against Fire Records
# =============================================================================
# Scores a model run's kept fires against the historical record (validated P26 script 176): for the fires at or above
# ResampleOption.MinimumFireSize in CalibrationPeriod's fire-record years, fires per year and area per year by size class,
# simulated / observed, plus the size quantiles. (Spatial skill against burned-area holdouts - AUC - is added with the MTBS
# import.) A seed run is scored for completeness but its sizes are not a calibrated population.
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
go <- read_ds("GrowthOption")
if (nrow(go) && isTRUE(go$SeedRun)) { say("seed run (spread days uniform 1-25): sizes are not a calibrated population, so no score - score a model run that uses the fitted spread days"); quit(save = "no") }
cp <- need("CalibrationPeriod", c("FireFirstYear", "FireLastYear")); NS <- need("RunControl", "MaximumIteration")$MaximumIteration
MINA <- need("ResampleOption", "MinimumFireSize")$MinimumFireSize
st <- need("OutputFireStatistic", c("Area", "ResampleStatus")); sim <- st[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA, Area]
H <- need("HistoricalFire", c("Date", "AreaHa")); fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))
if (any(!is.na(H$FireZone)) && nrow(fzt)) H <- H[FireZone %in% fzt$Name]
Y <- cp$FireFirstYear:cp$FireLastYear; obs <- H[as.integer(substr(Date, 1, 4)) %in% Y & AreaHa >= MINA, AreaHa]; NY <- length(Y)
B <- c(MINA, 100, 405, 1000, 3000, 10000, 30000, Inf); lab <- c(sprintf("%.0f-100", MINA), "100-405", "405-1,000", "1,000-3,000", "3,000-10,000", "10,000-30,000", "30,000+")
co <- cut(obs, B, right = FALSE, labels = lab); cs <- cut(sim, B, right = FALSE, labels = lab)
R <- rbindlist(lapply(lab, function(l) { no <- sum(co == l) / NY; ns <- sum(cs == l) / NS; ao <- sum(obs[co == l]) / NY; as <- sum(sim[cs == l]) / NS
  rbind(data.table(Test = "Fires per year", Group = l, Observed = no, Simulated = ns, Ratio = ns / no),
        data.table(Test = "Area per year (ha)", Group = l, Observed = ao, Simulated = as, Ratio = as / ao)) }))
tot <- rbind(data.table(Test = "Fires per year", Group = "TOTAL", Observed = length(obs) / NY, Simulated = length(sim) / NS),
             data.table(Test = "Area per year (ha)", Group = "TOTAL", Observed = sum(obs) / NY, Simulated = sum(sim) / NS))[, Ratio := Simulated / Observed]
qs <- rbindlist(lapply(c(0.5, 0.9, 0.99), function(p) data.table(Test = "Size quantile (ha)", Group = sprintf("p%g", 100 * p),
  Observed = quantile(obs, p, names = FALSE), Simulated = quantile(sim, p, names = FALSE))))[, Ratio := Simulated / Observed]
# by cause (C5 fits a spread-day curve per cause): the record's unknown causes are left out, so simulated counts, which include
# the unknowns C3 split between causes, run a little above the observed
Hc <- H[as.integer(substr(Date, 1, 4)) %in% Y & AreaHa >= MINA]; sc <- st[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA]
bc <- rbindlist(lapply(c("Human", "Lightning"), function(cz) { o <- Hc[Cause == cz, AreaHa]; s <- sc[Cause == cz, Area]
  rbind(data.table(Test = "By cause: fires per year", Group = cz, Observed = length(o) / NY, Simulated = length(s) / NS),
        data.table(Test = "By cause: area per year (ha)", Group = cz, Observed = sum(o) / NY, Simulated = sum(s) / NS),
        data.table(Test = "By cause: mean size (ha)", Group = cz, Observed = mean(o), Simulated = mean(s))) }))[, Ratio := Simulated / Observed]
# by origin vegetation (when the scenario has a vegetation map): recorded fires placed by their location, simulated fires by their
# origin cell (a reassigned pool fire keeps the origin it was grown from)
bv <- NULL; vm <- veg_map()
if (!is.null(vm) && all(c("Longitude", "Latitude") %in% names(Hc))) {
  Fl <- read_ds("OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell)]
  sc[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
  sc <- Fl[sc, on = c("bI", "bF")]; sc[, Veg := veg_of_cells(Cell, vm)]; Hc[, Veg := veg_of_lonlat(Longitude, Latitude, vm)]
  vn <- as.data.table(datasheet(prj, ds("VegetationClass"), lookupsAsFactors = FALSE))$Name; vn <- vn[vn %in% c(Hc$Veg, sc$Veg)]
  bv <- rbindlist(lapply(vn, function(v) { o <- Hc[Veg %in% v, AreaHa]; s <- sc[Veg %in% v, Area]
    rbind(data.table(Test = "By origin vegetation: fires per year", Group = v, Observed = length(o) / NY, Simulated = length(s) / NS),
          data.table(Test = "By origin vegetation: area per year (ha)", Group = v, Observed = sum(o) / NY, Simulated = sum(s) / NS),
          data.table(Test = "By origin vegetation: mean size (ha)", Group = v, Observed = if (length(o)) mean(o) else NA_real_, Simulated = if (length(s)) mean(s) else NA_real_)) }))[, Ratio := Simulated / Observed] }
R <- rbind(R, tot, qs, bc, bv)
for (k in c("Observed", "Simulated", "Ratio")) R[!is.finite(get(k)), (k) := NA_real_]   # a group with no recorded (or simulated) fires: blank, not Inf/NaN (SyncroSim refuses those)
write_ds(R, "OutputScore")
say("vs the record >= %.1f ha, %d-%d (%d fires, %.1f/yr, %.0f ha/yr):", MINA, min(Y), max(Y), length(obs), length(obs) / NY, sum(obs) / NY)
for (l in c(lab, "TOTAL")) { a <- R[Test == "Fires per year" & Group == l]; b <- R[Test == "Area per year (ha)" & Group == l]
  say("  %-14s fires/yr %6.2f obs %6.2f sim %5.2fx | ha/yr %8.0f obs %8.0f sim %5.2fx", l, a$Observed, a$Simulated, a$Ratio, b$Observed, b$Simulated, b$Ratio) }
say("by cause (the record without its unknown causes):")
for (cz in c("Human", "Lightning")) { a <- bc[Group == cz]
  say("  %-9s fires/yr %5.2f obs %5.2f sim %4.2fx | ha/yr %7.0f obs %7.0f sim %4.2fx | mean %5.0f obs %5.0f sim ha", cz,
      a$Observed[1], a$Simulated[1], a$Ratio[1], a$Observed[2], a$Simulated[2], a$Ratio[2], a$Observed[3], a$Simulated[3]) }
if (!is.null(bv)) { say("by origin vegetation (BpS; the record placed by its FPA-FOD location):")
  for (v in unique(bv$Group)) { a <- bv[Group == v]
    say("  %-9s fires/yr %5.2f obs %5.2f sim %4.2fx | ha/yr %7.0f obs %7.0f sim %4.2fx | mean %5.0f obs %5.0f sim ha", v,
        a$Observed[1], a$Simulated[1], a$Ratio[1], a$Observed[2], a$Simulated[2], a$Ratio[2], a$Observed[3], a$Simulated[3]) } }
