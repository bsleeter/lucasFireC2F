# =============================================================================
# C5 - Fit Spread Days
# =============================================================================
# Fits the spread-day distribution so C2F-W's fire sizes match the record (validated P26 script 204). Runs after a SEED run
# (GrowthOption.SeedRun: stages 1-5 with spread days drawn uniformly over 1..MaximumSpreadDays), which gives a size kernel
# for every day count. The target is the historical fires >= ResampleOption.MinimumFireSize (set by C3) in the fire-record
# years; the fit (calibration.R fit_spread_days) matches the size CDF plus AreaWeight x the log mean-size error.
# Strata (CalibrationOption.SpreadDaysBy): one curve for all fires, one per cause (1.5.0 default), one per ORIGIN VEGETATION
# (Landscape Maps' vegetation map, from BpS: potential vegetation, so recorded origins carry no fuel memory), or one per cause x
# vegetation. Each stratum is fitted to its own recorded fires and its own seed fires (placed by their origin cell / location).
# A stratum needs CalibrationOption.MinimumFiresPerCurve recorded fires (default 25) and 500 seed fires; one with fewer falls back:
# cause x vegetation -> vegetation -> vegetation group (VegetationClass.FallbackGroup: Forest = conifer + hardwood, Non-forest =
# shrub + grass + other) -> all fires. The group level keeps a small shrub or other stratum off the all-fires curve, which the
# timber fires dominate (P26 1.6.0: shrub- and other-origin fires 3x and 7x their recorded mean size on it). A group curve is
# written under each member vegetation that has no curve of its own. A curve for all fires is always written as the last resort;
# stage 3 gives each fire the most specific curve that matches it.
# Writes SpreadDays (used by every later model run that takes this scenario's inputs), the fit by size class, and the summary.
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); source(file.path(env$PackageDirectory, "src", "calibration.R"))
cp <- need("CalibrationPeriod", c("FireFirstYear", "FireLastYear")); co <- need("CalibrationOption")
if (!isTRUE(co$FitSpreadDays)) { say("spread days kept as given (Fit Spread Days is off)"); quit(save = "no") }
go <- read_ds("GrowthOption"); if (!nrow(go) || !isTRUE(go$SeedRun)) fail("C5 needs a seed run: set Fire Growth Options > Seed Run and run stages 1-5 before C5")
MINA <- need("ResampleOption", "MinimumFireSize")$MinimumFireSize
st <- need("OutputFireStatistic", c("Area", "SpreadDays")); Fl <- need("OutputFireList")
st[Fl, on = c("Iteration", "FireID"), `:=`(Pool = i.Pool, Cell = i.Cell)]; st <- st[Pool == FALSE | is.na(Pool)]
H <- need("HistoricalFire", c("Date", "AreaHa")); fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))
if (any(!is.na(H$FireZone)) && nrow(fzt)) H <- H[FireZone %in% fzt$Name]
H <- H[as.integer(substr(Date, 1, 4)) %in% (cp$FireFirstYear:cp$FireLastYear)]
AW <- if (!is.na(co$AreaWeight)) co$AreaWeight else 5; BH <- need("BurningHours", "Hours")$Hours[1]

# ---- strata ----------------------------------------------------------------------------------------------------------------------
BY_LABELS <- c("One curve for all fires", "Cause", "Origin vegetation", "Cause and origin vegetation")   # package.xml list, codes 0-3
by <- co[["SpreadDaysBy"]]; by <- if (is.null(by) || is.na(by)) 1L else if (is.character(by)) match(by, BY_LABELS) - 1L else as.integer(by)
if (is.na(by) || !by %in% 0:3) fail("Calibration Options: unknown Spread Days Fitted By value '%s'", co$SpreadDaysBy)
USE_C <- by %in% c(1L, 3L); USE_V <- by %in% c(2L, 3L)
H[, Cause := fifelse(Cause %in% c("Human", "Lightning"), Cause, NA_character_)]
if (USE_V) { vm <- veg_map(); if (is.null(vm)) fail("Spread Days Fitted By uses origin vegetation, but Landscape Maps has no vegetation map (set Data Sources > BpS and run P1)")
  H[, Veg := veg_of_lonlat(Longitude, Latitude, vm)]; st[, Veg := veg_of_cells(Cell, vm)] } else { H[, Veg := NA_character_]; st[, Veg := NA_character_] }
say("spread days fitted by: %s", tolower(BY_LABELS[by + 1L]))
if (USE_V) { t <- H[AreaHa >= MINA, .N, by = Veg][order(-N)]; say("recorded fires >= %.1f ha by origin vegetation: %s", MINA, paste(t$Veg, t$N, sep = " ", collapse = ", ")) }

# ---- fit each stratum, with fall-backs ----------------------------------------------------------------------------------------------
MINOBS <- if (!is.null(co$MinimumFiresPerCurve) && !is.na(co$MinimumFiresPerCurve)) as.integer(co$MinimumFiresPerCurve) else 25L; MINSEED <- 500L
say("a curve needs %d recorded fires >= %.1f ha and %d seed fires", MINOBS, MINA, MINSEED)
key <- function(cz, vg, lab = NULL) paste(ifelse(is.na(cz), "Any", cz), if (!is.null(lab)) lab else ifelse(is.na(vg[1]), "Any", vg[1]), sep = " / ")
fits <- list(); tried <- character(0)
fit1 <- function(cz, vg, lab = NULL) { k <- key(cz, vg, lab); if (k %in% names(fits)) return(TRUE); if (k %in% tried) return(FALSE); tried <<- c(tried, k)
  ho <- H[(is.na(cz) | Cause %in% cz) & (anyNA(vg) | Veg %in% vg)]; so <- st[(is.na(cz) | Cause %in% cz) & (anyNA(vg) | Veg %in% vg)]
  nobs <- sum(ho$AreaHa >= MINA)
  if (nobs < MINOBS || nrow(so) < MINSEED) { say("%s: %d recorded fires >= %.1f ha and %s seed fires - too few for its own curve", k, nobs, MINA, format(nrow(so), big.mark = ",")); return(FALSE) }
  r <- fit_spread_days(so$Area, so$SpreadDays, ho$AreaHa, MINA, AW); r$cause <- cz; r$veg <- if (is.null(lab)) vg else lab; r$members <- if (is.null(lab)) NULL else vg; fits[[k]] <<- r; TRUE }
causes <- if (USE_C) c("Human", "Lightning") else NA_character_
vcl <- if (USE_V) as.data.table(datasheet(prj, ds("VegetationClass"), optional = TRUE, lookupsAsFactors = FALSE)) else data.table(Name = NA_character_)
if (!"FallbackGroup" %in% names(vcl)) vcl[, FallbackGroup := NA_character_]
vegs <- vcl$Name; group_of <- function(vg) vcl$FallbackGroup[match(vg, vcl$Name)]
for (cz in causes) for (vg in vegs) {
  if (USE_V && !any(st$Veg %in% vg)) next                    # a class with no seed fires in this zone needs no curve
  if (fit1(cz, vg)) next
  if (!is.na(cz) && !is.na(vg) && fit1(NA_character_, vg)) next
  gp <- group_of(vg); if (!is.na(vg) && !is.na(gp) && nzchar(gp)) fit1(NA_character_, vcl[FallbackGroup %in% gp]$Name, paste(gp, "group")) }
fit1(NA_character_, NA_character_)                          # all fires: the last resort for any fire without a curve of its own
if (!length(fits)) fail("no spread-day curve could be fitted (too few recorded fires >= %.1f ha)", MINA)
# a group curve stands in for each member vegetation (seen in this zone) that has no curve of its own
own_v <- unlist(lapply(fits, function(r) if (is.null(r$members) && is.na(r$cause)) r$veg))
for (k in names(fits)) { r <- fits[[k]]; if (is.null(r$members)) next
  r$use <- intersect(setdiff(r$members, own_v), unique(st$Veg)); fits[[k]] <- r
  say("%s curve used for: %s", r$veg, if (length(r$use)) paste(r$use, collapse = ", ") else "(no vegetation - every member has its own curve)") }

say("target: fires >= %.2f ha, %d-%d; seed run: %s fires over spread days %d-%d", MINA, cp$FireFirstYear, cp$FireLastYear,
    format(nrow(st), big.mark = ","), min(st$SpreadDays), max(st$SpreadDays))
for (k in names(fits)) { r <- fits[[k]]; cv <- r$curve[r$curve$RelativeFrequency > 0, ]
  say("%s: %d recorded fires, mean %.0f ha; fitted KS %.3f, mean kept size / observed %.2f, mean spread days %.2f", k,
      r$summary$ObservedFires, r$summary$ObservedMeanHa, r$summary$KS, r$summary$AreaRatio, r$summary$MeanSpreadDays)
  say("  spread-day distribution: %s", paste(sprintf("%dd %.3f", cv$Days, cv$RelativeFrequency), collapse = ", "))
  for (i in seq_len(nrow(r$classes))) say("  %-14s observed %.3f  fitted %.3f", r$classes$SizeClass[i], r$classes$ObservedShare[i], r$classes$FittedShare[i]) }
cvs <- rbindlist(lapply(fits, function(r) { cv <- as.data.table(r$curve)[RelativeFrequency > 0]
  if (is.null(r$members)) return(cv[, `:=`(Cause = r$cause, Vegetation = r$veg)])
  rbindlist(lapply(r$use, function(v) copy(cv)[, `:=`(Cause = NA_character_, Vegetation = v)])) }), fill = TRUE)
write_ds(cvs, "SpreadDays")
lab <- function(x) ifelse(is.na(x), "Any", x)
write_ds(rbindlist(lapply(fits, function(r) data.table(Cause = lab(r$cause), Vegetation = lab(r$veg), r$classes, BurningHours = BH))), "OutputCalibrationFit")
write_ds(rbindlist(lapply(fits, function(r) { s <- r$summary
  data.table(Cause = lab(r$cause), Vegetation = lab(r$veg), BurningHours = BH, KS = s$KS, AreaRatio = s$AreaRatio, MeanSpreadDays = s$MeanSpreadDays, Selected = TRUE) })), "OutputCalibrationSummary")
