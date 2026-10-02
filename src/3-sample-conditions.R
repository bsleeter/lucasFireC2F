# =============================================================================
# 3 - Sample Burning Conditions
# =============================================================================
# Gives every sampled fire its duration, weather and fuel moisture (ported from the validated P26 script 201):
#   spread days   drawn from SpreadDays (relative frequencies); hours burning = BurningHours x spread days
#                 Seed run (GrowthOption.SeedRun, for calibration): season fires only, no pool, spread days uniform over
#                 1..CalibrationOption.MaximumSpreadDays, so stage C5 gets a size kernel at every day count
#   weather       a run of consecutive WeatherStream records of the fire's weather zone and season, starting at a random
#                 record (wrapping), one record per spread day; each burning hour takes its day's wind, converted from the
#                 stream's 10 m daily mean to C2F-W's 20-ft input (/ WeatherOption.WindSpeedTo20ft)
#   moisture      C2F-W takes one Scott & Burgan scenario per fire: dead class from day 1's equilibrium moisture content
#                 (Simard 1968, from temperature and humidity) against MoistureDeadClass, live class from the season;
#                 scenario = the wetter of the two
# Random streams are offset from RunControl.RandomSeed (seed - 1 spread days, seed - 2 weather, seed seed run) so that the
# package default seed 20260925 reproduces 201 exactly.
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
progressBar(type = "begin", totalSteps = 3)

rc <- need("RunControl", "RandomSeed"); SEED <- rc$RandomSeed
Fl <- need("OutputFireList"); setorder(Fl, Pool, Iteration, FireID)   # season fires, then the pool: stage 2's order
# duplicate identical rows are harmless (a blank row saved in Studio becomes a copy of the default when the scenario is copied)
bh <- unique(need("BurningHours", "Hours")[!is.na(Hours), .(Season, Hours)])
if (nrow(bh) > 1 || !is.na(bh$Season[1])) fail("Burning Hours by season is not supported yet - use one row with no season (found: %s)",
                                              paste(sprintf("%s %s h", ifelse(is.na(bh$Season), "all seasons", bh$Season), bh$Hours), collapse = "; "))
HPD <- bh$Hours[1]
wx <- need("WeatherStream", c("Season", "WeatherZone", "Temperature", "RelativeHumidity", "WindSpeed", "WindDirection"))
wo <- read_ds("WeatherOption"); W20 <- if (nrow(wo) && !is.na(wo$WindSpeedTo20ft)) wo$WindSpeedTo20ft else 1.15
dc <- need("MoistureDeadClass", c("Class", "MaximumEMC")); setorder(dc, Class)
go <- read_ds("GrowthOption"); SEEDRUN <- nrow(go) && isTRUE(go$SeedRun)
co <- read_ds("CalibrationOption"); MAXD <- if (nrow(co) && !is.na(co$MaximumSpreadDays)) co$MaximumSpreadDays else 25L
se <- as.data.table(datasheet(prj, ds("Season"), lookupsAsFactors = FALSE))
ms <- as.data.table(datasheet(prj, ds("MoistureScenario"), lookupsAsFactors = FALSE))

if (SEEDRUN) {
  Fl <- Fl[Pool == FALSE]; set.seed(SEED)
  Fl[, SpreadDays := sample.int(MAXD, .N, replace = TRUE)]
  say("SEED RUN: %s season fires, spread days uniform 1-%d, no extra pool", format(nrow(Fl), big.mark = ","), MAXD)
} else {
  sd <- need("SpreadDays", c("Days", "RelativeFrequency")); sd <- sd[RelativeFrequency > 0]   # only a model run needs the curve (a new zone's first seed run has none)
  for (k in c("Cause", "Vegetation")) if (!k %in% names(sd)) sd[, (k) := NA_character_]
  set.seed(SEED - 1L)
  if (all(is.na(sd$Cause)) && all(is.na(sd$Vegetation))) {   # one curve for every fire (calibrations before 1.5.0): the draw is unchanged
    Fl[, SpreadDays := sd$Days[sample.int(nrow(sd), .N, replace = TRUE, prob = sd$RelativeFrequency)]]   # by position: sample() on one value would draw from 1..value
    say("spread days: mean %.2f (distribution mean %.2f)", mean(Fl$SpreadDays), sum(sd$Days * sd$RelativeFrequency) / sum(sd$RelativeFrequency))
  } else {   # curves by cause and/or origin vegetation (C5): each fire takes the most specific curve that matches it - cause and
             # vegetation, then vegetation, then cause, then the curve with neither (a blank Cause or Vegetation matches any)
    Fl[, Veg := if (any(!is.na(sd$Vegetation))) veg_of_cells(Cell) else NA_character_]
    keys <- unique(sd[, .(Cause, Vegetation)]); Fl[, SpreadDays := NA_integer_]
    grp <- unique(Fl[, .(Cause, Veg)])[order(Cause, Veg, na.last = TRUE)]   # fixed order: with no vegetation map it is 1.5.0's per-cause order
    for (j in seq_len(nrow(grp))) { cz <- grp$Cause[j]; vg <- grp$Veg[j]
      k <- keys[(is.na(Cause) | Cause %in% cz) & (is.na(Vegetation) | Vegetation %in% vg)]
      if (!nrow(k)) fail("Spread Days has no curve for cause %s, vegetation %s and no curve for all fires", cz, ifelse(is.na(vg), "(none)", vg))
      k <- k[order(-(2L * (!is.na(Vegetation)) + (!is.na(Cause))))][1]   # parentheses matter: R's ! binds looser than + and *
      c1 <- sd[(Cause %in% k$Cause | (is.na(Cause) & is.na(k$Cause))) & (Vegetation %in% k$Vegetation | (is.na(Vegetation) & is.na(k$Vegetation)))]
      i <- which(Fl$Cause %in% cz & (Fl$Veg %in% vg | (is.na(Fl$Veg) & is.na(vg))))
      Fl[i, SpreadDays := c1$Days[sample.int(nrow(c1), length(i), replace = TRUE, prob = c1$RelativeFrequency)]]
      say("spread days, %s / %s (%s fires): mean %.2f, curve %s / %s", cz, ifelse(is.na(vg), "no vegetation", vg), format(length(i), big.mark = ","),
          mean(Fl$SpreadDays[i]), ifelse(is.na(k$Cause), "any cause", k$Cause), ifelse(is.na(k$Vegetation), "any vegetation", k$Vegetation)) }
    if (anyNA(Fl$SpreadDays)) fail("%d fires got no spread days", sum(is.na(Fl$SpreadDays)))
    Fl[, Veg := NULL]
  }
}
Fl[, HoursBurning := HPD * SpreadDays]
progressBar()

# ---- weather and moisture ---------------------------------------------------------------------------------------------
emc <- function(t_c, rh) { t <- t_c * 9 / 5 + 32
  ifelse(rh < 10, 0.03229 + 0.281073 * rh - 0.000578 * rh * t, ifelse(rh < 50, 2.22749 + 0.160107 * rh - 0.01478 * t,
         21.0606 + 0.005565 * rh^2 - 0.00035 * rh * t - 0.483199 * rh)) }
setorder(wx, WeatherZone, Season, Order)
wx[, dcls := findInterval(emc(Temperature, RelativeHumidity), c(-Inf, dc$MaximumEMC), left.open = TRUE)]
blocks <- split(seq_len(nrow(wx)), paste(wx$WeatherZone, wx$Season))
live <- setNames(se$LiveMoistureClass, se$Name)
set.seed(SEED - 2L); Fl[, seed := sample.int(.Machine$integer.max, .N)]
hasEv <- "EventWindSpeed" %in% names(wx) && any(!is.na(wx$EventWindSpeed))
hourly <- vector("list", nrow(Fl)); scen <- integer(nrow(Fl))
for (i in seq_len(nrow(Fl))) {
  b <- blocks[[paste(Fl$WeatherZone[i], Fl$Season[i])]]
  if (is.null(b)) fail("no weather stream records for weather zone '%s' in season %s", Fl$WeatherZone[i], Fl$Season[i])
  D <- Fl$SpreadDays[i]; set.seed(Fl$seed[i]); s <- sample.int(length(b), 1)
  d <- wx[b[((s - 1 + 0:(D - 1)) %% length(b)) + 1]]
  scen[i] <- max(d$dcls[1], live[[Fl$Season[i]]])
  k <- rep(seq_len(D), each = HPD)
  hourly[[i]] <- data.table(Iteration = Fl$Iteration[i], FireID = Fl$FireID[i], Hour = seq_along(k),
                            WindSpeed = round(d$WindSpeed[k] / W20, 2), WindDirection = d$WindDirection[k],
                            EventWindSpeed = if (hasEv) d$EventWindSpeed[k] else NA_real_)
  if (i %% 5000 == 0) say("weather drawn for %s of %s fires", format(i, big.mark = ","), format(nrow(Fl), big.mark = ","))
}
Fl[, MoistureScenario := ms$Name[match(scen, ms$ID)]]
if (anyNA(Fl$MoistureScenario)) fail("moisture scenario ids with no Fuel Moisture Scenario: %s", paste(unique(scen[is.na(Fl$MoistureScenario)]), collapse = ", "))
progressBar()

write_ds(Fl[, .(Iteration, FireID, Cell, Latitude, Longitude, Season, Cause, FireZone, WeatherZone, SpreadDays, HoursBurning, MoistureScenario, Pool,
                Seed = seed)], "OutputFireList")
write_ds(rbindlist(hourly), "OutputFireWeather")
tb <- Fl[, .N, by = .(Season, MoistureScenario)][order(Season, MoistureScenario)]
say("moisture scenario by season: %s", paste(sprintf("%s %s %d", tb$Season, tb$MoistureScenario, tb$N), collapse = "; "))
progressBar(); progressBar(type = "end")
