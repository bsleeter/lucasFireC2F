# =============================================================================
# R - Report Figures (optional; last in a Run pipeline)
# =============================================================================
# Report-quality figures of a model run and the tables behind Studio's Charts tab:
#   Fig1 inputs (fuel group, canopy bulk density, ignition probability); Fig2 burn probability, beside WRC when Data Sources
#   has a benchmark, with the large MTBS fires after the fuel vintage outlined; Fig3 fire intensity, Fig4 crown fire (stage 6);
#   Fig5 hazard and risk (stage 7); Fig6 fire sizes against the record and the spread-day distribution (C6); Fig7 spatial
#   skill against WRC (C7) and fire effects by fuel group (stage 6). A figure whose stage did not run is left out; a chart
#   panel without its table says so.
#   Charts: size classes (C6), spread days, validation periods (C7), each with its x position in Timestep.
# The drawing is in report.R (testable on a result's files: dev/report/test_report.R).
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf); library(data.table) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); source(file.path(env$PackageDirectory, "src", "report.R"))
sf_use_s2(FALSE); terraOptions(progress = 0); progressBar(type = "begin", totalSteps = 3)
TD <- env$TransferDirectory
first <- function(sheet, col) { x <- read_ds(sheet); if (nrow(x) && col %in% names(x) && !is.na(x[[col]][1]) && nzchar(x[[col]][1])) x[[col]][1] else NA_character_ }
coded <- function(sheet, metric_sheet) { x <- read_ds(sheet); if (!nrow(x)) return(list())
  x[, code := metric_code(metric_sheet, Metric)]; x <- x[!is.na(code) & !is.na(FileName)]; setNames(as.list(x$FileName), x$code) }

lr <- need("LandscapeRasters", c("ElevationFileName", "FireZoneFileName")); el <- rast(lr$ElevationFileName[1])
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); st <- read_ds("OutputFireStatistic")
# the zone outline: P1's boundary when the result carries it (a Run with the Prep result as a dependency), otherwise the cells of
# the run's fire zone(s) on the fire-zone map (a scenario that depends on a Run result alone carries no boundary)
zb <- read_ds("OutputZoneBoundary"); zname <- NA_character_
if (nrow(zb) && !is.na(zb$FileName[1]) && file.exists(zb$FileName[1])) {
  zs <- read_sf(zb$FileName[1]); zone <- st_sf(geometry = st_union(st_make_valid(zs))); zname <- zs$Name[1] %||% NA_character_
} else {
  zones <- if (nrow(st) && any(!is.na(st$FireZone))) unique(na.omit(st$FireZone)) else fzt$Name
  zid <- fzt$ID[match(zones, fzt$Name)]; fz <- rast(lr$FireZoneFileName[1])
  zr <- classify(fz, cbind(zid, 1L), others = NA); if (all(is.na(values(zr, mat = FALSE)))) fail("none of the fire zone(s) %s is on the fire-zone map", paste(zones, collapse = ", "))
  zone <- st_sf(geometry = st_union(st_make_valid(st_as_sf(as.polygons(zr, dissolve = TRUE))))); zname <- paste(zones, collapse = " + ")
  say("no Zone Boundary in this scenario: zone outline taken from the fire-zone map (%s)", zname) }
zone <- st_transform(zone, crs(el))
bp <- need("OutputBurnProbability", "FileName")$FileName[1]
fuel <- first("OutputEngineLandscape", "LandscapeFuelFileName"); if (is.na(fuel)) fuel <- need("LandscapeRasters", "FuelFileName")$FuelFileName[1]
zo <- read_ds("ZonePrepOption"); VINT <- if (nrow(zo) && !is.na(zo$LandfireVintage[1])) zo$LandfireVintage[1] else 2020L
vo <- read_ds("ValidationOption"); LAST <- if (nrow(vo) && !is.na(vo$LastYear[1])) vo$LastYear[1] else 2024L
MINHA <- if (nrow(vo) && !is.na(vo$MinimumFireHa[1])) vo$MinimumFireHa[1] else 405
NS <- need("RunControl", "MaximumIteration")$MaximumIteration[1]

# fire sizes as C6 counts them: kept fires at or above the resampling minimum; the record over the calibration years
MINA <- read_ds("ResampleOption")$MinimumFireSize[1] %||% 0; cp <- read_ds("CalibrationPeriod")
sim <- if (nrow(st)) st[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA, Area] else numeric()
H <- read_ds("HistoricalFire"); obs <- numeric()
if (nrow(H) && nrow(cp)) {
  if (any(!is.na(H$FireZone)) && nrow(fzt)) H <- H[FireZone %in% fzt$Name]
  obs <- H[as.integer(substr(Date, 1, 4)) %in% cp$FireFirstYear[1]:cp$FireLastYear[1] & AreaHa >= MINA, AreaHa] }

# large MTBS fires after the fuel vintage, inside the zone (outlines on the burn-probability maps)
mt <- NULL; mf <- read_data_sources("MtbsPerimeterFile", optional = TRUE)$MtbsPerimeterFile
if (length(mf) && !is.na(mf) && file.exists(mf)) {
  wc <- st_crs(read_sf(mf, query = sprintf("SELECT * FROM \"%s\" LIMIT 0", st_layers(mf)$name[1])))
  m <- read_sf(mf, wkt_filter = st_as_text(st_transform(st_as_sfc(st_bbox(zone)), wc)))
  canon <- c(incid_type = "Incid_Type", burnbndac = "BurnBndAc", ig_date = "Ig_Date"); hit <- match(tolower(names(m)), names(canon)); names(m)[!is.na(hit)] <- unname(canon[hit[!is.na(hit)]])
  m <- m[m$Incid_Type == "Wildfire" & !is.na(m$Ig_Date), ]; yr <- as.integer(format(as.Date(m$Ig_Date), "%Y"))
  m <- m[yr > VINT & yr <= LAST & m$BurnBndAc * 0.404686 >= MINHA, ]
  if (nrow(m)) mt <- suppressWarnings(st_intersection(st_geometry(st_make_valid(st_transform(m, crs(el)))), st_geometry(zone))) }
wrc <- read_data_sources("WrcBurnProbabilityFile", optional = TRUE)$WrcBurnProbabilityFile
if (!length(wrc) || is.na(wrc) || !file.exists(wrc)) wrc <- NA_character_
progressBar()

sc <- read_ds("OutputScore"); va <- read_ds("OutputValidation"); es <- read_ds("OutputEffectsSummary"); sd <- read_ds("SpreadDays")
I <- list(zone = zone, el = el, fuel = fuel, fuel_type = as.data.table(datasheet(prj, ds("FuelType"), lookupsAsFactors = FALSE)),
  cbd = { x <- first("CanopyRasters", "CanopyBulkDensityFileName"); if (is.na(x)) first("OutputEngineLandscape", "CanopyBulkDensityFileName") else x }, ignition = as.list(na.omit(read_ds("IgnitionGrid")$FileName)), bp = bp,
  effects = coded("OutputFireEffects", "EffectsMetric"), risk = coded("OutputRisk", "RiskMetric"), wrc = wrc, mtbs = mt,
  score = sc, validation = va, effects_summary = es, spread_days = sd, obs = obs, sim = sim,
  label = if (!is.na(zname)) zname else rsyncrosim::name(scn), seasons = NS, vintage = VINT, mtbs_years = sprintf("%d-%d", VINT + 1L, LAST))
say("report figures for %s: %s fire seasons; %d recorded and %s simulated fires >= %.1f ha; MTBS outlines %d; benchmark %s", I$label, format(NS, big.mark = ","),
    length(obs), format(length(sim), big.mark = ","), MINA, if (is.null(mt)) 0L else length(mt), if (is.na(wrc)) "none" else basename(wrc))
F <- report_figures(I, TD)
write_ds(F, "OutputReportFigure"); say("figures: %s", paste(basename(F$FileName), collapse = ", "))
progressBar()

C <- report_charts(sc, sd, va)
if (!is.null(C$size)) write_ds(C$size, "OutputChartSizeClass")
if (!is.null(C$spread)) write_ds(C$spread, "OutputChartSpreadDays")
if (!is.null(C$validation)) write_ds(C$validation, "OutputChartValidation")
say("chart tables: size classes %d rows, spread days %d, validation periods %d", NROW(C$size), NROW(C$spread), NROW(C$validation))
progressBar(); progressBar(type = "end")
