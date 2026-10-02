# Tests src/report.R on the Demo library's P18 Run result (scenario 6) from its files alone - no library access, so it can run
# while another scenario is running. The C6/C7/effects tables are typed in from result 6's run log.
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(progress = 0)
source("F:/lucasFireC2F/src/report.R")
D <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F-Demo.ssim.data"; S6 <- file.path(D, "Scenario-6"); DS <- "F:/lucas-master/Data Sources"
f <- function(...) file.path(S6, ...)
zone <- st_union(st_make_valid(read_sf(file.path(D, "Scenario-2/lucasFireC2F_OutputZoneBoundary/zone_boundary.gpkg")))) |> st_sf()
el <- rast(f("lucasFireC2F_LandscapeRasters/elevation.tif"))
mt <- read_sf(file.path(DS, "MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp"))
canon <- c(incid_type = "Incid_Type", burnbndac = "BurnBndAc", ig_date = "Ig_Date"); hit <- match(tolower(names(mt)), names(canon)); names(mt)[!is.na(hit)] <- unname(canon[hit[!is.na(hit)]])
mt <- mt[mt$Incid_Type == "Wildfire" & as.integer(format(as.Date(mt$Ig_Date), "%Y")) %in% 2021:2024 & mt$BurnBndAc * 0.404686 >= 405, ]
mt <- suppressWarnings(st_intersection(st_geometry(st_make_valid(st_transform(mt, crs(el)))), st_geometry(zone)))
score <- data.table(Test = rep(c("Fires per year", "Area per year (ha)"), each = 7),
  Group = rep(c("30-100", "100-405", "405-1,000", "1,000-3,000", "3,000-10,000", "10,000-30,000", "30,000+"), 2),
  Observed = c(1.28, .97, .45, .41, .17, .10, 0, 67, 227, 271, 740, 917, 1506, 0), Simulated = c(1.39, .94, .54, .34, .17, .07, .01, 81, 209, 348, 588, 944, 1009, 341))
validation <- data.table(Period = c("1984-2010 (long before the vintage)", "2021-2024 (after the fuel vintage)", "1984-2024 (all)"),
  AUC = c(.756, .422, .484), AUCBenchmark = c(.730, .489, .531), HottestDecile = c(.32, .04, .10), HottestDecileBenchmark = c(.25, .16, .14))
effects_summary <- data.table(FuelGroup = c("Timber-Understory", "Grass-Shrub", "Timber Litter", "Grass", "Shrub", "All"),
  BurnShare = c(.367, .237, .183, .127, .086, 1), CrownShare = c(.08, .01, 0, 0, .12, .04), FLEP4 = c(.10, .01, 0, .03, .36, .07), FLEP8 = c(.04, 0, 0, 0, .10, .02))
spread <- data.table(Days = c(1, 2, 3, 5, 6, 7, 8, 10, 11, 12, 14), RelativeFrequency = c(.5633, .0668, .2396, .0448, .0005, .0471, .0001, .001, .0002, .0359, .0007))
fpa <- read_sf(file.path(DS, "FPA-FOD/Data/FPA_FOD_20221014.gpkg"), query = "SELECT FIRE_SIZE, FIRE_YEAR, Shape FROM Fires WHERE FIRE_SIZE >= 73 AND FIRE_YEAR BETWEEN 1992 AND 2020")
fpa <- fpa[lengths(st_intersects(st_transform(fpa, st_crs(zone)), zone)) > 0, ]
per <- read_sf(f("lucasFireC2F_OutputFirePerimeter/fire_perimeters.gpkg")); sim <- per$AreaHa[per$AreaHa >= 29.5]
eff <- list.files(f("lucasFireC2F_OutputFireEffects"), full.names = TRUE); names(eff) <- sub("effects_(.*)\\.tif", "\\1", basename(eff))
rsk <- list.files(f("lucasFireC2F_OutputRisk"), full.names = TRUE); names(rsk) <- sub("risk_(.*)\\.tif", "\\1", basename(rsk))
I <- list(zone = zone, el = el, fuel = rast(f("lucasFireC2F_OutputEngineLandscape/landscape_fuel(2).tif")),
  fuel_type = fread("F:/lucasFireC2F/defaults/FuelType.csv"), cbd = f("lucasFireC2F_CanopyRasters/canopy_bulk_density.tif"),
  ignition = list.files(f("lucasFireC2F_IgnitionGrid"), full.names = TRUE), bp = list.files(f("lucasFireC2F_OutputBurnProbability"), full.names = TRUE)[1],
  effects = as.list(eff), risk = as.list(rsk), wrc = file.path(DS, "WRC/california_270m/wrc_bp_270m.tif"), mtbs = mt,
  score = score, validation = validation, effects_summary = effects_summary, spread_days = spread,
  obs = fpa$FIRE_SIZE * 0.404686, sim = sim, label = "P18 Southern Cascades (pyrome 18)", seasons = 2500, vintage = 2020, mtbs_years = "2021-2024")
cat("fire record fires >= 29.5 ha:", nrow(fpa), "| simulated:", length(sim), "\n")
t0 <- Sys.time(); R <- report_figures(I, "F:/lucasFireC2F/dev/report/out"); print(R); cat(format(Sys.time() - t0), "\n")
str(report_charts(score, spread, validation))
