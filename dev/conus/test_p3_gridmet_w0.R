# standalone check of P3's gridMET route: P26's weather zones, one year, against the California zone-daily table (script 07)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE)
say <- function(...) cat(sprintf(...), "\n"); fail <- function(...) stop(sprintf(...))
x <- readLines("F:/lucasFireC2F/src/shared.R"); i <- grep("^weather_zone_polygons <- function", x); eval(parse(text = x[i:(i + 9)]))
FIRE_COUNT_REF <- 8; WEIGHT_FLOOR <- 0.05; Y <- 2020L
src <- list(GridMetFolder = "F:/lucas-master/Data Sources/Climate/gridMET", WeatherZoneBoundaryFile = "F:/lucas-master/Data Sources/Study Areas/Fire Weather Zones/fz03mr26.shp",
            MtbsPerimeterFile = "F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp")
wzr <- rast("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim.data/Scenario-32/lucasFireC2F_LandscapeRasters/weatherzone.tif")
codes <- sort(unique(na.omit(values(wzr, mat = FALSE)))); g <- read_sf("F:/lucas-master/Models/California 30m BurnP3/data/zones/weather_zones.gpkg")
names_ <- g$Name[match(codes, g$ID)]
allz <- st_transform(weather_zone_polygons(src$WeatherZoneBoundaryFile), 4326); mine <- allz[allz$Name %in% names_, ]; cat("zones", length(codes), "found", nrow(mine), "\n")
gm <- rast(file.path(src$GridMetFolder, "vs", sprintf("vs_%d.nc", Y)))[[1]]; bb <- st_bbox(mine)
gm <- crop(gm, ext(bb[["xmin"]] - 0.25, bb[["xmax"]] + 0.25, bb[["ymin"]] - 0.25, bb[["ymax"]] + 0.25), snap = "out")
near <- allz[lengths(st_intersects(allz, st_as_sfc(st_bbox(gm)))) > 0, ]; near$ID <- seq_len(nrow(near))
zid <- cover(rasterize(vect(near), gm, field = "ID"), rasterize(vect(near), gm, field = "ID", touches = TRUE))
lyr <- st_layers(src$MtbsPerimeterFile)$name[1]; pc <- st_crs(read_sf(src$MtbsPerimeterFile, query = sprintf("SELECT * FROM \"%s\" LIMIT 0", lyr)))
per <- st_make_valid(st_transform(read_sf(src$MtbsPerimeterFile, wkt_filter = st_as_text(st_transform(st_as_sfc(st_bbox(gm)), pc))), 4326))
cnt <- rasterize(vect(per), gm, fun = "count", background = 0); wt <- cnt / FIRE_COUNT_REF + WEIGHT_FLOOR
w0 <- resample(rast("F:/lucas-master/Models/California 30m BurnP3/data/weather/fire_weight_gridmet.tif"), gm, "near")
cat(sprintf("weights: new vs CA (06) on these cells: identical %.1f%%, mean |d| %.4f\n", 100 * mean(abs(values(wt) - values(w0)) < 1e-9, na.rm = TRUE), mean(abs(values(wt) - values(w0)), na.rm = TRUE)))
key <- data.table(cell = seq_len(ncell(gm)), zone = near$Name[values(zid, mat = FALSE)], wt = values(w0, mat = FALSE))[zone %in% mine$Name & !is.na(wt)]
zmean <- function(v, y, circ = FALSE) { r <- crop(rast(file.path(src$GridMetFolder, v, sprintf("%s_%d.nc", v, y))), gm, snap = "near")
  m <- as.data.table(values(r)); m[, cell := .I]; m <- melt(m[key, on = "cell", nomatch = 0], id.vars = c("cell", "zone", "wt"), variable.name = "d", value.name = "x")[!is.na(x)]
  if (circ) m[, .(x = (atan2(sum(sin(x * pi / 180) * wt) / sum(wt), sum(cos(x * pi / 180) * wt) / sum(wt)) * 180 / pi) %% 360), by = .(zone, day = as.integer(d))] else m[, .(x = sum(x * wt) / sum(wt)), by = .(zone, day = as.integer(d))] }
d <- dcast(rbindlist(lapply(c("tmmx", "rmin", "vs", "th", "pr"), function(v) zmean(v, Y, v == "th")[, var := v])), zone + day ~ var, value.var = "x")
n <- d[, .(zone, date = as.Date(sprintf("%d-01-01", Y)) + day - 1, Temperature = tmmx - 273.15, RelativeHumidity = pmin(100, pmax(1, rmin)), WindSpeed = vs * 3.6, WindDirection = th, Precipitation = pr)]
o <- fread("F:/lucasFireC2F/Data/weather/zone_daily_raw.csv")[zone %in% codes & year(date) == Y]; o[, zone := g$Name[match(zone, g$ID)]]
m <- merge(n, o, by = c("zone", "date"), suffixes = c(".new", ".ca")); cat("rows matched", nrow(m), "of", nrow(o), "\n")
for (v in c("Temperature", "RelativeHumidity", "WindSpeed", "WindDirection", "Precipitation")) { a <- m[[paste0(v, ".new")]]; b <- m[[paste0(v, ".ca")]]
  dd <- if (v == "WindDirection") abs(((a - b) + 180) %% 360 - 180) else abs(a - b); cat(sprintf("  %-17s max |d| %.6g  mean |d| %.6g  identical %.1f%%\n", v, max(dd), mean(dd), 100 * mean(dd < 1e-6))) }
