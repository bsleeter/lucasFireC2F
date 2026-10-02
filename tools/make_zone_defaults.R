# =============================================================================
# make_zone_defaults.R - statewide Fire Zone and Weather Zone default records (defaults/FireZone.csv, WeatherZone.csv)
# =============================================================================
# Every pyrome of the conterminous US (RDS-2020-0020, 128: the FSim / WRC simulation units; Map Code = 1000 + PYROME, name
# "P<n> <NAME>" - the 27 California ones exactly as before) and every NWS fire-weather zone of the California zone set (Map Code =
# the weather-zone grid's ID; Latitude = centroid of its largest polygon, for the FWI day length), so every lucasFireC2F project
# starts with the same lists: zone libraries, and merges of them, always agree on names.
# Run from the repo root, then tools/inline_defaults.R:  Rscript tools/make_zone_defaults.R
# =============================================================================
suppressPackageStartupMessages(library(sf)); sf_use_s2(FALSE)
Z <- "F:/lucas-master/Models/California 30m BurnP3/data/zones"
p <- st_drop_geometry(read_sf("F:/lucas-master/Data Sources/Pyromes/Data/Pyromes_CONUS_20200206.shp")); p <- p[order(p$PYROME), ]
fz <- data.frame(Name = paste0("P", p$PYROME, " ", gsub(",", "", p$NAME)), ID = 1000L + as.integer(p$PYROME),
                 Description = sprintf("Pyrome %d (FSim / WRC simulation unit)", as.integer(p$PYROME)))
w <- read_sf(file.path(Z, "weather_zones.gpkg")); w <- w[order(w$ID), ]
ll <- st_coordinates(st_transform(suppressWarnings(st_centroid(w, of_largest_polygon = TRUE)), 4326))   # as the zone loaders computed it
wz <- data.frame(Name = w$Name, ID = as.integer(w$ID), Latitude = round(ll[, 2], 5), Description = paste("NWS fire weather zone", w$ZONE, w$STATE))
# then every other CONUS fire weather zone (fz03mr26; multi-record zones unioned as P1/P3 do), Map Codes 209 on in name order
x <- readLines("src/shared.R"); i <- grep("^weather_zone_polygons <- function", x); eval(parse(text = x[i:(i + 5)]))   # as in shared.R
z <- weather_zone_polygons("F:/lucas-master/Data Sources/Study Areas/Fire Weather Zones/fz03mr26.shp")
z <- z[!z$STATE %in% c("AK", "HI", "PR", "GU", "AS", "MP", "VI", "FM", "MH", "PW") & !z$Name %in% wz$Name, ]; z <- z[order(z$Name), ]
lz <- st_coordinates(st_transform(suppressWarnings(st_centroid(st_transform(z, 5070), of_largest_polygon = TRUE)), 4326))
wz <- rbind(wz, data.frame(Name = z$Name, ID = max(wz$ID) + seq_len(nrow(z)), Latitude = round(lz[, 2], 5), Description = paste("NWS fire weather zone", z$ZONE, z$STATE)))
stopifnot(!anyDuplicated(fz$Name), !anyDuplicated(fz$ID), !anyDuplicated(wz$Name), !anyDuplicated(wz$ID))
write.csv(fz, "defaults/FireZone.csv", row.names = FALSE, na = ""); write.csv(wz, "defaults/WeatherZone.csv", row.names = FALSE, na = "")
cat(sprintf("wrote defaults/FireZone.csv (%d pyromes) and defaults/WeatherZone.csv (%d weather zones)\n", nrow(fz), nrow(wz)))
