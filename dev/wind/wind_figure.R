# Figure for the terrain-wind check: WindNinja speed ratio and direction deviation for the most common stream direction
# (225 deg), and observed vs WindNinja station speed ratios (RAWS, stream days)
suppressPackageStartupMessages({ library(data.table); library(terra); library(sf) })
OUT <- "F:/lucasFireC2F/dev/wind/out"; M <- "F:/lucas-master/Models/California 30m BurnP3"
SR <- rast(file.path(OUT, "p026_speed_ratio.tif")); DV <- rast(file.path(OUT, "p026_direction_dev.tif"))
i <- which(names(SR) == "225"); sr <- SR[[i]]; dv <- DV[[i]]
zone <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zone <- st_transform(zone[zone$PYROME == 26, ], crs(sr))
st <- fread(file.path(M, "data/wind/p026_raws_stations.csv")); sp <- st_transform(st_as_sf(st, coords = c("longitude", "latitude"), crs = 4326), crs(sr))
bs <- fread(file.path(OUT, "station_summary.csv"))
png("F:/lucasFireC2F/dev/wind/Fig_terrain_wind_check.png", 2400, 1500, res = 170)
layout(matrix(1:3, 1), widths = c(1, 1, 1.25)); par(oma = c(0, 0, 3, 0))
pal1 <- colorRampPalette(c("#2c5c8a", "#9ec3dc", "#f4f1e8", "#f4a340", "#9b2b12"))(9)
plot(sr, breaks = c(0, .4, .6, .8, .9, 1.1, 1.25, 1.5, 2, 5), col = pal1, axes = FALSE, box = FALSE, mar = c(1, 1, 3, 5),
     main = "(a) Speed ratio, SW (225°) flow", plg = list(title = "x regional"))
plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1); plot(st_geometry(sp), add = TRUE, pch = 21, bg = "white", cex = .8)
pal2 <- colorRampPalette(c("#3b4cc0", "#aac7fd", "#f2f0ec", "#f7b89c", "#b40426"))(8)
plot(dv, breaks = c(-180, -40, -20, -10, 0, 10, 20, 40, 180), col = pal2, axes = FALSE, box = FALSE, mar = c(1, 1, 3, 5),
     main = "(b) Direction deviation (deg)", plg = list(title = "+ = clockwise"))
plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1)
par(mar = c(5, 5, 4, 2))
plot(bs$wn_ratio, bs$obs_ratio, log = "xy", pch = 21, bg = "#dd4b20", cex = 1.3, xlim = c(.4, 2.5), ylim = c(.3, 2.5),
     xlab = "WindNinja speed ratio at station", ylab = "Observed afternoon speed / zone gridMET wind",
     main = sprintf("(c) 48 RAWS, stream days 2006-24 (r = %.2f)", cor(log(bs$wn_ratio), log(bs$obs_ratio))))
abline(0, 1, lty = 2, col = "grey40")
mtext("Terrain winds (WindNinja, 150 m) vs RAWS, Pyrome 26: large simulated terrain effects, no measurable skill at the stations", outer = TRUE, cex = 1.1, font = 2)
dev.off(); cat("wrote Fig_terrain_wind_check.png\n")
