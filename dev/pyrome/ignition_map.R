# Ignition locations by cause for a zone result: (a) the ignition probability surface stage 2 draws from (percentile of zone cells)
# and (b) the origins of the run's kept fires >= the threshold (density per 3 km cell, per 1,000 seasons), each with the recorded
# FPA-FOD origins of fires >= the threshold (fire-record years) as points.
# Usage: Rscript ignition_map.R "<library>" <result id> <prep result id> <out png>
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); R <- rsyncrosim::scenario(L, as.integer(a[2])); P <- rsyncrosim::scenario(L, as.integer(a[3])); PNG <- a[4]
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
zone <- st_union(st_make_valid(read_sf(g(P, "OutputZoneBoundary")$FileName[1])))
ig <- g(R, "IgnitionGrid"); if (!nrow(ig)) ig <- g(P, "IgnitionGrid")
tmpl <- rast(ig$FileName[1]); zone <- st_transform(zone, crs(tmpl)); zv <- vect(zone)
MINA <- g(R, "ResampleOption")$MinimumFireSize[1]; NS <- g(R, "RunControl")$MaximumIteration[1]
cp <- g(R, "CalibrationPeriod"); Y1 <- if (nrow(cp)) cp$FireFirstYear else 1992; Y2 <- if (nrow(cp)) cp$FireLastYear else 2020
st <- g(R, "OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINA]
st[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
fl <- g(R, "OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell, FlCause = Cause)]; st <- fl[st, on = c("bI", "bF")]
H <- g(R, "HistoricalFire"); if (!nrow(H)) stop("no fire record visible to this result")
H <- H[AreaHa >= MINA & as.integer(substr(Date, 1, 4)) %in% Y1:Y2]
hp <- st_transform(st_as_sf(H, coords = c("Longitude", "Latitude"), crs = 4326), crs(tmpl)); hp <- hp[lengths(st_intersects(hp, zone)) > 0, ]
agg <- aggregate(rast(tmpl), 20)   # 3 km cells
png(PNG, 2200, 2300, res = 150); par(mfrow = c(2, 2), mar = c(1, 1, 4, 5), oma = c(2, 0, 3, 0))
for (cz in c("Human", "Lightning")) {
  s <- mask(crop(rast(ig[Cause == cz]$FileName[1]), zv), zv); v <- values(s, mat = FALSE); ok <- !is.na(v) & v > 0
  pc <- s; pv <- rep(NA_real_, length(v)); pv[ok] <- 100 * rank(v[ok]) / sum(ok); values(pc) <- pv
  obs <- hp[hp$Cause %in% cz, ]
  plot(pc, breaks = c(0, 50, 75, 90, 95, 99, 100), col = hcl.colors(6, "YlOrRd", rev = TRUE), axes = FALSE, mar = c(1, 1, 4, 5),
       main = sprintf("%s: ignition surface (percentile)", cz), plg = list(title = "pct"))
  lines(zv, lwd = .6); points(st_coordinates(obs), pch = 21, cex = .7, bg = "#1f5fbf", col = "white", lwd = .4)
  xy <- xyFromCell(tmpl, st[FlCause == cz, Cell]); d <- rasterize(xy, agg, fun = "count"); d <- mask(crop(d * 1000 / NS, zv), zv)
  plot(d, breaks = c(0, 0.5, 1, 2, 4, 8, 16, 1e6), col = hcl.colors(7, "Inferno", rev = TRUE), axes = FALSE, mar = c(1, 1, 4, 5),
       main = sprintf("%s: simulated fire origins (%s fires)", cz, format(sum(st$FlCause == cz), big.mark = ",")), plg = list(title = "per 3 km\nper 1,000 yr"))
  lines(zv, lwd = .6); points(st_coordinates(obs), pch = 21, cex = .7, bg = "#1f5fbf", col = "white", lwd = .4)
  mtext(sprintf("%d recorded fires", nrow(obs)), side = 1, line = 0, cex = .9) }
mtext(sprintf("Ignition locations by cause, fires >= %.1f ha (result %s); blue points: FPA-FOD origins %d-%d", MINA, a[2], Y1, Y2), outer = TRUE, cex = 1.2, font = 2)
dev.off(); cat("wrote", PNG, "\n")
cat(sprintf("recorded origins in zone: Human %d, Lightning %d; simulated kept fires: Human %d, Lightning %d\n", sum(hp$Cause == "Human"), sum(hp$Cause == "Lightning"),
            sum(st$FlCause == "Human"), sum(st$FlCause == "Lightning")))
