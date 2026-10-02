# Checks a merge result against its zone results: burn count on the union = the zones' counts added, less the cells burned by
# both zones' fires in the same season (a cell counts once per season); how much of each zone's burning comes from the
# other zone's fires; and a figure (merged BP, and the share of each cell's burns that came from across the boundary).
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(data.table); library(sf); library(arrow) }); sf_use_s2(FALSE)
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; M <- "F:/lucas-master/Models/California 30m BurnP3"
LM <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F P26+P29.ssim"))); RM <- rsyncrosim::scenario(LM, 2)
gm <- function(n, R = RM) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
bcm <- rast(gm("OutputBurnCount")$FileName[1]); U <- bcm; NS <- 2500
zones <- list(P26 = list(lib = "lucasFireC2F P26.ssim", sid = 12, py = 26), P29 = list(lib = "lucasFireC2F P29.ssim", sid = 12, py = 29))
bz <- list(); zp <- list()
for (z in names(zones)) { R <- rsyncrosim::scenario(suppressWarnings(ssimLibrary(file.path(D, zones[[z]]$lib))), zones[[z]]$sid)
  bz[[z]] <- extend(rast(gm("OutputBurnCount", R)$FileName[1]), U); bz[[z]][is.na(bz[[z]])] <- 0
  p <- read_sf(file.path(M, "data/zones/pyromes.gpkg")); zp[[z]] <- st_transform(p[p$PYROME == zones[[z]]$py, ], crs(U)) }
m <- values(bcm, mat = FALSE); a <- values(bz$P26, mat = FALSE); b <- values(bz$P29, mat = FALSE); m[is.na(m)] <- 0
cat(sprintf("union burn count %s; P26 %s + P29 %s = %s; same-season double burns %s (%.3f%%); cells where merged > sum: %d\n",
  format(sum(m), big.mark = ","), format(sum(a), big.mark = ","), format(sum(b), big.mark = ","), format(sum(a + b), big.mark = ","),
  format(sum(a + b - m), big.mark = ","), 100 * sum(a + b - m) / sum(a + b), sum(m > a + b)))
for (z in names(zones)) { inz <- !is.na(values(rasterize(vect(zp[[z]]), U), mat = FALSE)); own <- if (z == "P26") a else b; oth <- if (z == "P26") b else a
  cat(sprintf("inside %s: mean BP own run %.3f%%/yr, merged %.3f%%/yr (x%.3f); share of its burning from the other zone's fires %.2f%%; cells with any: %.1f%%\n",
    z, 100 * mean(own[inz]) / NS, 100 * mean(m[inz]) / NS, mean(m[inz]) / mean(own[inz]), 100 * sum(oth[inz]) / sum(own[inz] + oth[inz]), 100 * mean(oth[inz] > 0))) }
fx <- gm("OutputFireEffects"); em <- as.data.table(datasheet(rsyncrosim::project(LM, 1), "lucasFireC2F_EffectsMetric", lookupsAsFactors = FALSE)); fx[, Code := em$Code[match(Metric, em$Name)]]
cflm <- values(rast(fx[Code == "CFL"]$FileName[1]), mat = FALSE)
for (z in names(zones)) { R <- rsyncrosim::scenario(suppressWarnings(ssimLibrary(file.path(D, zones[[z]]$lib))), zones[[z]]$sid); f2 <- gm("OutputFireEffects", R)
  em2 <- as.data.table(datasheet(rsyncrosim::project(suppressWarnings(ssimLibrary(file.path(D, zones[[z]]$lib))), 1), "lucasFireC2F_EffectsMetric", lookupsAsFactors = FALSE)); f2[, Code := em2$Code[match(Metric, em2$Name)]]
  cz <- values(extend(rast(f2[Code == "CFL"]$FileName[1]), U), mat = FALSE); inz <- !is.na(values(rasterize(vect(zp[[z]]), U), mat = FALSE))
  k <- inz & !is.na(cz) & !is.na(cflm); cat(sprintf("conditional flame length inside %s: own run %.2f ft, merged %.2f ft (cell correlation %.3f)\n", z, mean(cz[k]), mean(cflm[k]), cor(cz[k], cflm[k]))) }
# figure
share <- ifelse(m > 0, pmin(a, b) / pmax(a + b, 1), NA); crossr <- setValues(rast(U), NA_real_)
ina <- !is.na(values(rasterize(vect(zp$P26), U), mat = FALSE)); inb <- !is.na(values(rasterize(vect(zp$P29), U), mat = FALSE))
xv <- rep(NA_real_, ncell(U)); xv[ina] <- ifelse(a[ina] + b[ina] > 0, b[ina] / (a[ina] + b[ina]), NA); xv[inb] <- ifelse(a[inb] + b[inb] > 0, a[inb] / (a[inb] + b[inb]), NA); values(crossr) <- xv
png("F:/lucasFireC2F/dev/pyrome/Fig_merge_P26_P29.png", 2400, 2000, res = 190); par(mfrow = c(1, 2), mar = c(1, 1, 3, 4))
bpm <- bcm / NS * 100; brk <- c(0, .1, .2, .35, .5, .7, 1, 1.5, 2, 3, 100); pal <- colorRampPalette(c("#f7f1e3", "#f8dc8a", "#f4a340", "#e0662b", "#b3261e", "#6b0f14"))(10)
plot(bpm, breaks = brk, col = pal, axes = FALSE, box = FALSE, main = "(a) Merged burn probability, P26 + P29 (%/yr)", plg = list(cex = .7))
plot(st_geometry(zp$P26), add = TRUE, border = "#20211f", lwd = 1.2); plot(st_geometry(zp$P29), add = TRUE, border = "#1d6fa5", lwd = 1.2)
plot(crossr, breaks = c(0, .01, .05, .1, .25, .5, 1), col = colorRampPalette(c("#f2f0f7", "#9e9ac8", "#54278f"))(6), axes = FALSE, box = FALSE,
     main = "(b) Share of a cell's burns from the OTHER zone's fires", plg = list(cex = .7))
plot(st_geometry(zp$P26), add = TRUE, border = "#20211f", lwd = 1.2); plot(st_geometry(zp$P29), add = TRUE, border = "#1d6fa5", lwd = 1.2)
dev.off(); cat("wrote Fig_merge_P26_P29.png\n")
