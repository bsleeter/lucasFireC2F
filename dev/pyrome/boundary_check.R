# Two neighbouring zone runs across their shared boundary (read-only): each zone's own BP, the merged BP (independent fire
# populations: 1 - (1 - a)(1 - b), what stage M gives up to same-season double burns), and the record (MTBS wildfires >= 405 ha,
# 1984-2024, cell x years / 41) in 1 km strips by signed distance from the shared boundary (negative = zone A side).
# Usage: Rscript boundary_check.R <out dir>   (zones set below)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE)
OUT <- commandArgs(TRUE)[1]; dir.create(OUT, FALSE, TRUE); D <- "F:/lucas-master/Models/lucasFireC2F Dev"
A <- list(name = "P26 North Sierra", lib = file.path(D, "lucasFireC2F P26.ssim"), rid = 17L,
          zone = "F:/lucas-master/Models/California 30m BurnP3/spatial/pyromes_150m_LF2020/pyrome_026/zone.gpkg")
B <- list(name = "P27 South Sierra", lib = file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim"), rid = 19L, prep = 2L)
rd <- function(Z, n) { L <- suppressWarnings(ssimLibrary(Z$lib)); as.data.table(datasheet(rsyncrosim::scenario(L, Z$rid), paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE)) }
zb <- function(Z) if (!is.null(Z$prep)) { L <- suppressWarnings(ssimLibrary(Z$lib)); datasheet(rsyncrosim::scenario(L, Z$prep), "lucasFireC2F_OutputZoneBoundary")$FileName[1] } else Z$zone
ba <- rast(rd(A, "OutputBurnProbability")$FileName[1]) * 100; bb <- rast(rd(B, "OutputBurnProbability")$FileName[1]) * 100
fa <- rast(rd(A, "OutputEngineLandscape")$FuelFileName[1]); fb <- rast(rd(B, "OutputEngineLandscape")$FuelFileName[1])
za <- st_transform(st_union(st_make_valid(read_sf(zb(A)))), crs(ba)); zbb <- st_transform(st_union(st_make_valid(read_sf(zb(B)))), crs(ba))
# common window: the union of the two grids, on their shared 150 m lattice
cat(sprintf("origin offsets (m): x %.3f, y %.3f\n", (xmin(ba) - xmin(bb)) %% 150, (ymax(ba) - ymax(bb)) %% 150))
U <- rast(union(ext(ba), ext(bb)), resolution = 150, crs = crs(ba))
put <- function(r) extend(r, U)
a <- put(ba); b <- put(bb); a[is.na(a)] <- 0; b[is.na(b)] <- 0
nbA <- put(fa); nbB <- put(fb)
m <- 1 - (1 - a / 100) * (1 - b / 100); m <- m * 100
# the two outlines come from different files: the shared boundary = B's outline within 300 m of A
line <- st_intersection(st_boundary(zbb), st_buffer(za, 300))
if (any(st_geometry_type(line) == "GEOMETRYCOLLECTION")) line <- st_collection_extract(line, "LINESTRING")
line <- st_cast(st_line_merge(st_union(line)), "LINESTRING")
cat(sprintf("shared boundary: %.0f km\n", sum(as.numeric(st_length(line))) / 1000))
W <- crop(U, ext(vect(st_buffer(st_union(line), 40000))))
ln <- rasterize(vect(line), W, touches = TRUE); dist <- distance(ln) / 1000
inA <- !is.na(values(rasterize(vect(za), W), mat = FALSE)); inB <- !is.na(values(rasterize(vect(zbb), W), mat = FALSE))
dv <- values(dist, mat = FALSE); sd <- ifelse(inA, -dv, ifelse(inB, dv, NA))
cw <- function(r) values(crop(r, W), mat = FALSE)
nb_codes <- c(91, 92, 93, 98, 99)
burnable <- !(cw(nbA) %in% nb_codes) & !(cw(nbB) %in% nb_codes) & !(is.na(cw(nbA)) & is.na(cw(nbB)))
# record: MTBS wildfire burn frequency per year
mt <- read_sf("F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp"); names(mt) <- tolower(names(mt))
mt <- mt[mt$incid_type == "Wildfire" & mt$burnbndac * 0.404686 >= 405, ]; mt$yr <- as.integer(substr(as.character(mt$ig_date), 1, 4))
mt <- st_transform(mt[mt$yr >= 1984 & mt$yr <= 2024, ], crs(ba)); mt <- mt[lengths(st_intersects(mt, st_as_sfc(st_bbox(W)))) > 0, ]
obs <- rep(0, ncell(W)); for (y in unique(mt$yr)) { r <- rasterize(vect(mt[mt$yr == y, ]), W, field = 1); v <- values(r, mat = FALSE); obs <- obs + !is.na(v) }
obs <- 100 * obs / 41
DT <- data.table(sd, own = ifelse(inA, cw(a), cw(b)), other = ifelse(inA, cw(b), cw(a)), runA = cw(a), runB = cw(b), merged = cw(m), obs, burnable)[!is.na(sd) & burnable & abs(sd) <= 30]
DT[, strip := floor(sd)]
P <- DT[, .(cells = .N, A_run = mean(runA), B_run = mean(runB), own_zone_run = mean(own), merged = mean(merged), record_1984_2024 = mean(obs)), by = strip][order(strip)]
fwrite(P, file.path(OUT, "boundary_profile.csv"))
side <- DT[, .(cells = .N, own_zone_run = mean(own), from_neighbour = mean(other), merged = mean(merged), record = mean(obs)),
           by = .(side = ifelse(sd < 0, A$name, B$name), band = cut(abs(sd), c(0, 2, 5, 10, 30), include.lowest = TRUE))][order(side, band)]
cat("\n== by side and distance from the boundary (km): mean BP %/yr over burnable cells; record = MTBS >= 405 ha burn frequency 1984-2024\n"); print(side[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n== 2 km strips next to the line (A side negative)\n"); print(P[strip >= -6 & strip < 6][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
w <- DT[abs(sd) <= 2]; cat(sprintf("\nwithin 2 km: own-run BP %s side %.3f vs %s side %.3f (ratio %.2f); merged %.3f vs %.3f (ratio %.2f); record %.3f vs %.3f\n",
  A$name, w[sd < 0, mean(own)], B$name, w[sd > 0, mean(own)], w[sd < 0, mean(own)] / w[sd > 0, mean(own)], w[sd < 0, mean(merged)], w[sd > 0, mean(merged)],
  w[sd < 0, mean(merged)] / w[sd > 0, mean(merged)], w[sd < 0, mean(obs)], w[sd > 0, mean(obs)]))

# ---- figure: merged map around the line, and the profile ---------------------------------------------------------------------------
png(file.path(OUT, "boundary_merge.png"), 2600, 1400, res = 150); layout(matrix(c(1, 2, 3, 4), 1), widths = c(1, 1, 1, 1.5)); par(mar = c(1, 1, 3, 1))
br <- c(0, 0.1, 0.2, 0.35, 0.5, 0.7, 1, 1.5, 2, 3, 100); pal <- hcl.colors(10, "YlOrRd", rev = TRUE); lv <- vect(line)
for (p in list(list(crop(a, W), paste(A$name, "run (result 17)")), list(crop(b, W), paste(B$name, "run (result 19)")), list(crop(m, W), "Merged"))) {
  plot(p[[1]], breaks = br, col = pal, axes = FALSE, legend = FALSE, main = p[[2]], mar = c(1, 1, 3, 1)); lines(vect(za), lwd = .5); lines(vect(zbb), lwd = .5); lines(lv, col = "#1f5fbf", lwd = 2) }
par(mar = c(5, 5, 3, 1)); yl <- c(0, max(unlist(P[, .(A_run, B_run, merged, record_1984_2024)])) * 1.05)
plot(P$strip + .5, P$merged, type = "l", lwd = 3, col = "black", ylim = yl, xlab = sprintf("distance from the shared boundary (km): %s <- | -> %s", A$name, B$name),
     ylab = "mean annual BP (%/yr), burnable cells", main = "Profile across the boundary (1 km strips)")
lines(P$strip + .5, P$A_run, col = "#1f77b4", lwd = 2); lines(P$strip + .5, P$B_run, col = "#d62728", lwd = 2)
lines(P$strip + .5, P$record_1984_2024, col = "grey40", lwd = 2, lty = 2); abline(v = 0, col = "grey60")
legend("topright", c("merged", paste(A$name, "fires"), paste(B$name, "fires"), "record: MTBS >= 405 ha, 1984-2024"), col = c("black", "#1f77b4", "#d62728", "grey40"), lwd = c(3, 2, 2, 2), lty = c(1, 1, 1, 2), bty = "n")
dev.off(); cat("wrote", file.path(OUT, "boundary_merge.png"), "\n")
