# Guide 8.5: burn probability along P18's southern boundary, P18 alone (Demo result 6) against the P18+P26+P29 merge (result 22).
# Reads result files only (no library access). Writes docs/user-guide/figures/merge/Merge_boundary.png and prints the band table.
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(progress = 0)
source("F:/lucasFireC2F/src/report.R")
D <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F-Demo.ssim.data"; OUT <- "F:/lucasFireC2F/docs/user-guide/figures/merge"; dir.create(OUT, FALSE, TRUE)
args <- commandArgs(TRUE); MERGE <- if (length(args)) as.integer(args[1]) else 22L
bpA <- rast(list.files(file.path(D, "Scenario-6/lucasFireC2F_OutputBurnProbability"), full.names = TRUE)[1])
bpM <- rast(list.files(file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputBurnProbability", MERGE)), full.names = TRUE)[1])
elM <- rast(file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape/engine_elevation.tif", MERGE)))
fuM <- rast(list.files(file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape", MERGE)), "^engine_fuel", full.names = TRUE)[1])
py <- read_sf("F:/lucas-master/Data Sources/Pyromes/Data/Pyromes_CONUS_20200206.shp"); py <- st_transform(py[py$PYROME %in% c(18, 26, 29), ], crs(bpM))
p18 <- st_union(st_make_valid(py[py$PYROME == 18, ])); south <- st_union(st_make_valid(py[py$PYROME %in% c(26, 29), ]))
# frame: the southern half of P18 and the neighbours just across the border
bb <- st_bbox(p18); fr <- ext(bb$xmin - 5000, bb$xmax + 5000, bb$ymin - 30000, bb$ymin + 0.55 * (bb$ymax - bb$ymin))
el <- crop(elM, fr); A <- extend(crop(bpA, fr), el); A <- resample(A, el, method = "near"); M <- crop(bpM, el)
a <- values(A, mat = FALSE); m <- values(M, mat = FALSE); a[is.na(a)] <- 0; m[is.na(m)] <- 0
fu <- values(crop(fuM, el), mat = FALSE); nb <- fu %in% c(91, 92, 93, 98, 99) | is.na(fu)
inz <- !is.na(values(rasterize(vect(st_union(c(p18, south))), el, field = 1), mat = FALSE))
in18 <- !is.na(values(rasterize(vect(p18), el, field = 1), mat = FALSE))
# distance inside P18 from the P26/P29 border, in km
dS <- values(distance(rasterize(vect(south), el, field = 1)), mat = FALSE) / 1000
B <- cut(dS, c(0, 5, 10, 20, 40, Inf), right = FALSE, labels = c("0-5 km", "5-10 km", "10-20 km", "20-40 km", "over 40 km"))
T <- data.table(band = B, a, m)[in18 & !nb & !is.na(band), .(cells = .N, alone = 100 * mean(a), merged = 100 * mean(m)), by = band][order(band)][, ratio := merged / alone]
print(T)
# shaded relief
slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
hv <- values(Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3, mat = FALSE)
qq <- quantile(hv, c(.02, .98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- .5; it <- ifelse(inz, .58 + .64 * hn, .87 + .19 * hn)
torgb <- function(v, brk, cols) { cm <- col2rgb(cols) / 255; cl <- cut(v, brk, include.lowest = TRUE, labels = FALSE)
  R <- rep(.925, length(v)); G <- rep(.920, length(v)); Bb <- rep(.909, length(v)); gg <- inz & (is.na(cl) | nb); R[gg] <- .795; G[gg] <- .805; Bb[gg] <- .818
  k <- inz & !is.na(cl) & !nb; R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; Bb[k] <- cm[3, cl[k]]
  o <- rast(el, nlyr = 3); values(o) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(Bb * it, 1) * 255)); o }
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
db <- c(0, .0001, .0005, .001, .002, .003, .005, .0075, .01, .02, 1); dl <- c("0", ".01", ".05", ".1", ".2", ".3", ".5", ".75", "1", ">2")
dcol <- colorRampPalette(c("#f7f4ee", "#cfe1ee", "#8db7d6", "#4a87b8", "#235a8c", "#0d2f57"))(10)
P <- list(list(v = a, brk = bpb, cols = RAMP(10), lab = bpl, t = "P18 alone", s = "only fires that start in P18 (result 6)"),
          list(v = m, brk = bpb, cols = RAMP(10), lab = bpl, t = "P18 merged with P26 and P29", s = sprintf("fires from all three zones (result %d)", MERGE)),
          list(v = pmax(m - a, 0), brk = db, cols = dcol, lab = dl, t = "Added by the merge (%/yr)", s = "merged minus P18 alone: fire from P26 and P29"))
E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1]); PW <- 560; PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- 3
W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB
png(file.path(OUT, "Merge_boundary.png"), width = W, height = H, res = 150, bg = "white")
par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
text(LM, H - 40, "Merging zones: burn probability along P18's southern boundary", adj = c(0, .5), cex = 1.25, font = 2, col = INK)
nt <- strwrap("Southern half of P18 Southern Cascades and the neighbouring P26 Northern Sierra Nevada and P29 Northern Sierran Foothills; 2,500 seasons; 150 m. Lines: pyrome boundaries. Alone, P18 counts only its own fires; merged, cells near the boundary also count fires that start across it.", floor((W - 2 * LM) / 9.5))
for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = .72, col = MUTED)
for (i in 1:n) { p <- P[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB
  par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
  plotRGB(torgb(p$v, p$brk, p$cols), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
  plot(st_geometry(py), add = TRUE, border = INK, lwd = 1.1, col = NA)
  par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  text(x0, y0 + PH + 60, sprintf("(%s) %s", letters[i], p$t), adj = c(0, .5), font = 2, cex = .9, col = INK); text(x0, y0 + PH + 30, p$s, adj = c(0, .5), cex = .62, col = MUTED)
  lw <- PW / 10; for (j in 1:10) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = p$cols[j], border = "white"); text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = .55, col = INK) } }
text(LM, 30, "Annual burn probability (%/yr)", adj = c(0, .5), cex = .7, col = MUTED)
dev.off(); cat("wrote Merge_boundary.png\n"); fwrite(T, file.path(OUT, "merge_boundary_bands.csv"))
