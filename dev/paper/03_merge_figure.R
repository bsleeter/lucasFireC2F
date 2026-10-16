# Paper Figure 8: burn probability over the four Sierra and foothill pyromes before (each zone's own run, kept inside its own
# pyrome and set side by side) and after the merge, and the percent increase. Reads result files only (Sierra Nevada & Foothills
# library: zone runs 54, 58, 72, 76; merge result 82 by default). Same rendering as dev/report/merge_full.R (user guide 8.5).
# Usage: Rscript 03_merge_figure.R [merge result id]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(progress = 0)
source("F:/lucasFireC2F/src/report.R")
S <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim.data"
OUT <- "F:/lucasFireC2F/dev/paper/figures"; dir.create(OUT, FALSE, TRUE)
args <- commandArgs(TRUE); MERGE <- if (length(args)) as.integer(args[1]) else 82L
bpf <- function(id) list.files(file.path(S, sprintf("Scenario-%d/lucasFireC2F_OutputBurnProbability", id)), "\\.tif$", full.names = TRUE)[1]
Z <- data.table(py = c(26, 27, 29, 28), id = c(54, 58, 72, 76))
M0 <- rast(bpf(MERGE))
py <- read_sf("F:/lucas-master/Data Sources/Pyromes/Data/Pyromes_CONUS_20200206.shp"); py <- st_transform(py[py$PYROME %in% Z$py, ], crs(M0))
U <- st_union(st_make_valid(py)); bb <- st_bbox(U)
el <- crop(rast(file.path(S, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape/engine_elevation.tif", MERGE))), ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500))
on <- function(f) { r <- if (inherits(f, "SpatRaster")) f else rast(f); if (!compareGeom(r, el, stopOnError = FALSE, crs = FALSE)) r <- resample(extend(crop(r, el, snap = "near"), el), el, method = "near"); v <- values(r, mat = FALSE); v[is.na(v)] <- 0; v }
m <- on(M0)
b <- rep(NA_real_, ncell(el)); pid <- values(rasterize(vect(py), el, field = "PYROME"), mat = FALSE)
for (i in seq_len(nrow(Z))) { v <- on(bpf(Z$id[i])); k <- !is.na(pid) & pid == Z$py[i]; b[k] <- v[k] }
inz <- !is.na(pid); fu <- on(list.files(file.path(S, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape", MERGE)), "^engine_fuel", full.names = TRUE)[1])
nb <- fu %in% c(0, 91, 92, 93, 98, 99)
zm <- function(v, k = inz & !nb) 100 * mean(v[k], na.rm = TRUE)
cat(sprintf("zone means before %.3f after %.3f %%/yr\n", zm(b), zm(m)))
slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
hv <- values(Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3, mat = FALSE)
qq <- quantile(hv, c(.02, .98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- median(hn, na.rm = TRUE); it <- ifelse(inz, .58 + .64 * hn, .87 + .19 * hn)
torgb <- function(v, brk, cols) { cm <- col2rgb(cols) / 255; cl <- cut(v, brk, include.lowest = TRUE, labels = FALSE)
  R <- rep(.925, length(v)); G <- rep(.920, length(v)); Bb <- rep(.909, length(v)); gg <- inz & (is.na(cl) | nb); R[gg] <- .795; G[gg] <- .805; Bb[gg] <- .818
  k <- inz & !is.na(cl) & !nb; R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; Bb[k] <- cm[3, cl[k]]
  o <- rast(el, nlyr = 3); values(o) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(Bb * it, 1) * 255)); o }
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
# percent increase, after / before - 1; a cell that burned only after the merge (before = 0) goes in the top class
pc <- ifelse(b > 0, 100 * (m / b - 1), ifelse(m > 0, Inf, 0)); pc[!is.na(pc) & pc < 0] <- 0
db <- c(0, 1, 5, 10, 25, 50, 100, 200, 500, Inf); dl <- c("<1", "1", "5", "10", "25", "50", "100", "200", ">500")
dcol <- colorRampPalette(c("#f7f4ee", "#cfe1ee", "#8db7d6", "#4a87b8", "#235a8c", "#0d2f57"))(9)
P <- list(list(v = b, brk = bpb, cols = RAMP(10), lab = bpl, t = "Each zone run alone (%/yr)", s = sprintf("own fires only, inside own pyrome; mean %.2f%%/yr", zm(b))),
          list(v = m, brk = bpb, cols = RAMP(10), lab = bpl, t = "Four zones merged (%/yr)", s = sprintf("fires from all four zones; mean %.2f%%/yr", zm(m))),
          list(v = pc, brk = db, cols = dcol, lab = dl, t = "Increase from merging (%)", s = "merged / alone - 1"))
lab <- st_point_on_surface(st_make_valid(py)); lab$txt <- paste0("P", lab$PYROME)
E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1]); PW <- 420; PH <- round(PW * AR); TOP <- 150; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- 3
W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB
png(file.path(OUT, "Fig8_merge.png"), width = W, height = H, res = 150, bg = "white")
par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
text(LM, H - 40, "Figure 8. Merging zones", adj = c(0, .5), cex = 1.25, font = 2, col = INK)
nt <- strwrap(sprintf("P26 Northern Sierra Nevada, P27 Southern Sierra Nevada, P28 Southern Sierran and Tehachapi Foothills and P29 Northern Sierran Foothills; zone runs 54, 58, 76 and 72 and merge result %d, 2,500 seasons each, 150 m. Grey: non-burnable. Lines: pyrome boundaries.", MERGE), floor((W - 2 * LM) / 9.5))
for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = .72, col = MUTED)
for (i in 1:n) { p <- P[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB
  par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
  plotRGB(torgb(p$v, p$brk, p$cols), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
  plot(st_geometry(py), add = TRUE, border = INK, lwd = 1, col = NA)
  if (i == 1) { xy <- st_coordinates(lab); text(xy[, 1], xy[, 2], lab$txt, cex = .75, font = 2, col = INK) }
  par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  text(x0, y0 + PH + 60, sprintf("(%s) %s", letters[i], p$t), adj = c(0, .5), font = 2, cex = .85, col = INK); text(x0, y0 + PH + 30, p$s, adj = c(0, .5), cex = .58, col = MUTED)
  nl <- length(p$cols); lw <- PW / nl; for (j in 1:nl) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = p$cols[j], border = "white"); text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = .5, col = INK) } }
dev.off(); cat("wrote Fig8_merge.png\n")
