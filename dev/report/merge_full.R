# Guide 8.5: burn probability over all three merged zones, before (each zone's own run, kept inside its own pyrome and set side by
# side) and after the merge, and what the merge added. Reads result files only.
# Usage: Rscript merge_full.R [merge result id, default 22]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(progress = 0)
source("F:/lucasFireC2F/src/report.R")
DEV <- "F:/lucas-master/Models/lucasFireC2F Dev"; D <- file.path(DEV, "lucasFireC2F-Demo.ssim.data"); S <- file.path(DEV, "lucasFireC2F Sierra Nevada & Foothills.ssim.data")
OUT <- "F:/lucasFireC2F/docs/user-guide/figures/merge"; dir.create(OUT, FALSE, TRUE)
args <- commandArgs(TRUE); MERGE <- if (length(args)) as.integer(args[1]) else 22L
bpf <- function(dir) list.files(file.path(dir, "lucasFireC2F_OutputBurnProbability"), "\\.tif$", full.names = TRUE)[1]
Z <- list(list(py = 18, f = bpf(file.path(D, "Scenario-6"))), list(py = 26, f = bpf(file.path(S, "Scenario-54"))), list(py = 29, f = bpf(file.path(S, "Scenario-72"))))
M0 <- rast(bpf(file.path(D, sprintf("Scenario-%d", MERGE))))
py <- read_sf("F:/lucas-master/Data Sources/Pyromes/Data/Pyromes_CONUS_20200206.shp"); py <- st_transform(py[py$PYROME %in% c(18, 26, 29), ], crs(M0))
U <- st_union(st_make_valid(py)); bb <- st_bbox(U)
el <- crop(rast(file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape/engine_elevation.tif", MERGE))), ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500))
on <- function(f) { r <- rast(f); if (!compareGeom(r, el, stopOnError = FALSE, crs = FALSE)) r <- resample(extend(crop(r, el, snap = "near"), el), el, method = "near"); v <- values(r, mat = FALSE); v[is.na(v)] <- 0; v }
m <- on(sources(M0))
# before: each zone's own map inside its own pyrome (what a set of single-zone runs gives when put side by side)
b <- rep(NA_real_, ncell(el)); pid <- values(rasterize(vect(py), el, field = "PYROME"), mat = FALSE)
for (z in Z) { v <- on(z$f); k <- !is.na(pid) & pid == z$py; b[k] <- v[k] }
inz <- !is.na(pid); fu <- on(list.files(file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputEngineLandscape", MERGE)), "^engine_fuel", full.names = TRUE)[1])
nb <- fu %in% c(0, 91, 92, 93, 98, 99)
zm <- function(v, k = inz & !nb) 100 * mean(v[k], na.rm = TRUE)
cat(sprintf("zone means before %.3f after %.3f %%/yr\n", zm(b), zm(m)))
for (p in c(18, 26, 29)) cat(sprintf("  P%d: before %.3f after %.3f %%/yr (x%.2f)\n", p, zm(b, inz & !nb & pid == p), zm(m, inz & !nb & pid == p), zm(m, inz & !nb & pid == p) / zm(b, inz & !nb & pid == p)))
slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
hv <- values(Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3, mat = FALSE)
qq <- quantile(hv, c(.02, .98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- .5; it <- ifelse(inz, .58 + .64 * hn, .87 + .19 * hn)
torgb <- function(v, brk, cols) { cm <- col2rgb(cols) / 255; cl <- cut(v, brk, include.lowest = TRUE, labels = FALSE)
  R <- rep(.925, length(v)); G <- rep(.920, length(v)); Bb <- rep(.909, length(v)); gg <- inz & (is.na(cl) | nb); R[gg] <- .795; G[gg] <- .805; Bb[gg] <- .818
  k <- inz & !is.na(cl) & !nb; R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; Bb[k] <- cm[3, cl[k]]
  o <- rast(el, nlyr = 3); values(o) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(Bb * it, 1) * 255)); o }
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
# percent increase, after / before - 1; a cell that burned only after the merge (before = 0) goes in the top class
pc <- ifelse(b > 0, 100 * (m / b - 1), ifelse(m > 0, Inf, 0)); pc[!is.na(pc) & pc < 0] <- 0
db <- c(0, 1, 5, 10, 25, 50, 100, 200, 500, Inf); dl <- c("<1", "1", "5", "10", "25", "50", "100", "200", ">500")
dcol <- colorRampPalette(c("#f7f4ee", "#cfe1ee", "#8db7d6", "#4a87b8", "#235a8c", "#0d2f57"))(9)
for (p in c(18, 26, 29)) { k <- inz & !nb & pid == p; cat(sprintf("  P%d: cells up >10%% %.1f%%, >100%% %.1f%%\n", p, 100 * mean(pc[k] > 10, na.rm = TRUE), 100 * mean(pc[k] > 100, na.rm = TRUE))) }
P <- list(list(v = b, brk = bpb, cols = RAMP(10), lab = bpl, t = "Before: each zone run alone", s = sprintf("each zone's own map inside its pyrome; mean %.2f%%/yr", zm(b))),
          list(v = m, brk = bpb, cols = RAMP(10), lab = bpl, t = "After: the three zones merged", s = sprintf("fires from all three zones counted everywhere; mean %.2f%%/yr", zm(m))),
          list(v = pc, brk = db, cols = dcol, lab = dl, t = "Increase in burn probability (%)", s = "after / before - 1: fire from the neighbouring zones"))
E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1]); PW <- 430; PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- 3
W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB
png(file.path(OUT, "Merge_full.png"), width = W, height = H, res = 150, bg = "white")
par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
text(LM, H - 40, "Merging zones: P18, P26 and P29 before and after", adj = c(0, .5), cex = 1.25, font = 2, col = INK)
nt <- strwrap("P18 Southern Cascades, P26 Northern Sierra Nevada and P29 Northern Sierran Foothills and Tuscan Flows; 2,500 seasons; 150 m. Lines: pyrome boundaries. Before the merge each pyrome counts only the fires that start in it; after, cells near a shared boundary also count fires that start across it.", floor((W - 2 * LM) / 9.5))
for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = .72, col = MUTED)
for (i in 1:n) { p <- P[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB
  par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
  plotRGB(torgb(p$v, p$brk, p$cols), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
  plot(st_geometry(py), add = TRUE, border = INK, lwd = 1, col = NA)
  par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  text(x0, y0 + PH + 60, sprintf("(%s) %s", letters[i], p$t), adj = c(0, .5), font = 2, cex = .9, col = INK); text(x0, y0 + PH + 30, p$s, adj = c(0, .5), cex = .6, col = MUTED)
  lw <- PW / 10; for (j in 1:10) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = p$cols[j], border = "white"); text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = .5, col = INK) } }
text(LM, 30, "(a, b) annual burn probability, %/yr; (c) percent increase", adj = c(0, .5), cex = .7, col = MUTED)
dev.off(); cat("wrote Merge_full.png\n")
