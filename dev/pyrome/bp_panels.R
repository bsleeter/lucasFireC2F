# Burn probability panels in the methods-paper style (01_figures.R: hillshade under the paper's BP ramp, grey non-burnable,
# MTBS 2021-2024 wildfires >= 405 ha outlined) for a zone library: several results plus WRC V2, one row.
# Usage: Rscript bp_panels.R <out png>   (zone, results and labels set below)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(rsyncrosim) }); sf_use_s2(FALSE); terraOptions(progress = 0)
OUTF <- commandArgs(TRUE)[1]
LIB <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim"; PREP <- 2L; NBFROM <- 27L
RES <- list(list(id = 11L, title = "Result 11: first P27 run", auc = 0.585),
            list(id = 19L, title = "Result 19: + by cause, remaps, die-off fuel", auc = 0.629),
            list(id = 27L, title = "Result 27: + recent-fire rules, vegetation strata", auc = 0.701))
WRC_AUC <- 0.874
L <- suppressWarnings(ssimLibrary(LIB)); g <- function(id, n) as.data.table(datasheet(rsyncrosim::scenario(L, id), paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
zone <- st_union(st_make_valid(read_sf(g(PREP, "OutputZoneBoundary")$FileName[1])))
EL0 <- rast(g(PREP, "LandscapeRasters")$ElevationFileName[1]); zone <- st_transform(zone, crs(EL0)); bb <- st_bbox(zone)
el <- crop(EL0, ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500))
cv <- function(f) { r <- rast(f); if (!compareGeom(r, EL0, stopOnError = FALSE)) r <- resample(r, EL0, method = "near"); values(crop(r, el), mat = FALSE) }
inz <- !is.na(values(rasterize(vect(zone), el, field = 1), mat = FALSE))
fu <- cv(g(NBFROM, "OutputEngineLandscape")$LandscapeFuelFileName[1]); nb <- fu %in% c(91, 92, 93, 98, 99)
slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
hs <- Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3
hv <- values(hs, mat = FALSE); qq <- quantile(hv, c(0.02, 0.98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- 0.5
it <- ifelse(inz, 0.58 + 0.64 * hn, 0.87 + 0.19 * hn); E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1])
mt <- read_sf("F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp"); names(mt) <- tolower(names(mt))
mt <- mt[mt$incid_type == "Wildfire" & mt$burnbndac * 0.404686 >= 405, ]; mt$yr <- as.integer(substr(as.character(mt$ig_date), 1, 4))
mt <- mt[mt$yr >= 2021 & mt$yr <= 2024, ]
mt <- suppressWarnings(st_intersection(st_geometry(st_make_valid(st_transform(mt, crs(el)))), st_geometry(zone)))
RAMP <- colorRampPalette(c("#f5efe3", "#fbe3a2", "#f7c344", "#ef8f2a", "#dd4b20", "#a81d1a", "#5c0f14"))
torgb <- function(v, brk, cols = RAMP(length(brk) - 1)) { cm <- col2rgb(cols) / 255; cl <- cut(v, brk, include.lowest = TRUE, labels = FALSE)
  R <- rep(0.925, length(v)); G <- rep(0.920, length(v)); B <- rep(0.909, length(v))
  gg <- inz & (is.na(cl) | nb); R[gg] <- 0.795; G[gg] <- 0.805; B[gg] <- 0.818
  k <- inz & !is.na(cl) & !nb; R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; B[k] <- cm[3, cl[k]]
  r <- rast(el, nlyr = 3); values(r) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(B * it, 1) * 255)); r }
figure <- function(file, panels, title, note, PW = 520) {
  PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- length(panels)
  W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB
  png(file, width = W, height = H, res = 150, bg = "white")
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  nt <- strwrap(note, floor((W - 2 * LM) / 9.5))
  text(LM, H - 40, title, adj = c(0, .5), cex = 1.25, font = 2, col = "#20211f"); for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = 0.72, col = "#55534c")
  for (i in seq_len(n)) { p <- panels[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB; cols <- RAMP(length(p$brk) - 1)
    par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
    plotRGB(torgb(p$v, p$brk, cols), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
    plot(mt, add = TRUE, border = "#00aec4", lwd = 1.1, col = NA); plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1.2, col = NA)
    par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
    tt <- strwrap(sprintf("(%s) %s", letters[i], p$title), floor(PW / 10.5)); for (k in seq_along(tt)) text(x0, y0 + PH + 60 + (length(tt) - k) * 24, tt[k], adj = c(0, .5), font = 2, cex = 0.9, col = "#20211f")
    text(x0, y0 + PH + 30, p$sub, adj = c(0, .5), cex = 0.62, col = "#55534c")
    nL <- length(cols); lw <- PW / nL
    for (j in seq_len(nL)) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = cols[j], border = "white"); text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = 0.55, col = "#20211f") } }
  dev.off(); cat("wrote", file, "\n") }
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
panels <- lapply(RES, function(r) { v <- cv(g(r$id, "OutputBurnProbability")$FileName[1]); v[is.na(v)] <- 0
  list(v = v, brk = bpb, lab = bpl, title = r$title, sub = sprintf("zone mean %.2f%%/yr; AUC vs 2021-24 MTBS fires %.3f", 100 * mean(v[inz & !nb]), r$auc)) })
wbp <- values(resample(crop(rast("F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"), el + 600), el, method = "near"), mat = FALSE)
if (max(wbp, na.rm = TRUE) > 1) wbp <- wbp / 1e4
panels[[length(panels) + 1]] <- list(v = wbp, brk = bpb, lab = bpl, title = "WRC V2 / FSim", sub = sprintf("zone mean %.2f%%/yr; AUC vs 2021-24 MTBS fires %.3f", 100 * mean(wbp[inz & !nb], na.rm = TRUE), WRC_AUC))
figure(OUTF, panels, "Pyrome 27 Southern Sierra Nevada: annual burn probability (%/yr)",
       "lucasFireC2F results on LANDFIRE 2020 fuels at 150 m, 2,500 fire seasons each, beside WRC V2. Grey: non-burnable, including the 0-2 year fire scars that result 27's recent-fire rules make non-burnable (Creek and Castle 2020). Outlines: MTBS wildfires >= 405 ha, 2021-2024 (after the fuel vintage).")
