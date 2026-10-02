# Figures for the methods paper from a lucasFireC2F result (dev/paper/result_<id>.rds). Writes dev/paper/figures/Fig*.png (300 dpi).
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(arrow) })
sf_use_s2(FALSE); terraOptions(progress = 0)
id <- as.integer(commandArgs(TRUE)[1]); D <- "F:/lucasFireC2F/dev/paper"; FD <- file.path(D, "figures"); dir.create(FD, showWarnings = FALSE)
X <- readRDS(file.path(D, sprintf("result_%d.rds", id)))
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, "spatial/pyromes_150m_LF2020/pyrome_026")
source(file.path(M, "src/00_config.R"))
zone <- read_sf(file.path(ZD, "zone.gpkg")); g <- rast(X$engine$FuelFileName[1])
el <- rast(file.path(ZD, "Elevation.tif")); ext(g) <- ext(el); bb <- st_bbox(zone)
EL0 <- el
set_frame <- function(e) { el <<- crop(EL0, e); inz <<- !is.na(values(rasterize(vect(zone), el, field = 1), mat = FALSE))
  fu <<- cv(X$engine$LandscapeFuelFileName[1]); nb <<- fu %in% c(91, 92, 93, 98, 99)
  slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
  hs <- Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3
  hv <- values(hs, mat = FALSE); qq <- quantile(hv, c(0.02, 0.98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- 0.5
  it <<- ifelse(inz, 0.58 + 0.64 * hn, 0.87 + 0.19 * hn); E <<- ext(el); AR <<- (E[4] - E[3]) / (E[2] - E[1]) }
cv <- function(f, g0 = g) { r <- rast(f); if (!compareGeom(r, g0, stopOnError = FALSE)) ext(r) <- ext(g0); values(crop(r, el), mat = FALSE) }
set_frame(ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500))
mt <- read_mtbs(); mt <- mt[mt$Incid_Type == "Wildfire" & !is.na(mt$Ig_Date), ]; mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y"))
mt <- mt[mt$yr >= 2021 & mt$yr <= 2024 & mt$BurnBndAc * 0.404686 >= 405, ]
mt <- suppressWarnings(st_intersection(st_geometry(st_make_valid(st_transform(mt, crs(el)))), st_geometry(zone)))
RAMP <- colorRampPalette(c("#f5efe3", "#fbe3a2", "#f7c344", "#ef8f2a", "#dd4b20", "#a81d1a", "#5c0f14"))
torgb <- function(v, brk, cols = RAMP(length(brk) - 1), paint_nb = FALSE) { cm <- col2rgb(cols) / 255
  cl <- if (is.null(brk)) v else cut(v, brk, include.lowest = TRUE, labels = FALSE)
  R <- rep(0.925, length(v)); G <- rep(0.920, length(v)); B <- rep(0.909, length(v))
  gg <- inz & (is.na(cl) | (nb & !paint_nb)); R[gg] <- 0.795; G[gg] <- 0.805; B[gg] <- 0.818
  k <- inz & !is.na(cl) & (paint_nb | !nb); R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; B[k] <- cm[3, cl[k]]
  r <- rast(el, nlyr = 3); values(r) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(B * it, 1) * 255)); r }

# a figure of n panels in one row; each panel = list(values, breaks, labels, title, subtitle, colours, paint_nb, outlines)
figure <- function(file, panels, title, note, PW = 520) {
  PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- length(panels)
  W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB
  png(file.path(FD, file), width = W, height = H, res = 150, bg = "white")
  par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
  nch <- floor((W - 2 * LM) / 9.5); nt <- strwrap(note, nch)
  text(LM, H - 40, title, adj = c(0, .5), cex = 1.25, font = 2, col = "#20211f"); for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = 0.72, col = "#55534c")
  for (i in seq_len(n)) { p <- panels[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB
    cols <- if (is.null(p$cols)) RAMP(length(p$brk) - 1) else p$cols
    par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
    plotRGB(torgb(p$v, p$brk, cols, isTRUE(p$paint_nb)), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
    if (!is.null(p$outline)) plot(p$outline, add = TRUE, border = p$ocol %||% "#00aec4", lwd = 1.1, col = NA)
    plot(st_geometry(zone), add = TRUE, border = "#20211f", lwd = 1.2, col = NA)
    par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
    tt <- strwrap(sprintf("(%s) %s", letters[i], p$title), floor(PW / 10.5)); for (k in seq_along(tt)) text(x0, y0 + PH + 60 + (length(tt) - k) * 24, tt[k], adj = c(0, .5), font = 2, cex = 0.9, col = "#20211f")
    text(x0, y0 + PH + 30, p$sub, adj = c(0, .5), cex = 0.62, col = "#55534c")
    nL <- length(cols); lw <- PW / nL
    for (j in seq_len(nL)) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = cols[j], border = "white")
      text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = 0.55, col = "#20211f", srt = if (nL > 8) 0 else 0) } }
  dev.off(); cat("wrote", file, "\n") }
`%||%` <- function(a, b) if (is.null(a)) b else a
ef <- function(code) cv(X$effects[Code == code]$FileName[1]); rk <- function(code) cv(X$risk[Code == code]$FileName[1])
bpv <- cv(X$bp); bpv[is.na(bpv)] <- 0
NOTE <- sprintf("Pyrome 26 Northern Sierra Nevada; lucasFireC2F result %d: %s fire seasons, LANDFIRE 2020 fuels, 150 m. Grey: non-burnable or too few burns.", id, format(X$run_control$MaximumIteration, big.mark = ","))

# ---- Fig 1 inputs ------------------------------------------------------------------------------------------------------
ft <- X$fuel_type; grp <- sub(" [A-Z]{2}[0-9]+$", "", ft$Description[match(fu, ft$ID)])
G <- c("Grass", "Grass-Shrub", "Shrub", "Timber-Understory", "Timber Litter", "Slash-Blowdown"); gi <- match(grp, G)
gcol <- c("#e6d77a", "#d8b35a", "#b0784a", "#5f8f4e", "#2f6b3a", "#7a5a44")
cbd <- cv(X$canopy$CanopyBulkDensityFileName[1]); cbd[is.na(cbd)] <- 0
ig <- file.path(dirname(dirname(X$engine$FuelFileName[1])), "lucasFireC2F_IgnitionGrid")
igf <- list.files(sub("Scenario-[0-9]+", "Scenario-1", ig), "\\.tif$", full.names = TRUE)
ign <- Reduce(`+`, lapply(igf, function(f) { v <- cv(f); v[is.na(v)] <- 0; v / sum(v) }))
q_ig <- quantile(ign[ign > 0 & inz], c(0, .2, .4, .6, .8, .9, .95, .99), na.rm = TRUE)
figure("Fig1_inputs.png", list(
  list(v = gi, brk = NULL, cols = gcol, lab = c("Grass", "Grass-shrub", "Shrub", "Timber-und.", "Timber litter", "Slash"), title = "Fuel group (LANDFIRE 2020 FBFM40)",
       sub = "after recent-fire rules (<=2 yr NB9, 3-5 yr TL1, 6-10 yr TL8)"),
  list(v = cbd, brk = c(0, .0001, .03, .06, .09, .12, .15, .2, .3, 10), lab = c("0", ".03", ".06", ".09", ".12", ".15", ".2", ".3", ">.3"),
       title = "Canopy bulk density (kg/m3)", sub = "LANDFIRE 2020 CBD, forested-pixel mean at 150 m"),
  list(v = ign, brk = c(0, q_ig[-1], 1), lab = c("<20", "20-40", "40-60", "60-80", "80-90", "90-95", "95-99", ">99"), title = "Ignition probability (percentile)",
       sub = "human + lightning surfaces, each normalized")), "Figure 1. Model inputs", NOTE)

# ---- Fig 2 burn probability vs WRC --------------------------------------------------------------------------------------
wbp <- values(resample(crop(rast("F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"), el + 600), el, method = "near"), mat = FALSE)
if (max(wbp, na.rm = TRUE) > 1) wbp <- wbp / 1e4
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
T4 <- fread(file.path(D, "tables/T4_spatial_validation_AUC.csv")); a <- T4[grepl("^2021-2024", Period)]
figure("Fig2_burn_probability.png", list(
  list(v = bpv, brk = bpb, lab = bpl, title = "Annual burn probability, lucasFireC2F (%/yr)", outline = mt,
       sub = sprintf("zone mean %.2f%%/yr; AUC vs 2021-24 MTBS fires %.3f", 100 * mean(bpv[inz & !nb]), a$AUC_lucasFireC2F)),
  list(v = wbp, brk = bpb, lab = bpl, title = "Annual burn probability, WRC V2 / FSim (%/yr)", outline = mt,
       sub = sprintf("zone mean %.2f%%/yr; AUC vs 2021-24 MTBS fires %.3f", 100 * mean(wbp[inz & !nb], na.rm = TRUE), a$AUC_WRC))),
  "Figure 2. Burn probability and spatial validation", paste(NOTE, "Outlines: MTBS wildfires >= 405 ha, 2021-2024 (after the fuel vintage)."))

# ---- Fig 3 intensity, Fig 4 crown fire -------------------------------------------------------------------------------------
pb <- c(0, .05, .1, .15, .2, .3, .4, .5, .6, .8, 1.0001); pl <- c("0", ".05", ".1", ".15", ".2", ".3", ".4", ".5", ".6", ".8+")
figure("Fig3_fire_intensity.png", list(
  list(v = ef("CFL"), brk = c(0, 2, 4, 6, 8, 12, 16, 20, 30, 40, 1000), lab = c("<2", "2-4", "4-6", "6-8", "8-12", "12-16", "16-20", "20-30", "30-40", ">40"),
       title = "Conditional flame length (ft)", sub = "mean flame length when the cell burns"),
  list(v = ef("FLEP4"), brk = pb, lab = pl, title = "P(flame length > 4 ft | burns)", sub = "FLEP4"),
  list(v = ef("FLEP8"), brk = pb, lab = pl, title = "P(flame length > 8 ft | burns)", sub = "FLEP8")),
  "Figure 3. Fire intensity (Rothermel surface fire, Cruz active crown fire, observed RAWS winds)", NOTE)
cb <- c(0, .0005, .001, .002, .003, .005, .0075, .01, .015, .02, 1); cbl <- c("<.05", ".05", ".1", ".2", ".3", ".5", ".75", "1", "1.5", ">2")
cbp <- ef("CROWNBP"); cbp[is.na(cbp)] <- 0
figure("Fig4_crown_fire.png", list(
  list(v = ef("PCROWN"), brk = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0001), lab = sprintf("%.1f", seq(0, .9, .1)), title = "P(crown fire | burns)", sub = "passive + active (Van Wagner initiation)"),
  list(v = ef("PACTIVE"), brk = c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0001), lab = sprintf("%.1f", seq(0, .9, .1)), title = "P(active crown fire | burns)", sub = "Cruz et al. (2005) active spread >= 3 / CBD"),
  list(v = cbp, brk = cb, lab = cbl, title = "Crown-fire probability (%/yr)", sub = "burn probability x P(crown | burns)")),
  "Figure 4. Crown fire", NOTE)

# ---- Fig 5 hazard and risk ---------------------------------------------------------------------------------------------------
hi <- rk("HIBP"); cr <- rk("CRPS"); rp <- rk("RPS"); rc <- rk("RPSCLASS")
figure("Fig5_hazard_risk.png", list(
  list(v = hi, brk = c(0, .0001, .0002, .0005, .001, .002, .003, .005, .0075, .01, 1), lab = c("<.01", ".01", ".02", ".05", ".1", ".2", ".3", ".5", ".75", ">1"),
       title = "High-intensity burn probability (%/yr)", sub = "BP x FLEP8"),
  list(v = cr, brk = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 95, 101), lab = c("<20", "20", "30", "40", "50", "60", "70", "80", "90", "95+"), paint_nb = TRUE,
       title = "Conditional risk to potential structures", sub = "% loss if a structure here burns (WRC response functions)"),
  list(v = rp, brk = c(0, .05, .1, .2, .35, .5, .75, 1, 1.25, 1.5, 1e9), lab = c("<.05", ".05", ".1", ".2", ".35", ".5", ".75", "1", "1.25", ">1.5"), paint_nb = TRUE,
       title = "Risk to potential structures (%/yr)", sub = "BP x cRPS; no housing layer"),
  list(v = rc, brk = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), cols = c("#fbf3d5", "#f7c344", "#ef8f2a", "#c93a1d", "#5c0f14"), paint_nb = TRUE,
       lab = c("<40th", "40-70", "70-90", "90-95", ">95th"), title = "Relative risk class", sub = "percentile of RPS across P26")), "Figure 5. Hazard and risk to potential structures", NOTE, PW = 430)

# ---- Fig 6 one simulated fire: MTBS-style severity, fire type, day of burning (per-burn table, stage 8) --------------------------
# FIG6_FIRE = "season:fire" picks the fire (default: season 443 fire 10, chosen from a contact sheet of large irregular fires)
f6 <- as.integer(strsplit(Sys.getenv("FIG6_FIRE", "443:10"), ":")[[1]]); s <- f6[1]; fid <- f6[2]
b6 <- as.data.table(arrow::open_dataset(X$burn_effects) |> dplyr::filter(Season == s, FireID == fid) |> dplyr::select(Cell, Hour, FireType, SeverityClass) |> dplyr::collect())
pf <- st_read(X$perimeters, quiet = TRUE); big <- st_transform(pf[pf$Season == s & pf$FireID == fid, ], crs(el))
bz <- st_bbox(big); cx <- (bz$xmin + bz$xmax) / 2; cy <- (bz$ymin + bz$ymax) / 2; hwid <- max(bz$xmax - bz$xmin, (bz$ymax - bz$ymin) / 1.3) / 2 + 3000
set_frame(ext(cx - hwid, cx + hwid, cy - 1.3 * hwid, cy + 1.3 * hwid)); inz <- rep(TRUE, ncell(el))   # fires run into the buffer: paint the whole window
k6 <- cellFromXY(el, xyFromCell(g, b6$Cell)); ok <- !is.na(k6)
m1 <- m2 <- m3 <- rep(NA_real_, ncell(el)); m1[k6[ok]] <- b6$SeverityClass[ok]; m2[k6[ok]] <- b6$FireType[ok]
day <- ceiling(b6$Hour / 2); ND <- max(day, na.rm = TRUE); m3[k6[ok]] <- day[ok]
db <- unique(round(c(0.5, seq(1.5, ND - 0.5, length.out = min(ND - 1, 8)), ND + 0.5))); dl <- sprintf("%d", ceiling(db[-1] - 0.5))
dl[1] <- "1"; dcol <- colorRampPalette(c("#fde9a9", "#f4a340", "#c8431c", "#5c0f14"))(length(db) - 1)
cat(sprintf("Fig 6 fire S%d F%d: %s burns, %d spread days, hours %d-%d\n", s, fid, format(nrow(b6), big.mark = ","), ND, min(b6$Hour), max(b6$Hour)))
figure("Fig6_fire_severity.png", list(
  list(v = m1, brk = NULL, cols = c("#9fd29a", "#f7e27b", "#f09a3a", "#b3261e"), lab = c("Unburned-low", "Low", "Moderate", "High"), paint_nb = TRUE, outline = st_geometry(big), ocol = "#20211f",
       title = "MTBS-style burn severity", sub = "per-fuel-group thresholds fitted to MTBS 2021-24"),
  list(v = m2, brk = NULL, cols = c("#f2c14e", "#e0662b", "#8c1c13"), lab = c("Surface", "Passive crown", "Active crown"), paint_nb = TRUE, outline = st_geometry(big), ocol = "#20211f",
       title = "Fire type", sub = "Rothermel surface fire; Van Wagner / Cruz crown fire"),
  list(v = m3, brk = db, cols = dcol, lab = dl, paint_nb = TRUE, outline = st_geometry(big), ocol = "#20211f",
       title = "Spread day of burning", sub = sprintf("C2F-W arrival time; %d spread days, 2 burning h/day", ND))),
  sprintf("Figure 6. One simulated large fire: %s ha (season %d, fire %d)", format(round(big$AreaHa), big.mark = ","), s, fid),
  paste(NOTE, "Thin black line: pyrome boundary (fires grow into the surrounding buffer). Stage 8 per-burn output: the fire events the LUCAS connector will apply as transitions."), PW = 430)

# ---- Fig 7 size distribution and calibration ----------------------------------------------------------------------------------
T2 <- fread(file.path(D, "tables/T2_size_classes_vs_FPA.csv"))[SizeClassHa != "Total"]
st <- X$fire_stat[ResampleStatus %in% c("Kept", "Reassigned") & Area >= X$resample$MinimumFireSize]
L <- suppressWarnings(rsyncrosim::ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
H <- as.data.table(rsyncrosim::datasheet(rsyncrosim::scenario(L, 1), "lucasFireC2F_HistoricalFire", lookupsAsFactors = FALSE, optional = TRUE))
obs <- H[as.integer(substr(Date, 1, 4)) %in% 1992:2020 & AreaHa >= X$resample$MinimumFireSize, AreaHa]
png(file.path(FD, "Fig7_size_calibration.png"), width = 2400, height = 1500, res = 200, bg = "white"); par(mfrow = c(2, 2), mar = c(5, 5, 3, 1), family = "sans")
barplot(t(as.matrix(T2[, .(ObsFiresPerYr, SimFiresPerYr)])), beside = TRUE, names.arg = T2$SizeClassHa, col = c("#5b5b5b", "#dd4b20"), las = 2, cex.names = .75,
        ylab = "Fires per year", main = "(a) Fire count by size class (ha)"); legend("topright", c("FPA-FOD 1992-2020", "lucasFireC2F"), fill = c("#5b5b5b", "#dd4b20"), bty = "n", cex = .8)
par(mar = c(5, 6.5, 3, 1)); bp_ <- barplot(t(as.matrix(T2[, .(ObsHaPerYr, SimHaPerYr)])), beside = TRUE, names.arg = T2$SizeClassHa, col = c("#5b5b5b", "#dd4b20"), las = 2, cex.names = .75,
        yaxt = "n", main = "(b) Area burned by size class (ha)"); axis(2, at = axTicks(2), labels = format(axTicks(2), big.mark = ",", scientific = FALSE), las = 1, cex.axis = .8)
mtext("Area burned (ha per year)", 2, line = 4.8, cex = .8); par(mar = c(5, 5, 3, 1))
ex <- function(x) { s <- sort(x, decreasing = TRUE); list(x = s, y = seq_along(s) / length(s)) }
eo <- ex(obs); es <- ex(st$Area)
plot(es$x, es$y, log = "xy", type = "l", col = "#dd4b20", lwd = 2, xlab = "Fire size (ha)", ylab = "Exceedance probability", main = "(c) Fire size exceedance", xaxt = "n", yaxt = "n")
axis(1, at = c(50, 100, 500, 1000, 5000, 1e4, 5e4, 1e5, 2.5e5), labels = c("50", "100", "500", "1,000", "5,000", "10,000", "50,000", "100,000", "250,000"), cex.axis = .7)
axis(2, at = 10^(-4:0), labels = c("0.0001", "0.001", "0.01", "0.1", "1"), las = 1, cex.axis = .75)
points(eo$x, eo$y, pch = 16, cex = .5, col = "#5b5b5b"); legend("bottomleft", c("FPA-FOD 1992-2020", "lucasFireC2F (2,500 seasons)"), col = c("#5b5b5b", "#dd4b20"), pch = c(16, NA), lty = c(NA, 1), lwd = 2, bty = "n", cex = .8)
sd <- X$spread_days; barplot(sd$RelativeFrequency, names.arg = sd$Days, col = "#ef8f2a", xlab = "Spread days", ylab = "Probability", main = sprintf("(d) Fitted spread-day distribution (mean %.2f d)", sum(sd$Days * sd$RelativeFrequency)))
dev.off(); cat("wrote Fig7_size_calibration.png\n")
