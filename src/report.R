# =============================================================================
# report.R - report figures and chart tables for a model run (sourced by stage R; no SyncroSim calls, so it can be tested
# on a result's files directly)
# =============================================================================
# Maps on a shaded-relief base of the zone's 150 m elevation, the zone outlined, the warm ramp of the methods paper
# (dev/paper/01_figures.R, generalised from P26 to any zone); charts of the C6 scores, the fitted spread days and the C7
# validation. Cells inside the zone that are non-burnable, or burned too few times for a conditional value, are grey.
# =============================================================================
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) })
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
# C6 size-class rows and C7 periods in a fixed order (a datasheet read returns rows in no particular order)
size_rows <- function(score, test) { x <- score[Test == test & Group != "TOTAL"]; x[order(as.numeric(gsub(",", "", sub("[-+].*", "", Group))))] }
period_rows <- function(v) if ("FirstYear" %in% names(v)) v[order(FirstYear, LastYear)] else v
RAMP <- colorRampPalette(c("#f5efe3", "#fbe3a2", "#f7c344", "#ef8f2a", "#dd4b20", "#a81d1a", "#5c0f14"))
INK <- "#20211f"; MUTED <- "#55534c"; OBS <- "#5b5b5b"; SIM <- "#dd4b20"; BENCH <- "#2f6f9f"

# I: zone (sf), el (150 m elevation), fuel (landscape fuel after recent-fire rules), fuel_type (ID, Description), cbd,
#    ignition (list of files), bp, effects / risk (named lists code -> file), wrc (file or NA), mtbs (sf, recent large fires),
#    score, validation, effects_summary, spread_days (data.tables), obs / sim (fire sizes, ha), label, seasons, vintage, mtbs_years
report_figures <- function(I, outdir) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE); out <- list()
  bb <- st_bbox(I$zone); e <- intersect(ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500), ext(I$el))
  el <- crop(I$el, e); E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1])
  inz <- !is.na(values(rasterize(vect(I$zone), el, field = 1), mat = FALSE))
  cv <- function(f) { if (is.null(f) || (is.character(f) && (is.na(f) || !nzchar(f)))) return(rep(NA_real_, ncell(el))); r <- if (inherits(f, "SpatRaster")) f else rast(f)
    if (!same.crs(r, el)) r <- project(r, el) else if (!compareGeom(r, el, stopOnError = FALSE, crs = FALSE)) r <- resample(crop(r, el + 600), el, method = "near")
    values(crop(r, el), mat = FALSE) }
  fu <- cv(I$fuel); nb <- fu %in% c(91, 92, 93, 98, 99) | is.na(fu)
  slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
  hs <- Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3
  hv <- values(hs, mat = FALSE); qq <- quantile(hv, c(0.02, 0.98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- 0.5
  it <- ifelse(inz, 0.58 + 0.64 * hn, 0.87 + 0.19 * hn)
  torgb <- function(v, brk, cols, paint_nb = FALSE) { cm <- col2rgb(cols) / 255
    cl <- if (is.null(brk)) v else cut(v, brk, include.lowest = TRUE, labels = FALSE)
    R <- rep(0.925, length(v)); G <- rep(0.920, length(v)); B <- rep(0.909, length(v))
    gg <- inz & (is.na(cl) | (nb & !paint_nb)); R[gg] <- 0.795; G[gg] <- 0.805; B[gg] <- 0.818
    k <- inz & !is.na(cl) & (paint_nb | !nb); R[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; B[k] <- cm[3, cl[k]]
    r <- rast(el, nlyr = 3); values(r) <- cbind(round(pmin(R * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(B * it, 1) * 255)); r }
  NOTE <- sprintf("%s; %s fire seasons; LANDFIRE %s fuels, 150 m. Grey inside the zone: non-burnable, or too few burns for a conditional value.",
                  I$label, format(I$seasons, big.mark = ","), I$vintage)
  # a figure of panels in one row; panel = list(v, brk, cols, lab, title, sub, paint_nb, outline)
  figure <- function(file, panels, title, note = NOTE, PW = 520) {
    PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- length(panels)
    W <- 2 * LM + n * PW + (n - 1) * GAP; H <- TOP + RT + PH + RB; f <- file.path(outdir, file)
    png(f, width = W, height = H, res = 150, bg = "white"); on.exit(dev.off())
    par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
    nt <- strwrap(note, floor((W - 2 * LM) / 9.5))
    text(LM, H - 40, title, adj = c(0, .5), cex = 1.25, font = 2, col = INK); for (k in seq_along(nt)) text(LM, H - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = 0.72, col = MUTED)
    for (i in seq_len(n)) { p <- panels[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB; cols <- p$cols %||% RAMP(length(p$brk) - 1)
      par(fig = c(x0 / W, (x0 + PW) / W, y0 / H, (y0 + PH) / H), new = TRUE, mar = c(0, 0, 0, 0))
      plotRGB(torgb(p$v, p$brk, cols, isTRUE(p$paint_nb)), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
      if (!is.null(p$outline) && length(p$outline)) plot(p$outline, add = TRUE, border = "#00aec4", lwd = 1.1, col = NA)
      plot(st_geometry(I$zone), add = TRUE, border = INK, lwd = 1.2, col = NA)
      par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, H), xaxs = "i", yaxs = "i")
      tt <- strwrap(sprintf("(%s) %s", letters[i], p$title), floor(PW / 10.5)); for (k in seq_along(tt)) text(x0, y0 + PH + 60 + (length(tt) - k) * 24, tt[k], adj = c(0, .5), font = 2, cex = 0.9, col = INK)
      text(x0, y0 + PH + 30, p$sub %||% "", adj = c(0, .5), cex = 0.62, col = MUTED)
      nL <- length(cols); lw <- PW / nL
      for (j in seq_len(nL)) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = cols[j], border = "white")
        text(x0 + (j - .5) * lw, y0 - 65, p$lab[j], cex = 0.55, col = INK) } }
    out[[length(out) + 1]] <<- data.table(Figure = title, FileName = f) }
  zmean <- function(v) mean(v[inz & !nb], na.rm = TRUE)

  # ---- 1 inputs ----------------------------------------------------------------------------------------------------------------
  G6 <- c("Grass", "Grass-Shrub", "Shrub", "Timber-Understory", "Timber Litter", "Slash-Blowdown")
  grp <- sub(" [A-Z]{2}[0-9]+$", "", I$fuel_type$Description[match(fu, I$fuel_type$ID)]); gi <- match(grp, G6)
  P1 <- list(list(v = gi, brk = NULL, cols = c("#e6d77a", "#d8b35a", "#b0784a", "#5f8f4e", "#2f6b3a", "#7a5a44"),
         lab = c("Grass", "Grass-shrub", "Shrub", "Timber-und.", "Timber litter", "Slash"), title = sprintf("Fuel group (LANDFIRE %s FBFM40)", I$vintage),
         sub = "after the recent-fire rules"))
  if (!is.null(I$cbd) && !is.na(I$cbd)) { cbd <- cv(I$cbd); cbd[is.na(cbd)] <- 0
    P1[[length(P1) + 1]] <- list(v = cbd, brk = c(0, .0001, .03, .06, .09, .12, .15, .2, .3, 10), lab = c("0", ".03", ".06", ".09", ".12", ".15", ".2", ".3", ">.3"),
         title = "Canopy bulk density (kg/m3)", sub = "forested-cell mean at 150 m") }
  if (length(I$ignition)) {                                       # a merged result has no ignition maps of its own: panel left out
    ign <- Reduce(`+`, lapply(I$ignition, function(f) { v <- cv(f); v[is.na(v)] <- 0; if (sum(v)) v / sum(v) else v }))
    q_ig <- quantile(ign[ign > 0 & inz], c(0, .2, .4, .6, .8, .9, .95, .99), na.rm = TRUE)
    P1[[length(P1) + 1]] <- list(v = ign, brk = unique(c(0, q_ig[-1], 1)), lab = c("<20", "20-40", "40-60", "60-80", "80-90", "90-95", "95-99", ">99"),
         title = "Ignition probability (percentile)", sub = "human + lightning surfaces, each normalised") }
  figure("Fig1_inputs.png", P1, "Model inputs")

  # ---- 2 burn probability (vs WRC when there is a benchmark) --------------------------------------------------------------------
  bpv <- cv(I$bp); bpv[is.na(bpv)] <- 0
  bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3")
  va <- if (is.null(I$validation)) NULL else period_rows(I$validation); aucs <- if (!is.null(va) && nrow(va)) va[grepl("after", Period)] else NULL
  sub_auc <- function(k) if (!is.null(aucs) && nrow(aucs) && !is.na(aucs[[k]][1])) sprintf("; AUC vs %s fires %.3f", I$mtbs_years, aucs[[k]][1]) else ""
  P <- list(list(v = bpv, brk = bpb, lab = bpl, title = "Annual burn probability, lucasFireC2F (%/yr)", outline = I$mtbs,
                 sub = sprintf("zone mean %.3f%%/yr%s", 100 * zmean(bpv), sub_auc("AUC"))))
  if (!is.na(I$wrc %||% NA)) { wbp <- cv(I$wrc); if (max(wbp, na.rm = TRUE) > 1) wbp <- wbp / 1e4
    P[[2]] <- list(v = wbp, brk = bpb, lab = bpl, title = "Annual burn probability, WRC (FSim) (%/yr)", outline = I$mtbs,
                   sub = sprintf("zone mean %.3f%%/yr%s", 100 * zmean(wbp), sub_auc("AUCBenchmark"))) }
  figure("Fig2_burn_probability.png", P, "Burn probability", paste(NOTE, sprintf("Outlines: MTBS wildfires of 405 ha or more, %s.", I$mtbs_years)))

  # ---- 3 intensity, 4 crown fire, 5 hazard and risk -------------------------------------------------------------------------------
  ef <- function(k) cv(I$effects[[k]]); rk <- function(k) cv(I$risk[[k]])
  if (length(I$effects)) {
    pb <-c(0, .05, .1, .15, .2, .3, .4, .5, .6, .8, 1.0001); pl <- c("0", ".05", ".1", ".15", ".2", ".3", ".4", ".5", ".6", ".8+")
  figure("Fig3_fire_intensity.png", list(
    list(v = ef("CFL"), brk = c(0, 2, 4, 6, 8, 12, 16, 20, 30, 40, 1000), lab = c("<2", "2-4", "4-6", "6-8", "8-12", "12-16", "16-20", "20-30", "30-40", ">40"),
         title = "Conditional flame length (ft)", sub = "mean flame length when the cell burns"),
    list(v = ef("FLEP4"), brk = pb, lab = pl, title = "P(flame length > 4 ft | burns)", sub = "FLEP4"),
    list(v = ef("FLEP8"), brk = pb, lab = pl, title = "P(flame length > 8 ft | burns)", sub = "FLEP8")), "Fire intensity")
  cbp <- ef("CROWNBP"); cbp[is.na(cbp)] <- 0; tb <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1.0001)
  figure("Fig4_crown_fire.png", list(
    list(v = ef("PCROWN"), brk = tb, lab = sprintf("%.1f", seq(0, .9, .1)), title = "P(crown fire | burns)", sub = "passive or active"),
    list(v = ef("PACTIVE"), brk = tb, lab = sprintf("%.1f", seq(0, .9, .1)), title = "P(active crown fire | burns)", sub = "active crown fire needs canopy cover >= 40%"),
    list(v = cbp, brk = c(0, .0005, .001, .002, .003, .005, .0075, .01, .015, .02, 1), lab = c("<.05", ".05", ".1", ".2", ".3", ".5", ".75", "1", "1.5", ">2"),
         title = "Crown-fire probability (%/yr)", sub = "burn probability x P(crown | burns)")), "Crown fire") }
  if (length(I$risk)) figure("Fig5_hazard_risk.png", list(
    list(v = rk("HIBP"), brk = c(0, .0001, .0002, .0005, .001, .002, .003, .005, .0075, .01, 1), lab = c("<.01", ".01", ".02", ".05", ".1", ".2", ".3", ".5", ".75", ">1"),
         title = "High-intensity burn probability (%/yr)", sub = "burn probability x FLEP8"),
    list(v = rk("CRPS"), brk = c(0, 20, 30, 40, 50, 60, 70, 80, 90, 95, 101), lab = c("<20", "20", "30", "40", "50", "60", "70", "80", "90", "95+"), paint_nb = TRUE,
         title = "Conditional risk to potential structures", sub = "% loss if a structure here burns"),
    list(v = rk("RPS"), brk = c(0, .05, .1, .2, .35, .5, .75, 1, 1.25, 1.5, 1e9), lab = c("<.05", ".05", ".1", ".2", ".35", ".5", ".75", "1", "1.25", ">1.5"), paint_nb = TRUE,
         title = "Risk to potential structures (%/yr)", sub = "burn probability x conditional risk; no housing layer"),
    list(v = rk("RPSCLASS"), brk = c(.5, 1.5, 2.5, 3.5, 4.5, 5.5), cols = c("#fbf3d5", "#f7c344", "#ef8f2a", "#c93a1d", "#5c0f14"), paint_nb = TRUE,
         lab = c("<40th", "40-70", "70-90", "90-95", ">95th"), title = "Relative risk class", sub = "percentile of annual risk over the zone")),
    "Hazard and risk to potential structures", PW = 430)

  # ---- 6 fire sizes and calibration (charts) ---------------------------------------------------------------------------------------
  sc <- I$score; f6 <- file.path(outdir, "Fig6_fire_sizes.png")
  if ((!is.null(sc) && nrow(sc)) || length(I$obs) || (!is.null(I$spread_days) && nrow(I$spread_days))) {   # a merged result has none of these: figure left out
  png(f6, width = 2400, height = 1500, res = 200, bg = "white"); par(mfrow = c(2, 2), mar = c(5, 5, 3, 1), family = "sans")
  if (!is.null(sc) && nrow(sc)) {
    fc <- size_rows(sc, "Fires per year"); ac <- size_rows(sc, "Area per year (ha)")
    barplot(rbind(fc$Observed, fc$Simulated), beside = TRUE, names.arg = fc$Group, col = c(OBS, SIM), las = 2, cex.names = .75, border = NA,
            ylab = "Fires per year", main = "(a) Fires by size class (ha)")
    legend("topright", c("Fire record", "lucasFireC2F"), fill = c(OBS, SIM), border = NA, bty = "n", cex = .8)
    par(mar = c(5, 6.5, 3, 1))
    barplot(rbind(ac$Observed, ac$Simulated), beside = TRUE, names.arg = ac$Group, col = c(OBS, SIM), las = 2, cex.names = .75, border = NA, yaxt = "n",
            main = "(b) Area burned by size class (ha)")
    axis(2, at = axTicks(2), labels = format(axTicks(2), big.mark = ",", scientific = FALSE), las = 1, cex.axis = .8); mtext("Area burned (ha per year)", 2, line = 4.8, cex = .8)
    par(mar = c(5, 5, 3, 1))
  } else { plot.new(); text(.5, .5, "no C6 scores in this result", col = MUTED); plot.new() }
  if (length(I$obs) && length(I$sim)) {
    ex <- function(x) { s <- sort(x, decreasing = TRUE); list(x = s, y = seq_along(s) / length(s)) }; eo <- ex(I$obs); es <- ex(I$sim)
    plot(es$x, es$y, log = "xy", type = "l", col = SIM, lwd = 2, xlab = "Fire size (ha)", ylab = "Exceedance probability", main = "(c) Fire size exceedance",
         xlim = range(c(eo$x, es$x)), ylim = range(c(eo$y, es$y)), xaxt = "n", yaxt = "n")
    at <- c(30, 100, 300, 1000, 3000, 1e4, 3e4, 1e5, 3e5); axis(1, at = at, labels = format(at, big.mark = ",", scientific = FALSE, trim = TRUE), cex.axis = .7)
    axis(2, at = 10^(-5:0), labels = c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1"), las = 1, cex.axis = .7)
    points(eo$x, eo$y, pch = 16, cex = .6, col = OBS)
    legend("bottomleft", c(sprintf("Fire record (%d fires)", length(I$obs)), sprintf("lucasFireC2F (%s fires)", format(length(I$sim), big.mark = ","))),
           col = c(OBS, SIM), pch = c(16, NA), lty = c(NA, 1), lwd = 2, bty = "n", cex = .8)
  } else { plot.new(); text(.5, .5, "no fire sizes", col = MUTED) }
  sd <- I$spread_days
  if (!is.null(sd) && nrow(sd)) { s1 <- sd[, .(p = sum(RelativeFrequency)), by = Days][order(Days)]; s1[, p := p / sum(p)]
    barplot(s1$p, names.arg = s1$Days, col = "#ef8f2a", border = NA, xlab = "Spread days", ylab = "Probability",
            main = sprintf("(d) Spread-day distribution (mean %.2f days)", sum(s1$Days * s1$p)))
  } else { plot.new(); text(.5, .5, "no spread-day distribution", col = MUTED) }
  dev.off(); out[[length(out) + 1]] <- data.table(Figure = "Fire sizes and calibration", FileName = f6) }

  # ---- 7 validation and effects by fuel group (charts) ------------------------------------------------------------------------------
  f7 <- file.path(outdir, "Fig7_validation.png")
  png(f7, width = 2400, height = 1500, res = 200, bg = "white"); par(mfrow = c(2, 2), mar = c(7, 5, 3, 1), family = "sans")
  if (!is.null(va) && nrow(va)) { hasB <- any(!is.na(va$AUCBenchmark)); nm <- sub(" \\(.*", "", va$Period)
    m <- if (hasB) rbind(va$AUC, va$AUCBenchmark) else rbind(va$AUC); cl <- if (hasB) c(SIM, BENCH) else SIM
    b <- barplot(m, beside = TRUE, names.arg = nm, col = cl, border = NA, ylim = c(0, 1), ylab = "AUC", main = "(a) Spatial skill: AUC by period")
    abline(h = .5, lty = 2, col = MUTED); text(b, m + .03, sprintf("%.2f", m), cex = .7)
    legend("topright", c("lucasFireC2F", if (hasB) "WRC (FSim)"), fill = cl, border = NA, bty = "n", cex = .8)
    m <- if (hasB) rbind(va$HottestDecile, va$HottestDecileBenchmark) else rbind(va$HottestDecile)
    b <- barplot(100 * m, beside = TRUE, names.arg = nm, col = cl, border = NA, ylim = c(0, max(100 * m, 20, na.rm = TRUE) * 1.15),
                 ylab = "% of burned ground", main = "(b) Burned ground in the map's hottest 10%")
    abline(h = 10, lty = 2, col = MUTED); text(b, 100 * m + 2, sprintf("%.0f%%", 100 * m), cex = .7)
  } else { plot.new(); text(.5, .5, "no C7 validation in this result", col = MUTED); plot.new() }
  es <- I$effects_summary
  if (!is.null(es) && nrow(es)) { es <- es[FuelGroup != "All"][order(-BurnShare)]; par(mar = c(9, 5, 3, 1))
    barplot(100 * es$BurnShare, names.arg = es$FuelGroup, col = "#b0784a", border = NA, las = 2, cex.names = .75, ylab = "% of burns", main = "(c) Where the fire burns, by fuel group")
    m <- rbind(es$FLEP4, es$FLEP8, es$CrownShare)
    barplot(m, beside = TRUE, names.arg = es$FuelGroup, col = c("#f7c344", "#dd4b20", "#5c0f14"), border = NA, las = 2, cex.names = .75, ylim = c(0, max(m, .1) * 1.45),
            ylab = "Share of burns", main = "(d) Intensity by fuel group")
    legend("top", horiz = TRUE, c("Flame length > 4 ft", "Flame length > 8 ft", "Crown fire"), fill = c("#f7c344", "#dd4b20", "#5c0f14"), border = NA, bty = "n", cex = .8)
  } else { plot.new(); text(.5, .5, "no fire effects summary", col = MUTED); plot.new() }
  dev.off(); out[[length(out) + 1]] <- data.table(Figure = "Validation and fire effects", FileName = f7)
  rbindlist(out)
}

# chart tables for Studio's Charts tab: Studio charts a value against Timestep, so Timestep carries the x position
report_charts <- function(score, spread_days, validation) {
  sz <- NULL; if (!is.null(score) && nrow(score)) {
    fc <- size_rows(score, "Fires per year"); ac <- size_rows(score, "Area per year (ha)")
    sz <- data.table(Iteration = 1L, Timestep = seq_len(nrow(fc)), FiresObserved = fc$Observed, FiresSimulated = fc$Simulated,
                     AreaObserved = ac$Observed[match(fc$Group, ac$Group)], AreaSimulated = ac$Simulated[match(fc$Group, ac$Group)]) }
  sd <- NULL; if (!is.null(spread_days) && nrow(spread_days)) {
    s1 <- spread_days[, .(p = sum(RelativeFrequency)), by = Days][order(Days)]; s1[, p := p / sum(p)]
    sd <- data.table(Iteration = 1L, Timestep = 1:max(s1$Days), Probability = 0)[s1, on = c(Timestep = "Days"), Probability := i.p] }
  if (!is.null(validation)) validation <- period_rows(validation)
  vd <- NULL; if (!is.null(validation) && nrow(validation))
    vd <- data.table(Iteration = 1L, Timestep = seq_len(nrow(validation)), AUC = validation$AUC, AUCBenchmark = validation$AUCBenchmark,
                     HottestDecile = validation$HottestDecile, HottestDecileBenchmark = validation$HottestDecileBenchmark)
  for (x in list(sz, sd, vd)) if (!is.null(x)) for (k in names(x)) if (is.numeric(x[[k]])) set(x, which(!is.finite(x[[k]])), k, NA)
  list(size = sz, spread = sd, validation = vd)
}
