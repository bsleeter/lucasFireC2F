# Two calibrations of one zone side by side (user guide 7.4): burn probability of both runs beside WRC, and fire sizes /
# spread days of both runs against their own records. Reads the Demo library read-only (no run may be active).
# Usage: Rscript compare_runs.R   (P18: results 14 = 1992-2020 calibration, 20 = 2011-2020; Calibrate results 4 and 17)
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(progress = 0)
source("F:/lucasFireC2F/src/report.R")                                  # RAMP, size_rows, INK/MUTED
LIB <- "F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F-Demo.ssim"; D <- paste0(LIB, ".data"); OUT <- "F:/lucasFireC2F/docs/user-guide/figures/compare"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
R <- list(list(run = 14, cal = 4, y = 1992:2020, lab = "1992-2020 calibration", rec = "#9aa5b1", mod = "#2f6f9f"),
          list(run = 20, cal = 17, y = 2011:2020, lab = "2011-2020 calibration", rec = "#efb592", mod = "#dd4b20"))
con <- DBI::dbConnect(RSQLite::SQLite(), LIB, flags = RSQLite::SQLITE_RO); q <- function(s) as.data.table(DBI::dbGetQuery(con, s))
H <- q("SELECT Date, AreaHa FROM lucasFireC2F_HistoricalFire WHERE ScenarioId = 14")[, yr := as.integer(substr(Date, 1, 4))]
for (i in seq_along(R)) { r <- R[[i]]
  R[[i]]$score <- q(sprintf("SELECT Test, \"Group\", Observed, Simulated FROM lucasFireC2F_OutputScore WHERE ScenarioId = %d", r$run))
  thr <- max(mean(H[yr %in% r$y, AreaHa]), 73 * 0.404686); R[[i]]$thr <- thr        # C3's FSim rule
  R[[i]]$obs <- H[yr %in% r$y & AreaHa >= thr, AreaHa]
  R[[i]]$sim <- q(sprintf("SELECT Area FROM lucasFireC2F_OutputFireStatistic WHERE ScenarioId = %d AND ResampleStatus IN ('Kept','Reassigned') AND Area >= %f", r$run, thr))$Area
  R[[i]]$sd <- q(sprintf("SELECT Days, RelativeFrequency FROM lucasFireC2F_SpreadDays WHERE ScenarioId = %d", r$cal))
  R[[i]]$bp <- file.path(D, sprintf("Scenario-%d/lucasFireC2F_OutputBurnProbability", r$run), q(sprintf("SELECT FileName FROM lucasFireC2F_OutputBurnProbability WHERE ScenarioId = %d", r$run))$FileName[1]) }
DBI::dbDisconnect(con)
for (r in R) cat(sprintf("%s: threshold %.2f ha, %d recorded, %s simulated fires, %s\n", r$lab, r$thr, length(r$obs), format(length(r$sim), big.mark = ","), basename(r$bp)))

# ---- maps: both runs and WRC on one shaded-relief frame (the machinery of report.R's figure()) -------------------------------------
S6 <- file.path(D, "Scenario-14")
zone <- st_sf(geometry = st_union(st_make_valid(read_sf(file.path(D, "Scenario-2/lucasFireC2F_OutputZoneBoundary/zone_boundary.gpkg")))))
el0 <- rast(file.path(S6, "lucasFireC2F_LandscapeRasters/elevation.tif")); zone <- st_transform(zone, crs(el0)); bb <- st_bbox(zone)
el <- crop(el0, intersect(ext(bb$xmin - 4500, bb$xmax + 4500, bb$ymin - 4500, bb$ymax + 4500), ext(el0))); E <- ext(el); AR <- (E[4] - E[3]) / (E[2] - E[1])
inz <- !is.na(values(rasterize(vect(zone), el, field = 1), mat = FALSE))
cv <- function(f) { r <- rast(f); if (!same.crs(r, el)) r <- project(r, el) else if (!compareGeom(r, el, stopOnError = FALSE, crs = FALSE)) r <- resample(crop(r, el + 600), el, method = "near"); values(crop(r, el), mat = FALSE) }
fu <- cv(list.files(file.path(S6, "lucasFireC2F_OutputEngineLandscape"), "^landscape_fuel", full.names = TRUE)[1]); nb <- fu %in% c(91, 92, 93, 98, 99) | is.na(fu)
slp <- terrain(el, "slope", unit = "radians"); asp <- terrain(el, "aspect", unit = "radians")
hv <- values(Reduce(`+`, lapply(c(300, 337, 15), function(d) shade(slp, asp, angle = 42, direction = d))) / 3, mat = FALSE)
qq <- quantile(hv, c(.02, .98), na.rm = TRUE); hn <- pmin(pmax((hv - qq[1]) / (qq[2] - qq[1]), 0), 1); hn[is.na(hn)] <- .5; it <- ifelse(inz, .58 + .64 * hn, .87 + .19 * hn)
torgb <- function(v, brk, cols) { cm <- col2rgb(cols) / 255; cl <- cut(v, brk, include.lowest = TRUE, labels = FALSE)
  Rr <- rep(.925, length(v)); G <- rep(.920, length(v)); B <- rep(.909, length(v)); gg <- inz & (is.na(cl) | nb); Rr[gg] <- .795; G[gg] <- .805; B[gg] <- .818
  k <- inz & !is.na(cl) & !nb; Rr[k] <- cm[1, cl[k]]; G[k] <- cm[2, cl[k]]; B[k] <- cm[3, cl[k]]
  o <- rast(el, nlyr = 3); values(o) <- cbind(round(pmin(Rr * it, 1) * 255), round(pmin(G * it, 1) * 255), round(pmin(B * it, 1) * 255)); o }
mt <- read_sf("F:/lucas-master/Data Sources/MTBS/Fire Perimeter/mtbs_perimeter_data/mtbs_perims_DD.shp")
canon <- c(incid_type = "Incid_Type", burnbndac = "BurnBndAc", ig_date = "Ig_Date"); hit <- match(tolower(names(mt)), names(canon)); names(mt)[!is.na(hit)] <- unname(canon[hit[!is.na(hit)]])
yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y")); mt <- mt[mt$Incid_Type == "Wildfire" & yr %in% 2021:2024 & mt$BurnBndAc * 0.404686 >= 405, ]
mt <- suppressWarnings(st_intersection(st_geometry(st_make_valid(st_transform(mt, crs(el)))), st_geometry(zone)))
bpb <- c(0, .001, .002, .0035, .005, .007, .01, .015, .02, .03, 1); bpl <- c("<0.1", "0.1", "0.2", "0.35", "0.5", "0.7", "1", "1.5", "2", ">3"); cols <- RAMP(10)
zm <- function(v) 100 * mean(v[inz & !nb], na.rm = TRUE)
V <- list(cv(R[[1]]$bp), cv(R[[2]]$bp), { w <- cv("F:/lucas-master/Data Sources/WRC/california_270m/wrc_bp_270m.tif"); if (max(w, na.rm = TRUE) > 1) w / 1e4 else w })
V <- lapply(V, function(v) { v[is.na(v)] <- 0; v })
P <- list(list(v = V[[1]], t = "lucasFireC2F, calibrated to 1992-2020", s = sprintf("zone mean %.2f%%/yr; 98 recorded fires >= 29.5 ha", zm(V[[1]]))),
          list(v = V[[2]], t = "lucasFireC2F, calibrated to 2011-2020", s = sprintf("zone mean %.2f%%/yr; 26 recorded fires >= 34.8 ha", zm(V[[2]]))),
          list(v = V[[3]], t = "WRC (FSim)", s = sprintf("zone mean %.2f%%/yr", zm(V[[3]]))))
PW <- 520; PH <- round(PW * AR); TOP <- 170; RT <- 90; RB <- 120; GAP <- 40; LM <- 40; n <- 3; W <- 2 * LM + n * PW + (n - 1) * GAP; Hh <- TOP + RT + PH + RB
png(file.path(OUT, "Compare_burn_probability.png"), width = W, height = Hh, res = 150, bg = "white")
par(mar = c(0, 0, 0, 0)); plot.new(); plot.window(c(0, W), c(0, Hh), xaxs = "i", yaxs = "i")
text(LM, Hh - 40, "Burn probability: two calibrations of P18 and WRC", adj = c(0, .5), cex = 1.25, font = 2, col = INK)
nt <- strwrap("P18 Southern Cascades; 10,000 fire seasons each; LANDFIRE 2020 fuels, 150 m. The two runs differ only in the fire record they are calibrated to. Grey inside the zone: non-burnable. Outlines: MTBS wildfires of 405 ha or more, 2021-2024.", floor((W - 2 * LM) / 9.5))
for (k in seq_along(nt)) text(LM, Hh - 80 - (k - 1) * 22, nt[k], adj = c(0, .5), cex = .72, col = MUTED)
for (i in 1:n) { p <- P[[i]]; x0 <- LM + (i - 1) * (PW + GAP); y0 <- RB
  par(fig = c(x0 / W, (x0 + PW) / W, y0 / Hh, (y0 + PH) / Hh), new = TRUE, mar = c(0, 0, 0, 0))
  plotRGB(torgb(p$v, bpb, cols), axes = FALSE, mar = NA, maxcell = Inf, smooth = TRUE)
  plot(mt, add = TRUE, border = "#00aec4", lwd = 1.1, col = NA); plot(st_geometry(zone), add = TRUE, border = INK, lwd = 1.2, col = NA)
  par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0), xpd = NA); plot.new(); plot.window(c(0, W), c(0, Hh), xaxs = "i", yaxs = "i")
  text(x0, y0 + PH + 60, sprintf("(%s) %s", letters[i], p$t), adj = c(0, .5), font = 2, cex = .9, col = INK); text(x0, y0 + PH + 30, p$s, adj = c(0, .5), cex = .62, col = MUTED)
  lw <- PW / 10; for (j in 1:10) { rect(x0 + (j - 1) * lw, y0 - 45, x0 + j * lw, y0 - 25, col = cols[j], border = "white"); text(x0 + (j - .5) * lw, y0 - 65, bpl[j], cex = .55, col = INK) } }
text(LM, 30, "Annual burn probability (%/yr)", adj = c(0, .5), cex = .7, col = MUTED)
dev.off(); cat("wrote Compare_burn_probability.png\n")

# ---- 4-panel chart: both runs against their own records ------------------------------------------------------------------------------
LAB <- c("30-100", "100-405", "405-1,000", "1,000-3,000", "3,000-10,000", "10,000-30,000", "30,000+")
tab <- function(r, test) { x <- size_rows(r$score, test); rbind(x$Observed, x$Simulated) }
F4 <- rbind(tab(R[[1]], "Fires per year"), tab(R[[2]], "Fires per year")); A4 <- rbind(tab(R[[1]], "Area per year (ha)"), tab(R[[2]], "Area per year (ha)"))
C4 <- c(R[[1]]$rec, R[[1]]$mod, R[[2]]$rec, R[[2]]$mod)
L4 <- c("Record 1992-2020", "Model, 1992-2020 calibration", "Record 2011-2020", "Model, 2011-2020 calibration")
png(file.path(OUT, "Compare_fire_sizes.png"), width = 2400, height = 1600, res = 200, bg = "white"); par(mfrow = c(2, 2), mar = c(6, 5, 3, 1), family = "sans")
barplot(F4, beside = TRUE, names.arg = LAB, col = C4, border = NA, las = 2, cex.names = .72, ylab = "Fires per year", main = "(a) Fires by size class (ha)")
legend("topright", L4, fill = C4, border = NA, bty = "n", cex = .72)
par(mar = c(6, 6.5, 3, 1))
barplot(A4, beside = TRUE, names.arg = LAB, col = C4, border = NA, las = 2, cex.names = .72, yaxt = "n", main = "(b) Area burned by size class (ha)")
axis(2, at = axTicks(2), labels = format(axTicks(2), big.mark = ",", scientific = FALSE), las = 1, cex.axis = .8); mtext("Area burned (ha per year)", 2, line = 4.8, cex = .75)
tot <- sapply(R, function(r) r$score[Test == "Area per year (ha)" & Group == "TOTAL", c(Observed, Simulated)])
legend("topleft", sprintf("Total: record %s / model %s ha/yr (%s)", format(round(tot[1, ]), big.mark = ","), format(round(tot[2, ]), big.mark = ","), c("1992-2020", "2011-2020")),
       bty = "n", cex = .7, text.col = c(R[[1]]$mod, R[[2]]$mod))
par(mar = c(5, 5, 3, 1))
ex <- function(x) { s <- sort(x, decreasing = TRUE); list(x = s, y = seq_along(s) / length(s)) }
E <- lapply(R, function(r) list(o = ex(r$obs), s = ex(r$sim)))
xr <- range(unlist(lapply(E, function(e) c(e$o$x, e$s$x)))); yr2 <- range(unlist(lapply(E, function(e) c(e$o$y, e$s$y))))
plot(NA, log = "xy", xlim = xr, ylim = yr2, xlab = "Fire size (ha)", ylab = "Exceedance probability", main = "(c) Fire size exceedance", xaxt = "n", yaxt = "n")
at <- c(30, 100, 300, 1000, 3000, 1e4, 3e4, 1e5, 3e5); axis(1, at = at, labels = format(at, big.mark = ",", scientific = FALSE, trim = TRUE), cex.axis = .7)
axis(2, at = 10^(-5:0), labels = c("0.00001", "0.0001", "0.001", "0.01", "0.1", "1"), las = 1, cex.axis = .7)
for (i in 1:2) { lines(E[[i]]$s$x, E[[i]]$s$y, col = R[[i]]$mod, lwd = 2); points(E[[i]]$o$x, E[[i]]$o$y, pch = 16, cex = .7, col = R[[i]]$rec) }
legend("bottomleft", c(sprintf("Record 1992-2020 (%d fires)", length(R[[1]]$obs)), "Model, 1992-2020 calibration", sprintf("Record 2011-2020 (%d fires)", length(R[[2]]$obs)), "Model, 2011-2020 calibration"),
       col = C4, pch = c(16, NA, 16, NA), lty = c(NA, 1, NA, 1), lwd = 2, bty = "n", cex = .72)
sd <- lapply(R, function(r) { s <- r$sd[, .(p = sum(RelativeFrequency)), by = Days]; s[, p := p / sum(p)] })
dd <- 1:max(unlist(lapply(sd, `[[`, "Days"))); M <- sapply(sd, function(s) { v <- rep(0, length(dd)); v[s$Days] <- s$p; v })
barplot(t(M), beside = TRUE, names.arg = dd, col = c(R[[1]]$mod, R[[2]]$mod), border = NA, xlab = "Spread days", ylab = "Probability",
        main = sprintf("(d) Spread-day distribution (mean %.2f and %.2f days)", sum(sd[[1]]$Days * sd[[1]]$p), sum(sd[[2]]$Days * sd[[2]]$p)))
legend("topright", c("1992-2020 calibration", "2011-2020 calibration"), fill = c(R[[1]]$mod, R[[2]]$mod), border = NA, bty = "n", cex = .75)
dev.off(); cat("wrote Compare_fire_sizes.png\n")
