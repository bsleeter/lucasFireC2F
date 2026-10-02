# Tables for the methods paper from a lucasFireC2F result (dev/paper/result_<id>.rds) plus validation data.
# Writes dev/paper/tables/T*.csv and prints them.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra); library(sf); library(arrow) })
sf_use_s2(FALSE); id <- as.integer(commandArgs(TRUE)[1]); D <- "F:/lucasFireC2F/dev/paper"; TD <- file.path(D, "tables"); dir.create(TD, showWarnings = FALSE)
X <- readRDS(file.path(D, sprintf("result_%d.rds", id)))
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, "spatial/pyromes_150m_LF2020/pyrome_026")
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
# the fire record lives on the baseline scenario (a result only carries sheets its stages use)
H <- as.data.table(datasheet(rsyncrosim::scenario(L, 1), "lucasFireC2F_HistoricalFire", lookupsAsFactors = FALSE, optional = TRUE))
if (!nrow(H)) stop("no historical fires on scenario 1")
w <- function(x, f) { fwrite(x, file.path(TD, f)); cat("\n==", f, "\n"); print(x, digits = 3, nrows = 100) }
NS <- X$run_control$MaximumIteration; MINA <- X$resample$MinimumFireSize; Y <- 1992:2020; NY <- length(Y)
st <- X$fire_stat; kept <- st[ResampleStatus %in% c("Kept", "Reassigned")]
H[, yr := as.integer(substr(Date, 1, 4))]; obs <- H[yr %in% Y & AreaHa >= MINA]
se <- data.table(Name = c("Spring", "Summer", "Autumn", "Winter"), a = c(3, 6, 10, 12), b = c(5, 9, 11, 2))
som <- function(d) { m <- as.integer(substr(d, 6, 7)); out <- rep(NA_character_, length(m)); for (i in 1:4) { k <- if (se$a[i] <= se$b[i]) m >= se$a[i] & m <= se$b[i] else m >= se$a[i] | m <= se$b[i]; out[k] <- se$Name[i] }; out }
obs[, Season := som(Date)]

# ---- T1 configuration ------------------------------------------------------------------------------------------------
g <- rast(X$engine$FuelFileName[1]); zone <- read_sf(file.path(ZD, "zone.gpkg"))
T1 <- data.table(Item = c("Fire zone", "Zone area (km2)", "Grid", "Cell size", "Simulated fire seasons", "Large-fire threshold (FSim rule)",
  "Ignitions per season (Gamma mean / SD)", "Extra-ignition pool (share of season draws)", "Spread-day distribution (mean days)",
  "Burning hours per spread day", "Weather stream", "Fuel moisture", "Crown fire (growth)", "Effects wind", "Active crown spread",
  "Canopy base height multiplier (effects)", "Foliar moisture (growth / effects)", "Kept fires", "Burned cells (all fires)", "Run time (100 processes)"),
  Value = c("Pyrome 26, Northern Sierra Nevada", format(round(as.numeric(sum(st_area(zone))) / 1e6), big.mark = ","),
    sprintf("%d x %d cells (%s)", nrow(g), ncol(g), format(ncell(g), big.mark = ",")), sprintf("%g m", res(g)[1]), format(NS, big.mark = ","),
    sprintf("%.1f ha", MINA), sprintf("%.2f / %.2f", X$ignition_count$Mean, X$ignition_count$DistributionSD), sprintf("%.2f", X$resample$ProportionExtraIgnition),
    sprintf("%.2f", sum(X$spread_days$Days * X$spread_days$RelativeFrequency) / sum(X$spread_days$RelativeFrequency)), "2",
    "gridMET 1992-2020 zone-daily, over-wintered FWI, days with FWI >= 19", "Scott & Burgan D1L1-D4L4 from day-1 EMC and season",
    "C2F-W crown fire on (--cros)", "RAWS burning-period ratio draw per fire-hour", "Cruz et al. (2005)", sprintf("%.1f", X$effects_option$CanopyBaseHeightMultiplier),
    sprintf("%g%% / %g%%", X$growth_option$FoliarMoisture, X$effects_option$FoliarMoisture), format(nrow(kept), big.mark = ","),
    format(nrow(arrow::read_parquet(X$burn_table, col_select = "Cell")), big.mark = ","),
    { tt <- grep("Total time: [0-9]", X$log, value = TRUE); tt <- tt[!grepl("post processing|saving results", tt)]; s <- sum(sapply(regmatches(tt, regexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", tt)), function(z) sum(as.numeric(strsplit(z, ":")[[1]]) * c(60, 1, 1/60)))); sprintf("%.0f min", s) }))
w(T1, "T1_configuration.csv")

# ---- T2 sizes by class vs FPA-FOD 1992-2020 ------------------------------------------------------------------------------
sim <- kept[Area >= MINA, Area]; B <- c(MINA, 100, 405, 1000, 3000, 10000, 30000, Inf)
lab <- c("37-100", "100-405", "405-1,000", "1,000-3,000", "3,000-10,000", "10,000-30,000", "30,000+")
co <- cut(obs$AreaHa, B, right = FALSE, labels = lab); cs <- cut(sim, B, right = FALSE, labels = lab)
T2 <- rbindlist(lapply(lab, function(l) data.table(SizeClassHa = l, ObsFiresPerYr = sum(co == l) / NY, SimFiresPerYr = sum(cs == l) / NS,
  ObsHaPerYr = sum(obs$AreaHa[co == l]) / NY, SimHaPerYr = sum(sim[cs == l]) / NS)))
T2 <- rbind(T2, data.table(SizeClassHa = "Total", ObsFiresPerYr = nrow(obs) / NY, SimFiresPerYr = length(sim) / NS, ObsHaPerYr = sum(obs$AreaHa) / NY, SimHaPerYr = sum(sim) / NS))
T2[, `:=`(FireRatio = SimFiresPerYr / ObsFiresPerYr, AreaRatio = SimHaPerYr / ObsHaPerYr)]
w(T2, "T2_size_classes_vs_FPA.csv")
T2b <- data.table(Statistic = c("Median fire size (ha)", "90th percentile (ha)", "99th percentile (ha)", "Largest fire (ha)", "Mean fire size (ha)"),
  Observed = c(quantile(obs$AreaHa, c(.5, .9, .99), names = FALSE), max(obs$AreaHa), mean(obs$AreaHa)),
  Simulated = c(quantile(sim, c(.5, .9, .99), names = FALSE), max(sim), mean(sim)))
T2b[, Ratio := Simulated / Observed]; w(T2b, "T2b_size_quantiles.csv")

# ---- T3 burned area by season and cause, and by fuel group ---------------------------------------------------------------
T3 <- merge(obs[, .(ObsShareFires = .N / nrow(obs), ObsShareArea = sum(AreaHa) / sum(obs$AreaHa)), by = .(Season, Cause = fifelse(is.na(Cause), "Unknown", Cause))],
            kept[Area >= MINA, .(SimShareFires = .N / length(sim), SimShareArea = sum(Area) / sum(sim)), by = .(Season, Cause)], by = c("Season", "Cause"), all = TRUE)
setorder(T3, -ObsShareArea); w(T3, "T3_season_cause_shares.csv")
bp <- values(rast(X$bp), mat = FALSE); fv <- values(rast(X$engine$LandscapeFuelFileName[1]), mat = FALSE); ft <- X$fuel_type
inz <- !is.na(values(rasterize(vect(zone), g, field = 1), mat = FALSE)); grp <- ft$Description[match(fv, ft$ID)]
grp <- sub(" [A-Z]{2}[0-9]+$", "", grp); grp[fv %in% 91:99] <- "Non-burnable"
NZ <- sum(inz); T3b <- data.table(grp, bp, inz)[inz == TRUE, .(ShareOfZone = .N / NZ, MeanBPpctPerYr = 100 * mean(bp, na.rm = TRUE), ShareOfBurnedArea = sum(bp, na.rm = TRUE)), by = .(FuelGroup = grp)]
T3b[, ShareOfBurnedArea := ShareOfBurnedArea / sum(ShareOfBurnedArea)]; setorder(T3b, -ShareOfZone); w(T3b, "T3b_burn_probability_by_fuel_group.csv")
T3c <- data.table(Statistic = c("Mean annual burn probability, zone (%/yr)", "Median BP of burnable cells (%/yr)", "95th percentile BP (%/yr)",
  "Simulated area burned inside the zone (ha/yr)", "Observed MTBS area burned inside the zone, 1992-2020 (ha/yr)"),
  Value = c(100 * mean(bp[inz], na.rm = TRUE), 100 * median(bp[inz & !(fv %in% 91:99)], na.rm = TRUE), 100 * quantile(bp[inz & !(fv %in% 91:99)], .95, na.rm = TRUE),
            sum(bp[inz], na.rm = TRUE) * prod(res(g)) / 1e4, NA))
source(file.path(M, "src/00_config.R")); mt <- read_mtbs()
mt <- mt[mt$Incid_Type == "Wildfire" & !is.na(mt$Ig_Date), ]; mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y"))
for (yy in list(1992:2020, 2021:2024)) { m <- mt[mt$yr %in% yy, ]; m <- st_transform(st_make_valid(m), st_crs(zone)); a <- suppressWarnings(st_intersection(st_geometry(m), st_union(zone)))
  T3c <- rbind(T3c, data.table(Statistic = sprintf("MTBS wildfire area inside the zone, %d-%d (ha/yr, overlaps counted)", min(yy), max(yy)), Value = sum(as.numeric(st_area(a))) / 1e4 / length(yy))) }
T3c <- T3c[!is.na(Value)]; w(T3c, "T3c_burned_area_summary.csv")

# ---- T4 spatial validation (script 162) and T5 selection (script 169) ----------------------------------------------------
l162 <- readLines(file.path(D, "logs/162.txt")); r <- grep("^[0-9]{4}-[0-9]{4} \\(", l162, value = TRUE)
T4 <- rbindlist(lapply(r, function(s) { lab_ <- sub("\\).*$", ")", s); rest <- sub("^[^)]*\\)", "", s); p <- c(NA, NA, regmatches(rest, gregexpr("[0-9][0-9,.]*%?", rest))[[1]])
  data.table(Period = trimws(lab_), BurnedCells270m = as.numeric(gsub(",", "", p[3])), AUC_lucasFireC2F = as.numeric(p[5]), AUC_WRC = as.numeric(p[6]),
             HottestDecileCapture_lucasFireC2F = p[7], HottestDecileCapture_WRC = p[8]) }))
w(T4, "T4_spatial_validation_AUC.csv")
l169 <- readLines(file.path(D, "logs/169.txt")); blocks <- c(FUEL = "FUEL", TSF = "TSF", ELEV = "ELEV"); T5 <- list()
for (b in names(blocks)) { i <- grep(sprintf("^%s ", b), l169); j <- grep("weighted correlation", l169); j <- j[j > i][1]
  rows <- l169[(i + 2):(j - 1)]; T5[[b]] <- rbindlist(lapply(rows, function(s) { p <- strsplit(trimws(s), "\\s{2,}|\\s\\|\\s|\\s/\\s")[[1]]; p <- p[nzchar(p) & p != "|"]
    data.table(Dimension = b, Group = p[1], ShareOfGround = p[2], Selection_lucasFireC2F = as.numeric(p[3]), Selection_WRC = as.numeric(p[4]), Selection_Observed2021_24 = as.numeric(p[5]),
               AUC_within_lucasFireC2F = as.numeric(p[7]), AUC_within_WRC = as.numeric(p[8])) })) }
T5 <- rbindlist(T5); w(T5, "T5_selection_by_fuel_TSF_elevation.csv")

# ---- T6 fire effects by fuel group vs MTBS severity and WRC; T7 severity classes ------------------------------------------
be <- as.data.table(arrow::read_parquet(X$burn_effects, col_select = c("Cell", "FuelCode", "FlameLengthFt", "FireType")))
fm <- as.data.table(datasheet(rsyncrosim::project(L, "P26 Northern Sierra Nevada"), "lucasFireC2F_FuelModel", lookupsAsFactors = FALSE))
be[, grp := fm$FuelGroup[match(FuelCode, fm$ID)]]; be <- be[inz[Cell]]
T6 <- be[, .(ShareOfBurns = .N / nrow(be), CrownShare = mean(FireType >= 2), ActiveShare = mean(FireType == 3), FLEP4 = mean(FlameLengthFt > 4),
             FLEP8 = mean(FlameLengthFt > 8), MeanFlameLengthFt = mean(FlameLengthFt)), by = .(FuelGroup = grp)][order(-ShareOfBurns)]
tg <- fread(file.path(M, "outputs/c2fw/p026_severity_target_h2_fx.csv"))
map <- c(`TU timber-understory` = "Timber-Understory", `SH shrub` = "Shrub", `GS grass-shrub` = "Grass-Shrub", `GR grass` = "Grass", `TL5-9 heavier litter` = "Timber Litter", `TL1-3 compact litter` = "Timber Litter")
tg[, FuelGroup := map[grp]]; tg <- tg[, .(MTBS_High = sum(high * px) / sum(px), MTBS_ModerateOrHigh = sum((mod + high) * px) / sum(px)), by = FuelGroup]
Wd <- "F:/lucas-master/Data Sources/WRC/california_270m/"; wr <- function(f) values(resample(crop(rast(file.path(Wd, f)), ext(g) + 600), g, method = "near"), mat = FALSE)
w4 <- wr("wrc_flep4_270m.tif"); w8 <- wr("wrc_flep8_270m.tif"); wbp <- wr("wrc_bp_270m.tif"); if (max(w8, na.rm = TRUE) > 50) { w4 <- w4 / 1000; w8 <- w8 / 1000 }
gc_ <- fm$FuelGroup[match(fv, fm$ID)]
wg <- data.table(g = gc_, w4, w8, wbp, inz)[inz == TRUE & !is.na(g) & !is.na(w8) & !is.na(wbp) & wbp > 0, .(WRC_FLEP4 = sum(w4 * wbp) / sum(wbp), WRC_FLEP8 = sum(w8 * wbp) / sum(wbp)), by = .(FuelGroup = g)]
T6 <- merge(merge(T6, tg, by = "FuelGroup", all.x = TRUE), wg, by = "FuelGroup", all.x = TRUE); setorder(T6, -ShareOfBurns)
T6 <- rbind(T6, data.table(FuelGroup = "All", ShareOfBurns = 1, CrownShare = mean(be$FireType >= 2), ActiveShare = mean(be$FireType == 3), FLEP4 = mean(be$FlameLengthFt > 4),
  FLEP8 = mean(be$FlameLengthFt > 8), MeanFlameLengthFt = mean(be$FlameLengthFt)), fill = TRUE)
w(T6, "T6_fire_effects_vs_MTBS_and_WRC.csv")
T7 <- dcast(X$severity_fit, FuelGroup ~ SeverityClass, value.var = c("Target", "Model")); w(T7, "T7_severity_classes_vs_MTBS.csv")

# ---- T8 risk vs WRC (150 m, inside the zone) -----------------------------------------------------------------------------
rk <- X$risk; WR <- "F:/lucas-master/Data Sources/WRC/p026"
ext_fix <- function(r) { ext(r) <- ext(g); r }
T8 <- rbindlist(lapply(c("CRPS", "RPS"), function(k) { a <- values(ext_fix(rast(rk[Code == k]$FileName[1])), mat = FALSE)
  b <- values(ext_fix(rast(file.path(WR, sprintf("%s_150m.tif", tolower(k))))), mat = FALSE); i <- inz & !is.na(a) & !is.na(b)
  data.table(Metric = k, lucasFireC2F = mean(a[i]), WRC = mean(b[i]), Ratio = mean(a[i]) / mean(b[i]), Spearman = cor(a[i], b[i], method = "spearman")) }))
w(T8, "T8_risk_vs_WRC.csv")
cat("\nwrote", length(list.files(TD)), "tables to", TD, "\n")
