# Why does a zone result miss the fire that burned after the fuel vintage? Read-only diagnostic for any lucasFireC2F library.
#   A  each MTBS wildfire >= MINHA of FIRST-LAST (default 2021-2024) inside the zone: area in the zone, the mean percentile of its
#      cells in the model's and the benchmark's burn probability (zone burnable cells = 0-100), and the fuel group / elevation it burned
#   B  placement vs spread, 1992-2020 (the placement_vs_spread.R method): per stratum, model vs observed share of large-fire origins
#      (FPA-FOD >= MINHA) and of burned ground (MTBS >= MINHA); burned = placement x spread. Strata: fuel group (fuels after the
#      recent-fire rules), 500 m elevation band, slope class
#   C  a map: model BP | benchmark BP, FIRST-LAST fires outlined
# Usage: Rscript diagnose_postvintage.R "<library.ssim>" <result id> <prep result id> <out dir> [min ha = 405] [first = 2021] [last = 2024]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(arrow); library(dplyr); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); LIBF <- a[1]; RID <- as.integer(a[2]); PID <- as.integer(a[3]); OUT <- a[4]; dir.create(OUT, FALSE, TRUE)
MINHA <- if (length(a) > 4) as.numeric(a[5]) else 405; Y1 <- if (length(a) > 5) as.integer(a[6]) else 2021L; Y2 <- if (length(a) > 6) as.integer(a[7]) else 2024L
L <- suppressWarnings(ssimLibrary(LIBF)); R <- rsyncrosim::scenario(L, RID); P <- rsyncrosim::scenario(L, PID); PR <- rsyncrosim::project(L, 1)
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
dsrc <- as.data.table(datasheet(PR, "lucasFireC2F_DataSource", optional = TRUE))

# ---- grids: model BP, benchmark BP on the model grid, fuels after the rules, elevation, slope, zone -------------------------
bp <- rast(g(R, "OutputBurnProbability")$FileName[1]); eng <- g(R, "OutputEngineLandscape")
fu <- rast(eng$LandscapeFuelFileName[1]); el <- rast(eng$ElevationFileName[1])
zone <- st_union(st_make_valid(read_sf(g(P, "OutputZoneBoundary")$FileName[1]))); zone <- st_transform(st_as_sf(zone), crs(bp))
inz <- !is.na(values(rasterize(vect(zone), bp), mat = FALSE))
fm <- as.data.table(datasheet(PR, "lucasFireC2F_FuelModel", lookupsAsFactors = FALSE))
fv <- values(fu, mat = FALSE); grp <- fm$FuelGroup[match(fv, fm$ID)]; grp[is.na(grp)] <- "Non-burnable"
ok <- inz & grp != "Non-burnable"
wrc <- if (!is.na(dsrc$WrcBurnProbabilityFile[1])) terra::project(rast(dsrc$WrcBurnProbabilityFile[1]), bp, method = "near") else NULL
bp <- bp * 100; if (!is.null(wrc)) wrc <- wrc * 100   # both maps hold annual probability as a fraction; work in %/yr
bv <- values(bp, mat = FALSE); bv[is.na(bv)] <- 0; wv <- if (!is.null(wrc)) values(wrc, mat = FALSE) else rep(NA_real_, length(bv))
ev <- values(el, mat = FALSE); sl <- values(terrain(el, "slope", unit = "degrees"), mat = FALSE)
pct <- function(v) { e <- ecdf(v[ok & !is.na(v)]); r <- rep(NA_real_, length(v)); r[ok] <- 100 * e(v[ok]); r }
bpp <- pct(bv); wpp <- pct(wv)
ebr <- seq(0, 4500, 500); S <- data.table(cell = seq_along(bv), ok, fuel = grp, elev = cut(ev, c(-Inf, ebr[-1], Inf), dig.lab = 5),
                                          slope = cut(sl, c(-Inf, 10, 20, 30, Inf), c("<10 deg", "10-20", "20-30", ">30")))
cat(sprintf("zone: %s burnable cells; model BP mean %.3f%%/yr, benchmark %.3f%%/yr (burnable, in zone)\n",
            format(sum(ok), big.mark = ","), mean(bv[ok]), mean(wv[ok], na.rm = TRUE)))

# ---- A: the post-vintage fires -----------------------------------------------------------------------------------------------
mt <- read_sf(dsrc$MtbsPerimeterFile[1])   # MTBS releases differ in column case; canonical names as in C7
canon <- c(incid_type = "Incid_Type", incid_name = "Incid_Name", burnbndac = "BurnBndAc", ig_date = "Ig_Date"); hit <- match(tolower(names(mt)), names(canon))
names(mt)[!is.na(hit)] <- unname(canon[hit[!is.na(hit)]]); mt <- mt[mt$Incid_Type == "Wildfire" & !is.na(mt$Ig_Date), ]
mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y")); mt <- st_transform(mt[mt$BurnBndAc * 0.404686 >= MINHA, ], crs(bp))
mt <- mt[lengths(st_intersects(mt, zone)) > 0, ]
cellsof <- function(p) { r <- rasterize(vect(p), bp, field = 1); w <- which(!is.na(values(r, mat = FALSE))); w[ok[w]] }
post <- mt[mt$yr >= Y1 & mt$yr <= Y2, ]
A <- rbindlist(lapply(seq_len(nrow(post)), function(i) { w <- cellsof(post[i, ]); if (!length(w)) return(NULL)
  ft <- sort(table(grp[w]), decreasing = TRUE)
  data.table(Fire = post$Incid_Name[i], Year = post$yr[i], HaInZone = round(length(w) * 2.25), ModelBP = mean(bv[w]), ModelPct = mean(bpp[w]),
             WrcBP = mean(wv[w], na.rm = TRUE), WrcPct = mean(wpp[w], na.rm = TRUE), MedianElev = round(median(ev[w], na.rm = TRUE)),
             MainFuel = sprintf("%s %.0f%%", names(ft)[1], 100 * ft[1] / length(w))) }))[order(-HaInZone)]
cat(sprintf("\n== A: %d MTBS wildfires >= %g ha, %d-%d, in the zone (percentile: 50 = a typical burnable cell)\n", nrow(A), MINHA, Y1, Y2))
print(A[, .(Fire, Year, HaInZone, ModelBP = round(ModelBP, 3), ModelPct = round(ModelPct), WrcBP = round(WrcBP, 3), WrcPct = round(WrcPct), MedianElev, MainFuel)])
cat(sprintf("area-weighted percentile: model %.0f, benchmark %.0f\n", weighted.mean(A$ModelPct, A$HaInZone), weighted.mean(A$WrcPct, A$HaInZone)))
fwrite(A, file.path(OUT, "A_postvintage_fires.csv"))
# burned ground of those fires by stratum vs the model's and the benchmark's share of annual burned area
pw <- unique(unlist(lapply(seq_len(nrow(post)), function(i) cellsof(post[i, ]))))
sel <- function(by) { d <- S[ok == TRUE, .(ground = .N, obs = sum(cell %in% pw), mod = sum(bv[cell]), wrc = sum(wv[cell], na.rm = TRUE)), by = by]
  d[, `:=`(ground = ground / sum(ground), obs = obs / sum(obs), mod = mod / sum(mod), wrc = wrc / sum(wrc))]
  d[, .(stratum = get(by), ground = round(ground, 3), obs_sel = round(obs / ground, 2), model_sel = round(mod / ground, 2), wrc_sel = round(wrc / ground, 2))][order(stratum)] }
for (b in c("fuel", "elev")) { cat(sprintf("\nselection (share of burned / share of ground), %d-%d fire vs the maps, by %s\n", Y1, Y2, b)); print(sel(b)) }

# ---- B: placement vs spread, 1992-2020 -----------------------------------------------------------------------------------------
st <- g(R, "OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned") & Area >= MINHA]
st[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
fl <- g(R, "OutputFireList")[, .(bI = as.integer(Iteration), bF = as.integer(FireID), Cell)]; st <- fl[st, on = c("bI", "bF")]
bt <- as.data.table(open_dataset(g(R, "OutputBurnTable")$FileName[1]) |> select(Iteration, FireID, Cell) |> collect())[st[, .(Iteration = bI, FireID = bF)], on = c("Iteration", "FireID"), nomatch = 0]
mb <- bt[, .N, by = Cell]; mi <- st[, .N, by = Cell]
fpa <- read_sf(dsrc$FpaFodFile[1], query = sprintf("SELECT FIRE_SIZE, Shape FROM Fires WHERE FIRE_SIZE >= %f AND FIRE_YEAR BETWEEN 1992 AND 2020", MINHA / 0.404686))
oc <- cellFromXY(bp, st_coordinates(st_transform(fpa, crs(bp)))); oc <- oc[!is.na(oc) & inz[oc]]; oi <- data.table(Cell = oc)[, .N, by = Cell]
old <- mt[mt$yr >= 1992 & mt$yr <= 2020, ]
ob <- rbindlist(lapply(split(seq_len(nrow(old)), old$yr), function(k) { r <- rasterize(vect(old[k, ]), bp, field = 1); data.table(Cell = which(!is.na(values(r, mat = FALSE)))) }))[, .N, by = Cell]
cat(sprintf("\n== B: 1992-2020, fires >= %g ha: model %s kept fires; observed %d FPA-FOD origins, %d MTBS perimeters\n", MINHA,
            format(nrow(st), big.mark = ","), length(oc), nrow(old)))
PS <- rbindlist(lapply(c("fuel", "elev", "slope"), function(by) {
  x <- S[ok == TRUE, .(cell, s = get(by))]
  sh <- function(d) { m <- x[d, on = c(cell = "Cell"), nomatch = 0][, .(n = sum(N)), by = s]; m[, n := n / sum(n)] }
  d <- x[, .(ground = .N), by = s][, ground := ground / sum(ground)]
  for (k in list(list("ign_obs", oi), list("ign_mod", mi), list("burn_obs", ob), list("burn_mod", mb))) d <- sh(k[[2]])[d, on = "s"][, (k[[1]]) := fifelse(is.na(n), 0, n)][, n := NULL]
  d[, .(dimension = by, stratum = s, ground = round(ground, 3), ign_obs = round(ign_obs, 3), ign_mod = round(ign_mod, 3), burn_obs = round(burn_obs, 3), burn_mod = round(burn_mod, 3),
        burned = round(burn_mod / burn_obs, 2), placement = round(ign_mod / ign_obs, 2), spread = round((burn_mod / ign_mod) / (burn_obs / ign_obs), 2))][order(stratum)] }))
print(PS); fwrite(PS, file.path(OUT, "B_placement_spread.csv"))
split_share <- PS[is.finite(placement) & is.finite(spread) & burn_obs > 0, .(placement_share = sum(burn_obs * abs(log(placement))) / sum(burn_obs * (abs(log(placement)) + abs(log(spread))))), by = dimension]
cat("burn-weighted |log error| share due to placement (rest = spread):\n"); print(split_share[, .(dimension, placement_share = round(placement_share, 2))])

# ---- C: map ---------------------------------------------------------------------------------------------------------------------
png(file.path(OUT, "C_bp_vs_benchmark.png"), 1800, 1300, res = 150); par(mfrow = c(1, 2), mar = c(1, 1, 3, 5))
br <- c(0, 0.1, 0.2, 0.35, 0.5, 0.7, 1, 1.5, 2, 3, 100); pal <- hcl.colors(length(br) - 1, "YlOrRd", rev = TRUE)
zb <- vect(zone); pv <- vect(post)
for (k in list(list(bp, "lucasFireC2F"), list(wrc, "WRC V2"))) { if (is.null(k[[1]])) next
  plot(crop(mask(k[[1]], zb), zb), breaks = br, col = pal, axes = FALSE, main = sprintf("%s annual BP (%%/yr), %d-%d fires outlined", k[[2]], Y1, Y2), mar = c(1, 1, 3, 5))
  lines(zb, lwd = 0.6); lines(pv, col = "#1f5fbf", lwd = 1.2) }
dev.off(); cat("\nwrote", file.path(OUT, c("A_postvintage_fires.csv", "B_placement_spread.csv", "C_bp_vs_benchmark.png")), sep = "\n  ")
