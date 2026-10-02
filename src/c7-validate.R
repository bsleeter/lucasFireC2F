# =============================================================================
# C7 - Validate Burn Probability
# =============================================================================
# Scores a result's burn probability against where large fires actually burned (MTBS wildfire perimeters >= MinimumFireHa,
# default 405 ha), and against a benchmark burn-probability map (e.g. WRC V2) scored on the same cells. Ported from the
# development scripts 162 (AUC) and 169 (selection).
#   cells        the modelled fire zones (the zones of the result's fires) on the Fire Zone grid; at least half burnable. With a
#                benchmark, scored on the benchmark's grid (our BP averaged onto it, as for WRC's 270 m); without, on ours.
#   AUC          rank AUC of BP against cells inside any perimeter of each Validation Period (default: 1984 to the fuel vintage
#                minus 10; the post-vintage window; the whole record). FUEL MEMORY: fires inside the fuel map's own vintage are
#                already in it as scars, so a period that overlaps the vintage rewards avoiding them - the post-vintage window is
#                the honest test.
#   hottest decile   share of a period's burned cells that fall in the map's top 10% of cells
#   selection    for the post-vintage window: share of the map's BP (or of burned cells) in a group / the group's share of ground,
#                by fuel group (the landscape's own fuels), time since fire as of the vintage (MTBS perimeters), elevation band
#                and third of the zone (south-north); AUC inside each group; and the ground-weighted correlation of log selection
#                with the observed (the numbers in the methods paper's Table 6)
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE); terraOptions(progress = 0)
progressBar(type = "begin", totalSteps = 4)
dsrc <- read_data_sources("MtbsPerimeterFile"); bench <- read_data_sources("WrcBurnProbabilityFile", optional = TRUE)$WrcBurnProbabilityFile
vo <- read_ds("ValidationOption"); MINHA <- if (nrow(vo) && !is.na(vo$MinimumFireHa)) vo$MinimumFireHa else 405
VINT <- if (nrow(vo) && !is.na(vo$FuelVintage)) vo$FuelVintage else 2020L; LAST <- if (nrow(vo) && !is.na(vo$LastYear)) vo$LastYear else 2024L
per <- read_ds("ValidationPeriod")
if (!nrow(per)) per <- data.table(Name = c(sprintf("1984-%d (long before the vintage)", VINT - 10), sprintf("%d-%d (after the fuel vintage)", VINT + 1, LAST), sprintf("1984-%d (all)", LAST)),
                                  FirstYear = c(1984L, VINT + 1L, 1984L), LastYear = c(VINT - 10L, LAST, LAST))
bp <- rast(need("OutputBurnProbability", "FileName")$FileName[1]); bp[is.na(bp)] <- 0
lr <- need("LandscapeRasters", c("FuelFileName", "FireZoneFileName", "ElevationFileName"))
fu <- rast(lr$FuelFileName[1]); fz <- rast(lr$FireZoneFileName[1]); el <- rast(lr$ElevationFileName[1])
for (r in list(fu, fz, el)) if (!compareGeom(r, bp, stopOnError = FALSE)) fail("landscape rasters and the burn probability map are not on one grid")
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE)); st <- read_ds("OutputFireStatistic")
zones <- if (nrow(st) && any(!is.na(st$FireZone))) unique(na.omit(st$FireZone)) else fzt$Name
zid <- fzt$ID[match(zones, fzt$Name)]; if (anyNA(zid)) fail("fire zones without a Map Code: %s", paste(zones[is.na(zid)], collapse = ", "))
fm <- as.data.table(datasheet(prj, ds("FuelModel"), lookupsAsFactors = FALSE)); fv <- values(fu, mat = FALSE)
burnable <- !(fv %in% 91:99) & !is.na(fv); inz <- values(fz, mat = FALSE) %in% zid
auc <- function(s, l, nmin = 5) { n1 <- as.numeric(sum(l)); n0 <- as.numeric(sum(!l)); if (n1 < nmin || n0 < nmin) return(NA_real_)
  r <- rank(s); (sum(as.numeric(r[l])) - n1 * (n1 + 1) / 2) / (n1 * n0) }
top <- function(s) { set.seed(1); r <- rank(s, ties.method = "random"); r > 0.9 * length(s) }
say("zones scored: %s; MTBS wildfires >= %g ha; fuel vintage %d; benchmark %s", paste(zones, collapse = ", "), MINHA, VINT,
    if (!is.na(bench) && nzchar(bench)) basename(bench) else "none")

# ---- MTBS perimeters (field names vary between releases) ----------------------------------------------------------------------
mt <- read_sf(dsrc$MtbsPerimeterFile)
canon <- c(incid_type = "Incid_Type", burnbndac = "BurnBndAc", ig_date = "Ig_Date"); hit <- match(tolower(names(mt)), names(canon)); names(mt)[!is.na(hit)] <- unname(canon[hit[!is.na(hit)]])
mt <- mt[mt$Incid_Type == "Wildfire" & !is.na(mt$Ig_Date), ]; mt$yr <- as.integer(format(as.Date(mt$Ig_Date), "%Y"))
mt <- st_make_valid(st_transform(mt[mt$yr >= 1984 & mt$yr <= LAST & mt$BurnBndAc * 0.404686 >= MINHA, ], crs(bp)))
mt <- mt[lengths(st_intersects(mt, st_as_sfc(st_bbox(bp)))) > 0, ]
say("MTBS perimeters in the frame: %d (%d-%d)", nrow(mt), min(mt$yr), max(mt$yr))
progressBar()

# ---- AUC and hottest decile by period ------------------------------------------------------------------------------------------
if (!is.na(bench) && nzchar(bench) && res(rast(bench))[1] < res(bp)[1] - 1e-6) {   # finer than the model (WRC 30 m, CONUS): averaged onto our grid
  wr0 <- rast(bench); wr0 <- if (!same.crs(wr0, bp)) project(crop(wr0, project(ext(bp), crs(bp), crs(wr0))), bp, method = "average") else resample(crop(wr0, ext(bp) + 300), bp, method = "average")
  if (max(values(wr0, mat = FALSE), na.rm = TRUE) > 1) wr0 <- wr0 / 1e4                     # WRC BP stored x 10,000 (image service, some releases)
  G <- bp; vO <- values(bp, mat = FALSE); vB <- values(wr0, mat = FALSE); sel <- inz & burnable & !is.na(vB)
} else if (!is.na(bench) && nzchar(bench)) {
  wr <- crop(rast(bench), ext(bp)); o <- resample(bp, wr, method = "average"); bf <- resample(ifel(burnable_r <- setValues(rast(bp), as.numeric(burnable)), burnable_r, 0), wr, method = "average")
  zin <- resample(setValues(rast(bp), as.numeric(inz)), wr, method = "near"); G <- wr
  vO <- values(o, mat = FALSE); vB <- values(wr, mat = FALSE); sel <- values(zin, mat = FALSE) == 1 & !is.na(vO) & !is.na(vB) & values(bf, mat = FALSE) >= 0.5
} else { G <- bp; vO <- values(bp, mat = FALSE); vB <- rep(NA_real_, length(vO)); sel <- inz & burnable }
lab <- function(y0, y1) { s <- mt[mt$yr >= y0 & mt$yr <= y1, ]; if (!nrow(s)) return(rep(FALSE, ncell(G)))
  v <- values(rasterize(vect(s), G, fun = "count", background = 0), mat = FALSE); !is.na(v) & v > 0 }
tO <- top(vO[sel]); tB <- if (all(is.na(vB))) NULL else top(vB[sel])
V <- rbindlist(lapply(seq_len(nrow(per)), function(i) { L <- lab(per$FirstYear[i], per$LastYear[i])[sel]
  data.table(Period = per$Name[i], FirstYear = per$FirstYear[i], LastYear = per$LastYear[i], BurnedCells = sum(L), BurnedShare = mean(L),
             AUC = auc(vO[sel], L), AUCBenchmark = if (is.null(tB)) NA_real_ else auc(vB[sel], L),
             HottestDecile = if (sum(L)) mean(tO[L]) else NA_real_, HottestDecileBenchmark = if (is.null(tB) || !sum(L)) NA_real_ else mean(tB[L])) }))
write_ds(V, "OutputValidation")
say("cells scored: %s (%s grid)", format(sum(sel), big.mark = ","), if (is.null(tB)) "the model's" else "the benchmark's")
for (i in seq_len(nrow(V))) with(V[i], say("  %-40s burned %5.1f%%  AUC %.3f%s  hottest decile %2.0f%%%s", Period, 100 * BurnedShare, AUC,
  if (is.na(AUCBenchmark)) "" else sprintf(" (benchmark %.3f)", AUCBenchmark), 100 * HottestDecile, if (is.na(HottestDecileBenchmark)) "" else sprintf(" (benchmark %.0f%%)", 100 * HottestDecileBenchmark)))
progressBar()

# ---- selection, post-vintage window, on the model's grid ------------------------------------------------------------------------
idx <- which(inz & burnable); xy <- xyFromCell(bp, idx)
ours <- values(bp, mat = FALSE)[idx]; bm <- if (!is.na(bench) && nzchar(bench)) terra::extract(rast(bench), vect(xy, crs = crs(bp)))[, 2] else rep(NA_real_, length(idx))
burned <- function(y0, y1) { s <- mt[mt$yr >= y0 & mt$yr <= y1, ]; if (!nrow(s)) return(rep(FALSE, length(idx)))
  v <- values(rasterize(vect(s), bp, fun = "count", background = 0), mat = FALSE)[idx]; !is.na(v) & v > 0 }
last <- rep(NA_integer_, length(idx)); for (y in 1984:VINT) last[burned(y, y)] <- y
tsf <- VINT - last + 1; tsfc <- as.character(cut(tsf, c(0, 5, 10, 20, 40), labels = c("1-5 yr", "6-10 yr", "11-20 yr", "21-33 yr"))); tsfc[is.na(tsf)] <- "no fire since 1984"
Lp <- burned(VINT + 1, LAST)
d <- data.table(fuel = fm$FuelGroup[match(fv[idx], fm$ID)], tsf = tsfc,
                elev = as.character(cut(values(el, mat = FALSE)[idx], c(-Inf, 500, 1000, 1500, 2000, 2500, Inf), labels = c("<500 m", "500-1000", "1000-1500", "1500-2000", "2000-2500", ">2500"))),
                third = as.character(cut(xy[, 2], quantile(xy[, 2], c(0, 1/3, 2/3, 1)), labels = c("south", "central", "north"), include.lowest = TRUE)),
                ours, bm, Lp)
if (any(!is.na(d$bm))) d <- d[!is.na(bm)]
sel_by <- function(by) { r <- d[, .(ground = .N, ours = sum(ours), bm = sum(bm, na.rm = TRUE), ob = sum(Lp), a1 = auc(ours, Lp, 20), a2 = if (all(is.na(bm))) NA_real_ else auc(bm, Lp, 20)), by = by]
  setnames(r, by, "Group"); r[, `:=`(ground = ground / sum(ground), ours = ours / sum(ours), bm = if (sum(bm) > 0) bm / sum(bm) else NA_real_, ob = ob / sum(ob))]
  r[, .(Dimension = by, Group, ShareOfGround = ground, Selection = ours / ground, SelectionBenchmark = bm / ground, SelectionObserved = ob / ground,
        AUCWithin = a1, AUCWithinBenchmark = a2)][ShareOfGround >= 0.01][order(-ShareOfGround)] }
S <- rbindlist(lapply(c("fuel", "tsf", "elev", "third"), sel_by))
write_ds(S, "OutputSelection")
say("selection, fires %d-%d (%.1f%% of the ground burned):", VINT + 1, LAST, 100 * mean(Lp))
lw <- function(s) log(pmax(s, 0.01))
for (dm in unique(S$Dimension)) { z <- S[Dimension == dm & is.finite(SelectionObserved)]
  cs <- function(m) if (all(is.na(m)) || nrow(z) < 3) NA_real_ else cov.wt(cbind(lw(m), lw(z$SelectionObserved)), wt = z$ShareOfGround, cor = TRUE)$cor[1, 2]
  say("  %-5s weighted correlation of log selection with observed: model %.2f%s", dm, cs(z$Selection), if (all(is.na(z$SelectionBenchmark))) "" else sprintf(", benchmark %.2f", cs(z$SelectionBenchmark))) }
progressBar(); progressBar(type = "end")
