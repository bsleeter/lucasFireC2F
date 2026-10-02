# =============================================================================
# 5 - Summarize Burn Probability
# =============================================================================
# The fire population a season reports, and the maps (ported from the validated P26 script 203, the BurnP3+ rule):
#   resampling   each season fire of at least ResampleOption.MinimumFireSize is Kept; a smaller one is Discarded and
#                replaced, season by season in order, by the next extra-pool fire that reached the minimum (pool fires below
#                it are Discarded; pool fires never reached are Not Used). A replacement is Reassigned to the season with a
#                new Fire ID; OriginalFireID keeps its pool ID (its burned cells stay under Iteration 0 in the burn table).
#                When the spread-day curves are fitted by stratum (cause and/or origin vegetation), a failed fire is replaced
#                from its own stratum's pool first, so each stratum keeps its share of the ignitions.
#   burn count   cells burned by the kept fires, counted once per season; burn probability = burn count / seasons
# A seed run (GrowthOption.SeedRun) has no pool: every fire is kept as grown, for stage C5.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(arrow) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
progressBar(type = "begin", totalSteps = 3)

rc <- need("RunControl", "MaximumIteration"); NS <- rc$MaximumIteration
rso <- read_ds("ResampleOption"); MINA <- if (nrow(rso) && !is.na(rso$MinimumFireSize)) rso$MinimumFireSize else 0
st <- need("OutputFireStatistic", c("Iteration", "FireID", "Area"))
Fl <- need("OutputFireList"); st[Fl, on = c("Iteration", "FireID"), Pool := i.Pool]
bt <- need("OutputBurnTable", "FileName"); BT <- as.data.table(arrow::read_parquet(bt$FileName[1], col_select = c("Iteration", "FireID", "Cell")))
g <- terra::rast(read_ds("OutputEngineLandscape")$FuelFileName[1])

MERGED <- "ResampleStatus" %in% names(st) && all(!is.na(st$ResampleStatus) & nzchar(st$ResampleStatus))
if (MERGED) {   # a merge scenario (stage M): every zone was resampled to its own minimum in its own run - keep those statuses
  S <- copy(st); kept <- S[ResampleStatus %in% c("Kept", "Reassigned")]
  say("%d seasons: %s fires kept by their zones' own resampling (%.2f per season); resampling not repeated", NS, format(nrow(kept), big.mark = ","), nrow(kept) / NS)
} else {
orig <- st[Pool == FALSE][order(Iteration, FireID)]; pool <- st[Pool == TRUE][order(FireID)]
orig[, `:=`(ResampleStatus = ifelse(Area >= MINA, "Kept", "Discarded"), OriginalFireID = NA_integer_)]
pool[, `:=`(ResampleStatus = "Not Used", OriginalFireID = FireID)]
# Replacement WITHIN STRATUM - the strata the spread-day curves were fitted by (cause, origin vegetation, or both; SpreadDays). A
# replacement from any stratum over-represents strata whose fires survive the threshold well (P27: hardwood-origin fires 1.6x their
# share of ignitions, conifer 0.9x). A failed fire takes the next pool fire of its own stratum that reached the minimum; only when that
# stratum's pool is used up does it take the next one of any stratum. With one curve for all fires this is the former rule exactly.
sdk <- read_ds("SpreadDays"); USE_C <- "Cause" %in% names(sdk) && any(!is.na(sdk$Cause)); USE_V <- "Vegetation" %in% names(sdk) && any(!is.na(sdk$Vegetation))
if (USE_C || USE_V) { Fl2 <- Fl[, .(Iteration, FireID, Cell, FlCause = Cause)]; vm <- if (USE_V) veg_map() else NULL
  kf <- function(d) { d <- Fl2[d[, .(Iteration, FireID)], on = c("Iteration", "FireID")]
    paste(if (USE_C) d$FlCause else "", if (USE_V) veg_of_cells(d$Cell, vm) else "", sep = "|") }
  orig[, key := kf(orig)]; pool[, key := kf(pool)] } else { orig[, key := ""]; pool[, key := ""] }
fail <- orig[ResampleStatus == "Discarded", .(Iteration, key)]      # in season order
okp <- which(pool$Area >= MINA); taken <- rep(FALSE, nrow(pool)); assign <- rep(NA_integer_, nrow(fail))
for (k in unique(fail$key)) { f <- which(fail$key == k); q <- okp[pool$key[okp] == k]; m <- min(length(f), length(q))
  if (m) { assign[f[seq_len(m)]] <- q[seq_len(m)]; taken[q[seq_len(m)]] <- TRUE } }
own <- sum(!is.na(assign)); rest <- which(is.na(assign)); spare <- okp[!taken[okp]]; m <- min(length(rest), length(spare))
if (m) { assign[rest[seq_len(m)]] <- spare[seq_len(m)]; taken[spare[seq_len(m)]] <- TRUE }
cross <- m; short <- sum(is.na(assign)); p <- sum(!is.na(assign)); ok <- okp
reas <- data.table(k = assign, Iteration = fail$Iteration)[!is.na(k)]
for (k in unique(pool$key[taken])) { last <- max(which(taken & pool$key == k)); w <- which(pool$key == k & seq_len(nrow(pool)) <= last & pool$Area < MINA); pool[w, ResampleStatus := "Discarded"] }
if (USE_C || USE_V) say("replacement within stratum (%s): %s of %s from the fire's own stratum, %s from another stratum (own pool used up), %s unreplaced",
  paste(c(if (USE_C) "cause", if (USE_V) "origin vegetation"), collapse = " x "), format(own, big.mark = ","), format(nrow(fail), big.mark = ","), format(cross, big.mark = ","), format(short, big.mark = ","))
orig[, key := NULL]; pool[, key := NULL]
pool[reas$k, `:=`(ResampleStatus = "Reassigned", Iteration = reas$Iteration)]
mx <- orig[, .(mx = max(FireID)), by = Iteration]
pool[ResampleStatus == "Reassigned", FireID := { m <- mx$mx[match(Iteration, mx$Iteration)]; m + seq_len(.N) }, by = Iteration]
S <- rbind(orig, pool, fill = TRUE)
kept <- S[ResampleStatus %in% c("Kept", "Reassigned")]
say("%d seasons: kept %s fires (%.2f per season); %.1f%% of season fires below %.1f ha replaced; %d replacements from %d eligible pool fires; %d unreplaced",
    NS, format(nrow(kept), big.mark = ","), nrow(kept) / NS, 100 * mean(orig$ResampleStatus == "Discarded"), MINA, p, length(ok), short)
}
progressBar()

# ---- burn count and burn probability --------------------------------------------------------------------------------------
kk <- kept[, .(bIter = ifelse(ResampleStatus == "Reassigned", 0L, Iteration), bFire = ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID),
               Season = Season, Iteration)]
B <- BT[kk, on = c(Iteration = "bIter", FireID = "bFire"), .(Iteration = i.Iteration, Season = i.Season, Cell), nomatch = 0]
bc_of <- function(x) { u <- unique(x[, .(Iteration, Cell)]); v <- numeric(ncell(g)); t <- u[, .N, by = Cell]; v[t$Cell] <- t$N; v }
TD <- env$TransferDirectory
wr <- function(v, f, dt) { p <- file.path(TD, f); writeRaster(setValues(rast(g), v), p, overwrite = TRUE, datatype = dt, gdal = "COMPRESS=DEFLATE"); p }
bc <- bc_of(B)
bpo <- data.frame(Iteration = 1L, Timestep = 0L, FileName = wr(bc / NS, "burn_probability.tif", "FLT4S"))
bco <- data.frame(Iteration = 1L, Timestep = 0L, FileName = wr(bc, "burn_count.tif", "INT4U"))
progressBar()
write_ds(bpo, "OutputBurnProbability"); write_ds(bco, "OutputBurnCount")
write_ds(S[, .(Iteration, FireID, Season, Cause, FireZone, FuelType, SpreadDays, HoursBurning, Area, ResampleStatus, OriginalFireID)], "OutputFireStatistic")
say("burned cell-seasons %s; kept fires %.0f ha vs burn count %.0f ha (overlap within a season makes the second smaller); mean BP %.4f%%/yr",
    format(sum(bc), big.mark = ","), sum(kept$Area), sum(bc) * prod(res(g)) / 1e4, 100 * mean(bc) / NS)
progressBar(); progressBar(type = "end")
