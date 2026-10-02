# Preview of within-stratum replacement for a result: stage 5 now replaces a failed season fire with ANY pool fire that reached the
# threshold, so strata that survive well (e.g. hardwood-origin) end up over-represented among kept fires. Within-stratum
# replacement restores each stratum's share of SEASON ignitions. Preview = reweight each kept fire by
# w_s = (season-ignition share of its stratum) / (kept share of its stratum), then recompute BP (burns summed per cell / seasons;
# same-season overlaps ignored). Also: each region's BP split by the origin vegetation of the fires that burned it.
# Strata: cause x origin vegetation (BpS). Usage: Rscript replacement_preview.R "<library>" <result id> <prep id> <vegetation tif> <out dir>
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table); library(arrow); library(dplyr); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); R <- rsyncrosim::scenario(L, as.integer(a[2])); P <- rsyncrosim::scenario(L, as.integer(a[3]))
vr <- rast(a[4]); OUT <- a[5]; PR <- rsyncrosim::project(L, 1)
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
vc <- as.data.table(datasheet(PR, "lucasFireC2F_VegetationClass", lookupsAsFactors = FALSE)); vv <- values(vr, mat = FALSE)
bp <- rast(g(R, "OutputBurnProbability")$FileName[1]); NS <- g(R, "RunControl")$MaximumIteration[1]; MINA <- g(R, "ResampleOption")$MinimumFireSize[1]
fl <- g(R, "OutputFireList")[, .(Iteration = as.integer(Iteration), FireID = as.integer(FireID), Cell, Cause, Pool)]
fl[, Veg := vc$Name[match(vv[Cell], vc$ID)]]; fl[, s := paste(Cause, Veg, sep = " / ")]
st <- g(R, "OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned")]
st[, `:=`(bI = ifelse(ResampleStatus == "Reassigned", 0L, as.integer(Iteration)), bF = as.integer(ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID)))]
st <- fl[, .(bI = Iteration, bF = FireID, s, Veg)][st, on = c("bI", "bF")]
seas <- fl[Pool == FALSE, .N, by = s][, share_season := N / sum(N)]
kept <- st[, .N, by = s][, share_kept := N / sum(N)]
W <- merge(seas[, .(s, share_season)], kept[, .(s, share_kept)], by = "s", all = TRUE); W[is.na(share_kept), share_kept := 0]; W[is.na(share_season), share_season := 0]
W[, w := fifelse(share_kept > 0, share_season / share_kept, 0)]
cat("== strata: share of season ignitions vs share of kept fires (w = the within-stratum correction)\n"); print(W[order(-share_kept)][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
st[W, on = "s", w := i.w]
bt <- as.data.table(open_dataset(g(R, "OutputBurnTable")$FileName[1]) |> select(Iteration, FireID, Cell) |> collect())
bt <- bt[st[, .(Iteration = bI, FireID = bF, w, Veg)], on = c("Iteration", "FireID"), nomatch = 0]
cat(sprintf("burns of kept fires: %s\n", format(nrow(bt), big.mark = ",")))
nc <- ncell(bp); cur <- tabulate(bt$Cell, nc) / NS; new <- numeric(nc); agg <- bt[, .(x = sum(w)), by = Cell]; new[agg$Cell] <- agg$x / NS
byveg <- dcast(bt[, .N, by = .(Cell, Veg)], Cell ~ Veg, value.var = "N", fill = 0)
zone <- st_transform(st_union(st_make_valid(read_sf(g(P, "OutputZoneBoundary")$FileName[1]))), crs(bp)); inz <- !is.na(values(rasterize(vect(zone), bp), mat = FALSE))
xy <- xyFromCell(bp, seq_len(nc)); yr <- range(xy[inz, 2]); third <- cut(xy[, 2], yr[1] + diff(yr) * c(-1e-6, 1/3, 2/3, 1 + 1e-6), c("south", "central", "north"))
# the northwest: the north third's western half (by the zone's x range within the north third)
xn <- range(xy[inz & third %in% "north", 1]); region <- as.character(third); region[third %in% "north" & xy[, 1] < mean(xn)] <- "north-west"; region[third %in% "north" & xy[, 1] >= mean(xn)] <- "north-east"
D <- data.table(cell = seq_len(nc), region, cur = 100 * cur, new = 100 * new)[inz]
S <- D[, .(cells = .N, bp_now = round(mean(cur), 3), bp_within_stratum = round(mean(new), 3), change = round(mean(new) / mean(cur), 2)), by = region][order(region)]
cat("\n== mean BP (%/yr) by region, now vs previewed within-stratum replacement\n"); print(S)
vcols <- setdiff(names(byveg), "Cell"); byveg <- byveg[D[, .(Cell = cell, region)], on = "Cell", nomatch = 0]
V <- byveg[, lapply(.SD, sum), by = region, .SDcols = vcols]; V <- melt(V, id.vars = "region", variable.name = "origin", value.name = "burns")[, share := round(burns / sum(burns), 3), by = region]
cat("\n== share of each region's burns by the origin vegetation of the fires that burned it\n"); print(dcast(V, region ~ origin, value.var = "share"))
cat(sprintf("\nzone total area now %.0f ha/yr -> previewed %.0f ha/yr (x%.2f)\n", sum(cur[inz]) * 2.25, sum(new[inz]) * 2.25, sum(new[inz]) / sum(cur[inz])))
fwrite(S, file.path(OUT, "M_replacement_preview_regions.csv")); fwrite(W, file.path(OUT, "M_replacement_preview_weights.csv"))
r <- setValues(rast(bp), ifelse(cur > 0, new / cur, NA)); writeRaster(r, file.path(OUT, "M_replacement_preview_ratio.tif"), overwrite = TRUE)
