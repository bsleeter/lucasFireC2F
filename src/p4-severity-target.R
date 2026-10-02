# =============================================================================
# P4 - MTBS Severity Target
# =============================================================================
# The Severity Target stage 8 fits its MTBS-style classes to: MTBS thematic burn severity of the fires after the fuel vintage
# (ZonePrepOption SeverityFirstYear-SeverityLastYear, default 2021-2024) on the PRE-fire LANDFIRE FBFM40 at 30 m (the vintage's
# own map: fires before it are already in it), inside the zone (P1's boundary). Classes 1 unburned-low, 2 low, 3 moderate,
# 4 high; 5/6 (increased greenness, no data) excluded. Shares by the package's fuel groups; a group with fewer than
# MinimumPixels (1,000) takes Timber-Understory's shares (Slash-Blowdown, as the P26 default does), and a zone with too little
# post-vintage fire keeps the target it has, with a note (use a neighbouring zone's, or pool zones).
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(sf) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R")); sf_use_s2(FALSE)
progressBar(type = "begin", totalSteps = 3)
dsrc <- read_data_sources(c("LandfireFolder", "MtbsSeverityFolder")); zo <- need("ZonePrepOption")
zb <- need("OutputZoneBoundary", "FileName")$FileName[1]
V <- if (!is.na(zo$LandfireVintage)) as.character(zo$LandfireVintage) else "2020"
Y1 <- if (!is.na(zo$SeverityFirstYear)) zo$SeverityFirstYear else 2021L; Y2 <- if (!is.na(zo$SeverityLastYear)) zo$SeverityLastYear else 2024L
MINPX <- 1000L
f <- landfire_file(dsrc$LandfireFolder, V, "FBFM40")
f30 <- rast(f); zv <- vect(st_transform(read_sf(zb), crs(f30))); f30 <- crop(f30, zv, snap = "out"); levels(f30) <- NULL
zin <- !is.na(values(rasterize(zv, f30, field = 1), mat = FALSE)); fv <- values(f30, mat = FALSE)
fm <- as.data.table(datasheet(prj, ds("FuelModel"), lookupsAsFactors = FALSE)); grp <- fm$FuelGroup[match(fv, fm$ID)]
progressBar()
obs <- rbindlist(lapply(Y1:Y2, function(y) { p <- file.path(dsrc$MtbsSeverityFolder, sprintf("mtbs_CONUS_%d.tif", y))
  if (!file.exists(p)) { say("%d: no MTBS severity mosaic (%s)", y, basename(p)); return(NULL) }
  s <- crop(rast(p), f30); if (!compareGeom(s, f30, stopOnError = FALSE)) s <- resample(s, f30, method = "near")
  sv <- values(s, mat = FALSE); k <- zin & sv %in% 1:4 & !is.na(grp)
  say("%d: %s burned 30 m pixels in the zone (classes 1-4)", y, format(sum(k), big.mark = ",")); data.table(g = grp[k], sev = sv[k]) }))
progressBar()
if (!nrow(obs) || nrow(obs) < 5 * MINPX) { say("only %s burned pixels %d-%d: too few for a target - the Severity Target is left as it is", format(nrow(obs), big.mark = ","), Y1, Y2)
} else {
  T <- obs[, .(Pixels = .N, UnburnedLow = mean(sev == 1), Low = mean(sev == 2), Moderate = mean(sev == 3), High = mean(sev == 4)), by = .(FuelGroup = g)][order(-Pixels)]
  thin <- setdiff(unique(na.omit(fm$FuelGroup)), T[Pixels >= MINPX]$FuelGroup)            # groups too thin (or absent) for their own shares
  tu <- T[FuelGroup == "Timber-Understory" & Pixels >= MINPX]
  if (length(thin) && nrow(tu)) { T <- rbind(T[!FuelGroup %in% thin], tu[rep(1, length(thin))][, `:=`(FuelGroup = thin, Pixels = 0L)])
    say("groups with under %d pixels take Timber-Understory's shares: %s", MINPX, paste(thin, collapse = ", ")) }
  write_ds(T[, .(FuelGroup, UnburnedLow = round(UnburnedLow, 4), Low = round(Low, 4), Moderate = round(Moderate, 4), High = round(High, 4))], "SeverityTarget")
  for (i in seq_len(nrow(T))) with(T[i], say("%-18s %9s px  %.3f / %.3f / %.3f / %.3f", FuelGroup, format(Pixels, big.mark = ","), UnburnedLow, Low, Moderate, High))
}
progressBar(); progressBar(type = "end")
