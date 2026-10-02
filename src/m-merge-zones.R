# =============================================================================
# M - Merge Zone Results
# =============================================================================
# Combines finished zone runs (each: stages 1-5, its own calibration, its own resampling) into one landscape so that stages
# 5-8 produce the merged products without growing a fire again. Each zone's fires ignite only inside its zone but spread
# freely into the buffer, which is its neighbours' ground; the zones' fire populations are independent, so season i of every
# zone together is a valid combined season and the pooled burns are what a joint run would have produced.
#   grid        the union of the zones' engine grids (all on the same 150 m lattice); every raster is mosaicked, and the
#               overlap is checked cell for cell (the zones are prepared from the same national layers, so it must agree)
#   fires       fire lists, hourly weather, fire statistics (WITH each zone's resampling statuses) and burn tables, cells and
#               source cells remapped to the union grid, Fire IDs offset per zone (MergeSource.FireIdOffset, default
#               (k - 1) x 1,000,000) so no two zones share an ID
#   wind        each zone's Burning-Period Wind Ratio Table, tagged with the zone, so stage 6 draws a fire's burning-period
#               wind from its own zone's RAWS
# Then run 5 (resampling is kept, not repeated), 6, 7 and 8. Every zone must have run the same number of seasons.
# The merge scenario's project must list every zone's Fire Zone and Weather Zones (names as in the zone libraries).
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(arrow) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
ms <- need("MergeSource", "SourceScenarioId"); NS <- need("RunControl", "MaximumIteration")$MaximumIteration
progressBar(type = "begin", totalSteps = nrow(ms) + 3)
TD <- env$TransferDirectory
OPTS <- c("EffectsOption", "WeatherOption", "RiskOption", "FireDetailOption", "OutputOption", "SeverityTarget")   # filled from source 1 when empty

# ---- read every zone result -------------------------------------------------------------------------------------------------
Z <- list()
for (k in seq_len(nrow(ms))) {
  f <- ms$LibraryFileName[k]; same <- is.null(f) || is.na(f) || !nzchar(f)                 # blank = a result in this library
  if (!same && !file.exists(f)) fail("merge source %d: library not found (%s)", k, f)
  L <- if (same) lib else suppressWarnings(rsyncrosim::ssimLibrary(f)); R <- rsyncrosim::scenario(L, as.integer(ms$SourceScenarioId[k]))
  if (same) f <- "this library"
  g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE, optional = TRUE))
  rc <- g("RunControl"); if (rc$MaximumIteration != NS) fail("merge source %d ran %d seasons; this scenario is set to %d", k, rc$MaximumIteration, NS)
  st <- g("OutputFireStatistic"); if (!nrow(st) || all(is.na(st$ResampleStatus))) fail("merge source %d (scenario %d) has no resampled fire statistics - run its stages 1-5 first", k, ms$SourceScenarioId[k])
  zn <- unique(na.omit(st$FireZone)); if (length(zn) != 1) fail("merge source %d: expected one fire zone, found %s", k, paste(zn, collapse = ", "))
  off <- if ("FireIdOffset" %in% names(ms) && !is.na(ms$FireIdOffset[k])) as.integer(ms$FireIdOffset[k]) else as.integer((k - 1L) * 1000000L)
  Z[[k]] <- list(zone = zn, off = off, el = g("OutputEngineLandscape"), lr = g("LandscapeRasters"), fl = g("OutputFireList"), fw = g("OutputFireWeather"),
                 st = st, bt = g("OutputBurnTable")$FileName[1], wr = g("WindRatio"), opt = setNames(lapply(OPTS, g), OPTS))
  say("source %d: %s, %s (scenario %d): %s fires, %s kept, Fire ID offset %s", k, zn, basename(f), ms$SourceScenarioId[k], format(nrow(st), big.mark = ","),
      format(sum(st$ResampleStatus %in% c("Kept", "Reassigned")), big.mark = ","), format(off, big.mark = ","))
  progressBar()
}
maxid <- max(sapply(Z, function(z) max(c(z$st$FireID, z$st$OriginalFireID, z$fl$FireID), na.rm = TRUE)))

# ---- option sheets: this scenario's own if set, else source 1's; say where the zones disagree --------------------------------
strip <- function(x) { x <- as.data.frame(x); x <- x[, setdiff(names(x), grep("Id$", names(x), value = TRUE)), drop = FALSE]
  # list columns come back as labels; write them back as the codes (as effects.R reads them)
  if ("WindMode" %in% names(x) && is.character(x$WindMode)) x$WindMode <- as.integer(!grepl("^Observed", x$WindMode))
  if ("CrownModel" %in% names(x) && is.character(x$CrownModel)) x$CrownModel <- as.integer(!grepl("^Cruz", x$CrownModel))
  x }
for (sh in OPTS) { mine <- read_ds(sh); s1 <- strip(Z[[1]]$opt[[sh]])
  if (!nrow(mine) && nrow(s1)) { write_ds(s1, sh); say("%s: empty here - taken from source 1 (%s)", sh, Z[[1]]$zone) }
  for (k in seq_along(Z)[-1]) { sk <- strip(Z[[k]]$opt[[sh]]); if (nrow(sk) && nrow(s1) && !isTRUE(all.equal(s1, sk, check.attributes = FALSE)))
    warning(sprintf("%s differs between source 1 (%s) and source %d (%s); the merge uses %s", sh, Z[[1]]$zone, k, Z[[k]]$zone, if (nrow(mine)) "this scenario's" else "source 1's")) } }

# ---- every zone name the fires carry must be in this project's lists (the package ships every California zone) -----------------
fzp <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))$Name; wzp <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE))$Name
mf <- setdiff(unique(unlist(lapply(Z, function(z) z$fl$FireZone))), c(fzp, NA)); mw <- setdiff(unique(unlist(lapply(Z, function(z) z$fl$WeatherZone))), c(wzp, NA))
if (length(mf) || length(mw)) fail("this project's lists lack %s%s%s - add them to Fire Zones / Weather Zones (a new project has every pyrome)",
  if (length(mf)) paste("fire zone(s):", paste(mf, collapse = "; ")) else "", if (length(mf) && length(mw)) " and " else "", if (length(mw)) paste("weather zone(s):", paste(mw, collapse = "; ")) else "")
offs <- sapply(Z, `[[`, "off"); if (length(Z) > 1 && min(diff(sort(offs))) <= maxid) fail("Fire ID offsets %s are closer than the largest Fire ID (%d)", paste(offs, collapse = ", "), maxid)

# ---- union grid and mosaics -------------------------------------------------------------------------------------------------
gz <- lapply(Z, function(z) terra::rast(z$el$FuelFileName[1])); CS <- res(gz[[1]])[1]
for (r in gz) { if (abs(res(r)[1] - CS) > 1e-6 || !same.crs(r, gz[[1]])) fail("zone grids differ in cell size or CRS")
  if (abs(((xmin(r) - xmin(gz[[1]])) / CS) - round((xmin(r) - xmin(gz[[1]])) / CS)) > 1e-6) fail("zone grids are not on one lattice") }
ee <- sapply(gz, function(r) as.vector(ext(r)))                                   # xmin, xmax, ymin, ymax per zone
U <- rast(ext(min(ee[1, ]), max(ee[2, ]), min(ee[3, ]), max(ee[4, ])), resolution = CS, crs = crs(gz[[1]])); NCU <- ncol(U)
say("union grid %d x %d (%s cells) from %d zones", nrow(U), ncol(U), format(ncell(U), big.mark = ","), length(Z))
mosaic <- function(paths, fname, dt, check = FALSE) {
  rs <- lapply(paths, function(p) extend(terra::rast(p), U))
  if (check && length(rs) > 1) { a <- values(rs[[1]], mat = FALSE); for (j in 2:length(rs)) { b <- values(rs[[j]], mat = FALSE); k <- !is.na(a) & !is.na(b)
    d <- sum(abs(a[k] - b[k]) > 1e-6); if (d) warning(sprintf("%s: %d of %d overlapping cells differ between zones", fname, d, sum(k)))
    say("%s: %s overlapping cells, %d differ", fname, format(sum(k), big.mark = ","), d) } }
  m <- Reduce(function(x, y) cover(x, y), rs); p <- file.path(TD, fname); writeRaster(m, p, overwrite = TRUE, datatype = dt, gdal = "COMPRESS=DEFLATE"); p }
el <- data.frame(FuelFileName = mosaic(sapply(Z, function(z) z$el$FuelFileName[1]), "engine_fuel.tif", "INT2S", TRUE),
                 ElevationFileName = mosaic(sapply(Z, function(z) z$el$ElevationFileName[1]), "engine_elevation.tif", "FLT8S"),
                 CanopyBaseHeightFileName = mosaic(sapply(Z, function(z) z$el$CanopyBaseHeightFileName[1]), "engine_cbh.tif", "FLT8S", TRUE),
                 CanopyBulkDensityFileName = mosaic(sapply(Z, function(z) z$el$CanopyBulkDensityFileName[1]), "engine_cbd.tif", "FLT8S"),
                 CanopyCoverFileName = mosaic(sapply(Z, function(z) z$el$CanopyCoverFileName[1]), "engine_ccf.tif", "FLT8S"),
                 CanopyHeightFileName = if (all(sapply(Z, function(z) !is.na(z$el$CanopyHeightFileName[1])))) mosaic(sapply(Z, function(z) z$el$CanopyHeightFileName[1]), "engine_hm.tif", "FLT8S") else NA,
                 LandscapeFuelFileName = mosaic(sapply(Z, function(z) z$el$LandscapeFuelFileName[1]), "landscape_fuel.tif", "INT2S"))
lr <- data.frame(FuelFileName = mosaic(sapply(Z, function(z) z$lr$FuelFileName[1]), "fuel_type.tif", "INT2S"),
                 ElevationFileName = el$ElevationFileName, FireZoneFileName = mosaic(sapply(Z, function(z) z$lr$FireZoneFileName[1]), "fire_zone.tif", "INT4S", TRUE),
                 WeatherZoneFileName = mosaic(sapply(Z, function(z) z$lr$WeatherZoneFileName[1]), "weather_zone.tif", "INT4S"),
                 FireAgeFileName = if (all(sapply(Z, function(z) !is.na(z$lr$FireAgeFileName[1])))) mosaic(sapply(Z, function(z) z$lr$FireAgeFileName[1]), "years_since_fire.tif", "INT2S") else NA)
progressBar()

# ---- fires: remap cells, offset IDs, pool ---------------------------------------------------------------------------------------
remap <- function(cell, z) { r <- terra::rast(z$el$FuelFileName[1]); nc <- ncol(r)
  r0 <- as.integer(round((ymax(U) - ymax(r)) / CS)); c0 <- as.integer(round((xmin(r) - xmin(U)) / CS))
  ifelse(is.na(cell), NA_integer_, as.integer(((cell - 1L) %/% nc + r0) * NCU + ((cell - 1L) %% nc + c0) + 1L)) }
FL <- rbindlist(lapply(Z, function(z) { x <- copy(z$fl); x[, `:=`(Cell = remap(Cell, z), FireID = FireID + z$off)]; x }), fill = TRUE)
FW <- rbindlist(lapply(Z, function(z) { x <- copy(z$fw); x[, FireID := FireID + z$off]; x }), fill = TRUE)
ST <- rbindlist(lapply(Z, function(z) { x <- copy(z$st); x[, FireID := FireID + z$off]; x[!is.na(OriginalFireID), OriginalFireID := OriginalFireID + z$off]; x }), fill = TRUE)
BT <- rbindlist(lapply(Z, function(z) { x <- as.data.table(arrow::read_parquet(z$bt)); x[, `:=`(Cell = remap(Cell, z), FireID = FireID + z$off)]
  if ("Source" %in% names(x)) x[, Source := remap(Source, z)]; x }), fill = TRUE)
if (anyDuplicated(FL[, .(Iteration, FireID)])) fail("merged fire list has duplicate Iteration x Fire ID keys - check the offsets")
bp <- file.path(TD, "burn_table.parquet"); arrow::write_parquet(BT, bp)
say("merged: %s fires (%s kept, %.2f per season), %s burned-cell rows", format(nrow(ST), big.mark = ","),
    format(sum(ST$ResampleStatus %in% c("Kept", "Reassigned")), big.mark = ","), sum(ST$ResampleStatus %in% c("Kept", "Reassigned")) / NS, format(nrow(BT), big.mark = ","))
WR <- rbindlist(lapply(Z, function(z) { x <- copy(z$wr); if (!nrow(x)) return(NULL); if (!"FireZone" %in% names(x)) x[, FireZone := NA_character_]
  x[is.na(FireZone), FireZone := z$zone]; x[, .(FireZone, LowerWindSpeed, UpperWindSpeed, Quantile, Ratio)] }))
progressBar()

write_ds(el, "OutputEngineLandscape"); write_ds(lr, "LandscapeRasters")
if (nrow(WR)) write_ds(WR, "WindRatio")
write_ds(FL, "OutputFireList"); write_ds(FW, "OutputFireWeather"); write_ds(ST, "OutputFireStatistic")
write_ds(data.frame(FileName = bp, Description = sprintf("merged burned cells of %d zones", length(Z))), "OutputBurnTable")
say("wind ratio tables: %s", paste(sprintf("%s (%d rows)", WR[, .N, by = FireZone]$FireZone, WR[, .N, by = FireZone]$N), collapse = "; "))
progressBar(); progressBar(type = "end")
