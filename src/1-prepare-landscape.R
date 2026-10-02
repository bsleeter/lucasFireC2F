# =============================================================================
# 1 - Prepare Landscape
# =============================================================================
# Turns the scenario's landscape and canopy maps into the grids C2F-W reads (ported from the validated P26 script 200):
#   fuel        recent-fire rules first (FireAgeRule: a burnable cell burned MinimumYears-MaximumYears ago takes the rule's
#               replacement fuel type; an empty sheet with a Years Since Fire map takes the package defaults 0-2 yr -> NB9,
#               3-5 -> TL1, 6-10 -> TL8), then each fuel type's EngineFuelModel (the Scott & Burgan model C2F-W is given,
#               which remaps the models C2F-W v1.0.1 lacks) -> its FBFM40 code; non-burnable types -> their NB code (91-99).
#   canopy      a cell with no canopy base height or bulk density is non-forest: CBH 99 m, CBD 0 (no crown fire).
#               Cover -> fraction (NA -> 0). Tree height: -9999 on non-forest (C2F-W's "unknown"), else at least CBH + 1 m
#               (C2F-W stops when height < CBH).
# Every map must share the fuel map's grid. Grids are written as C2F-W's own test data are (uncompressed Float64,
# one-row strips) because its TIFF reader fails silently on others; stage 4 converts windows to ASCII.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
progressBar(type = "begin", totalSteps = 4)

lr <- need("LandscapeRasters", c("FuelFileName", "ElevationFileName", "WeatherZoneFileName"))
cr <- need("CanopyRasters", c("CanopyBaseHeightFileName", "CanopyBulkDensityFileName", "CanopyCoverFileName"))
rr <- function(sheet, col) read_raster(sheet, col)
fuel <- rr("LandscapeRasters", "FuelFileName"); elev <- rr("LandscapeRasters", "ElevationFileName")
for (k in list(list("ElevationFileName", elev), list("WeatherZoneFileName", rr("LandscapeRasters", "WeatherZoneFileName"))))
  if (!compareGeom(fuel, k[[2]], stopOnError = FALSE)) fail("%s is not on the fuel map's grid", k[[1]])

ft <- as.data.table(datasheet(prj, ds("FuelType"), lookupsAsFactors = FALSE))
fm <- as.data.table(datasheet(prj, ds("FuelModel"), lookupsAsFactors = FALSE))
fv <- values(fuel, mat = FALSE)
if (anyNA(fv)) fail("the fuel map has %d NA cells - give them a non-burnable fuel type", sum(is.na(fv)))
codes <- sort(unique(fv))
miss <- setdiff(codes, ft$ID); if (length(miss)) fail("fuel map codes with no Fuel Type: %s", paste(miss, collapse = ", "))
bad <- ft[ID %in% codes & !NonBurnable & !(EngineFuelModel %in% fm$Name)]
if (nrow(bad)) fail("burnable fuel types whose 'Fuel Model Sent to C2F-W' is not a Fuel Model: %s", paste(bad$Name, collapse = ", "))
progressBar()

# ---- recent-fire rules --------------------------------------------------------------------------------------------------
rules <- read_ds("FireAgeRule")
burnable <- !(fv %in% ft[NonBurnable == TRUE, ID])
# An empty rules sheet used to mean no rules, silently: P27's scenarios built in Studio ran without them and burned 2020's Creek and
# Castle scars as unburned timber. With a Years Since Fire map and no rules, the package defaults apply (the rules P26 and P29 were
# calibrated with); to run without rules, leave the Years Since Fire map out of the landscape.
if (!nrow(rules) && !is.na(lr$FireAgeFileName[1]) && nzchar(lr$FireAgeFileName[1])) {
  rules <- data.table(MinimumYears = c(0L, 3L, 6L), MaximumYears = c(2L, 5L, 10L), FuelType = NA_character_, ReplacementFuelType = c("NB9", "TL1", "TL8"))
  say("Recent Fire Fuel Rules is empty: using the package defaults (0-2 yr -> NB9, 3-5 yr -> TL1, 6-10 yr -> TL8)") }
if (nrow(rules)) {
  if (is.na(lr$FireAgeFileName[1])) fail("Recent Fire Fuel Rules are set but the landscape has no Years Since Fire map")
  age <- rr("LandscapeRasters", "FireAgeFileName")
  if (!compareGeom(fuel, age, stopOnError = FALSE)) fail("Years Since Fire is not on the fuel map's grid")
  av <- values(age, mat = FALSE); fv0 <- fv
  for (i in seq_len(nrow(rules))) { r <- rules[i]
    to <- ft$ID[match(as.character(r$ReplacementFuelType), ft$Name)]
    if (is.na(to)) fail("rule %d: replacement fuel type %s is not a Fuel Type", i, r$ReplacementFuelType)
    k <- burnable & !is.na(av) & av >= r$MinimumYears & av <= r$MaximumYears
    if (!is.na(r$FuelType)) k <- k & fv0 == ft$ID[match(as.character(r$FuelType), ft$Name)]
    fv[k] <- to
    say("recent-fire rule %d (%d-%d yr%s -> %s): %s cells", i, r$MinimumYears, r$MaximumYears,
        if (is.na(r$FuelType)) "" else paste0(", ", r$FuelType), r$ReplacementFuelType, format(sum(k), big.mark = ","))
  }
}
land <- setValues(rast(fuel), fv)

# ---- engine fuel codes ----------------------------------------------------------------------------------------------------
ft[, EngineCode := ifelse(NonBurnable, ifelse(ID >= 91 & ID <= 99, ID, 99L), fm$ID[match(EngineFuelModel, fm$Name)])]
ev <- ft$EngineCode[match(fv, ft$ID)]
if (anyNA(ev)) fail("%d cells have no engine fuel code", sum(is.na(ev)))
rmp <- ft[ID %in% unique(fv) & !NonBurnable & EngineFuelModel != FuelModel]
for (i in seq_len(nrow(rmp))) say("sent to C2F-W as %s: %s (%s cells)", rmp$EngineFuelModel[i], rmp$Name[i], format(sum(fv == rmp$ID[i]), big.mark = ","))
eng <- setValues(rast(fuel), ev)
progressBar()

# ---- canopy -----------------------------------------------------------------------------------------------------------------
cbh <- rr("CanopyRasters", "CanopyBaseHeightFileName"); cbd <- rr("CanopyRasters", "CanopyBulkDensityFileName")
cc  <- rr("CanopyRasters", "CanopyCoverFileName");      ch  <- rr("CanopyRasters", "CanopyHeightFileName")
for (x in list(cbh, cbd, cc, ch)) if (!is.null(x) && !compareGeom(fuel, x, stopOnError = FALSE)) fail("a canopy map is not on the fuel map's grid")
nonfor <- is.na(cbh) | is.na(cbd)
cbh2 <- ifel(nonfor, 99, cbh); cbd2 <- ifel(nonfor, 0, cbd)
ccf <- cc / 100; ccf <- ifel(is.na(ccf), 0, ccf)
hm <- if (is.null(ch)) NULL else ifel(nonfor | is.na(ch), -9999, max(ch, cbh + 1))
nf <- sum(!values(nonfor, mat = FALSE))
say("forest cells %s of %s; CBH median %.1f m, CBD median %.3f%s", format(nf, big.mark = ","), format(ncell(fuel), big.mark = ","),
    median(values(cbh, mat = FALSE), na.rm = TRUE), median(values(cbd, mat = FALSE), na.rm = TRUE),
    if (is.null(hm)) "; no canopy height map (C2F-W assumes height = 2 x CBH)" else "")
chk <- function(r, nm, lo, hi) { v <- values(r, mat = FALSE); if (anyNA(v)) fail("%s has %d NA cells after preparation", nm, sum(is.na(v)))
  g <- range(v); if (g[1] < lo || g[2] > hi) fail("%s range %.3f-%.3f is outside %g-%g", nm, g[1], g[2], lo, hi) }
chk(eng, "engine fuel", 91, 204); chk(cbh2, "CBH", 0, 100); chk(cbd2, "CBD", 0, 1); chk(ccf, "canopy cover", 0, 1)
ne <- sum(is.na(values(elev, mat = FALSE))); ele2 <- ifel(is.na(elev), 0, elev); if (ne) say("elevation: %d NA cells set to 0", ne)
progressBar()

# ---- write --------------------------------------------------------------------------------------------------------------------
TD <- env$TransferDirectory
w <- function(r, f) { p <- file.path(TD, f); writeRaster(r, p, overwrite = TRUE, datatype = "FLT8S", NAflag = -9999,
                                                          gdal = c("COMPRESS=NONE", "BLOCKYSIZE=1", "TILED=NO")); p }
out <- data.frame(FuelFileName = w(eng, "engine_fuel.tif"), ElevationFileName = w(ele2, "engine_elevation.tif"),
                  CanopyBaseHeightFileName = w(cbh2, "engine_cbh.tif"), CanopyBulkDensityFileName = w(cbd2, "engine_cbd.tif"),
                  CanopyCoverFileName = w(ccf, "engine_ccf.tif"), CanopyHeightFileName = if (is.null(hm)) NA else w(hm, "engine_hm.tif"),
                  LandscapeFuelFileName = w(land, "landscape_fuel.tif"))
write_ds(out, "OutputEngineLandscape")
tab <- sort(table(ev), decreasing = TRUE); n8 <- min(8, length(tab))
say("engine fuel codes (top %d by cells): %s", n8, paste(sprintf("%s %.1f%%", names(tab)[1:n8], 100 * tab[1:n8] / length(ev)), collapse = ", "))
progressBar()
progressBar(type = "end")
