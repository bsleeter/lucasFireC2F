# =============================================================================
# C1 - Import Fire Record
# =============================================================================
# Reads the FPA-FOD national fire occurrence database (Short 2022) and keeps every fire whose ignition point falls inside
# the fire zone boundary: discovery date, size, cause (Human; Natural -> Lightning; anything else unknown) and location.
# ALL sizes are kept - FSim's large-fire threshold (C3) is the zone's mean fire size over every record. Each fire's fire
# zone is read from the landscape's fire-zone map where that map exists. Existing FPA-FOD rows in HistoricalFire are
# replaced; rows from other sources (e.g. a user CSV) are kept.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(sf); library(terra) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
sf_use_s2(FALSE); progressBar(type = "begin", totalSteps = 2)
fi <- read_ds("FireRecordImport"); if (!nrow(fi)) fi <- data.table(FPAFODFileName = NA_character_, ZoneBoundaryFileName = NA_character_)
# blanks fall back to the project's Data Sources (FPA-FOD) and the zone boundary P1 wrote
if (is.na(fi$FPAFODFileName[1]) || !nzchar(fi$FPAFODFileName[1])) fi$FPAFODFileName <- read_data_sources("FpaFodFile")$FpaFodFile
if (is.na(fi$ZoneBoundaryFileName[1]) || !nzchar(fi$ZoneBoundaryFileName[1])) {
  zb <- read_ds("OutputZoneBoundary"); if (!nrow(zb)) fail("no zone boundary: set Fire Record Import or run P1 - Prepare Zone Landscape")
  fi$ZoneBoundaryFileName <- zb$FileName[1] }
if (!file.exists(fi$FPAFODFileName)) fail("FPA-FOD file not found: %s", fi$FPAFODFileName)
if (!file.exists(fi$ZoneBoundaryFileName)) fail("zone boundary file not found: %s", fi$ZoneBoundaryFileName)
zone <- st_union(st_make_valid(read_sf(fi$ZoneBoundaryFileName)))
bb <- st_bbox(st_transform(zone, 4326))
q <- sprintf("SELECT FOD_ID, DISCOVERY_DATE, FIRE_SIZE, NWCG_CAUSE_CLASSIFICATION AS CAUSE, LATITUDE, LONGITUDE, Shape FROM Fires
              WHERE LONGITUDE >= %f AND LONGITUDE <= %f AND LATITUDE >= %f AND LATITUDE <= %f AND FIRE_SIZE > 0",
             bb$xmin - 0.01, bb$xmax + 0.01, bb$ymin - 0.01, bb$ymax + 0.01)
f <- read_sf(fi$FPAFODFileName, query = q)
f <- f[st_within(st_transform(f, st_crs(zone)), zone, sparse = FALSE)[, 1], ]
progressBar()
d <- as.Date(f$DISCOVERY_DATE, format = "%m/%d/%Y"); if (anyNA(d)) d[is.na(d)] <- as.Date(f$DISCOVERY_DATE[is.na(d)])
if (anyNA(d)) fail("%d FPA-FOD discovery dates could not be read", sum(is.na(d)))
H <- data.table(Source = "FPA-FOD", FireID = as.character(f$FOD_ID), Date = format(d), AreaHa = round(f$FIRE_SIZE * 0.404686, 3),
                Cause = ifelse(f$CAUSE == "Human", "Human", ifelse(f$CAUSE == "Natural", "Lightning", NA_character_)),
                Latitude = f$LATITUDE, Longitude = f$LONGITUDE, FireZone = NA_character_)
fzp <- read_ds("LandscapeRasters")$FireZoneFileName
if (length(fzp) && !is.na(fzp[1])) { fz <- terra::rast(fzp[1]); fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))
  code <- terra::extract(fz, terra::project(vect(st_transform(st_geometry(f), 4326)), crs(fz)))[, 2]; H[, FireZone := fzt$Name[match(code, fzt$ID)]] }
old <- read_ds("HistoricalFire"); old <- old[Source != "FPA-FOD"]
write_ds(rbind(old[, names(H), with = FALSE], H), "HistoricalFire")
yr <- as.integer(substr(H$Date, 1, 4))
# C3 sets the large-fire threshold for the calibration years; here the same FSim rule (mean size, 73-ac floor) over the whole record
thr <- max(mean(H$AreaHa), 73 * 0.404686)
say("imported %s FPA-FOD fires inside the zone boundary, %d-%d (%s >= %.1f ha, the FSim-rule threshold of this record; C3 sets it for the calibration years; causes: %s)",
    format(nrow(H), big.mark = ","), min(yr), max(yr), format(sum(H$AreaHa >= thr), big.mark = ","), thr,
    paste(names(table(H$Cause, useNA = "ifany")), table(H$Cause, useNA = "ifany"), collapse = ", "))
progressBar(); progressBar(type = "end")
