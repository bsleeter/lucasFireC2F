# MTBS severity target for one pyrome, in the package's SeverityTarget form (class shares by fuel group).
# MTBS thematic severity of the post-vintage fires (2021-2024) on the PRE-fire LF2020 FBFM40 at 30 m (as script 215 did for P26):
# classes 1 unburned-low, 2 low, 3 moderate, 4 high (5/6 excluded). Fuel groups as the package's FuelType groups.
# Slash-Blowdown copies Timber-Understory when too few pixels (as the P26 default does).
# Usage: Rscript severity_target.R <pyrome number> <out csv> [first year = 2021] [last year = 2024]
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); OUT <- a[2]; Y1 <- if (length(a) > 2) as.integer(a[3]) else 2021L; Y2 <- if (length(a) > 3) as.integer(a[4]) else 2024L
SRC <- "F:/lucas-master/Data Sources"
zone <- read_sf("F:/lucas-master/Models/California 30m BurnP3/data/zones/pyromes.gpkg"); zone <- zone[zone$PYROME == PY, ]
f30 <- rast(Sys.glob(file.path(SRC, "FBFM/LF2020_FBFM40_CONUS*/LF2020_FBFM40_CONUS/Tif/LF2020_FBFM40_CONUS.tif")))
zv <- vect(st_transform(zone, crs(f30))); f30 <- crop(f30, zv, snap = "out"); levels(f30) <- NULL
zin <- !is.na(values(rasterize(zv, f30, field = 1), mat = FALSE)); fv <- values(f30, mat = FALSE)
grp <- function(x) { y <- rep(NA_character_, length(x)); y[x %in% 101:109] <- "Grass"; y[x %in% 121:124] <- "Grass-Shrub"; y[x %in% 141:149] <- "Shrub"
  y[x %in% 161:165] <- "Timber-Understory"; y[x %in% 181:189] <- "Timber Litter"; y[x %in% 201:204] <- "Slash-Blowdown"; y }
obs <- rbindlist(lapply(Y1:Y2, function(y) { s <- crop(rast(file.path(SRC, "MTBS/Burn Severity", sprintf("mtbs_CONUS_%d.tif", y))), f30)
  if (!compareGeom(s, f30, stopOnError = FALSE)) s <- resample(s, f30, method = "near"); sv <- values(s, mat = FALSE); k <- zin & sv %in% 1:4
  cat(sprintf("%d: %s burned 30 m pixels (classes 1-4)\n", y, format(sum(k), big.mark = ","))); data.table(fb = fv[k], sev = sv[k]) }))
obs[, g := grp(fb)]
T <- obs[!is.na(g), .(Pixels = .N, UnburnedLow = mean(sev == 1), Low = mean(sev == 2), Moderate = mean(sev == 3), High = mean(sev == 4)), by = .(FuelGroup = g)][order(-Pixels)]
if (!"Slash-Blowdown" %in% T$FuelGroup || T[FuelGroup == "Slash-Blowdown"]$Pixels < 1000) {
  T <- T[FuelGroup != "Slash-Blowdown"]; T <- rbind(T, T[FuelGroup == "Timber-Understory"][, `:=`(FuelGroup = "Slash-Blowdown", Pixels = 0L)]) }
print(T[, lapply(.SD, function(x) if (is.numeric(x) && max(x) <= 1) round(x, 4) else x)])
fwrite(T[, .(FuelGroup, UnburnedLow = round(UnburnedLow, 4), Low = round(Low, 4), Moderate = round(Moderate, 4), High = round(High, 4), Pixels)], OUT)
cat("wrote", OUT, "\n")
