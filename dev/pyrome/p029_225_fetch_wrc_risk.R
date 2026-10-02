# =============================================================================
# 225  WRC V2 cRPS and RPS for pyrome 26, onto the C2F-W 150 m grid
# =============================================================================
# The pyrome-scale sibling of 135 (hexagon era): the same public IIPP image services, the same scale factors
# (cRPS x 0.1, RPS x 0.01, verified on hexagon 21 data), exported at WRC's native 30 m in 3,000-px tiles (the
# P26 frame is ~6,300 x 13,000 px, beyond one export), mosaicked, then averaged 5 x 5 onto the 150 m zone grid.
# User-approved download 2026-09-25 (P26); P29 copy approved 2026-09-27.
# Usage: ZONE_SYSTEM=pyrome Rscript src/225_fetch_wrc_risk_pyrome.R
# Writes: F:/lucas-master/Data Sources/WRC/p026/<layer>_30m_<i>_<j>.tif (as served) and <layer>_150m.tif (scaled)
# =============================================================================
suppressMessages({library(terra)})
source("F:/lucas-master/Models/California 30m BurnP3/src/00_config.R")
terraOptions(progress = 0)
say <- function(...) cat(sprintf(...), "\n", sep = "")
SVC <- "https://imagery.geoplatform.gov/iipp/rest/services/Fire_Aviation/USFS_EDW_RMRS_WRC_%s/ImageServer/exportImage"
RAW <- "F:/lucas-master/Data Sources/WRC/p029"; dir.create(RAW, showWarnings = FALSE, recursive = TRUE)
LAYERS <- list(crps = list(svc = "ConditionalRiskToPotentialStructures", scale = 0.1),
               rps  = list(svc = "RiskToPotentialStructures",            scale = 0.01))
g <- rast(file.path(zone_folder(2020L, 1029L), "Fuels.tif")); e <- as.vector(ext(g))
TP <- 3000; TS <- TP * 30
xs <- seq(e[1], e[2] - 1, by = TS); ys <- seq(e[3], e[4] - 1, by = TS)
say("frame %.0f x %.0f km -> %d x %d tiles of %d px at 30 m", (e[2] - e[1]) / 1000, (e[4] - e[3]) / 1000, length(xs), length(ys), TP)
for (key in names(LAYERS)) {
  L <- LAYERS[[key]]; fs <- character(0); t0 <- Sys.time()
  for (i in seq_along(xs)) for (j in seq_along(ys)) {
    x0 <- xs[i]; y0 <- ys[j]; x1 <- min(x0 + TS, e[2]); y1 <- min(y0 + TS, e[4])
    W <- round((x1 - x0) / 30); H <- round((y1 - y0) / 30)
    f <- file.path(RAW, sprintf("%s_30m_%d_%d.tif", key, i, j)); fs <- c(fs, f)
    if (file.exists(f) && file.info(f)$size > 1000) next
    q <- paste0(sprintf(SVC, L$svc), "?bbox=", paste(c(x0, y0, x1, y1), collapse = ","),
                "&bboxSR=5070&imageSR=5070&size=", W, ",", H,
                "&format=tiff&pixelType=U16&interpolation=RSP_NearestNeighbor&noData=&f=image")
    ok <- tryCatch(download.file(q, f, mode = "wb", quiet = TRUE, method = "libcurl"), error = function(e) 1)
    if (ok != 0 || file.info(f)$size < 1000) stop(sprintf("%s tile %d,%d: export failed: %s", key, i, j,
      paste(tryCatch(readLines(f, warn = FALSE)[1:2], error = function(e) ""), collapse = " ")))
    r <- rast(f); stopifnot(ncol(r) == W, nrow(r) == H, abs(xmin(r) - x0) < 1, abs(ymax(r) - y1) < 1)
  }
  m <- if (length(fs) > 1) do.call(merge, lapply(fs, rast)) else rast(fs)
  m <- m * L$scale
  a <- resample(aggregate(m, 5, fun = "mean", na.rm = TRUE), g, method = "near")
  writeRaster(a, file.path(RAW, sprintf("%s_150m.tif", key)), overwrite = TRUE)
  mm <- global(a, c("min", "max", "mean"), na.rm = TRUE)
  if (anyNA(unlist(mm))) stop(key, ": summary is NA")                     # [[checks-must-not-return-na]]
  say("%-5s %d tiles in %.0f s; 150 m range %.3f-%.3f, mean %.3f", key, length(fs),
      as.numeric(difftime(Sys.time(), t0, units = "secs")), mm$min, mm$max, mm$mean)
}
