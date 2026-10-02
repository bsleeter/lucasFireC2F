# WRC V2 cRPS and RPS for one zone, onto that zone's 150 m prep grid (generalises p029_225_fetch_wrc_risk.R to any SN&F zone).
# Source: public IIPP image services (imagery.geoplatform.gov, USFS_EDW_RMRS_WRC_*), scale cRPS x 0.1, RPS x 0.01, exported at
# WRC's native 30 m in 3,000-px tiles, mosaicked, averaged 5 x 5 onto the zone grid. User-requested download 2026-09-30 (P27, P28).
# Usage: Rscript fetch_wrc_risk_zone.R <tag e.g. p027> <prep result id in the SN&F library>
suppressMessages(library(terra)); terraOptions(progress = 0); say <- function(...) cat(sprintf(...), "\n", sep = "")
a <- commandArgs(TRUE); TAG <- a[1]; PREP <- as.integer(a[2])
SVC <- "https://imagery.geoplatform.gov/iipp/rest/services/Fire_Aviation/USFS_EDW_RMRS_WRC_%s/ImageServer/exportImage"
RAW <- file.path("F:/lucas-master/Data Sources/WRC", TAG); dir.create(RAW, showWarnings = FALSE, recursive = TRUE)
LAYERS <- list(crps = list(svc = "ConditionalRiskToPotentialStructures", scale = 0.1), rps = list(svc = "RiskToPotentialStructures", scale = 0.01))
g <- rast(list.files(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim.data/Scenario-%d/lucasFireC2F_LandscapeRasters", PREP), "^fuel", full.names = TRUE)[1])
e <- as.vector(ext(g)); TP <- 3000; TS <- TP * 30; xs <- seq(e[1], e[2] - 1, by = TS); ys <- seq(e[3], e[4] - 1, by = TS)
say("%s: frame %.0f x %.0f km -> %d x %d tiles of %d px at 30 m", TAG, (e[2] - e[1]) / 1000, (e[4] - e[3]) / 1000, length(xs), length(ys), TP)
for (key in names(LAYERS)) { L <- LAYERS[[key]]; fs <- character(0); t0 <- Sys.time()
  for (i in seq_along(xs)) for (j in seq_along(ys)) { x0 <- xs[i]; y0 <- ys[j]; x1 <- min(x0 + TS, e[2]); y1 <- min(y0 + TS, e[4])
    W <- round((x1 - x0) / 30); H <- round((y1 - y0) / 30); f <- file.path(RAW, sprintf("%s_30m_%d_%d.tif", key, i, j)); fs <- c(fs, f)
    if (file.exists(f) && file.info(f)$size > 1000) next
    q <- paste0(sprintf(SVC, L$svc), "?bbox=", paste(c(x0, y0, x1, y1), collapse = ","), "&bboxSR=5070&imageSR=5070&size=", W, ",", H,
                "&format=tiff&pixelType=U16&interpolation=RSP_NearestNeighbor&noData=&f=image")
    ok <- tryCatch(download.file(q, f, mode = "wb", quiet = TRUE, method = "libcurl"), error = function(e) 1)
    if (ok != 0 || file.info(f)$size < 1000) stop(sprintf("%s tile %d,%d: export failed", key, i, j))
    r <- rast(f); stopifnot(ncol(r) == W, nrow(r) == H, abs(xmin(r) - x0) < 1, abs(ymax(r) - y1) < 1) }
  m <- if (length(fs) > 1) do.call(merge, lapply(fs, rast)) else rast(fs); m <- m * L$scale
  out <- resample(aggregate(m, 5, fun = "mean", na.rm = TRUE), g, method = "near"); writeRaster(out, file.path(RAW, sprintf("%s_150m.tif", key)), overwrite = TRUE)
  mm <- global(out, c("min", "max", "mean"), na.rm = TRUE); if (anyNA(unlist(mm))) stop(key, ": summary is NA")
  say("%-5s %d tiles in %.0f s; 150 m range %.3f-%.3f, mean %.3f", key, length(fs), as.numeric(difftime(Sys.time(), t0, units = "secs")), mm$min, mm$max, mm$mean) }
