# =============================================================================
# 219  Download RAWS hourly observations for a pyrome from FEMS (public, no key)
# =============================================================================
# Why: fire effects need burning-period winds. gridMET gives only a daily mean at 10 m, which never reaches the
# 40-50 km/h 20-ft wind active crown fire needs (217). RAWS measure at 20 ft (Rothermel's reference height), hourly,
# with gusts. FEMS (USDA Forest Service Fire Environment Mapping System) serves them without an account:
#   https://fems.fs2c.usda.gov/api/climatology/download-weather?stationIds=<id>&startDate=..&endDate=..
#     &dataset=observation&dataFormat=csv&dataIncrement=hourly
# Catalogue: data/wind/fems_ca_raws_catalogue.json (stationMetaData GraphQL, /api/climatology/graphql);
# station list for the pyrome: data/wind/p029_raws_stations.csv (inside P26 or within 10 km).
# The hourly archive starts in 2006 (tested: 2004 empty, 2006 complete).
# Writes data/wind/raws_hourly/<station_id>_<year>.csv; existing files are skipped (resumable).
# Usage: Rscript src/219_raws_download.R [first_year=2006] [last_year=2024] [parallel=4]
# =============================================================================
suppressMessages(library(parallel))
a <- commandArgs(TRUE)
Y0 <- as.integer(if (length(a) >= 1) a[1] else 2006); Y1 <- as.integer(if (length(a) >= 2) a[2] else 2024)
NP <- as.integer(if (length(a) >= 3) a[3] else 4)
M <- "F:/lucas-master/Models/California 30m BurnP3"
OUT <- file.path(M, "data/wind/raws_hourly"); dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
st <- read.csv(file.path(M, "data/wind/p029_raws_stations.csv"))
jobs <- expand.grid(id = st$station_id, yr = Y0:Y1)
jobs$f <- file.path(OUT, sprintf("%d_%d.csv", jobs$id, jobs$yr))
todo <- jobs[!file.exists(jobs$f), ]
cat(sprintf("%d stations x %d years = %d files; %d to fetch\n", nrow(st), Y1 - Y0 + 1, nrow(jobs), nrow(todo)))
get1 <- function(i) {
  j <- todo[i, ]
  u <- sprintf("https://fems.fs2c.usda.gov/api/climatology/download-weather?stationIds=%d&startDate=%d-01-01T00:00:00Z&endDate=%d-12-31T23:00:00Z&dataset=observation&dataFormat=csv&dataIncrement=hourly",
               j$id, j$yr, j$yr)
  tmp <- paste0(j$f, ".part")
  for (k in 1:4) {
    ok <- tryCatch({ utils::download.file(u, tmp, quiet = TRUE, mode = "wb"); TRUE }, error = function(e) FALSE)
    if (ok && file.exists(tmp) && file.size(tmp) > 100) { file.rename(tmp, j$f); return(file.size(j$f)) }
    if (ok && file.exists(tmp)) { file.rename(tmp, j$f); return(file.size(j$f)) }   # tiny = no data that year: keep as a marker
    Sys.sleep(5 * k)
  }
  NA
}
t0 <- Sys.time()
cl <- makePSOCKcluster(NP)                                           # a few at a time: a public server
clusterExport(cl, c("todo"))
sz <- unlist(parLapplyLB(cl, seq_len(nrow(todo)), get1))
stopCluster(cl)
cat(sprintf("fetched %d files, %.0f MB, %d failed, %.1f min\n", sum(!is.na(sz)), sum(sz, na.rm = TRUE) / 1e6, sum(is.na(sz)),
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))
