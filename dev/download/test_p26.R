# Data Download test on P26: a test library and test home, the download, then Prep (P1-P5) from the home with blank paths, compared with
# SN&F's P26 prep (result 32) and national wind table (result 81). Rscript dev/download/test_p26.R <check|download|prep|compare> [Datasets, comma separated]
# EXCEPT = datasets whose paths are set by hand until their download is tested (the LANDFIRE Product Service needs an email).
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucasFireC2F/dev/libs"; DS <- "F:/lucas-master/Data Sources"; HOME <- file.path(D, "Data Sources Test")
a <- commandArgs(TRUE); mode <- a[1]; datasets <- if (length(a) > 1) a[2] else NA_character_
ZONE <- "P26 Northern Sierra Nevada"; LIB <- file.path(D, "lucasFireC2F Data Download test.ssim")
# SN&F is on an older package version: read straight from its database, read only, never through the console (which might upgrade it)
SNF <- file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim")
sq <- function(q) { con <- DBI::dbConnect(RSQLite::SQLite(), SNF, flags = RSQLite::SQLITE_RO); on.exit(DBI::dbDisconnect(con)); as.data.table(DBI::dbGetQuery(con, q)) }
ref_file <- function(sid, sheet, col) { v <- sq(sprintf("SELECT %s v FROM lucasFireC2F_%s WHERE ScenarioId = %d", col, sheet, sid))$v; if (!length(v) || is.na(v[1])) return(NA_character_)
  p <- Sys.glob(file.path(paste0(SNF, ".data"), sprintf("Scenario-%d", sid), paste0("lucasFireC2F_", sheet), v[1])); if (length(p)) p[1] else NA_character_ }
L <- suppressWarnings(if (file.exists(LIB)) ssimLibrary(LIB) else ssimLibrary(LIB, packages = "lucasFireC2F"))
P <- rsyncrosim::project(L, "Definitions")
if (!file.exists(file.path(HOME, "manifest.csv")) && mode == "check") dir.create(HOME, recursive = TRUE, showWarnings = FALSE)
if (mode %in% c("check", "download")) {
  saveDatasheet(P, data.frame(DataSourcesHome = HOME), "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
  S <- rsyncrosim::scenario(P, "P26 Data Download")
  description(S) <- "Data Download test: pyrome P26 + 60 km into the test home (Data Sources Test); By region; every dataset unless the run names them. Compared with the local copies by dev/download/test_p26.R."
  saveDatasheet(S, data.frame(FireZone = ZONE), "lucasFireC2F_DataDownloadZone", append = FALSE)
  saveDatasheet(S, data.frame(CheckOnly = mode == "check", Datasets = datasets, Connections = 4L), "lucasFireC2F_DataDownloadOptions", append = FALSE)
  saveDatasheet(S, data.frame(StageNameId = "D - Data Download", RunOrder = 1L), "core_Pipeline", append = FALSE)
  t0 <- Sys.time(); r <- tryCatch(run(S), error = function(e) { cat("run error:", conditionMessage(e), "\n"); NULL })
  R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
  cat(sprintf("result %d, %.1f min\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  print(as.data.table(datasheet(R, "lucasFireC2F_OutputDataDownload", optional = TRUE))[, .(DataSource, Status, Files, SizeMb, Minutes, Next = substr(Next, 1, 110))])
  cat(tail(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]], 25), sep = "\n") }
if (mode == "prep") {
  zp <- as.data.frame(sq("SELECT * FROM lucasFireC2F_ZonePrepOption WHERE ScenarioId = 31")); zp$ZonePrepOptionId <- NULL; zp$ScenarioId <- NULL; zp$FireZone <- ZONE
  zp$CanopyHeightVintage <- NA   # the LF2020 CH zip holds the file LF2022 CH does (LC22_CH_220)
  hand <- list(); if (length(a) > 1 && a[2] == "hand") hand <- list(StatewideElevationFile = file.path(DS, "DEM/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif"), RoadsFile = Sys.getenv("ROADS"),
    BpsFile = file.path(DS, "BpS/LF2020_BPS_CONUS/LF2020_BPS_CONUS/Tif/LF2020_BPS_CONUS.tif"))
  saveDatasheet(P, as.data.frame(c(list(DataSourcesHome = HOME), hand)), "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
  S <- rsyncrosim::scenario(P, "P26 Prep from the home")
  description(S) <- "Data Download test: P1-P5 for P26 with every Data Sources path blank but those the run sets by hand (a=hand: elevation, roads, BpS), reading the test home. Options as SN&F scenario 31 with the canopy height of the LF2020 archive. Compared with SN&F results 32 and 81."
  saveDatasheet(S, zp, "lucasFireC2F_ZonePrepOption", append = FALSE)
  pipe <- c("P1 - Prepare Zone Landscape", "P2 - Fit Ignition Surfaces", "P3 - Extract Zone Weather", "P4 - MTBS Severity Target", "P5 - Burning-Period Wind (RAWS)")
  saveDatasheet(S, data.frame(StageNameId = pipe, RunOrder = seq_along(pipe)), "core_Pipeline", append = FALSE)
  t0 <- Sys.time(); r <- tryCatch(run(S), error = function(e) { cat("run error:", conditionMessage(e), "\n"); NULL })
  R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
  cat(sprintf("result %d, %.1f min\n", scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat(tail(strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]], 60), sep = "\n")
  mode <- "compare" }
if (mode == "compare") {
  ss <- as.data.table(rsyncrosim::scenario(P, summary = TRUE)); R <- rsyncrosim::scenario(P, ss[IsResult == "Yes" & grepl("Prep", Name), max(ScenarioId)]); cat("comparing result", scenarioId(R), "\n")
  g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), optional = TRUE))
  cmp <- function(a, b, nm) { if (is.null(a) || is.na(a) || is.null(b) || is.na(b)) return(cat(sprintf("  %-22s missing (%s / %s)\n", nm, a, b)))
    A <- rast(a); B <- rast(b); if (!compareGeom(A, B, stopOnError = FALSE)) { cat(sprintf("  %-22s GRID DIFFERS: %s vs %s\n", nm, paste(dim(A)[1:2], collapse = "x"), paste(dim(B)[1:2], collapse = "x"))); B <- resample(B, A, "near") }
    va <- values(A, mat = FALSE); vb <- values(B, mat = FALSE); both <- !is.na(va) & !is.na(vb)
    cat(sprintf("  %-22s cells %s | NA only in new %s, only in ref %s | both: %s differ (max |d| %s)\n", nm, format(length(va), big.mark = ","), format(sum(is.na(va) & !is.na(vb)), big.mark = ","),
      format(sum(!is.na(va) & is.na(vb)), big.mark = ","), format(sum(abs(va[both] - vb[both]) > 1e-6), big.mark = ","), signif(max(c(0, abs(va[both] - vb[both]))), 4))) }
  for (sh in c("LandscapeRasters", "CanopyRasters", "IgnitionGrid")) { a1 <- g(R, sh); for (col in grep("FileName", names(a1), value = TRUE)) cmp(a1[[col]][1], tryCatch(ref_file(32, sh, col), error = function(e) NA_character_), sub("FileName", "", col)) }
  print(g(R, "OutputDataSources")[, .(Stage, DataSource, Origin, Files, Sha = substr(Sha256, 1, 10), Note)])
  print(g(R, "SeverityTarget")[1:6]); print(sq("SELECT * FROM lucasFireC2F_SeverityTarget WHERE ScenarioId = 32 LIMIT 6"))
  w1 <- g(R, "WindRatio"); w0 <- sq("SELECT * FROM lucasFireC2F_WindRatio WHERE ScenarioId = 81")
  if (nrow(w1)) for (b in sort(unique(w1$LowerWindSpeed))) { q1 <- w1[LowerWindSpeed == b]; q0 <- w0[LowerWindSpeed == b]; f <- function(q, p) approx(q$Quantile, q$Ratio, p, ties = "ordered")$y
    cat(sprintf("  wind %4g km/h: %.2f / %.2f / %.2f  vs  %.2f / %.2f / %.2f\n", b, f(q1, .5), f(q1, .9), f(q1, .99), f(q0, .5), f(q0, .9), f(q0, .99))) }
  d1 <- g(R, "DailyWeather"); d0 <- fread("F:/lucas-master/Models/California 30m BurnP3/data/weather/zone_daily_raw.csv"); cat("daily weather rows", nrow(d1), "zones", uniqueN(d1$WeatherZone), "dates", min(d1$Date), max(d1$Date), "\n") }
