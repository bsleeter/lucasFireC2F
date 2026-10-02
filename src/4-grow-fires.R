# =============================================================================
# 4 - Grow Fires (C2F-W)
# =============================================================================
# Burns every sampled fire with Cell2Fire-World (Scott & Burgan), one call per fire (ported from the validated P26
# script 202). C2F-W cannot pair a weather file with an ignition inside one call, and a whole-landscape call spends most
# of its time parsing, so each fire runs in a WINDOW around its origin: half-width 100 cells, doubled (200, 400, then
# the whole grid) whenever the final burn touches a window edge that is not the landscape's own edge.
#   inputs    stage 1's engine grids, written per window as ASCII grids (C2F-W v1.0.1 silently drops elevation and canopy
#             from GeoTIFF input); stage 3's hourly wind and moisture scenario; foliar moisture; crown fire (--cros)
#   origins   on a non-burnable cell move to the nearest burnable cell within 3 cells, else the fire burns 0 ha
#   outputs   a burned-cell table (parquet: Iteration, FireID, Cell [landscape cell, 1-based row-major], Source [the cell
#             that ignited it, NA at the origin] and Period [arrival minute] when the spread network is saved) and one
#             statistics row per fire
# Fires are dealt to GrowthOption.WorkersPerJob parallel C2F-W processes (0 = all cores but one), longest first.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra); library(parallel); library(arrow) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))

EXE <- file.path(env$PackageDirectory, "bin", "windows", "Cell2Fire.exe"); LUT <- file.path(env$PackageDirectory, "bin", "spain_lookup_table.csv")
if (.Platform$OS.type != "windows") fail("this version bundles the Windows C2F-W binary only")
if (!file.exists(EXE)) fail("C2F-W binary not found at %s", EXE)
el <- need("OutputEngineLandscape", c("FuelFileName", "ElevationFileName", "CanopyBaseHeightFileName", "CanopyBulkDensityFileName", "CanopyCoverFileName"))
Fl <- need("OutputFireList", c("Cell", "SpreadDays", "MoistureScenario", "Seed"))
hw <- need("OutputFireWeather", c("WindSpeed", "WindDirection"))
go <- read_ds("GrowthOption")
FMC <- if (nrow(go) && !is.na(go$FoliarMoisture)) go$FoliarMoisture else 100
CROWN <- !nrow(go) || !isFALSE(go$CrownFire); NET <- nrow(go) && isTRUE(go$SaveSpreadNetwork)
NW <- if (nrow(go) && !is.na(go$WorkersPerJob) && go$WorkersPerJob > 0) go$WorkersPerJob else max(1, detectCores() - 1)
ms <- as.data.table(datasheet(prj, ds("MoistureScenario"), lookupsAsFactors = FALSE))
Fl[, scen := ms$ID[match(MoistureScenario, ms$Name)]]
g <- rast(el$FuelFileName[1]); NR <- nrow(g); NC <- ncol(g); E <- as.vector(ext(g)); CS <- res(g)[1]; CELL_HA <- prod(res(g)) / 1e4
files <- c(fuels = el$FuelFileName[1], elevation = el$ElevationFileName[1], cbh = el$CanopyBaseHeightFileName[1],
           cbd = el$CanopyBulkDensityFileName[1], ccf = el$CanopyCoverFileName[1])
if (!is.na(el$CanopyHeightFileName[1])) files <- c(files, hm = el$CanopyHeightFileName[1])
Fl[, `:=`(row = rowFromCell(g, Cell), col = colFromCell(g, Cell))]
hourly <- split(hw[, .(WindSpeed, WindDirection, Iteration, FireID, Hour)], by = c("Iteration", "FireID"), keep.by = FALSE)
Fl[, key := paste(Iteration, FireID, sep = ".")]
if (!all(Fl$key %in% names(hourly))) fail("%d fires have no hourly weather", sum(!Fl$key %in% names(hourly)))
say("%s fires on a %d x %d grid; %d parallel C2F-W processes; foliar moisture %g%%; crown fire %s; spread network %s",
    format(nrow(Fl), big.mark = ","), NR, NC, NW, FMC, if (CROWN) "on" else "off", if (NET) "saved" else "not saved")

WORK <- file.path(env$TempDirectory, "c2fw"); OUTD <- file.path(env$TempDirectory, "burn")
unlink(c(WORK, OUTD), recursive = TRUE)                    # the temp folder can hold an earlier run's chunks
dir.create(WORK, showWarnings = FALSE); dir.create(OUTD, showWarnings = FALSE)
ARGS <- c("--fmc", FMC, if (CROWN) "--cros", if (NET) "--output-messages")
CHUNK <- as.integer(max(1, min(100, ceiling(nrow(Fl) / (NW * 4)))))   # small runs: keep every process busy
ord <- order(-Fl$SpreadDays, Fl$Iteration, Fl$FireID)
chunks <- split(ord, ceiling(seq_along(ord) / CHUNK))

run_chunk <- function(ci) {
  suppressMessages({ library(terra); library(data.table) })
  if (!exists("L", envir = .GlobalEnv)) {
    assign("L", lapply(files, function(f) as.matrix(rast(f), wide = TRUE)), .GlobalEnv)
    wd <- file.path(WORK, sprintf("w%05d", Sys.getpid())); dir.create(wd, showWarnings = FALSE)
    file.copy(LUT, wd, overwrite = TRUE); assign("WD", wd, .GlobalEnv)
  }
  burnable <- function(v) !is.na(v) & !(v >= 91 & v <= 99)
  write_win <- function(r0, r1, c0, c1) {
    hdr <- c(sprintf("ncols %d", c1 - c0 + 1), sprintf("nrows %d", r1 - r0 + 1), sprintf("xllcorner %.1f", E[1] + (c0 - 1) * CS),
             sprintf("yllcorner %.1f", E[4] - r1 * CS), sprintf("cellsize %g", CS), "NODATA_value -9999")
    for (nm in names(L)) { m <- L[[nm]][r0:r1, c0:c1, drop = FALSE]; m[is.na(m)] <- -9999
      txt <- if (nm == "fuels") apply(m, 1, function(x) paste(as.integer(x), collapse = " ")) else
        apply(m, 1, function(x) paste(formatC(x, format = "f", digits = 3, drop0trailing = TRUE), collapse = " "))
      writeLines(c(hdr, txt), file.path(WD, paste0(nm, ".asc"))) }
  }
  idx <- chunks[[ci]]; st <- vector("list", length(idx)); bt <- vector("list", length(idx))
  for (j in seq_along(idx)) {
    f <- Fl[idx[j]]; r <- f$row; c <- f$col; moved <- FALSE
    if (!burnable(L$fuels[r, c])) { best <- NULL; bd <- Inf
      for (dr in -3:3) for (dc in -3:3) { rr <- r + dr; cc <- c + dc
        if (rr < 1 || cc < 1 || rr > NR || cc > NC) next
        d <- dr^2 + dc^2; if (burnable(L$fuels[rr, cc]) && d < bd) { bd <- d; best <- c(rr, cc) } }
      if (is.null(best)) { st[[j]] <- data.frame(Iteration = f$Iteration, FireID = f$FireID, Cells = 0L, Window = 0L, Reruns = 0L, OriginMoved = NA); next }
      r <- best[1]; c <- best[2]; moved <- TRUE }
    w <- hourly[[f$key]]
    tt <- format(as.POSIXct("2000-07-01 00:00:00", tz = "UTC") + 3600 * (seq_len(nrow(w)) - 1), "%Y-%m-%dT%H:%M:%S")
    writeLines(c("Instance,datetime,WS,WD,FireScenario", sprintf("f,%s,%s,%s,%d", tt, w$WindSpeed, w$WindDirection, f$scen)), file.path(WD, "Weather.csv"))
    hw <- 100L; reruns <- 0L
    repeat {
      r0 <- max(1, r - hw); r1 <- min(NR, r + hw); c0 <- max(1, c - hw); c1 <- min(NC, c + hw)
      write_win(r0, r1, c0, c1); nc <- c1 - c0 + 1
      writeLines(c("Year,Ncell", sprintf("1,%d", (r - r0) * nc + (c - c0) + 1)), file.path(WD, "Ignitions.csv"))
      unlink(file.path(WD, c("Data.csv", "out")), recursive = TRUE); dir.create(file.path(WD, "out"))
      s <- system2(EXE, c("--input-instance-folder", shQuote(paste0(WD, "/")), "--output-folder", shQuote(paste0(WD, "/out/")),
                          "--nsims", "1", "--sim", "S", "--scenario", f$scen, "--ignitions", "--final-grid",
                          "--seed", f$Seed %% 100000L + 1L, "--nthreads", "1", ARGS), stdout = FALSE, stderr = FALSE)
      gf <- list.files(file.path(WD, "out/Grids/Grids1"), "^ForestGrid[0-9]+\\.csv$", full.names = TRUE)
      if (s != 0 || !length(gf)) stop(sprintf("C2F-W failed on iteration %d fire %d (status %s)", f$Iteration, f$FireID, s))
      gf <- gf[order(as.integer(gsub("\\D", "", basename(gf))))]
      m <- as.matrix(read.csv(gf[length(gf)], header = FALSE)); b <- which(m == 1, arr.ind = TRUE)
      edge <- nrow(b) > 0 && ((any(b[, 1] == 1) && r0 > 1) || (any(b[, 1] == nrow(m)) && r1 < NR) ||
                              (any(b[, 2] == 1) && c0 > 1) || (any(b[, 2] == ncol(m)) && c1 < NC))
      if (!edge || (r0 == 1 && c0 == 1 && r1 == NR && c1 == NC)) break
      hw <- if (hw >= 400L) max(NR, NC) else hw * 2L; reruns <- reruns + 1L
    }
    gcell <- as.integer((b[, 1] + r0 - 2) * NC + (b[, 2] + c0 - 1))
    src <- rep(NA_integer_, length(gcell)); per <- rep(0L, length(gcell)); ros <- rep(NA_real_, length(gcell))
    if (NET) { mf <- list.files(file.path(WD, "out/Messages"), "^MessagesFile.*\\.csv$", full.names = TRUE)
      li <- (b[, 1] - 1) * ncol(m) + b[, 2]
      if (length(mf) && file.size(mf[1]) > 0) { mm <- as.matrix(read.csv(mf[1], header = FALSE))
        mm <- mm[order(mm[, 2], mm[, 3]), , drop = FALSE]; mm <- mm[!duplicated(mm[, 2]), , drop = FALSE]
        k <- match(li, mm[, 2]); ok <- !is.na(k)
        wc <- mm[k[ok], 1]; src[ok] <- as.integer((((wc - 1) %/% ncol(m) + 1) + r0 - 2) * NC + (((wc - 1) %% ncol(m) + 1) + c0 - 1))
        per[ok] <- as.integer(mm[k[ok], 3]); if (ncol(mm) >= 4) ros[ok] <- round(mm[k[ok], 4], 3) } }   # col 4: C2F-W's ROS, m/min
    bt[[j]] <- data.frame(Iteration = f$Iteration, FireID = f$FireID, Cell = gcell, Source = src, Period = per, ROSC2FW = ros)
    st[[j]] <- data.frame(Iteration = f$Iteration, FireID = f$FireID, Cells = length(gcell), Window = hw, Reruns = reruns, OriginMoved = moved)
  }
  arrow::write_parquet(do.call(rbind, bt[!vapply(bt, is.null, TRUE)]), file.path(OUTD, sprintf("chunk_%04d.parquet", ci)))
  do.call(rbind, st)
}

t0 <- Sys.time(); cl <- makePSOCKcluster(NW)
clusterExport(cl, c("EXE", "LUT", "files", "WORK", "OUTD", "ARGS", "NET", "Fl", "hourly", "chunks", "NR", "NC", "E", "CS"))
# Chunks are dealt one at a time as processes free up (longest fires first) and SyncroSim's progress bar advances as each
# chunk finishes - one step per chunk, a run-log line every ~10%. (parallel's own dispatcher, as clusterApplyLB uses it.)
nch <- length(chunks); nw <- min(length(cl), nch); stats <- vector("list", nch); done_fires <- 0L; nxt <- nw + 1L
progressBar(type = "begin", totalSteps = nch)
send <- function(node, ci) parallel:::sendCall(cl[[node]], run_chunk, list(ci), tag = ci)
for (i in seq_len(nw)) send(i, i)
for (k in seq_len(nch)) {
  d <- parallel:::recvOneResult(cl)
  if (inherits(d$value, "try-error") || inherits(d$value, "condition")) { stopCluster(cl); fail("C2F-W chunk %d failed: %s", d$tag, as.character(d$value)) }
  stats[[d$tag]] <- d$value; done_fires <- done_fires + length(chunks[[d$tag]])
  if (nxt <= nch) { send(d$node, nxt); nxt <- nxt + 1L }
  progressBar()
  if (k %% max(1L, nch %/% 10L) == 0L || k == nch)
    say("grew %s of %s fires (%.1f min)", format(done_fires, big.mark = ","), format(length(ord), big.mark = ","), as.numeric(difftime(Sys.time(), t0, units = "mins")))
}
stopCluster(cl)
S <- rbindlist(stats)

# ---- outputs ------------------------------------------------------------------------------------------------------------------
TD <- env$TransferDirectory; bf <- file.path(TD, "burn_table.parquet")
pq <- file.path(OUTD, sprintf("chunk_%04d.parquet", seq_along(chunks))); pq <- pq[file.exists(pq)]
if (length(pq) != length(chunks)) fail("%d of %d chunk outputs missing", length(chunks) - length(pq), length(chunks))
BT <- rbindlist(lapply(pq, function(f) as.data.table(arrow::read_parquet(f))))
setorder(BT, Iteration, FireID); arrow::write_parquet(BT, bf); rm(BT)
write_ds(data.frame(FileName = bf, Description = sprintf("burned cells of %d fires%s", nrow(S), if (NET) " with spread network" else "")), "OutputBurnTable")
lf <- read_ds("OutputEngineLandscape")$LandscapeFuelFileName[1]; ft <- as.data.table(datasheet(prj, ds("FuelType"), lookupsAsFactors = FALSE))
fo <- values(rast(lf), mat = FALSE)[Fl$Cell]
Fl[S, on = c("Iteration", "FireID"), `:=`(Cells = i.Cells, OriginMoved = i.OriginMoved)]
write_ds(Fl[, .(Iteration, FireID, Season, Cause, FireZone, FuelType = ft$Name[match(fo, ft$ID)], SpreadDays, HoursBurning,
                Area = Cells * CELL_HA)], "OutputFireStatistic")
say("done in %.1f min: %s burned cells; fires burning 0 ha %d; origins moved %d; window reruns %d", as.numeric(difftime(Sys.time(), t0, units = "mins")),
    format(sum(S$Cells), big.mark = ","), sum(S$Cells == 0), sum(S$OriginMoved, na.rm = TRUE), sum(S$Reruns))
progressBar(type = "end")
