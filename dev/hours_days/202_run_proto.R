# =============================================================================
# 202  Run C2F-W (Scott & Burgan) on the paired fire list, one fire per call
# =============================================================================
# C2F-W cannot pair a given weather file with a given ignition inside one call
# (weather files are drawn at random per simulation and every simulation uses
# the same ignition row), so each fire is its own call. A call on the whole
# 3.3 M-cell P26 grid costs ~45 s, almost all of it parsing the landscape, so
# each fire runs in a WINDOW around its origin: half-width 100 cells (15 km),
# doubled (200, 400, then the full grid) whenever the final burn touches a
# window edge that is not the grid's own edge. Only the final grid is written.
#
# Origins on non-fuel in FBFM40 (BurnP3+ ignited them on its own fuel map) move
# to the nearest burnable cell within 3 cells; otherwise the fire burns 0 ha.
#
# Output: <tag>/runs_r<RES>/chunk_NNNN.rds, each a list of
#   data.frame(key, area_ha, cells_burnt, window_hw, reruns, origin_moved)
#   and burnt  = named list key -> integer vector of GLOBAL cell ids (1-based,
#                row-major on the landscape grid)
# Chunks already on disk are skipped, so a stopped run resumes.
#
# Usage: Rscript 202_c2fw_run.R [tag=p026_LF2020] [result=4] [workers=60]
# =============================================================================
suppressMessages({ library(terra); library(parallel) })
args <- commandArgs(TRUE)
TAG  <- if (length(args) >= 1) args[1] else "p026_LF2020"
RES  <- as.integer(if (length(args) >= 2) args[2] else 4)
NW   <- as.integer(if (length(args) >= 3) args[3] else 60)
CHUNK <- as.integer(Sys.getenv("C2FW_CHUNK", "100"))   # prototype copy: chunk size from env (small sets need small chunks)

MODEL_DIR <- "F:/lucas-master/Models/California 30m BurnP3"
BASE <- file.path(MODEL_DIR, "data/c2fw", TAG)
EXE  <- file.path(MODEL_DIR, "tools/c2fw/bin/Cell2Fire.exe")
SMOKE <- as.integer(Sys.getenv("C2FW_SMOKE", "0"))   # >0: a random sample of this many fires, own folder
# C2FW_FMC (2026-09-24): foliar moisture content, %, for Van Wagner crown initiation (C2F-W default 100).
# Late-season Sierra conifer foliage runs ~80-95%; user chose 85.
FMC  <- Sys.getenv("C2FW_FMC", "")
FMC_ARG <- if (nzchar(FMC)) c("--fmc", FMC) else character(0)
cat(sprintf("foliar moisture: %s\n", if (nzchar(FMC)) paste0(FMC, "%") else "C2F-W default (100%)"))
# C2FW_CROWN=1 (2026-09-24): also write C2F-W's crown-state grid (CrownFire/Crown<sim>.asc, 1 = crowned) and keep
# the crowned cells per fire (chunk element `crown`, global cell ids) - for the foliar-moisture test
CROWN_ARG <- if (Sys.getenv("C2FW_CROWN") == "1") "--out-crown" else character(0)
# C2FW_EFFECTS=1 (2026-09-24): fire-effects outputs for every burned cell of every fire - chunk element `fx`, per fire
# an integer matrix aligned with `burnt`: fl_cm (max of surface/crown flame length), si (surface intensity kW/m),
# ci (crown intensity kW/m), crown (0/1), cfb_pct (crown fraction burned x 100). Summarised by 213.
EFFECTS <- Sys.getenv("C2FW_EFFECTS") == "1"
# C2FW_CBH_MULT (2026-09-24): multiply canopy base height in forest cells (non-forest keeps its 99 m sentinel).
# LANDFIRE CBH in P26 timber is low (TU5 median 1.5 m); crown share runs 2.1x the MTBS high-severity share there.
CBH_MULT <- suppressWarnings(as.numeric(Sys.getenv("C2FW_CBH_MULT", "1"))); if (is.na(CBH_MULT)) CBH_MULT <- 1
if (CBH_MULT != 1) cat(sprintf("canopy base height x %.2f in forest cells\n", CBH_MULT))
if (EFFECTS) CROWN_ARG <- c("--out-crown", "--out-fl", "--out-intensity", "--out-cfb")
# C2FW_MESSAGES=1 (2026-09-24): keep C2F-W's spread network (--output-messages: source cell, target cell, fire period,
# ROS). For each burned cell the FIRST message that reached it gives the cell that ignited it (-> spread direction,
# for slope along the spread path and the angle to the wind) and its arrival period (-> the hour it burned).
# Chunk element `msg`, per fire an integer matrix aligned with `burnt`: src (global cell id, NA for the ignition
# cell) and period (0 for the ignition cell). Used by 217.
MESSAGES <- Sys.getenv("C2FW_MESSAGES") == "1"
if (MESSAGES) CROWN_ARG <- c(CROWN_ARG, "--output-messages")
RUNS <- file.path(BASE, sprintf("runs_r%d%s", RES, if (SMOKE > 0) "_smoke" else ""))
dir.create(RUNS, showWarnings = FALSE)
WORK <- file.path(BASE, "work");                   dir.create(WORK, showWarnings = FALSE)

fl <- readRDS(file.path(BASE, sprintf("fires_r%d.rds", RES)))
fires <- fl$fires
if (SMOKE > 0) { set.seed(1); fires <- fires[sort(sample.int(nrow(fires), SMOKE)), ] }
hourly <- split(fl$hourly[, c("WS", "WD")], fl$hourly$key)[fires$key]
# longest fires first so the tail of the run is not one 25-day fire
ord <- order(-fires$FireDuration, fires$key)
chunks <- split(ord, ceiling(seq_along(ord) / CHUNK))
todo <- which(!file.exists(file.path(RUNS, sprintf("chunk_%04d.rds", seq_along(chunks)))))
cat(sprintf("%s fires in %d chunks; %d to run on %d workers\n", format(nrow(fires), big.mark = ","),
            length(chunks), length(todo), NW))
if (!length(todo)) quit(save = "no")

run_chunk <- function(ci) {
  suppressMessages(library(terra))
  if (!exists("L", envir = .GlobalEnv)) {             # once per worker: landscape into memory
    g <- rast(file.path(BASE, "landscape/fuels.tif"))
    lyr <- c(fuels = "fuels", elevation = "elevation", cbh = "cbh", cbd = "cbd", ccf = "ccf")
    if (file.exists(file.path(BASE, "landscape/hm.tif"))) lyr <- c(lyr, hm = "hm")   # tree height, if 200 built it
    L <- lapply(lyr,
                function(f) as.matrix(rast(file.path(BASE, "landscape", paste0(f, ".tif"))), wide = TRUE))
    if (CBH_MULT != 1) { fo <- !is.na(L$cbh) & L$cbh < 99; L$cbh[fo] <- L$cbh[fo] * CBH_MULT
      if (!is.null(L$hm)) { h <- !is.na(L$hm) & L$hm > 0 & !is.na(L$cbh); L$hm[h] <- pmax(L$hm[h], L$cbh[h] + 1) } }   # keep height > CBH
    assign("L", L, .GlobalEnv); assign("G", list(nr = nrow(g), nc = ncol(g), ext = as.vector(ext(g)), crs = crs(g)), .GlobalEnv)
    wd <- file.path(WORK, sprintf("w%05d", Sys.getpid())); dir.create(wd, showWarnings = FALSE)
    file.copy(file.path(BASE, "landscape/spain_lookup_table.csv"), wd, overwrite = TRUE)
    assign("WD", wd, .GlobalEnv)
  }
  burnable <- function(v) !is.na(v) & !(v >= 91 & v <= 99)
  # ASCII grids, NOT GeoTIFF (fixed 2026-09-24): C2F-W v1.0.1's DataGenerator picks the extension from
  # fuels.tif/.asc but then loads the optional grids only under names ending ".asc", so with .tif input
  # elevation, cbh, cbd and ccf are silently dropped (Data.csv columns empty) - no slope, no crown fire.
  # Every C2F-W run before this fix burned flat, surface-only.
  write_win <- function(r0, r1, c0, c1) {
    e <- G$ext
    unlink(file.path(WD, paste0(names(L), ".tif")))
    hdr <- c(sprintf("ncols %d", c1 - c0 + 1), sprintf("nrows %d", r1 - r0 + 1),
             sprintf("xllcorner %.1f", e[1] + (c0 - 1) * 150), sprintf("yllcorner %.1f", e[4] - r1 * 150),
             "cellsize 150", "NODATA_value -9999")
    for (nm in names(L)) {
      m <- L[[nm]][r0:r1, c0:c1, drop = FALSE]
      m[is.na(m)] <- -9999
      txt <- if (nm == "fuels") apply(m, 1, function(x) paste(as.integer(x), collapse = " ")) else
        apply(m, 1, function(x) paste(formatC(x, format = "f", digits = 3, drop0trailing = TRUE), collapse = " "))
      writeLines(c(hdr, txt), file.path(WD, paste0(nm, ".asc")))
    }
  }
  idx <- chunks[[ci]]
  res <- vector("list", length(idx)); burnt <- vector("list", length(idx)); names(burnt) <- fires$key[idx]
  crown <- burnt; fx <- burnt; msg <- burnt
  for (j in seq_along(idx)) {
    f <- fires[idx[j], ]
    r <- f$row; c <- f$col; moved <- FALSE
    if (!burnable(L$fuels[r, c])) {                    # nearest burnable within 3 cells
      best <- NULL; bd <- Inf
      for (dr in -3:3) for (dc in -3:3) {
        rr <- r + dr; cc <- c + dc
        if (rr < 1 || cc < 1 || rr > G$nr || cc > G$nc) next
        d <- dr^2 + dc^2
        if (burnable(L$fuels[rr, cc]) && d < bd) { bd <- d; best <- c(rr, cc) }
      }
      if (is.null(best)) {
        res[[j]] <- data.frame(key = f$key, area_ha = 0, cells_burnt = 0L, window_hw = 0L, reruns = 0L, origin_moved = NA)
        burnt[[j]] <- integer(0); next
      }
      r <- best[1]; c <- best[2]; moved <- TRUE
    }
    w <- hourly[[f$key]]
    tt <- format(as.POSIXct("2000-07-01 00:00:00", tz = "UTC") + 3600 * (seq_len(nrow(w)) - 1), "%Y-%m-%dT%H:%M:%S")
    writeLines(c("Instance,datetime,WS,WD,FireScenario",
                 sprintf("f,%s,%s,%s,%d", tt, w$WS, w$WD, f$scenario)), file.path(WD, "Weather.csv"))
    hw <- 100L; reruns <- 0L
    repeat {
      r0 <- max(1, r - hw); r1 <- min(G$nr, r + hw); c0 <- max(1, c - hw); c1 <- min(G$nc, c + hw)
      write_win(r0, r1, c0, c1)
      nc <- c1 - c0 + 1
      writeLines(c("Year,Ncell", sprintf("1,%d", (r - r0) * nc + (c - c0) + 1)), file.path(WD, "Ignitions.csv"))
      unlink(file.path(WD, c("Data.csv", "out")), recursive = TRUE); dir.create(file.path(WD, "out"))
      st <- system2(EXE, c("--input-instance-folder", shQuote(paste0(WD, "/")), "--output-folder", shQuote(paste0(WD, "/out/")),
                           "--nsims", "1", "--sim", "S", "--scenario", f$scenario, "--ignitions", "--final-grid", "--cros",
                           "--seed", f$seed %% 100000L + 1L, "--nthreads", "1", FMC_ARG, CROWN_ARG), stdout = FALSE, stderr = FALSE)
      gf <- list.files(file.path(WD, "out/Grids/Grids1"), "^ForestGrid[0-9]+\\.csv$", full.names = TRUE)
      if (st != 0 || !length(gf)) stop(sprintf("C2F-W failed on %s (status %s)", f$key, st))
      gf <- gf[order(as.integer(gsub("\\D", "", basename(gf))))]
      m <- as.matrix(read.csv(gf[length(gf)], header = FALSE))
      b <- which(m == 1, arr.ind = TRUE)
      edge <- nrow(b) > 0 && ((any(b[, 1] == 1) && r0 > 1) || (any(b[, 1] == nrow(m)) && r1 < G$nr) ||
                              (any(b[, 2] == 1) && c0 > 1) || (any(b[, 2] == ncol(m)) && c1 < G$nc))
      full <- r0 == 1 && c0 == 1 && r1 == G$nr && c1 == G$nc
      if (!edge || full) break
      hw <- if (hw >= 400L) max(G$nr, G$nc) else hw * 2L; reruns <- reruns + 1L
    }
    gcell <- as.integer((b[, 1] + r0 - 2) * G$nc + (b[, 2] + c0 - 1))
    burnt[[j]] <- gcell
    if (length(CROWN_ARG)) {
      cf <- list.files(file.path(WD, "out/CrownFire"), "^Crown.*\\.asc$", full.names = TRUE)
      cm <- if (length(cf)) as.matrix(read.table(cf[1], skip = 6)) else matrix(0L, nrow(m), ncol(m))
      k <- which(cm == 1, arr.ind = TRUE)
      crown[[j]] <- as.integer((k[, 1] + r0 - 2) * G$nc + (k[, 2] + c0 - 1))
    }
    if (MESSAGES) {                                    # first message into each burned cell: source + arrival period
      mf <- list.files(file.path(WD, "out/Messages"), "^MessagesFile.*\\.csv$", full.names = TRUE)
      li <- (b[, 1] - 1) * ncol(m) + b[, 2]            # window cell ids (1-based row-major) of the burned cells
      src <- rep(NA_integer_, length(li)); per <- rep(0L, length(li))
      if (length(mf) && file.size(mf[1]) > 0) {
        mm <- as.matrix(read.csv(mf[1], header = FALSE))
        mm <- mm[order(mm[, 2], mm[, 3]), , drop = FALSE]
        mm <- mm[!duplicated(mm[, 2]), , drop = FALSE]  # earliest arrival per target
        k <- match(li, mm[, 2])
        wc2g <- function(wc) { rr <- (wc - 1) %/% ncol(m) + 1; cc <- (wc - 1) %% ncol(m) + 1; as.integer((rr + r0 - 2) * G$nc + (cc + c0 - 1)) }
        ok <- !is.na(k); src[ok] <- wc2g(mm[k[ok], 1]); per[ok] <- as.integer(mm[k[ok], 3])
      }
      msg[[j]] <- cbind(src = src, period = per)
    }
    if (EFFECTS) {                                     # values at this fire's burned cells, window row-major
      li <- (b[, 1] - 1) * ncol(m) + b[, 2]
      rd <- function(dir, pre) { p <- list.files(file.path(WD, "out", dir), paste0("^", pre, ".*\\.asc$"), full.names = TRUE)
        if (!length(p)) return(rep(0, length(li)))
        v <- scan(p[1], skip = 6, quiet = TRUE); v <- v[li]; v[is.na(v) | v < 0] <- 0; v }
      fx[[j]] <- cbind(fl_cm = as.integer(round(100 * rd("MaxFlameLength", "MaxFlameLength"))),
                       si = as.integer(round(rd("SurfaceIntensity", "SurfaceIntensity"))),
                       ci = as.integer(round(rd("CrownIntensity", "CrownIntensity"))),
                       crown = as.integer(rd("CrownFire", "Crown") == 1),
                       cfb_pct = as.integer(round(100 * rd("CrownFractionBurn", "Cfb"))))
    }
    res[[j]] <- data.frame(key = f$key, area_ha = length(gcell) * 2.25, cells_burnt = length(gcell),
                           window_hw = hw, reruns = reruns, origin_moved = moved)
  }
  saveRDS(list(res = do.call(rbind, res), burnt = burnt, crown = if (length(CROWN_ARG)) crown else NULL,
               fx = if (EFFECTS) fx else NULL, msg = if (MESSAGES) msg else NULL),
          file.path(RUNS, sprintf("chunk_%04d.rds", ci)))
  ci
}

t0 <- Sys.time()
cl <- makePSOCKcluster(NW)
clusterExport(cl, c("BASE", "EXE", "RUNS", "WORK", "fires", "hourly", "chunks", "FMC_ARG", "CROWN_ARG", "EFFECTS", "CBH_MULT", "MESSAGES"))
done <- parLapplyLB(cl, todo, run_chunk)
stopCluster(cl)
cat(sprintf("done %d chunks in %.1f min\n", length(done), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
