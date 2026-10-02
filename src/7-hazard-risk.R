# =============================================================================
# 7 - Hazard and Risk
# =============================================================================
# The Wildfire Risk to Communities V2 stack (Scott et al. 2024) without assets (ported from script 224):
#   HIBP        high-intensity burn probability = BP x FLEP8
#   CRPS        conditional risk to potential structures = sum over the six FSim flame-length classes of P(class | burns)
#               x the class's response for the cell's lifeform (Lifeform datasheet, % change); the loss a structure would
#               suffer if it stood in the cell and the cell burned
#   RPS         risk to potential structures = BP x cRPS (expected annual % loss), no housing layer - "potential"
#   RPSCLASS    percentile class of RPS over the fire zone's ground: < 40th, 40-70, 70-90, 90-95, > 95th
# BP and cRPS are "oozed" into non-burnable ground from burnable patches of at least RiskOption.MinimumPatchHa
# (RiskOption.OozePasses passes of an OozeWindowCells x OozeWindowCells mean), so towns carry their surroundings' risk.
# Lifeform per cell from the fuel type C2F-W burned (engine fuel code -> Fuel Type -> Lifeform).
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
progressBar(type = "begin", totalSteps = 3)
ro <- need("RiskOption"); WIN <- ro$OozeWindowCells; NP <- ro$OozePasses; MINP <- ro$MinimumPatchHa
el <- need("OutputEngineLandscape"); g <- terra::rast(el$FuelFileName[1]); NC <- ncell(g); CELL_HA <- prod(res(g)) / 1e4
fv <- values(g, mat = FALSE)
ft <- as.data.table(datasheet(prj, ds("FuelType"), lookupsAsFactors = FALSE)); lf <- as.data.table(datasheet(prj, ds("Lifeform"), lookupsAsFactors = FALSE))
life <- ft$Lifeform[match(fv, ft$ID)]; life[fv >= 91 & fv <= 99] <- NA; burnable <- !is.na(life)
miss <- setdiff(unique(life[burnable]), lf$Name); if (length(miss)) fail("fuel-type lifeform(s) with no Lifeform response: %s", paste(miss, collapse = ", "))
fx <- need("OutputFireEffects", c("Metric", "FileName")); fx[, code := metric_code("EffectsMetric", Metric)]; gv <- function(m) { f <- fx[code == m]$FileName; if (!length(f)) fail("stage 6 map %s missing", m); values(terra::rast(f[1]), mat = FALSE) }
FLP <- sapply(1:6, function(k) gv(paste0("FLP", k))); f8 <- gv("FLEP8")
ok <- burnable & !is.na(FLP[, 1])
chk <- rowSums(FLP[ok, , drop = FALSE]); if (anyNA(chk) || max(abs(chk - 1)) > 1e-5) fail("flame-length class shares do not sum to 1 (max |diff| %s)", format(max(abs(chk - 1))))
bpv <- values(terra::rast(read_ds("OutputBurnProbability")$FileName[1]), mat = FALSE); bpv[is.na(bpv) & burnable] <- 0; bpv[!burnable] <- NA
RF <- as.matrix(lf[, paste0("Response", 1:6), with = FALSE]); rownames(RF) <- lf$Name
cr <- rep(NA_real_, NC); for (L in lf$Name) { w <- which(ok & life == L); cr[w] <- -as.vector(FLP[w, , drop = FALSE] %*% RF[L, ]) }
hibp <- bpv * f8; hibp[burnable & !ok & !is.na(bpv)] <- 0
progressBar()

# ---- ooze into non-burnable ground ------------------------------------------------------------------------------------
tpl <- rast(g); pat <- patches(setValues(tpl, ifelse(burnable, 1, NA)), directions = 8)
pv <- values(pat, mat = FALSE); fq <- as.data.table(freq(pat)); big <- burnable & !(pv %in% fq[count * CELL_HA < MINP, value])
Wm <- matrix(1, WIN, WIN)
ooze <- function(v, decay) { src <- ifelse(big, v, if (decay) 0 else NA); src[burnable & !big] <- NA
  z <- setValues(tpl, src); for (i in seq_len(NP)) z <- focal(z, Wm, fun = "mean", na.rm = TRUE, na.policy = "all")
  o <- values(z, mat = FALSE); o[burnable] <- v[burnable]; o }
BPo <- ooze(bpv, TRUE); cRPSo <- ooze(cr, FALSE); RPS <- cRPSo * BPo
# percentile domain: cells whose fire-zone code is a defined Fire Zone (the map may carry neighbouring zones as context)
lr <- read_ds("LandscapeRasters"); fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))
inz <- if (!is.na(lr$FireZoneFileName[1]) && nrow(fzt)) values(terra::rast(lr$FireZoneFileName[1]), mat = FALSE) %in% fzt$ID else rep(TRUE, NC)
say("risk classes over %s cells of the defined fire zone(s)", format(sum(inz), big.mark = ","))
q <- quantile(RPS[inz & !is.na(RPS) & RPS > 0], c(.4, .7, .9, .95), na.rm = TRUE)
rcls <- ifelse(is.na(RPS), NA, 1L + findInterval(RPS, q)); rcls[!is.na(RPS) & RPS == 0] <- 1L
progressBar()

TD <- env$TransferDirectory
M <- list(HIBP = hibp, CRPS = cRPSo, RPS = RPS, RPSCLASS = rcls)
out <- rbindlist(lapply(names(M), function(k) { p <- file.path(TD, sprintf("risk_%s.tif", k))
  writeRaster(setValues(tpl, M[[k]]), p, overwrite = TRUE, datatype = if (k == "RPSCLASS") "INT1U" else "FLT4S", gdal = "COMPRESS=DEFLATE")
  data.table(Iteration = 1L, Timestep = 0L, Metric = metric_names("RiskMetric")[[k]], FileName = p) }))
write_ds(out, "OutputRisk")
k <- inz & ok; w <- bpv[k]
say("burn-weighted over the fire zone: cRPS %.1f (tree %.1f, shrub %.1f, grass %.1f); mean RPS %.3f %%/yr; RPS class edges %s",
    weighted.mean(cr[k], w), weighted.mean(cr[k & life == "Tree"], bpv[k & life == "Tree"]), weighted.mean(cr[k & life == "Shrub"], bpv[k & life == "Shrub"]),
    weighted.mean(cr[k & life == "Grass"], bpv[k & life == "Grass"]), mean(RPS[inz & burnable], na.rm = TRUE), paste(signif(q, 3), collapse = " / "))
progressBar(); progressBar(type = "end")
