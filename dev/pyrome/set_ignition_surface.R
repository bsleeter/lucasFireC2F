# Puts a refitted ignition surface pair into a pyrome library (baseline + calibration scenarios), masked as 158 masks:
# surface x burnable, zero outside the zone. Writes <dev>/P<NN>/inputs/ign_{human,lightning}<tag>.tif.
# Usage: Rscript set_ignition_surface.R <pyrome> <tag>
suppressPackageStartupMessages({ library(terra); library(sf); library(rsyncrosim) }); sf_use_s2(FALSE)
a <- commandArgs(TRUE); PY <- as.integer(a[1]); TG <- a[2]
M <- "F:/lucas-master/Models/California 30m BurnP3"; ZD <- file.path(M, sprintf("spatial/pyromes_150m_LF2020/pyrome_%03d", PY)); IN <- sprintf("F:/lucas-master/Models/lucasFireC2F Dev/P%02d/inputs", PY)
fu <- rast(file.path(IN, "fuel_type.tif")); zone <- st_transform(read_sf(file.path(ZD, "zone.gpkg")), crs(fu))
inz <- !is.na(rasterize(vect(zone), fu)); burn <- !(fu %in% c(91, 92, 93, 98, 99))
out <- c()
for (cz in c("human", "lightning")) { r <- rast(file.path(M, sprintf("data/ignition/ign_%s_150m_LF2020%s.tif", cz, TG)))
  if (!compareGeom(r, fu, stopOnError = FALSE)) r <- resample(r, fu, "near")
  r <- ifel(inz & burn & !is.na(r), r, 0); f <- file.path(IN, sprintf("ign_%s%s.tif", cz, TG))
  writeRaster(r, f, overwrite = TRUE, datatype = "FLT8S"); out[cz] <- f
  cat(sprintf("%s: %s (sum %.4g, %d nonzero cells)\n", cz, f, global(r, "sum", na.rm = TRUE)[1, 1], global(r > 0, "sum", na.rm = TRUE)[1, 1])) }
L <- suppressWarnings(ssimLibrary(sprintf("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P%02d.ssim", PY))); P <- rsyncrosim::project(L, 1)
SC <- if (length(a) > 2) strsplit(a[3], ";")[[1]] else c(sprintf("P%02d Baseline LF2020", PY), sprintf("P%02d Calibration 1992-2020", PY))   # "none" = files only
for (nm in setdiff(SC, "none")) {
  saveDatasheet(rsyncrosim::scenario(P, nm), data.frame(Cause = c("Human", "Lightning"), FileName = unname(out[c("human", "lightning")])), "lucasFireC2F_IgnitionGrid")
  cat(nm, ": ignition grids set\n") }
