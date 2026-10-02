# standalone check of src/covariates.R: P26's P2 window (fit domain + zone grid), built from the national sources, against the
# California fit stack (Data/ignition_covariates), layer by layer, cell by cell
suppressPackageStartupMessages({ library(terra); library(sf); library(data.table) }); sf_use_s2(FALSE); terraOptions(memfrac = 0.5, progress = 0)
say <- function(...) cat(sprintf(...), "\n"); fail <- function(...) stop(sprintf(...)); LATTICE_X0 <- -2362425; LATTICE_Y0 <- 2585625
source("F:/lucasFireC2F/src/covariates.R")
DS <- "F:/lucas-master/Data Sources"
gf <- rast("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F Sierra Nevada & Foothills.ssim.data/Scenario-32/lucasFireC2F_LandscapeRasters/fuel_type.tif")
z <- read_sf("F:/lucasFireC2F/Data/zones/pyromes.gpkg"); z <- st_transform(z[z$ID == 1026, ], crs(gf)); dom <- st_buffer(st_union(z), 20000)
b <- st_bbox(dom); g <- as.vector(ext(gf)); RES <- 150
win <- ext(LATTICE_X0 + floor((min(b[["xmin"]], g[1]) - LATTICE_X0) / RES) * RES, LATTICE_X0 + ceiling((max(b[["xmax"]], g[2]) - LATTICE_X0) / RES) * RES,
           LATTICE_Y0 - ceiling((LATTICE_Y0 - min(b[["ymin"]], g[3])) / RES) * RES, LATTICE_Y0 - floor((LATTICE_Y0 - max(b[["ymax"]], g[4])) / RES) * RES)
cat("window", paste(as.vector(win), collapse = ", "), "\n")
src <- list(dem = file.path(DS, "DEM/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif"), roads = file.path(DS, "Roads/LF2023_Roads_CONUS/Tif/LF2023_Roads_CONUS.tif"),
            nlcd = file.path(DS, "NLCD/Annual NLCD/Annual_NLCD_LndCov_2010_CU_C1V0.tif"), wui = file.path(DS, "WUI/CONUS_WUI_cp12.shp"),
            prism = file.path(DS, "Climate/prism/normals/PRISM_ppt_30yr_normal_4kmM4_annual_bil/PRISM_ppt_30yr_normal_4kmM4_annual_bil.bil"), gridmet = file.path(DS, "Climate/gridMET"))
t0 <- Sys.time(); cf <- build_ignition_covariates(win, src, "F:/lucasFireC2F/dev/conus/cov_test"); cat(sprintf("built in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
for (f in cf) { a <- rast(f); r <- rast(file.path("F:/lucasFireC2F/Data/ignition_covariates", basename(f))); r <- crop(r, a, snap = "near")
  if (!compareGeom(a, r, stopOnError = FALSE)) { cat(sprintf("%-10s GRID DIFFERS %s vs %s\n", basename(f), paste(as.vector(ext(a)), collapse = ","), paste(as.vector(ext(r)), collapse = ","))); next }
  va <- values(a, mat = FALSE); vr <- values(r, mat = FALSE); both <- !is.na(va) & !is.na(vr); dd <- abs(va[both] - vr[both])
  cat(sprintf("%-10s cells %s | NA new/ref only %s/%s | differ %s (%.4f%%), max |d| %g\n", sub("[.]tif$", "", basename(f)), format(length(va), big.mark = ","),
              format(sum(is.na(va) & !is.na(vr)), big.mark = ","), format(sum(!is.na(va) & is.na(vr)), big.mark = ","), format(sum(dd > 0), big.mark = ","), 100 * mean(dd > 0), max(c(0, dd)))) }
