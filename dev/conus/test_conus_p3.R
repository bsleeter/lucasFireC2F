# Phase 3 CONUS test: P2 with covariates built per zone (no covariate folder). (1) "CA inputs" project, P2 only on the CA prep
# result -> compare surfaces and fits with SN&F result 32. (2) "National inputs", P2 only on the national prep result 6.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
D <- "F:/lucas-master/Models/lucasFireC2F Dev"; DS <- "F:/lucas-master/Data Sources"
L <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F CONUS test.ssim"))); SNF <- suppressWarnings(ssimLibrary(file.path(D, "lucasFireC2F Sierra Nevada & Foothills.ssim")))
g <- function(S, n) as.data.table(datasheet(S, paste0("lucasFireC2F_", n), optional = TRUE, lookupsAsFactors = FALSE))
cov <- list(StatewideElevationFile = file.path(DS, "DEM/LF2020_Elev_220_CONUS/Tif/LC20_Elev_220.tif"), RoadsFile = file.path(DS, "Roads/LF2023_Roads_CONUS/Tif/LF2023_Roads_CONUS.tif"),
            NlcdLandCoverFile = file.path(DS, "NLCD/Annual NLCD/Annual_NLCD_LndCov_2010_CU_C1V0.tif"), HousingDensityFile = file.path(DS, "WUI/CONUS_WUI_cp12.shp"),
            PrismPrecipFile = file.path(DS, "Climate/prism/normals/PRISM_ppt_30yr_normal_4kmM4_annual_bil/PRISM_ppt_30yr_normal_4kmM4_annual_bil.bil"), GridMetFolder = file.path(DS, "Climate/gridMET"))
runp2 <- function(pn, dep, nm) { P <- rsyncrosim::project(L, pn); d <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE)
  d$IgnitionCovariateFolder <- NA; for (k in names(cov)) d[[k]] <- cov[[k]]; d$DataSourceId <- NULL; d$ProjectId <- NULL
  saveDatasheet(P, d, "lucasFireC2F_DataSource", append = FALSE, force = TRUE)
  S <- rsyncrosim::scenario(P, nm, sourceScenario = rsyncrosim::scenario(P, "P26 Prep test"), overwrite = TRUE)
  saveDatasheet(S, data.frame(StageNameId = "P2 - Fit Ignition Surfaces", RunOrder = 1L), "core_Pipeline"); dependency(S) <- dep
  description(S) <- sprintf("CONUS Phase 3 test (2.2.0): P2 only, ignition covariates built per zone from national sources (no covariate folder), on prep result %d.", dep)
  t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
  R <- rsyncrosim::scenario(P, max(rsyncrosim::scenario(P)[, "ScenarioId"])); lg <- sub(" +$", "", strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]])
  cat(sprintf("\n==== %s: result %d (%.1f min)\n", pn, scenarioId(R), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  cat(unique(grep("covariates|fit domain|presences|background|cross-validated|surface|Failure|Error|error", lg, value = TRUE)), sep = "\n"); R }
R1 <- runp2("CA inputs", 2L, "P26 P2 built covariates"); R2 <- runp2("National inputs", 6L, "P26 P2 national")
ref <- rsyncrosim::scenario(SNF, 32); cat("\n==== CA-inputs surfaces vs SN&F result 32\n")
a <- g(R1, "IgnitionGrid"); b <- g(ref, "IgnitionGrid")
for (cz in a$Cause) { x <- values(rast(a[Cause == cz]$FileName[1]), mat = FALSE); y <- values(rast(b[Cause == cz]$FileName[1]), mat = FALSE)
  cat(sprintf("  %-9s cells %s, nonzero %s/%s, max |rel d| %.3g, correlation %.6f\n", cz, format(length(x), big.mark = ","), format(sum(x > 0), big.mark = ","), format(sum(y > 0), big.mark = ","),
              max(abs(x - y)) / max(y), cor(x, y))) }
print(rbind(cbind(src = "built", g(R1, "OutputIgnitionFit")[, .(Cause, Presences, Background, CrossValidatedAUC)]), cbind(src = "ref r32", g(ref, "OutputIgnitionFit")[, .(Cause, Presences, Background, CrossValidatedAUC)])))
cat("\n==== national fit\n"); print(g(R2, "OutputIgnitionFit")[, .(Cause, Presences, Background, CrossValidatedAUC)])
