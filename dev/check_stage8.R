# Stage 8 outputs of a result scenario: per-burn table aggregates to stage 6's maps; perimeters; severity fit; maps.
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra); library(arrow); library(sf) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); R <- rsyncrosim::scenario(L, id)
g <- function(n) as.data.table(datasheet(R, paste0("lucasFireC2F_", n), lookupsAsFactors = FALSE))
be <- as.data.table(arrow::read_parquet(g("OutputBurnEffects")$FileName[1]))
cat(sprintf("per-burn table: %s rows, %d columns; C2F-W ROS present on %.1f%% of rows\n", format(nrow(be), big.mark = ","), ncol(be), 100 * mean(!is.na(be$ROSC2FW))))
fx <- g("OutputFireEffects"); em <- as.data.table(datasheet(rsyncrosim::project(L, 1), "lucasFireC2F_EffectsMetric")); fx[, Metric := em$Code[match(Metric, em$Name)]]; cfl <- values(rast(fx[Metric == "CFL"]$FileName[1]), mat = FALSE); pc <- values(rast(fx[Metric == "PCROWN"]$FileName[1]), mat = FALSE)
a8 <- be[, .(n = .N, cfl = mean(FlameLengthFt), pc = mean(FireType >= 2)), by = Cell][n >= 10]
cat(sprintf("aggregated vs stage 6 maps (%d cells >= 10 burns): max |CFL diff| %.4f ft (table rounded to 0.01 ft); max |P(crown) diff| %.1e\n",
            nrow(a8), max(abs(a8$cfl - cfl[a8$Cell])), max(abs(a8$pc - pc[a8$Cell]))))
pf <- st_read(g("OutputFirePerimeter")$FileName[1], quiet = TRUE); st <- g("OutputFireStatistic")[ResampleStatus %in% c("Kept", "Reassigned")]
ra <- as.numeric(st_area(pf)) / 1e4 / pf$AreaHa
cat(sprintf("perimeters: %d (kept fires %d); polygon area / fire area median %.3f, range %.3f-%.3f; fire stats high-severity share filled on %d fires\n",
            nrow(pf), nrow(st), median(ra), min(ra), max(ra), sum(!is.na(st$HighSeverityShare))))
print(dcast(g("OutputSeverityFit"), FuelGroup ~ SeverityClass, value.var = "Model")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
print(dcast(g("OutputSeverityFit"), FuelGroup ~ SeverityClass, value.var = "Target")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
sm <- g("OutputSeverityMap"); smm <- as.data.table(datasheet(rsyncrosim::project(L, 1), "lucasFireC2F_SeverityMetric")); sm[, Metric := smm$Code[match(Metric, smm$Name)]]; cat("severity maps:", nrow(sm), "files for seasons", paste(range(sm$Iteration), collapse = "-"), "\n")
m1 <- values(rast(sm[Iteration == min(Iteration) & Metric == "MTBS"]$FileName[1]), mat = FALSE); cat("season", min(sm$Iteration), "MTBS classes:", paste(names(table(m1)), table(m1), collapse = ", "), "\n")
