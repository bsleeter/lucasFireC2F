# Stages 4-5 (a result scenario) vs the validated own-draw run (202 chunks + 203 kept / bp): per-fire areas, kept population, BP map
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table); library(terra) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim")); S <- rsyncrosim::scenario(L, id)
B <- "F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own"
st <- as.data.table(datasheet(S, "lucasFireC2F_OutputFireStatistic", lookupsAsFactors = FALSE))
res <- rbindlist(lapply(list.files(file.path(B, "runs_r4"), "^chunk_\\d+\\.rds$", full.names = TRUE), function(f) as.data.table(readRDS(f)$res)))
st[, key := sprintf("i%05d_f%03d", ifelse(ResampleStatus == "Reassigned", 0L, Iteration), ifelse(ResampleStatus == "Reassigned", OriginalFireID, FireID))]
m <- merge(st, res[, .(key, area_ha)], by = "key")
d <- abs(m$Area - m$area_ha)
cat(sprintf("fires matched %d of %d (202: %d); identical area %d (%.2f%%); median |diff| %.1f ha; area total package %.0f vs 202 %.0f ha\n",
            nrow(m), nrow(st), nrow(res), sum(d < 1e-6), 100 * mean(d < 1e-6), median(d), sum(m$Area), sum(m$area_ha)))
k <- fread(file.path(B, "kept_r4.csv"))
cat(sprintf("kept fires: package %d, 203 %d; kept area %.0f vs %.0f ha\n", st[ResampleStatus %in% c("Kept", "Reassigned"), .N], nrow(k),
            st[ResampleStatus %in% c("Kept", "Reassigned"), sum(Area)], sum(k$c2fw_ha)))
bp <- rast(datasheet(S, "lucasFireC2F_OutputBurnProbability")$FileName[1]); ref <- rast(file.path(B, "bp_r4.tif")); ext(ref) <- ext(bp)
a <- values(bp, mat = FALSE); b <- values(ref, mat = FALSE); b[is.na(b)] <- 0; a[is.na(a)] <- 0
cat(sprintf("BP: mean package %.5f vs 203 %.5f; correlation %.4f; cells identical %.2f%%\n", mean(a), mean(b), cor(a, b), 100 * mean(abs(a - b) < 1e-9)))
