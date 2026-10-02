# Stage 2 fire list (a result scenario) vs 221's firelist_drawn.csv for p026_LF2020_own (same seed -> same fires)
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
id <- as.integer(commandArgs(TRUE)[1])
L <- suppressWarnings(ssimLibrary("F:/lucas-master/Models/lucasFireC2F Dev/lucasFireC2F P26.ssim"))
a <- as.data.table(datasheet(rsyncrosim::scenario(L, id), "lucasFireC2F_OutputFireList", lookupsAsFactors = FALSE))
b <- fread("F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own/firelist_drawn.csv")
setorder(a, Iteration, FireID); setorder(b, Iteration, FireID)
cat(sprintf("fires: package %d, 221 %d\n", nrow(a), nrow(b)))
if (nrow(a) == nrow(b)) {
  d <- max(abs(a$Latitude - b$Latitude)) + max(abs(a$Longitude - b$Longitude))
  cat(sprintf("same iteration/fire ids: %s; max |dlat|+|dlon| %.2e deg; seasons same %s; causes same %s\n",
      identical(a$Iteration, b$Iteration) && identical(a$FireID, b$FireID), d,
      all(a$Season == c(`401` = "Autumn", `402` = "Spring", `403` = "Summer", `404` = "Winter")[as.character(b$Season)]),
      all(a$Cause == c(`405` = "Human", `406` = "Lightning")[as.character(b$Cause)])))
  cat(if (d < 1e-6) "STAGE 2 REPRODUCES 221\n" else "STAGE 2 DIFFERS\n")
}
