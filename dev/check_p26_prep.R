# Checks prep_zone_landscape.R output for P26 against the validated own-draw run's C2F-W inputs (script 200 + scar rule)
suppressMessages(library(terra))
I <- "F:/lucas-master/Models/lucasFireC2F Dev/P26/inputs"; O <- "F:/lucas-master/Models/California 30m BurnP3/data/c2fw/p026_LF2020_own/landscape"
fb <- rast(file.path(I, "fuel_type.tif")); yr <- rast(file.path(I, "years_since_fire.tif")); ref <- rast(file.path(O, "fuels.tif"))
ext(ref) <- ext(fb)
REMAP <- c(`109` = 108, `141` = 142, `184` = 183, `187` = 188, `201` = 186, `202` = 145, `203` = 145, `204` = 145)
v <- values(fb, mat = FALSE); v <- ifelse(as.character(v) %in% names(REMAP), REMAP[as.character(v)], v)
y <- values(yr, mat = FALSE); burn <- !(v >= 91 & v <= 99)
v[burn & !is.na(y) & y <= 2] <- 99; v[burn & !is.na(y) & y > 2 & y <= 5] <- 181; v[burn & !is.na(y) & y > 5 & y <= 10] <- 188
r <- values(ref, mat = FALSE)
d <- sum(v != r, na.rm = TRUE); cat(sprintf("fuel cells differing from the own run: %d of %d (NA ref %d)\n", d, length(r), sum(is.na(r))))
if (d) print(head(table(pkg = v[v != r], own = r[v != r]), 20))
for (p in c("cbh", "cbd")) { a <- values(rast(file.path(I, c(cbh = "canopy_base_height_m.tif", cbd = "canopy_bulk_density.tif")[p])), mat = FALSE)
  b <- values(rast(file.path(O, paste0(p, ".tif"))), mat = FALSE); fo <- !is.na(a)
  cat(sprintf("%s: forest cells %d; own run non-forest code on them %d; max |diff| on forest %.2e\n", p, sum(fo),
      sum(b[fo] %in% c(99, 0) & p == "cbh"), max(abs(a[fo] - b[fo]))))
}
