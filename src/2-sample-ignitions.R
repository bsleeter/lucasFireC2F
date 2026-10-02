# =============================================================================
# 2 - Sample Ignitions
# =============================================================================
# Draws the fire population the way the validated P26 workflow does (script 221), with the scenario's random seed:
#   count     fires per season N ~ IgnitionCount (Gamma by mean and SD; Normal, Poisson or Fixed also allowed), rounded,
#             at least the distribution's minimum (default 1)
#   pool      extra ignitions = ProportionExtraIgnition x all seasons' fires, one shared pool (Iteration 0) used in order by
#             stage 5 to replace fires smaller than the minimum fire size
#   what      season x cause from IgnitionDistribution (relative likelihoods)
#   where     a cell drawn in proportion to the cause's ignition probability map (negative or missing -> 0)
#   zones     weather zone (and fire zone) read at that cell; the fire's point is the cell centre
# The random-number order matches 221, so the same seed gives the same fires. A per-season IgnitionCount (rows with a
# Season) draws each season's count separately and its cause from that season's rows of IgnitionDistribution.
# =============================================================================
suppressPackageStartupMessages({ library(rsyncrosim); library(terra) })
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
progressBar(type = "begin", totalSteps = 3)

rc  <- need("RunControl", c("MaximumIteration", "RandomSeed")); NS <- rc$MaximumIteration; set.seed(rc$RandomSeed)
ic  <- need("IgnitionCount", c("DistributionType", "Mean"))
dist <- need("IgnitionDistribution", c("Season", "Cause", "RelativeLikelihood"))
ig  <- need("IgnitionGrid", c("Cause", "FileName"))
rso <- read_ds("ResampleOption"); PX <- if (nrow(rso) && !is.na(rso$ProportionExtraIgnition)) rso$ProportionExtraIgnition else 0
wzr <- read_raster("LandscapeRasters", "WeatherZoneFileName"); fzr <- read_raster("LandscapeRasters", "FireZoneFileName")
wzt <- as.data.table(datasheet(prj, ds("WeatherZone"), lookupsAsFactors = FALSE))
fzt <- as.data.table(datasheet(prj, ds("FireZone"), lookupsAsFactors = FALSE))

grids <- lapply(split(ig$FileName, ig$Cause), function(f) { r <- terra::rast(f)
  if (!compareGeom(r, wzr, stopOnError = FALSE)) fail("ignition map %s is not on the landscape grid", basename(f))
  v <- values(r, mat = FALSE); v[is.na(v) | v < 0] <- 0; if (!sum(v)) fail("ignition map %s is all zero", basename(f)); v })
miss <- setdiff(unique(dist$Cause), names(grids)); if (length(miss)) fail("no ignition probability map for cause: %s", paste(miss, collapse = ", "))
wzv <- values(wzr, mat = FALSE); fzv <- if (is.null(fzr)) NULL else values(fzr, mat = FALSE)

count <- function(r, n) { m <- r$Mean; s <- if (is.na(r$DistributionSD)) 0 else r$DistributionSD
  x <- switch(as.character(r$DistributionType), Gamma = if (s > 0) rgamma(n, (m / s)^2, m / s^2) else rep(m, n),
              Normal = rnorm(n, m, s), Poisson = rpois(n, m), Fixed = rep(m, n), fail("unknown distribution %s", r$DistributionType))
  lo <- if (is.na(r$DistributionMin)) 1 else r$DistributionMin; x <- pmax(lo, as.integer(round(x)))
  if (!is.na(r$DistributionMax)) x <- pmin(x, as.integer(r$DistributionMax)); x }
draw <- function(n, d) {
  k <- sample.int(nrow(d), n, replace = TRUE, prob = d$RelativeLikelihood); cell <- integer(n)
  for (cz in unique(d$Cause[k])) { i <- which(d$Cause[k] == cz); g <- grids[[cz]]; cell[i] <- sample.int(length(g), length(i), replace = TRUE, prob = g) }
  data.table(Cell = cell, Season = d$Season[k], Cause = d$Cause[k]) }

if (nrow(ic) == 1 && is.na(ic$Season)) {
  N <- count(ic, NS); npool <- as.integer(round(PX * sum(N)))
  orig <- draw(sum(N), dist); orig[, `:=`(Iteration = rep(seq_len(NS), N), FireID = sequence(N), Pool = FALSE)]
} else {
  if (anyNA(ic$Season)) fail("Ignition Count: give every row a season, or use a single row with no season")
  orig <- rbindlist(lapply(seq_len(nrow(ic)), function(j) { r <- ic[j]; N <- count(r, NS); d <- dist[Season == r$Season]
    if (!nrow(d)) fail("Ignition Distribution has no rows for season %s", r$Season)
    x <- draw(sum(N), d); x[, Iteration := rep(seq_len(NS), N)] }))
  setorder(orig, Iteration); orig[, `:=`(FireID = seq_len(.N), Pool = FALSE), by = Iteration]
  N <- orig[, .N, by = Iteration]$N; npool <- as.integer(round(PX * nrow(orig)))
}
pool <- if (npool > 0) draw(npool, dist)[, `:=`(Iteration = 0L, FireID = seq_len(npool), Pool = TRUE)] else NULL
Fl <- rbind(orig, pool)
progressBar()

xy <- xyFromCell(wzr, Fl$Cell); ll <- crds(terra::project(vect(xy, crs = crs(wzr)), "EPSG:4326"))
Fl[, `:=`(Longitude = ll[, 1], Latitude = ll[, 2])]
wc <- wzv[Fl$Cell]; Fl[, WeatherZone := wzt$Name[match(wc, wzt$ID)]]
if (anyNA(Fl$WeatherZone)) fail("%d fires fell on weather-zone codes with no Weather Zone (codes: %s)", sum(is.na(Fl$WeatherZone)),
                                paste(head(unique(wc[is.na(Fl$WeatherZone)]), 10), collapse = ", "))
Fl[, FireZone := if (is.null(fzv)) NA_character_ else fzt$Name[match(fzv[Cell], fzt$ID)]]
progressBar()

write_ds(Fl[, .(Iteration, FireID, Cell, Latitude, Longitude, Season, Cause, FireZone, WeatherZone, Pool)], "OutputFireList")
say("%d seasons: %s fires (mean %.2f per season), extra pool %s", NS, format(sum(!Fl$Pool), big.mark = ","), mean(N), format(sum(Fl$Pool), big.mark = ","))
tb <- Fl[Pool == FALSE, .N, by = .(Season, Cause)][, share := round(N / sum(N), 3)][order(-N)]
say("season x cause of the season fires: %s", paste(sprintf("%s/%s %.3f", tb$Season, tb$Cause, tb$share), collapse = "; "))
progressBar(); progressBar(type = "end")
