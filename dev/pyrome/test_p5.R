# Test stage P5 on an existing prep result: sets the RAWS Data Sources (if blank), makes "<tag> Wind test" (pipeline P5 only,
# depends on the prep result), runs it, and compares the table with a reference quantile CSV (dev script 220 output).
# Usage: Rscript test_p5.R "<library>" <tag> <prep result> [reference csv]
suppressPackageStartupMessages({ library(rsyncrosim); library(data.table) })
a <- commandArgs(TRUE); L <- suppressWarnings(ssimLibrary(a[1])); P <- rsyncrosim::project(L, 1); TAG <- a[2]; PR <- as.integer(a[3])
W <- "F:/lucas-master/Models/California 30m BurnP3/data/wind"
d <- datasheet(P, "lucasFireC2F_DataSource", optional = TRUE)
if (is.na(d$RawsHourlyFolder) || is.na(d$RawsCatalogueFile)) { d$RawsHourlyFolder <- file.path(W, "raws_hourly"); d$RawsCatalogueFile <- file.path(W, "fems_ca_raws_catalogue.json")
  saveDatasheet(P, d, "lucasFireC2F_DataSource", append = FALSE); cat("Data Sources set:", d$RawsHourlyFolder, "|", d$RawsCatalogueFile, "\n") }
S <- rsyncrosim::scenario(P, paste(TAG, "Wind test"))
saveDatasheet(S, data.frame(StageNameId = "P5 - Burning-Period Wind (RAWS)", RunOrder = 1L), "core_Pipeline"); dependency(S) <- PR
description(S) <- sprintf("TEST of stage P5 (lucasFireC2F 1.9.0) on %s prep result %d: pipeline P5 only; stations from the FEMS catalogue within 10 km of the zone; RAWS 2006-2024, Jun-Nov, 12-19 local; ratio to the zone Daily Weather (P3) wind of the station weather zone. Compared with the dev-script 220 table (ratio to the gridMET cell at the station).", TAG, PR)
t0 <- Sys.time(); tryCatch(run(S), error = function(e) cat("run error:", conditionMessage(e), "\n"))
R <- rsyncrosim::scenario(L, max(rsyncrosim::scenario(P)[, "ScenarioId"]))
lg <- sub(" +$", "", strsplit(paste(unlist(runLog(R)), collapse = "\n"), "\n")[[1]])
cat(unique(grep("RAWS|station|download|burning-period|daily wind|median ratio|Table|bins|Failure|Error|error", lg, value = TRUE)), sep = "\n")
cat(sprintf("\n%.1f min; RESULT %d\n", as.numeric(difftime(Sys.time(), t0, units = "mins")), scenarioId(R)))
Q <- as.data.table(datasheet(R, "lucasFireC2F_WindRatio", optional = TRUE)); cat("WindRatio rows:", nrow(Q), "\n")
if (length(a) > 3 && nrow(Q)) { ref <- fread(a[4]); cat("\nbin | P5 median / p90 / p99 | reference median / p90 / p99\n")
  for (lo in sort(unique(Q$LowerWindSpeed))) { q <- Q[LowerWindSpeed == lo]; r <- ref[abs(lo - lo) < 1e-9 & ref$lo == lo]
    f <- function(x, p) approx(x$Quantile %||% x$p, x$Ratio %||% x$ratio, xout = p)$y
    cat(sprintf("%4g | %.2f / %.2f / %.2f | %s\n", lo, approx(q$Quantile, q$Ratio, .5)$y, approx(q$Quantile, q$Ratio, .9)$y, approx(q$Quantile, q$Ratio, .99)$y,
      if (nrow(r)) sprintf("%.2f / %.2f / %.2f", approx(r$p, r$ratio, .5)$y, approx(r$p, r$ratio, .9)$y, approx(r$p, r$ratio, .99)$y) else "-")) } }
