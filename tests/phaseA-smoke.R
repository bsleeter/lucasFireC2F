# =============================================================================
# phaseA-smoke.R - the package installs, a library opens, defaults load, a stage launches
# =============================================================================
# Usage: Rscript tests/phaseA-smoke.R <scratch folder>
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
a <- commandArgs(TRUE); D <- if (length(a)) a[1] else tempdir()
f <- file.path(D, "lucasFireC2F-smoke.ssim"); unlink(c(f, paste0(f, c(".data", ".temp", ".backup"))), recursive = TRUE)
L <- ssimLibrary(f, packages = "lucasFireC2F", overwrite = TRUE)
P <- project(L, "Definitions")
ok <- TRUE
chk <- function(nm, n) { x <- datasheet(P, paste0("lucasFireC2F_", nm)); good <- nrow(x) == n
  cat(sprintf("%-18s %3d rows (expect %d) %s\n", nm, nrow(x), n, if (good) "ok" else "FAIL")); if (!good) ok <<- FALSE; x }
chk("FuelModel", 40); cw <- chk("FuelType", 45); chk("MoistureScenario", 4)
chk("Season", 4); chk("Cause", 2); chk("FlameLengthClass", 6); rf <- chk("Lifeform", 3)
print(head(cw[cw$Name %in% c("GR9", "SH1", "SB2", "TU5"), ], 4)); print(head(rf, 3))
S <- scenario(P, "Smoke")
rc <- datasheet(S, "lucasFireC2F_RunControl"); cat("RunControl defaults:", paste(names(rc), unlist(rc), collapse = ", "), "\n")
dc <- datasheet(S, "lucasFireC2F_MoistureDeadClass"); cat("MoistureDeadClass rows in a new scenario:", nrow(dc), "\n")
# a stage launches and fails with the Phase A message (proves script paths and shared.R resolve)
pipe <- datasheet(S, "core_Pipeline"); pipe <- data.frame(StageNameId = "1 - Prepare Landscape", RunOrder = 1)
saveDatasheet(S, pipe, "core_Pipeline")
r <- tryCatch(run(S), error = function(e) conditionMessage(e))
cat("run result:", if (is.character(r)) r else "result scenario created", "\n")
cat(if (ok) "SMOKE OK\n" else "SMOKE FAILED\n")
