# =============================================================================
# Utility - Build Wind Table (RAWS)
# =============================================================================
# Phase A stub: reads its input datasheets and reports them. Implementation follows the plan (Phase B/C/D).
# =============================================================================
suppressPackageStartupMessages(library(rsyncrosim))
source(file.path(ssimEnvironment()$PackageDirectory, "src", "shared.R"))
not_yet("Utility - Build Wind Table (RAWS)", c("RAWSStation", "WindTableOption", "DailyWeather"))
