# rothermel() must equal Rothermel::ros() for all 40 Scott & Burgan models (package defaults) and Anderson FM10, at the four
# moisture scenarios and every lookup wind speed (0-80 km/h by 0.25), on ROS, reaction intensity, SAV and packing ratio.
suppressPackageStartupMessages(library(Rothermel))
source("F:/lucasFireC2F/src/rothermel.R"); data(SFM_metric); data(scenarios)
fm <- read.csv("F:/lucasFireC2F/defaults/FuelModel.csv"); U <- seq(0, 80, by = 0.25)
SC <- scenarios[c("D1L1", "D2L2", "D3L3", "D4L4"), 1:5]
worst <- 0; bad <- character()
chk <- function(name, pars, ref) for (k in 1:4) {
  a <- rothermel(pars, as.numeric(SC[k, ]), U)
  o <- suppressWarnings(ros(modeltype = as.character(ref$Fuel_Model_Type), w = ref[2:6], s = ref[7:11], delta = ref$Fuel_Bed_Depth,
                            mx.dead = ref$Mx_dead, h = ref[14:18], m = SC[rep(k, length(U)), ], u = U, slope = 0))
  d <- max(abs(a$R - o[["ROS [m/min]"]]), abs(a$IR - o[["Reaction intensity [kW/m2]"]]), abs(a$SAV - o[["Characteristic SAV [m2/m3]"]]),
           abs(a$beta - o[["Packing ratio [dimensionless]"]]), na.rm = FALSE)
  if (is.na(d) || d > 1e-9) bad <<- c(bad, sprintf("%s %s (%s)", name, rownames(SC)[k], format(d)))
  worst <<- max(worst, d, na.rm = TRUE) }
for (i in seq_len(nrow(fm))) chk(fm$Name[i], as.list(fm[i, ]), SFM_metric[fm$Name[i], ])
chk("A10", ANDERSON_FM10, SFM_metric["A10", ])
cat(sprintf("%d model x scenario cases, %d wind speeds each; largest difference %.2e\n", 4 * (nrow(fm) + 1), length(U), worst))
if (length(bad)) cat("DIFFER:", head(bad, 20), sep = "\n  ") else cat("ROTHERMEL PORT MATCHES THE PACKAGE EXACTLY\n")
