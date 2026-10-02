# =============================================================================
# rothermel.R - Rothermel (1972) surface fire spread with Albini (1976) / Scott & Burgan (2005) dynamic fuel models
# =============================================================================
# A direct port of Rothermel::ros() (R package Rothermel 1.4.1, Vacchiano & Ascoli 2015), so stage 6 gives the results it
# gave with that package, including its output rounding (ROS, reaction intensity and characteristic SAV to 2 decimals,
# packing ratio to 4). Units in: loads t/ha, SAV 1/m, depth cm, moistures %, heat kJ/kg, wind km/h. Internally English
# units as Rothermel. Fuel parameters come from the project's Fuel Models sheet (Scott & Burgan 2005, metric); every
# Scott & Burgan model uses 10-h SAV 358 1/m and 100-h SAV 98 1/m and one heat content for all classes.
# =============================================================================

# Anderson (1982) fuel model 10 (timber, litter and understory): Rothermel's (1991) active crown fire spread reference
ANDERSON_FM10 <- list(Dynamic = FALSE, Load1h = 6.74, Load10h = 4.49, Load100h = 11.23, LoadLiveHerb = 0, LoadLiveWoody = 4.49,
                      SAV1h = 6562, SAVLiveHerb = 0, SAVLiveWoody = 4921, DepthCm = 30, DeadExtinctionMoisture = 25, HeatContent = 18622)

# fm: one fuel model (list or one-row data frame with the Fuel Models columns); moist: 5 moistures % (1h, 10h, 100h, live herb,
# live woody); u: wind speeds km/h (vector); slope %. Returns ros (m/min), reaction intensity (kW/m2), characteristic SAV
# (1/m) and packing ratio, one row per wind speed.
rothermel <- function(fm, moist, u, slope = 0) {
  n <- length(u); m <- matrix(rep(moist / 100, each = n), n, 5)
  w <- matrix(rep(c(fm$Load1h, fm$Load10h, fm$Load100h, fm$LoadLiveHerb, fm$LoadLiveWoody) / 10 * 0.2048, each = n), n, 5)
  s <- matrix(rep(c(fm$SAV1h, 358, 98, fm$SAVLiveHerb, fm$SAVLiveWoody) / 3.281, each = n), n, 5)
  h <- matrix(fm$HeatContent * 0.429922614, n, 5)
  delta <- fm$DepthCm * 0.0328084; mxd <- fm$DeadExtinctionMoisture / 100
  U <- u * 54.6806649; sl <- slope / 100
  if (any(m[, 4] > 0 & m[, 4] < 0.3)) stop("Moisture of herbaceous fuels should be >= 30%")
  if (isTRUE(fm$Dynamic)) {                                     # curing: herb load moves to dead in proportion
    kt <- ifelse(m[, 4] >= 0.3 & m[, 4] < 1.2, (1.2 - m[, 4]) / 0.9, 0)
    f1 <- w[, 1] * s[, 1] / 32; f4 <- (w[, 4] * kt) * s[, 4] / 32
    s[, 1] <- (f1 * s[, 1] + f4 * s[, 4]) / (f1 + f4)
    w[, 1] <- w[, 1] + w[, 4] * kt; w[, 4] <- w[, 4] - w[, 4] * kt
  }
  rhop <- 32; st <- 0.0555; se <- 0.01
  a <- s * w / rhop; a.dead <- a[, 1] + a[, 2] + a[, 3]; a.live <- a[, 4] + a[, 5]; a.tot <- a.dead + a.live
  f <- cbind(a[, 1:3] / a.dead, a[, 4:5] / a.live)
  f[a.live == 0, 4:5] <- 0; f[a.dead == 0, 1:3] <- 0
  f.dead <- a.dead / a.tot; f.live <- a.live / a.tot
  wn <- w * (1 - st)
  wn.dead <- f[, 1] * wn[, 1] + f[, 2] * wn[, 2] + f[, 3] * wn[, 3]; wn.live <- wn[, 4] + wn[, 5]
  mf.dead <- f[, 1] * m[, 1] + f[, 2] * m[, 2] + f[, 3] * m[, 3]; mf.live <- f[, 4] * m[, 4] + f[, 5] * m[, 5]
  sigma.dead <- f[, 1] * s[, 1] + f[, 2] * s[, 2] + f[, 3] * s[, 3]; sigma.live <- f[, 4] * s[, 4] + f[, 5] * s[, 5]
  sigma <- f.dead * sigma.dead + f.live * sigma.live
  h.dead <- f[, 1] * h[, 1] + f[, 2] * h[, 2] + f[, 3] * h[, 3]; h.live <- f[, 4] * h[, 4] + f[, 5] * h[, 5]
  beta <- 1 / delta * rowSums(w / rhop)
  ed <- exp(-138 / s[, 1:3]); el <- exp(-500 / s[, 4:5])
  W <- rowSums(w[, 1:3] * ed) / rowSums(w[, 4:5] * el)
  mfpd <- rowSums(w[, 1:3] * m[, 1:3] * ed) / rowSums(w[, 1:3] * ed)
  mxl <- 2.9 * W * (1 - mfpd / mxd) - 0.226
  mxl[mxl < mxd] <- mxd; mxl[W == Inf] <- mxd
  ns <- 0.174 * se^(-0.19)
  nm <- function(x) 1 - 2.59 * x + 5.11 * x^2 - 3.52 * x^3
  nm.dead <- nm(mf.dead / mxd); nm.live <- nm(mf.live / mxl)
  nm.dead[mf.dead > mxd] <- 0; nm.live[mf.live > mxl] <- 0
  beta.op <- 3.348 * sigma^(-0.8189); rpr <- beta / beta.op
  gmax <- sigma^1.5 / (495 + 0.0594 * sigma^1.5); A <- 133 * sigma^(-0.7913)
  ir <- gmax * (rpr * exp(1 - rpr))^A * (wn.dead * h.dead * nm.dead * ns + wn.live * h.live * nm.live * ns)
  xi <- (192 + 0.2595 * sigma)^(-1) * exp((0.792 + 0.681 * sigma^0.5) * (beta + 0.1))
  C <- 7.47 * exp(-0.133 * sigma^0.55); B <- 0.02526 * sigma^0.54; E <- 0.715 * exp(-3.59e-4 * sigma)
  fw <- C * U^B * rpr^(-E); fs <- 5.275 * beta^(-0.3) * sl^2
  rhob <- (1 / delta) * rowSums(w)
  qig <- 250 + 1116 * m; qig[w == 0] <- 0
  eps <- f.dead * rowSums(f[, 1:3] * qig[, 1:3] * exp(-138 / s[, 1:3])) + f.live * rowSums(f[, 4:5] * qig[, 4:5] * exp(-138 / s[, 4:5]))
  r <- 0.3048 * (ir * xi * (1 + fw + fs)) / (rhob * eps)
  data.frame(u = u, R = round(r, 2), IR = round(ir * 0.1893, 2), SAV = round(sigma * 3.281, 2), beta = round(beta, 4))
}
