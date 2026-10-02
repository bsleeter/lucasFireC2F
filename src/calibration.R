# =============================================================================
# calibration.R - functions shared by the calibration stages (C3, C5, C6); no SyncroSim calls, so they can be tested alone
# =============================================================================

# FSim's large-fire threshold (Dillon et al. 2023): the fire size where the Lorenz curve of sizes has slope 1 = the MEAN
# size of all recorded fires, with a 73-acre floor. Sizes in ha. (Validated P26 script 184.)
fsim_threshold <- function(ha, floor_ha = 73 * 0.404686) max(mean(ha[ha > 0]), floor_ha)

# fires at or above `min_ha` per year, zeros filled over `years`: mean and SD for a Gamma count (184)
count_fit <- function(year, ha, min_ha, years) {
  n <- tabulate(match(year[ha >= min_ha], years), nbins = length(years))
  list(mean = mean(n), sd = sd(n), n = n)
}

# season x cause shares of fires at or above `min_ha`; unknown causes (NA) split within each season by that season's
# human share (the whole record's share where a season has no known causes) (validated P26 script 158)
cause_mix <- function(season, cause, ha, min_ha) {
  k <- ha >= min_ha & !is.na(season); s <- season[k]; c <- cause[k]
  t <- as.data.frame.matrix(table(factor(s), factor(ifelse(is.na(c), "Unknown", c), levels = c("Human", "Lightning", "Unknown"))))
  gh <- sum(t$Human) / (sum(t$Human) + sum(t$Lightning))
  fh <- ifelse(t$Human + t$Lightning > 0, t$Human / (t$Human + t$Lightning), gh)
  H <- t$Human + t$Unknown * fh; L <- t$Lightning + t$Unknown * (1 - fh)
  d <- rbind(data.frame(Season = rownames(t), Cause = "Human", n = H), data.frame(Season = rownames(t), Cause = "Lightning", n = L))
  d <- d[d$n > 0, ]; d <- d[order(d$Season, d$Cause), ]
  d$RelativeLikelihood <- round(d$n / sum(d$n), 8); d
}

# season name of a date from the Season table (FirstMonth..LastMonth, wrapping over the new year)
season_of <- function(date, seasons) {
  m <- as.integer(format(as.Date(date), "%m")); out <- rep(NA_character_, length(m))
  for (i in seq_len(nrow(seasons))) { a <- seasons$FirstMonth[i]; b <- seasons$LastMonth[i]
    k <- if (a <= b) m >= a & m <= b else m >= a | m <= b; out[k] <- seasons$Name[i] }
  out
}

# Fit the spread-day distribution to observed fire sizes (validated P26 script 204). A seed run burned each fire at a known
# spread-day count D; `sim_ha` / `sim_d` are ALL its season fires, before resampling. The reported population keeps fires
# >= min_ha, so its size distribution is sum_d w_d P(size >= min | d) kernel_d(size | size >= min) / norm. Objective: squared
# error on the observed size CDF at 40 log-spaced sizes + area_w x log(mean kept size / observed mean)^2, over the day counts
# the seed sampled, from three starting points (the seed's own frequencies, uniform, 90% on D = 1).
fit_spread_days <- function(sim_ha, sim_d, obs_ha, min_ha, area_w = 5) {
  obs <- obs_ha[obs_ha >= min_ha]
  K <- split(sim_ha, sim_d); D <- as.integer(names(K))
  X <- exp(seq(log(min_ha), log(max(obs) * 1.5), length.out = 40)); Fobs <- ecdf(obs)(X)
  surv <- sapply(K, function(k) mean(k >= min_ha))
  cdfk <- sapply(K, function(k) { k <- k[k >= min_ha]; if (!length(k)) rep(1, length(X)) else ecdf(k)(X) })
  mk <- sapply(K, function(k) { k <- k[k >= min_ha]; if (!length(k)) 0 else mean(k) })
  pred <- function(w) { p <- w * surv; p <- p / sum(p); list(cdf = as.vector(cdfk %*% p), mean = sum(p * mk)) }
  obj <- function(th) { w <- exp(th) / sum(exp(th)); q <- pred(w); sum((q$cdf - Fobs)^2) + area_w * log(q$mean / mean(obs))^2 }
  w0 <- as.numeric(table(factor(sim_d, levels = D))); w0 <- w0 / sum(w0)
  best <- NULL
  for (st in list(log(w0 + 1e-4), rep(0, length(D)), log(c(0.9, rep(0.1 / (length(D) - 1), length(D) - 1))))) {
    o <- optim(st, obj, method = "BFGS", control = list(maxit = 2000)); if (is.null(best) || o$value < best$value) best <- o }
  w <- exp(best$par) / sum(exp(best$par)); w[w < 1e-4] <- 0; w <- w / sum(w)
  q <- pred(w); B <- c(min_ha, 100, 405, 1000, 3000, 10000, 30000, Inf)
  cls <- function(cdf) diff(c(0, approx(X, cdf, xout = B[-c(1, length(B))], rule = 2)$y, 1))
  lab <- levels(cut(obs, B, right = FALSE))
  list(curve = data.frame(Days = D, RelativeFrequency = round(w, 6)),
       classes = data.frame(SizeClass = lab, ObservedShare = as.numeric(table(cut(obs, B, right = FALSE))) / length(obs), FittedShare = cls(q$cdf)),
       summary = data.frame(KS = max(abs(q$cdf - Fobs)), AreaRatio = q$mean / mean(obs), MeanSpreadDays = sum(w * D),
                            ObservedFires = length(obs), ObservedMeanHa = mean(obs), SeedFires = length(sim_ha)))
}
