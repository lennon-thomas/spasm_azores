#' \code{approx_spr} aproximates equilibrium SPR from mean f across patches
#'
#' @param fish
#' @param fleet
#' @param f
#'
#' @return an aproximate spr
#' @export
#'
approx_spr <- function(fish, fleet,f) {


  mean_f <- mean(f)

  ages <- seq(fish$min_age, fish$max_age, by = fish$time_step)

  l_a <- fish$linf * (1 - exp(-fish$vbk * (ages - fish$t0)))

  w_a <- fish$weight_a * l_a ^ fish$weight_b

  m_a <-
    logistic_curve(l_a, fish$length_50_mature, fish$delta_mature)

  s_a <- logistic_curve(l_a, fleet$length_50_sel, fleet$delta)

  n_a <- rep(0, length(ages))

  z_a <- fish$time_step * (fish$m + mean_f * s_a)

  n_a[1] <-  1

  n_unfished <- 1 * exp(-fish$m * ages)

  for (a in 2:length(ages)) {
    n_a[a] <- n_a[a - 1] * exp(-z_a[a - 1])
  }

  spawning_potential_unfished <- sum(n_unfished * w_a * m_a)

  spawning_potential_fished <- n_a * w_a * m_a

  spr <- sum(spawning_potential_fished) / sum(spawning_potential_unfished)

}