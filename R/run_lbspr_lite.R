#' \code{lbspr_lite} runs a simplified version of LBSPR assuming selectivity is known
#'
#' @param f fishing mortality
#' @param fish a fish object containing life history data
#' @param fleet a fleet object containing fleet data
#' @param length_comps length composition data numbers by length bin
#' @param use set to find-f to use in optim
#'
#' @return either ss of length comps or SPR
#' @export
#'
lbspr_lite <- function(f,
                        fish,
                        fleet,
                        length_comps,
                        use = 'find-f') {
  ages <- seq(fish$min_age, fish$max_age, by = fish$time_step)

  l_a <- fish$linf * (1 - exp(-fish$vbk * (ages - fish$t0)))

  w_a <- fish$weight_a * l_a ^ fish$weight_b

  m_a <-
    logistic_curve(l_a, fish$length_50_mature, fish$delta_mature)

  s_a <- logistic_curve(l_a, fleet$length_50_sel, fleet$delta)

  n_a <- rep(0, length(ages))

  z_a <- fish$time_step * (fish$m + f * s_a)

  n_a[1] <-  1

  n_unfished <- 1 * exp(-fish$m * ages)

  for (a in 2:length(ages)) {
    n_a[a] <- n_a[a - 1] * exp(-z_a[a - 1])
  }

  n_caught <- n_a * s_a

  spawning_potential_unfished <- sum(n_unfished * w_a * m_a)

  spawning_potential_fished <- n_a * w_a * m_a

  spr <-
    sum(spawning_potential_fished) / sum(spawning_potential_unfished)
  l_comp_hat <-
    data_frame(length_bin = floor(l_a), n_caught = n_caught) %>%
    group_by(length_bin) %>%
    summarise(numbers_hat = sum(n_caught))

  ss_mat <- length_comps %>%
    right_join(l_comp_hat, by = 'length_bin') %>%
    gather(source, value, numbers, numbers_hat) %>%
    group_by(source) %>%
    mutate(p_numbers = value / sum(value)) %>%
    select(-value) %>%
    spread(source, p_numbers) %>%
    mutate(se = (numbers - numbers_hat) ^ 2)

  ss <- sum(ss_mat$se)

  if (use == 'find-f') {
    out <- ss
  } else {
    out <- spr

  }

  return(out)


}