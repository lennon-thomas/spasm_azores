#' \code{run_catch_curve} runs a weighted catch curve on spasm data
#'
#' @param length_comps a data frame with columns length_bin and numbers
#' @param fish a spasm fish object containing life history
#'
#' @return total mortality estimated by catch curve, z
#' @export
#'
run_catch_curve <- function(length_comps, fish) {
  age_comps <-  length_to_age(
    length_samples = length_comps,
    cv = fish$cv_len,
    k = fish$vbk,
    linf = fish$linf,
    t0 = fish$t0,
    max_age = fish$max_age,
    min_age = fish$min_age,
    time_step = fish$time_step
  )
  cc_dat <- age_comps %>%
    ungroup() %>%
    mutate(log_numbers = ifelse(numbers > 0, log(numbers), NA))

  peak_age <- cc_dat$age[cc_dat$numbers == max(cc_dat$numbers)][1]

  # first_zero <- cc_dat$age[cc_dat$age > peak_age & cc_dat$numbers <= 1][1]
  #
  # if(is.na(first_zero)){first_zero <-  max(cc_dat$age) + 1}
  cc_dat <- cc_dat %>%
    filter(age >= peak_age, age < max(age, na.rm = T))

  pos_ages <- cc_dat$numbers > 0

  if (sum(pos_ages) > 2 & nrow(cc_dat) > 0) {
    cc <- lm(jitter(log_numbers, factor = .001) ~ jitter(age, factor = 0.001), data = cc_dat, singular.ok = T)



    cc_weights <- rep(0, length(pos_ages))

    ln_hat <- predict(cc) %>% as.numeric()
    ln_hat <-
      (ln_hat - min(ln_hat)) / sum(ln_hat - min(ln_hat, na.rm = T))
    cc_weights[pos_ages] <- ln_hat
    cc <-
      lm(
        jitter(log_numbers, .001) ~ jitter(age, .001),
        data = cc_dat,
        weights = cc_weights,
        singular.ok = T
      )

    z <- (cc$coefficients['jitter(age, 0.001)'] %>% as.numeric())
  } else{
    z <- 0
  }

  if (is.na(z)) {
    z = 0
  }
  return(z)

}