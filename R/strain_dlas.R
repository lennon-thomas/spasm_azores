#' \code{strain_dlas} test sensitivity of dlas to input parameters
#'
#' @param fish
#' @param fleet
#' @param linf_error
#' @param vbk_error
#' @param sigma_r
#' @param rec_ac
#' @param cv_len
#' @param m_error
#' @param year_sampled
#' @param l50_error
#'
#' @return a data frame of error terms
#' @export
#'
strain_dlas <- function(fish,
                        fleet,
                        linf_error = 1,
                        vbk_error = 1,
                        m_error = 1,
                        l50_error = 1,
                        sigma_r = 0,
                        rec_ac = 0,
                        cv_len = 0,
                        year_sampled = 1) {
  # simulate a population
  fish$cv_len <- cv_len

  fish$sigma_r <- sigma_r

  fish$rec_ac <-  rec_ac

  fish <- update_fish(fish)

  fleet <- update_fleet(fleet = fleet, fish = fish)

  samples <-
    sim_sampling(
      fish = fish,
      fleet = fleet,
      percent_sampled = 1,
      sim_years = 50
    )

  years <- samples$year

  sample_year <- years[round(length(years) * year_sampled)]

  year_sample <- samples %>%
    filter(year == sample_year)

  length_comps <-
    year_sample %>% select(catch_length_samples) %>% unnest() %>%
    mutate(year = 1)

  mean_f <- mean(year_sample$fishery[[1]]$f)

  true_z <-  mean_f + fish$m

  true_spr <- approx_spr(fish = fish, fleet = fleet, mean_f)

  # babel fish

  babel_fish <- fish

  babel_fish$linf <-  babel_fish$linf * linf_error

  babel_fish$vbk <- babel_fish$vbk * vbk_error

  babel_fish$m <- babel_fish$m * m_error

  babel_fish <- update_fish(fish = babel_fish)

  babel_fleet <- fleet

  babel_fleet$length_50_sel <- babel_fleet$length_50_sel * l50_error

  babel_fleet <-
    update_fleet(fish = babel_fish, fleet = babel_fleet)

  # run catch curve
  cc <-
    run_catch_curve(fish = babel_fish, length_comps = length_comps)
  cc_m <- babel_fish$m

  cc_z <- -cc

  cc_f <- cc_z - cc_m

  # run  LBSPR lite

  lbspr_lite_f <-
    optim(
      .01,
      lbspr_lite,
      fish = babel_fish,
      fleet = babel_fleet,
      length_comps = length_comps,
      use = 'find-f',
      lower = 0,
      upper = 2
    )

  lbspr_f <- lbspr_lite_f$par

  lbspr_lite_spr <-
    lbspr_lite(
      lbspr_f,
      fish = fish,
      fleet = fleet,
      length_comps = length_comps,
      use = 'spr'
    )

  out <- data_frame(
    linf_error  = linf_error,
    vbk_error = vbk_error,
    m_error = m_error,
    l50_error = l50_error,
    sigma_r = sigma_r,
    rec_ac = rec_ac,
    year_sampled = year_sampled,
    cv_len = cv_len,
    true_spr = true_spr,
    true_z = true_z,
    true_f = mean_f,
    lbspr_spr = lbspr_lite_spr,
    lbspr_f = lbspr_f,
    cc_z = cc_z,
    cc_f = cc_f
  )


}
