
#' sim_sampling
#'
#' @param fish
#' @param fleet
#' @param sim_years
#' @param burn_year
#' @param percent_sampled
#' @param num_patches
#'
#' @return length and age samples
#' @export
#'
sim_sampling <- function(fish, fleet, sim_years = 25, burn_year = 25,percent_sampled = 0.25,num_patches = 1) {

  fishery <- sim_fishery(
    fish = fish,
    fleet = fleet,
    manager  = create_manager(mpa_size = 0, year_mpa = 10),
    num_patches = num_patches,
    burn_year = burn_year,
    sim_years = sim_years)

  length_and_age_comps <- fishery %>%
    select(year,patch,age, numbers, numbers_caught) %>%
    nest(-year,-patch, .key = n_at_age)

  length_and_age_comps <- length_and_age_comps %>%
    mutate(pop_length = map(n_at_age, ~sample_lengths(n_at_age = .x,
                                                      cv = fish$cv_len,
                                                      k = fish$vbk,
                                                      linf = fish$linf,
                                                      t0 = fish$t0,
                                                      sample_type = 'population',
                                                      percent_sampled = 1,
                                                      time_step = fish$time_step)))

  length_and_age_comps <- length_and_age_comps %>%
    mutate(pop_ages = map(pop_length, ~length_to_age(length_samples = .x,
                                                     cv = fish$cv_len,
                                                     k = fish$vbk,
                                                     linf = fish$linf,
                                                     t0 = fish$t0,
                                                     max_age = fish$max_age,
                                                     min_age = fish$min_age,
                                                     time_step = fish$time_step)))


  length_and_age_comps <- length_and_age_comps %>%
    mutate(catch_length_samples = map(n_at_age, ~sample_lengths(n_at_age = .x,
                                                                cv = fish$cv_len,
                                                                k = fish$vbk,
                                                                linf = fish$linf,
                                                                t0 = fish$t0,
                                                                sample_type = 'catch',
                                                                time_step = fish$time_step,
                                                                percent_sampled = percent_sampled)))

  length_and_age_comps <- length_and_age_comps %>%
    mutate(catch_ages_samples = map(catch_length_samples, ~length_to_age(length_samples = .x,
                                                                         cv = fish$cv_len,
                                                                         k = fish$vbk,
                                                                         linf = fish$linf,
                                                                         t0 = fish$t0,
                                                                         max_age = fish$max_age,
                                                                         min_age = fish$min_age,
                                                                         time_step = fish$time_step)))

  length_and_age_comps <- length_and_age_comps %>%
    mutate(n0_at_age = list(n_at_age[[1]])) %>%
    mutate(fishery = list(fishery))

  return(length_and_age_comps)




}