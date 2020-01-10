#' sample_lenghts
#'
#' @param n_at_age
#' @param cv
#' @param k
#' @param linf
#' @param t0
#' @param sample_type
#' @param percent_sampled
#'
#' @return a data frame of sampled length frequencies
#' @export
#'
sample_lengths <-
  function(n_at_age,
           cv,
           k,
           linf,
           t0,
           time_step = 1,
           sample_type = 'catch',
           percent_sampled = .1,
           sample_col = NA,
           linf_buffer = 10) {

    if (sample_type == 'catch') {
      sample_col <- quo(numbers_caught)

    } else if (sample_type == 'population') {
      sample_col <- quo(numbers)
    }

    if (percent_sampled <=1){
    length_comp_samples <- n_at_age %>%
      summarise(samps = percent_sampled * (sum(!!sample_col))) %>%  {
        .$samps
      }
    } else{ # sample a fixed number instead

      length_comp_samples = percent_sampled
    }

    min_age <- min(n_at_age$age)

    max_age <- max(n_at_age$age)

    mean_length_at_age <-
      linf * (1 - exp(-k * (seq(
        min_age, max_age, by = time_step
      ) - t0)))

    p_n_at_age <- n_at_age %>%
      group_by(age) %>%
      summarise(numbers = sum(!!sample_col)) %>%
      ungroup() %>%
      mutate(p_sampled_at_age = numbers / sum(numbers))

    p_length_at_age <- generate_length_at_age_key(max_age = max_age,
                                                  min_age = min_age,
                                                  cv = cv,
                                                  linf = linf,
                                                  k = k,
                                                  t0 = t0,
                                                  time_step = time_step,
                                                  linf_buffer = linf_buffer)
    p_length_at_age <- p_length_at_age %>%
      left_join(p_n_at_age, by = 'age')

    p_sampling_length_bin <- p_length_at_age %>%
      group_by(length_bin) %>%
      summarise(prob_sampled = sum(p_bin * p_sampled_at_age))
    if (length_comp_samples > 0) {
      length_comps <-
        rmultinom(1, size = length_comp_samples, prob = p_sampling_length_bin$prob_sampled) %>% as.numeric()

      length_comps <-
        data_frame(length_bin = unique(p_length_at_age$length_bin),
                   numbers = length_comps)

    } else {
      length_comps <-
        data_frame(length_bin = unique(p_length_at_age$length_bin),
                   numbers = 0)

    }
    return(length_comps)

  }