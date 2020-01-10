#' length_to_age
#'
#' @param length_samples
#' @param cv
#' @param k
#' @param linf
#' @param t0
#' @param max_age
#'
#' @return a data frame of estimated numbers at age
#' @export
#'
length_to_age <-
  function(length_samples,
           cv,
           k,
           linf,
           t0,
           max_age,
           min_age = 0,
           time_step = 1,
           aging_method = "vbk") {

    if (aging_method == "vbk"){

      age_comp <- length_samples %>%
        mutate(next_bin = lead(length_bin)) %>%
        mutate(mid_bin = map2_dbl(length_bin, next_bin, ~mean(c(.x,.y), na.rm = T))) %>%
        mutate(expected_age = (log(1 - pmin(0.99*linf,mid_bin) / linf) / -k) - t0) %>%
        mutate(rounded_age = plyr::round_any(expected_age, time_step, f = floor)) %>%
        group_by(rounded_age) %>%
        summarise(numbers = sum(numbers)) %>%
        rename(age = rounded_age)

      blank_ages <- data_frame(age = seq(min_age, max_age, by = time_step), blanks = 0)

      age_comp <- age_comp %>%
        right_join(blank_ages, by = "age") %>%
        ungroup() %>%
        mutate(numbers = ifelse(is.na(numbers), 0 , numbers)) %>%
        select(-blanks)


    } else if (aging_method == "key") {
      warning("aging by key DOES NOT work correctly right now")

    mean_length_at_age <-
      linf * (1 - exp(-k * (seq(
        min_age, max_age, by = time_step
      ) - t0)))

    length_at_age_vars <- data_frame(
      age =  seq(min_age, max_age, by = time_step),
      mean_length_at_age = mean_length_at_age,
      sigma_at_age = cv * mean_length_at_age
    ) #calculate standard deviation of length at age for each age bin

    # now calculate the probability of being in each length bin at each age

    p_length_at_age <-
      expand.grid(
        age =  seq(min_age, max_age, by = time_step),
        length_bin = 0:(10 * linf)
      ) %>%
      as_data_frame() %>%
      left_join(length_at_age_vars, by = 'age') %>%
      arrange(age, length_bin)

    p_length_at_age <- p_length_at_age %>%
      group_by(age) %>%
      mutate(next_length_bin = lead(length_bin, 1)) %>%
      mutate(p_bin = ifelse(
        is.na(next_length_bin) == F,
        pnorm(next_length_bin, mean_length_at_age, sigma_at_age),
        1
      ) -
        pnorm(length_bin, mean_length_at_age, sigma_at_age))

    #rescale probabilities by the probability of being in an age bin at a given length``
    p_length_at_age <- p_length_at_age %>%
      group_by(length_bin) %>%
      mutate(p_age_at_length = p_bin / sum(p_bin, na.rm = T))

    p_length_at_age$p_age_at_length[is.na(p_length_at_age$p_age_at_length)] <- 0

    # p_length_at_age %>%
    #   ggplot(aes(age, p_age_at_length)) +
    #   geom_col() +
    #   facet_wrap(~length_bin)
    #

    # Bin lengths into age bins, and then join the probability of being in each age as a function
    # of length size
    lengths_to_ages <- length_samples %>%
      left_join(p_length_at_age %>% select(length_bin, age, p_age_at_length),
                by = 'length_bin')

    # lengths_to_ages <- data_frame(lengths = lengths) %>%
    #   mutate(length_bin = floor(lengths)) %>%
    #   group_by(length_bin) %>%
    #   summarise(samples = length(lengths)) %>%
    #   left_join(p_length_at_age %>% select(length_bin, age, p_age_at_length),
    #             by = 'length_bin')


    # Assign lengths to ages in proportion to the probability of age at length

    age_comp <- lengths_to_ages %>%
      ungroup() %>%
      mutate(numbers = numbers * p_age_at_length) %>%
      group_by(age) %>%
      summarise(numbers = sum(numbers, na.rm = T)) %>%
      mutate(age = age)

    }


    return(age_comp)

  }