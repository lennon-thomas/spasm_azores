#' \code{catch_target} finds the right effort to produce a target level of catch
#'
#' @param total_effort
#' @param target_catch
#' @param pop
#' @param num_patches
#' @param mpa
#' @param fleet
#'
#' @return a total effort to be distributed by the fleet model
#' @export
#'
catch_target <- function(total_effort,
                         target_catch,
                         pop,
                         num_patches,
                         mpa,
                         fleet,
                         use = 'opt',
                         fish,
                         prior_profits,
                         year,
                         burn_years) {

  efforts <- distribute_fleet(
    pop = pop,
    effort = total_effort,
    fleet = fleet,
    num_patches = num_patches,
    mpa = mpa,
    prior_profits = prior_profits,
    year = year,
    burn_years = burn_years
  )

  fs <- efforts * fleet$q

  pop$f <- fs

  caught <- pop %>%
    dplyr::group_by(patch) %>%
    dplyr::mutate(biomass_caught = grow_and_die(
      numbers = numbers,
      f = f,
      mpa = mpa,
      fish = fish,
      fleet = fleet
    )$caught * weight_at_age) %>%
    ungroup() %>%
    {
      .$biomass_caught
    }

  alive <- pop %>%
    dplyr::group_by(patch) %>%
    dplyr::mutate(survivors = grow_and_die(
      numbers = numbers,
      f = f,
      mpa = mpa,
      fish = fish,
      fleet = fleet
    )$survivors) %>%
    ungroup() %>%
    {
      .$survivors
    }


  catch <- caught %>% sum()

  ss <- (catch - target_catch) ^ 2

  if (use == 'opt') {
    out <- ss
  } else {
    out <- alive

  }

  return(out)


}