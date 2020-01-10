#' Title
#'
#' @param effort
#' @param fish
#' @param fleet
#' @param sim_years
#' @param burn_year
#' @param mpa_size
#' @param mpa_year
#' @param num_patches
#' @param use
#'
#' @return an estiamte of MSY
#' @export
#'
estimate_msy <-
  function(effort,
           fish,
           fleet,
           sim_years = 25,
           burn_years = 25,
           mpa_size = 0,
           mpa_year = 100,
           num_patches = 1,
           use = "fit",
           seed = 42) {


     fleet <-
      spasm::update_fleet(
        fleet = purrr::list_modify(
          fleet,
          fleet_model = "constant-effort",
          initial_effort = effort,
          sigma_effort = 0
        ),
        fish = fish
      )

    set.seed(seed)

    sim <- spasm::sim_fishery(
      fish = fish,
      fleet = fleet,
      manager = create_manager(mpa_size = 0),
      num_patches = num_patches,
      sim_years = sim_years,
      burn_years = burn_years,
      time_step = fish$time_step,
      est_msy = F
    )


    yields <- sim %>%
      filter(year >= (max(year) - 10)) %>%
      group_by(year) %>%
      summarise(yield = sum(biomass_caught),
                biomass = sum(biomass),
                revenue = sum(biomass_caught * price))

    if (use == "fit"){
      out <- -(mean(yields$yield))
    } else{

      out <- list(b_msy = mean(yields$biomass),
                  r_msy = mean(yields$revenue))

    }

    return(out)

  }