#' Tune for Depletion
#'
#' tunes a control variable to achieve a target depletion level
#'
#' @param control_variable
#' @param target_depletion
#' @param fish
#' @param fleet
#' @param sim_years
#' @param burn_years
#' @param num_patches
#'
#' @return ss for depletion
#' @export
#'
tune_for_depletion <- function(control_variable,
                               target_depletion,
                               fish,
                               fleet,
                               sim_years,
                               burn_years,
                               num_patches){

  # target_depletion = 0.5
  #
  # fleet <- sim_grid$fleet[[1]]
  #
  # fish <- sim_grid$fish[[1]]
  #
  # control_variable <- 100

  if (fleet$fleet_model == "constant-catch"){

    fleet$target_catch <- control_variable

  } else if (fleet$fleet_model == "constant-effort"){

    fleet$initial_effort <- control_variable

  } else if (fleet$fleet_model == "open-access"){

    message("Not yet you don't")
  }


  set.seed(42)
  sim <- spasm::sim_fishery(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = 0),
    num_patches = 1,
    sim_years = sim_years,
    burn_years = burn_years,
    time_step = fish$time_step,
    est_msy = F,
    tune_costs = F,
    b_v_bmsy_oa = 0.5
  )

  b0 <- sum(sim$biomass[sim$year == min(sim$year)])

  b_final <- sum(sim$biomass[sim$year == max(sim$year)])

  depletion <- b_final / b0

  ss <- (target_depletion - depletion)^2

  return(ss)

}