#' estimate costs that produce a given B/Bmsy open access
#'
#' @param fish
#' @param fleet
#' @param msy
#' @param e_msy
#' @param b_msy
#' @param p_response
#' @param sim_years
#' @param burn_year
#' @param num_patches
#' @param use
#' @param max_cr_ratio the maximum cost to revenue ratio
#' @param b_v_bmsy_oa
#' @param lags
#'
#' @return an estimate of costs
#' @export
#'
estimate_costs_az <-
  function(fleet = fleet,
           fish = fish,
           pops = pop[pop$year == y,],
           efforts = effort[y]) {

    total_biomass<-sum(pops$biomass)

     price<-fish$price

     cost <-(price * total_biomass)/(2*efforts)

     out<- cost

    return(out)

  }