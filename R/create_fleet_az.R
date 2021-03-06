#' create_fleet
#'
#' @param eq_f
#' @param length_50_sel
#' @param delta cm above length50 at 95 selectivity
#' @param mpa_reaction
#' @param fish
#' @param cost
#' @param beta
#' @param q
#' @param fleet_model
#' @param effort_allocation
#' @param initial_effort
#' @param cost_function
#' @param cost_slope
#' @param tech_rate
#' @param target_catch
#' @param catches
#' @param sigma_effort
#' @param profit_lags
#' @param theta_tuner
#' @param q_cv
#' @param q_ac
#' @param cost_cv
#' @param cost_ac
#' @param max_perc_change_f
#' @param max_cr_ratio
#' @param q_slope
#' @param oa_ratio
#' @param mey_buffer
#' @param effort_ac
#'
#' @return a fleet object
#' @export
#'
#' @examples
#' \dontrun{
#' create_fleet(eq_f = 2,length_50_sel = 25, length_95_sel = 27, fish = bluefish)
#' }
create_fleet_az <- function(eq_f = NA,
                         length_50_sel = 1,
                         fish,
                         mpa_reaction = 'concentrate',
                         beta = 1.3,
                         b_ref_oa = 0.25,
                         q = 1e-3,
                         cost_slope = 0,
                         cost_intercept = 1,
                        # cost_intercept_hl = NA,
                        # cost_intercept_bll = NA,
                         fleet_model = 'constant-effort',
                         effort_allocation = 'gravity',
                         cost_function = 'constant',
                         profit_lags = 1,
                         initial_effort = 0.2,
                         delta = 2,
                         q_cv = 0.00,
                         q_ac = 0,
                         q_slope = 0,
                         cost_ac = 0,
                         cost_cv = 0,
                         L = 0
                         ) {


  length_bins <- as.numeric(colnames(fish$length_at_age_key))

  sel_at_bin <- ((1 / (1 + exp(-log(
    19
  ) * ((length_bins - length_50_sel) / (delta)
  )))))

  p_sel_at_age <- (as.matrix(fish$length_at_age_key) %*% sel_at_bin)

  length_95_sel <- (length_50_sel + delta)

  sel_at_age <- p_sel_at_age

  #mey_buffer <- mey_buffer

  rm(fish)

  fleet <- list(mget(ls()))

  fleet <- fleet[[1]]

  return(fleet)
}
