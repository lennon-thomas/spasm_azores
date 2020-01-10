#' Title
#'
#' @param fish
#' @param fleet
#' @param length_comps
#' @param true_f
#'
#' @return assessment outputs
#' @export
#'
run_assessments <- function(fish, fleet, length_comps, true_f){

  mean_f <- true_f

  true_spr <- approx_spr(fish = fish, fleet = fleet, mean_f)

  cc <-
    run_catch_curve(fish = fish, length_comps = length_comps)

  cc_z <- -cc

  cc_m <- fish$m

  cc_f <- cc_z - cc_m

  lbspr_lite_f <-
    optim(
      .01,
      lbspr_lite,
      fish = fish,
      fleet = fleet,
      length_comps = length_comps,
      use = 'find-f',
      lower = 0,
      upper = 10
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


  out <- data_frame(true_f = mean_f,
                    true_spr = true_spr,
                    est_cc_f = cc_f,
                    est_lbspr_f = lbspr_f,
                    est_spr = lbspr_lite_spr)


}