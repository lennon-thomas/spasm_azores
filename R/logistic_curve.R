#' calculate probabilities from a logistic curve
#'
#' @param values
#' @param fifty 50 percent selection
#' @param delta delta on top of 50 percent to calculate 95 percent
#'
#' @return probabilities at each value
#' @export
#'
logistic_curve <- function(values, fifty, delta) {

  prob <- ((1 / (1 + exp(-log(
    19
  ) * ((values - fifty) / ( (fifty + delta) - fifty)
  )))))


}