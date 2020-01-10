#' create manager
#'
#' @param year_mpa
#'
#' @return a manager object
#' @export
#'
#' @examples
#'
#' \dontrun{
#' create_manager(year_mpa = 12)
#' }
create_manager <- function(year_mpa = 15,
                           mpa_size = 0.25,
                           mpa_locations = NA){



  manager <- list(year_mpa = year_mpa,
                   mpa_size = mpa_size,
                  mpa_locations = mpa_locations)

  return(manager)

}