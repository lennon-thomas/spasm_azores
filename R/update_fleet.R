#' update_fleet
#'
#' @param fleet
#' @param fish
#'
#' @return an updated fish list
#' @export
#'
#' @examples
#' \dontrun{
#' update_fleet(fleet)
#' }
update_fleet <- function(fleet,fish){

  fleet_vars <- formals(create_fleet) %>% names() #look up arguments to create fish

  update_fleet_vars <- fleet[fleet_vars] %>% names() #find the components of fish that are part of that

  update_fleet_vars <- update_fleet_vars[is.na(update_fleet_vars) == F] #remove missing components

  new_fleet <- pmap(fleet[update_fleet_vars], create_fleet, fish = fish) %>%
    flatten() #run create fleet with new variables

}