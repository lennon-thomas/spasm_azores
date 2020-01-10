#' update_fish
#'
#' @param fish
#'
#' @return an updated fish list
#' @export
#'
#' @examples
#' \dontrun{
#' update_fish(fish %>% mutate(linf = 222))
#' }
update_fish <- function(fish){

  fish_vars <- formals(create_fish) %>% names() #look up arguments to create fish

  update_fish_vars <- fish[fish_vars] %>% names() #find the components of fish that are part of that

  update_fish_vars <- update_fish_vars[is.na(update_fish_vars) == F] #remove missing components

  new_fish <- pmap(fish[update_fish_vars], create_fish) %>%
    flatten() #run create fish with new variables

}