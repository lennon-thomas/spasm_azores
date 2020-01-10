#' \code{move_fish} moves fish via a movement function
#'
#' @param here_pop
#'
#' @return relocated adult fish
#' @export
#'
move_fish <- function(here_pop, num_patches, fish, move_matrix){

  # from Siegal et al. 2003

  # move_foo <- function(numbers, move_matrix) {
  #
  #   moved <- as.numeric(numbers %*% move_matrix)
  #
  there_pop <- here_pop %>%
    dplyr::group_by(age) %>%
    # mutate(numbers = eigen_mat_mult(matrix(numbers) %>% t(), move_matrix) %>% as.numeric()) %>%
   dplyr:: mutate(numbers = crossprod(here_pop$numbers, move_matrix) %>% as.numeric(),
                  biomass = crossprod(here_pop$biomass, move_matrix) %>% as.numeric(),
                  ssb = crossprod(here_pop$ssb, move_matrix) %>% as.numeric(),) %>%
    # mutate(numbers = move_foo(numbers, move_matrix)) %>%
    dplyr::ungroup()

  return(there_pop)

}
