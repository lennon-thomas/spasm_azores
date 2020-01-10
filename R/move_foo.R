#' move function
#'
#' @param numbers
#' @param move_matrix
#'
#' @return moved fish
#' @export
#'
move_foo <- function(numbers, move_matrix) {

  moved <- as.numeric(numbers %*% move_matrix)

}