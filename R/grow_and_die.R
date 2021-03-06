#' \code{grow_and_die} runs growth mortality and fishing
#'
#' @param numbers
#' @param fish
#' @param fleet
#' @param f
#' @param mpa
#' @param y
#'
#' @return numbers and catch at age for all ages above recruits
#' @export
#'
#' @examples
#' \dontrun{
#' grow_and_die(numbers, fish, fleet)
#' }
grow_and_die <- function(numbers, f, mpa, fish, fleet,y) {
  survivors <- vector(mode = 'numeric', length = length(numbers))

# survival <- exp(-fish$time_step*(fish$m + (f * fleet$sel_at_age)))
 survival <- 1 -(fish$time_step*f*fleet$sel_at_age)-fish$m

  death <-  1 - survival

  max_index <- length(survivors)

  survivors[2:max_index] <-
    numbers[1:(max_index - 1)] * survival[1:(max_index - 1)]

  survivors[max_index] <-
    survivors[max_index] + numbers[max_index] * survival[max_index]

 caught <- (numbers * (1-f))
 # caught <-
 #   (fish$time_step * f * fleet$sel_at_age) / (fish$time_step * ((1-exp(-fish$m))+ (f * fleet$sel_at_age))) *  (numbers * death)
  
# caught <-
 #   (fish$time_step * f * fleet$sel_at_age) / ((fish$time_step * fish$m )- (fish$time_step * fleet$sel_at_age)) *  (numbers * death)
  # return(survivors)
  
  # return(survivors)

# print(y)
  # if (y > 10){browser()}
  return(list(survivors = survivors, caught = caught))


}
