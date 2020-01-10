#' calculate density gradient
#'
#' Calculates a modifier for the distance based adult movement matrix based
#' on biomass
#'
#' @param pop
#' @param y
#' @param num_patches
#' @param density_modifier
#'
#' @return a movement matrix modifier
#' @export
#'
calc_density_gradient <- function(pop, y, num_patches, density_modifier, b0_buffer = 1.25) {

adults <- pop %>%
  dplyr::filter(year == y) %>%
  dplyr::mutate(biomass = numbers * weight_at_age) %>%
  dplyr::group_by(patch) %>%
  dplyr::summarise(b = sum(biomass))

adults_zero <- pop %>%
  dplyr::filter(year == min(pop$year)) %>%
  dplyr::mutate(biomass = numbers * weight_at_age) %>%
  dplyr::group_by(patch) %>%
  dplyr::summarise(b0 = b0_buffer * sum(biomass))

adults <- adults %>%
  dplyr::left_join(adults_zero, by = "patch") %>%
  dplyr::mutate(density = pmin(1,(b / b0) * density_modifier)) %>%
  dplyr::select(patch, density)


density_gradient <-
  expand.grid(
    source = 1:num_patches,
    sink = 1:num_patches
  ) %>%
  dplyr::left_join(adults %>% rename(source_density = density), by = c("source" = "patch")) %>%
  dplyr::left_join(adults %>% rename(sink_density = density), by = c("sink" = "patch")) %>%
  dplyr::mutate(gradient = source_density - sink_density + 1) %>%
  # dplyr::mutate(dgrad = dnorm(gradient,1, max(1e-6,fish$density_movement_modifier))) %>%
  dplyr::select(source, sink, gradient) %>%
  tidyr::spread(sink, gradient) %>%
  dplyr::select(-source) %>%
  as.matrix()

# density_gradient <-
#   expand.grid(
#     source = 1:num_patches,
#     sink = 1:num_patches
#   ) %>%
#   dplyr::left_join(adults %>% rename(source_density = density), by = c("source" = "patch")) %>%
#   dplyr::left_join(adults %>% rename(sink_density = density), by = c("sink" = "patch")) %>%
#   dplyr::mutate(gradient = pmax(0,source_density - sink_density)) %>%
#   dplyr::mutate(dgrad = dnorm(gradient,1, max(1e-6,fish$density_movement_modifier))) %>%
#   dplyr::select(source, sink, dgrad) %>%
#   tidyr::spread(sink, dgrad) %>%
#   dplyr::select(-source) %>%
#   as.matrix()


return(density_gradient)

}
