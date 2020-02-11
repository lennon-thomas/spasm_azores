#' determine_effort
#'
#' \code{determine_effort} determines the effort in the current time period based
#' on different functional forms
#'
#' @param last_effort effort in the last time period
#' @param fleet the fleet object
#' @param fish the fish object
#' @param y the current year
#' @param burn_years the last year of the burn period
#' @param pop the population object
#' @param mpa the mpa object
#' @param num_patches the number of patches in the system
#'
#' @return the effort in the current year
#' @export
#'
#'# This function should return a single annual effort value. If burn year it should be 0. Otherwise, use relationship between

determine_effort_az <-
  function(fleet,
           fish,
           y,
           pops,
           mpa,
           num_patches,
           boxdir) {

data<-read.csv(paste0(boxdir,"jabba_b_and_f.csv")) %>%
  mutate(q=fleet$q) %>%
  mutate(d_effort = f/q)

biomass<-sum(pops$ssb,na.rm = TRUE)

ggplot(data,aes(y=f,x=B)) +
  geom_point() +
  geom_smooth(method='lm',fullrange=TRUE) +
  theme_bw() +
  theme(axis.title=element_text(size=16)) +
  ylab("f")



lm_effort<-lm(data$f~data$B)
intercept<-lm_effort$coefficients[[1]]
slope<-lm_effort$coefficients[[2]]
#y=a+bX

new_f<-intercept + slope * biomass
#new_effort<-new_effort / fleet$q
return(new_f)
}
