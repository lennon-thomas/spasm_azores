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
  mutate(q=0.00014) %>%
  mutate(d_effort = catch/(q*B))

biomass<-sum(pops$biomass,na.rm = TRUE)

ggplot(data,aes(y=effort,x=B)) +
  geom_point() +
  geom_smooth(method='lm',fullrange=TRUE) +
  theme_bw() +
  #xlim(c(0,12000)) +
  #ylim(c(0,26)) +
  theme(axis.title=element_text(size=16)) +
  ylab("effort")

# geom_point(x=12976,y=24.28,col="red",size=3)

#ggplot(data,aes(y=biomass,x=year)) +
 # geom_line() +
#  geom_line(data=data,aes(y=TAC),col="red",show.legend = TRUE) +
  #ylim(c(0,7000)) +
 # xlim(c(,2015)) +
  #theme_bw() +
  #theme(axis.title=element_text(size=16))

lm_f<-lm(data$effort~data$B)
intercept<-lm_f$coefficients[[1]]
slope<-lm_f$coefficients[[2]]
#y=a+bX

new_effort<-intercept + slope * biomass
#new_effort<-new_effort * fleet$q
return(new_effort)
}
