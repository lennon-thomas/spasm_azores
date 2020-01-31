
determine_and_distribute_effort_az<- function (
  L= fleet$L,
  pops = pop %>% filter(year == y),
  fish = fish,
  fleet = fleet,
  price = fish$price,
  cost_slope = fleet$cost_slope,
  cost_intercept = fleet$cost_intercept,
  num_patches = num_patches,
  beta = fleet$beta,
  manager = manager
){
  
  fun <- function (x,b,c) (price*b*exp(-x))-(beta*c*x^(beta-1))-L#(x^1.3)-(10*x)+10
  pop_summary<- pops %>%
    group_by(patch) %>%
    summarise(patch_biomass=sum(biomass),
              distance = unique(distance))
  #add cost per patch
  pop_summary <- pop_summary %>% mutate(patch_cost = cost_intercept + (cost_slope * distance))
  #solve for E
  epatch<-vector()
  for (i in 1:dim(pop_summary)[1]){
    #epatch[i] <- uniroot(fun, c(0, 1000000),pop_summary$biomass[i],pop_summary$patch_cost[i])$root
    errortry <- try( d <- uniroot(fun, c(0, 1000000),pop_summary$patch_biomass[i],pop_summary$patch_cost[i])$root,silent = TRUE)
    if (class(errortry) == "try-error") {
      epatch[i] <- 0
    }else{
      epatch[i] <- d
    }
  }
  
  pops$effort<-epatch %>%
    rep(each = length(unique(pops$age)))
 
  pops$effort[pops$mpa==TRUE]<-0
  
  return(pops$effort) 
}
