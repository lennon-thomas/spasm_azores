
determine_and_distribute_effort_az<- function (
  L= 0.1,
  pops = pop %>% filter(year == y),
  fish = fish,
  fleet = fleet,
  price = fish$price,
  cost_slope = fleet$cost_slope,
  cost_intercept = fleet$cost_intercept,
  num_patches = num_patches,
  beta = fleet$beta
){
  
  fun <- function (x,b,c) (price*b*exp(-x))-(beta*c*x^beta-1)-L#(x^1.3)-(10*x)+10
  pop_summary<- pops %>%
    group_by(patch) %>%
    summarise(patch_ssb=sum(ssb),
              distance = unique(distance))
  #add cost per patch
  pop_summary <- pop_summary %>% mutate(patch_cost = cost_intercept + (cost_slope * distance))
  #solve for E
  epatch<-vector()
  for (i in 1:dim(pop_summary)[1]){
    epatch[i] <- uniroot(fun, c(0, 1000000),pop_summary$patch_ssb[i],pop_summary$patch_cost[i])$root 
  } 
  
  pops$effort<-epatch %>%
    rep(each = length(unique(pops$age)))
 
  return(pops$effort) 
}
