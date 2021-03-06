
#'
#' @param pops
#' @param effort
#' @param fleet
#'
#' @return a redistributed vector of effort in space
#' @export
#'
#' @examples
#' \dontrun
#'
#' #p_summary

distribute_fleet_az<-
  function(
    dev_profit = opt_dev_profit,
    pops = pop %>% filter(year == y),
    cell_lookup = cell_lookup,
    year = y,
    fish = fish,
    burn_years = burn_years,
    total_effort = f[y],
    fleet = fleet,
    num_patches = num_patches,
    mpa = mpa,
    beta = fleet$beta,
    cost_slope = fleet$cost_slope,
    cost_intercept = fleet$cost_intercept,
    price = fish$price,
    q = fleet$q[1]
  )
  {
 
  #  total_ssb<- sum(pops$ssb,na.rm = TRUE) 
 

   
    
  #  total_avg_cost<-cost_intercept + cost_slope*mean(dist)
    
   # total_profit<-(price*q*total_effort*total_ssb)-((total_avg_cost*(total_effort^beta)))
    
  #  effort<-((price*q* total_ssb-dev_profit)/(beta*total_avg_cost))^(1/(beta-1))
#browser()
    pop_summary<- pops %>%
      group_by (patch) %>%
      summarise (patch_ssb = sum(ssb,na.rm = TRUE),
                 distance = unique(distance)) %>%
      ungroup() %>%
      mutate(patch_cost = cost_intercept + cost_slope * distance,
              p_f = 1-(patch_cost/((fish$price-dev_profit)*patch_ssb))) %>%
      mutate(p_f=replace(p_f,p_f<0,0)) %>% mutate(p_f=replace(p_f,p_f>1,1)) 
   

    
    
  #  pop_summary$p_effort<-ifelse(pop_summary$p_effort<0,0,pop_summary$p_effort)
  #  pop_summary$p_effort<-ifelse(is.na(pop_summary$p_effort),0,pop_summary$p_effort)
 #   pop_summary$p_f<-ifelse(is.na( pop_summary$p_f),0, pop_summary$p_f)
   # pop_summary$p_f<-min(max(0,pop_summary$p_f),1)
#  total_avg_costs<-mean(pop_summary$patch_cost) 
 # total_profit<-sum(profit)
  #effort<-sum(p_effort)
  # we assume beta equals 1.3 which implies increasing effort, increases the unit of cost   
    pops$p_f<-pop_summary$p_f%>%
      rep(each = length(unique(pops$age)))
   
     pops$p_f[pops$mpa==TRUE]<-0
    
return (pops$p_f)
  }

