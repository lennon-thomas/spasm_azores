find_L_az<-
  function(dev_profit,
           pops = pop %>% filter(year == y),
           cell_lookup = cell_lookup,
           year = y,
           fish = fish,
           burn_years = burn_years,
           total_effort = effort[y],
           fleet = fleet,
           num_patches = num_patches,
           beta = fleet$beta,
           cost_slope = fleet$cost_slope,
           cost_intercept = fleet$cost_intercept, #853.3343
           price = fish$price,
           q = fleet$q[1]) 
         {
  
    
 
    
    
    
  #  dev_profit<-(price*q*total_ssb)-beta*total_avg_cost*total_effort^(beta-1)
  #  effort<-((price*q* total_ssb-dev_profit)/(beta*total_avg_cost))^(1/(beta-1))
    
    pop_summary<- pops %>%
      group_by (patch) %>%
      summarise (patch_biomass = sum(biomass,na.rm = TRUE),
                 distance = unique(distance)) %>%
      ungroup() %>%
      mutate(patch_cost = cost_intercept + cost_slope * distance,
             p_f = 1-(patch_cost/(price-L)*patch_biomass))
    
    #  pop_summary$p_effort<-ifelse(pop_summary$p_effort<0,0,pop_summary$p_effort)
    #  pop_summary$p_effort<-ifelse(is.na(pop_summary$p_effort),0,pop_summary$p_effort)
    pop_summary$p_f<-ifelse(is.na( pop_summary$p_f),0, pop_summary$p_f)
    
    #  total_avg_costs<-mean(pop_summary$patch_cost) 
    # total_profit<-sum(profi
    
   
    
    pop_summary$p_f<-ifelse(is.na( pop_summary$p_f),0, pop_summary$p_f)
    
    sse<-(total_effort-total_estimated_effort)^2
    
    return (sse) }







