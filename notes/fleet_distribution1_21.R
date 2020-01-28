# Fleet model- Distributing total effort by patch

#total_f<-0.2226327 # This is known from the JABBA model
#q <-0.00014 # taken from JABBA model
#total_profit<-1579.55
distribute_fleet_test<-
  function(
    dev_profit,
  
    price = 14.5 * 1000,
    beta = 1.3,
    num_patches = 20,
    patch = c(1:20),
    cost_slope = 2,
    cost_intercept = 440.6,
    total_effort= 1569.597,
    q = 0.00014) {
    biomass = as.vector(c(5337,554,554,554,554,554,554,554,554,1107,105,105,105,105,105,105,105,105,105,105))
    
    distance=as.vector(c(207,183,170,182,157,177,164,183,169,20,20,25.9,51.8,51.8,202,193,210,223,243,276))      

pop_summary<-as.data.frame(cbind(patch,biomass,distance))

total_biomass<-sum(pop_summary$biomass) 

#est_profit<-0.15*total_biomass*price

#total_avg_cost<-(price*total_effort*q*total_biomass-test_profit)/(total_effort^beta)

#cost_intercept<-total_avg_cost-cost_slope*mean(distance)

total_avg_cost<-cost_intercept + cost_slope*mean(distance)

total_profit<-(price*q*total_effort*total_biomass)-((total_avg_cost*(total_effort^beta)))
#total_profit
# Derivative of total profit as a function of f
#dev_profit<-(price*q*total_biomass)-beta*total_avg_cost*total_effort^(beta-1)

#dev_profit

effort<-((price*q* total_biomass-dev_profit)/(beta*total_avg_cost))^(1/(beta-1))
#total_effort-effort


# Patch costs will be different for each patch and constant patch over time.
pop_summary <- pop_summary  %>%
  mutate(biomas = biomass,
         L = dev_profit/num_patches,
         patch_cost = cost_intercept + cost_slope * distance,
         first_term = price * q * biomass - L,
         second_term = (beta * patch_cost),
         p_effort = ((price * q * biomass - L)/(beta * patch_cost))^(1/(beta-1)),
         p_f = p_effort *q)
         # p_f = ((price*biomass-L)/(beta*patch_cost))^(1/0.3),
       #  p_b= price*biomass,
        # beta_cost = beta* patch_cost,
         #profit_check = price *p_f * biomass-patch_cost*p_f^beta)
         #p_revenue = price*biomass*p_f,
         #p_fishing_cost = (patch_cost * p_f) ^ beta,
         #profit_check = p_revenue - p_fishing_cost)

pop_summary$p_effort<-ifelse(pop_summary$p_effort<0,0,pop_summary$p_effort)

total_estimated_effort<-sum(pop_summary$p_effort,na.rm = TRUE)

sse<-(total_effort-total_estimated_effort)^2

return (sse)
}


distribute_fleet_test(dev_profit = 15323.86385)

guess3<-c(1,1e+07)

optimize(distribute_fleet_test, interval = c(10000.00000,100000.00000))

dev_profit<--1795420

# total_avg_cost
# mean(pop_summary$patch_cost)
# ggplot(pop_summary,(aes(x=distance,y=patch_cost)))+
#   geom_point()+
#   xlab("Distance to shore (km)") +
#   ylab("Cost per unit of f")+
#   theme_bw()
# # plot(pops$distance, pops$patch_cost)
# 
# # Calculate effort for each patch assuming that profits across patches are equal
# 
# pop_summary <-
#   pop_summary %>%
#  # mutate(patch_profit = total_profit / num_patches) %>%
# #  mutate (e_patch = patch_profit/((price * q * biomass) - (patch_cost)^beta)) %>%
#   mutate(dev_patch_profit = dev_profit/num_patches) %>%
#   mutate(f_patch=exp(log((price * biomass- dev_patch_profit)/(beta*patch_cost))/0.3)) %>%
#   mutate(revenue=price*f_patch*biomass,
#          cost=(patch_cost*f_patch^1.3),
#          cr= cost/revenue,
#          p=revenue-cost)
#  # mutate(e_patch = ((price * q * biomass- dev_patch_profit)/(beta*patch_cost))^(10/3))
# 
# 
# pop_summary$e_patch<-ifelse(pop_summary$e_patch<0,0,pop_summary$e_patch)
# 
# pop_summary
# 
# sum(pop_summary$e_patch,na.rm=TRUE)
# 
# sse <-(total_f)-sum(pop_summary$f_patch,na.rm=TRUE)
# 
# sse
