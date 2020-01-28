# Fleet model- Distributing total effort by patch
p<-14.5 * 1000 #price
num_patches<-20
patch<-c(1:num_patches)
cost_slope = 2
cost_intercept = 413.7
biomass <- as.vector(c(5337,554,554,554,554,554,554,554,554,1107,105,105,105,105,105,105,105,105,105,105))
distance<-as.vector(c(207,183,170,182,157,177,164,183,169,20,20,25.9,51.8,51.8,202,193,210,223,243,276))
pop_summary<-as.data.frame(cbind(patch,biomass,distance))

L=0.5*p*min(pop_summary$biomass) #0.5 of max value# we can pick a number that is reasonable and stick with it for every timestep. 
L =100000                               # 10% of the minimum population for cell (currently)
fun <- function (x,b,c) (p*b*exp(-x))-(1.3*c*x^0.3)-L#(x^1.3)-(10*x)+10
#add cost per patch
pop_summary <- pop_summary %>% mutate(patch_cost = cost_intercept + (cost_slope * distance))
#solve for E
epatch<-vector()
for (i in 1:dim(pop_summary)[1]){
  epatch[i] <- uniroot(fun, c(0, 1000000),pop_summary$biomass[i],pop_summary$patch_cost[i])$root %>%
}
pop_summary$epatch<-epatch
pop_summary<-pop_summary %>%
  mutate(f = epatch*q)
#harvest
sum(pop_summary$biomass*(1-exp(-pop_summary$epatch)))
