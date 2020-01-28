# Explore relationship between L and cost and effort
#1334 is what you would want for total effort at this biomass level according to linear relationship
#L	0.64646465
#cost_intercept = 1e-03
library(viridis)
library(tidyr)

L<-seq(0,1,length.out=100)
cost_slope <- 1e-06
c_intercept=c(1e-6,1e-5,1e-4,1e-3,1e-2)
Lande<-expand.grid(L,c_intercept)
N <- dim(Lande)[1]
total_effort=rep(NA,N)
Lande<-cbind(Lande,total_effort)
names(Lande) <- c('L','c_intercept','total_effort')


#cost_intercept<-c(1e-06,1,10,100,1000,0000)



for(s in 1:N) {
  fun <- function (x,b,c) (price*b*exp(-x))-(beta*c*x^beta-1)-Lande$L[s]#(x^1.3)-(10*x)+10
  pop_summary<- pops %>%
    group_by(patch) %>%
    summarise(patch_ssb=sum(ssb),
              distance = unique(distance))
  #add cost per patch
  pop_summary <- pop_summary %>% mutate(patch_cost =Lande$c_intercept[s] + (cost_slope * distance))
  #solve for E
  epatch<-vector()
  print(s)
  for (i in 1:dim(pop_summary)[1]){
    epatch[i] <- uniroot(fun, c(0, 10000000),pop_summary$patch_ssb[i],pop_summary$patch_cost[i])$root 
  } 
  Lande$total_effort [s] = sum(epatch)
#  Lande$c_intercept[s] = cost_intercept[c]
}

ggplot(Lande,aes(x=L,y=total_effort,col=as.factor(c_intercept)))+
  geom_point() +
  scale_color_viridis_d("cost_intercept")+
  theme_bw()

# cols<-c("black","blue","red","yellow","green","purple")
# cost_intercept=c(1e+06,1,100,1000,5000,10000)
# c=1
# data<-Lande %>%
#   filter(c_intercept==cost_intercept[c])
# plot<-ggplot(data)+geom_point(aes(x=L,y=total_effort),col=cols[c],alpha=0.5) + theme_bw()
# for(c in 2:length(cost_intercept)){
#   data2<-Lande %>%
#     filter(c_intercept==cost_intercept[c])
#     plot<-plot+geom_point(data=data2,aes(x=L,y=total_effort),col=cols[c],alpha=0.5)
# }
  
  

 
