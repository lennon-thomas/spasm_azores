# Explore relationship between L and cost and effort
#1334 is what you would want for total effort at this biomass level according to linear relationship
#L	0.64646465
#cost_intercept = 1e-03
library(viridis)
library(tidyr)

#L<-seq(1,600,length.out=100)
L<-seq(0,10,length.out=100)
cost_slope <- 1e-6
c_intercept<-c(0.1,10,100,465,1000)
Lande<-expand.grid(L,c_intercept)
N <- dim(Lande)[1]
total_effort=rep(NA,N)
Lande<-cbind(Lande,total_effort)
names(Lande) <- c('L','c_intercept','total_effort')
pops<-simple%>%filter(year==3)
p<-fish$price
beta<-fleet$beta
#cost_intercept<-c(1e-06,1,10,100,1000,0000)

pop_summary <- pops %>%
  group_by(patch) %>%
  summarise(
    patch_ssb=sum(ssb,na.rm=TRUE),
    distance=unique(distance)
  )

for(s in 1:N) {
fun <- function (x,b,c) (p*b*exp(-x))-(1.3*c*x^0.3)-Lande$L[s]#(x^1.3)-(10*x)+10
#add cost per patch
pop_summary <- pop_summary %>% mutate(patch_cost = Lande$c_intercept[s] + (cost_slope * distance))
#solve for E
epatch<-vector()
for (i in 1:dim(pop_summary)[1]){
  #epatch[i] <- uniroot(fun, c(0, 1000000),pop_summary$biomass[i],pop_summary$patch_cost[i])$root
  errortry <- try( d <- uniroot(fun, c(0, 1000000),pop_summary$patch_ssb[i],pop_summary$patch_cost[i])$root,silent = TRUE)
  if (class(errortry) == "try-error") {
    epatch[i] <- 0
  }else{
    epatch[i] <- d
  }
}

epatch
Lande$total_effort[s]<-sum(epatch)
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
  
  

 
