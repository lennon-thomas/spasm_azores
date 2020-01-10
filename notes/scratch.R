p=8
q=1
b=100000
beta = 1.3
c=.01
effort = 100
profit<- p*q*b-beta*c*effort^(beta-1)

effort<-exp(log((p*q*b-profit)/(beta*c))/0.3)

effort2<-((p*q*b-profit)/(beta*c))^(10/3)
