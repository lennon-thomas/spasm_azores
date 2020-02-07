# Explore relationship between L and cost and effort
#1334 is what you would want for total effort at this biomass level according to linear relationship
#L	0.64646465
#cost_intercept = 1e-03
library(viridis)
library(tidyr)
library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
#library(rgl)
#library(surrogate)
library(landscapetools)

source('~/GitHub/spasm_azores/R/find_L_az.R')
source('R/sim_fishery_az.R')
source('R/plot-spasm_az.R')
source('R/get_traits_az.R')
source('R/create_fish_az.R')
source('R/distribute_fleet_az.R')
source('R/determine_effort_az.R')
source('R/sim_fishery_az.R')
source('R/estimate-costs_az.R')
source('R/create_fleet_az.R')
source('R/move_fish_az.R')
source('~/GitHub/spasm_azores/R/create_fleet_az.R')
source('R/determine_and_distribute_effort_az.R')
source('~/GitHub/spasm_azores/R/get_traits_az.R')

boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"raster_output_4k"
#runname<-"low_res_run"
area<-raster(paste0(boxdir,runname,"all_habitat.tif"))
area[area==0]<-NA
patches<-Which(!is.na(area),cells=TRUE)
num_patches<-length(patches)

#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
#adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))


cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))

area[cell_lookup$cell_no]<-cell_lookup$hab_qual


num_patches<-nrow(cell_lookup)

adult_movement<-200
cost_slope<-10
sim_years<-50
year_mpa<-75
burn_years<-1
price<-14500
#L<-seq(1,600,length.out=100)
L<-seq(1e-08,10000,length.out=10)
cost_intercept_vec<-seq(1e-8,600,length.out=5)
cost_slope_vec<-c(0.001,0.1,2,length.out=3)

L_cost_B<-expand.grid(L,cost_intercept_vec,cost_slope_vec)
N <- dim(L_cost_B)[1]
B_ratio=rep(NA,N)
sse=rep(NA,N)
L_cost_B<-cbind(L_cost_B,B_ratio,sse)
names(L_cost_B) <- c('L','c_intercept',"c_slope",'B_ratio',"sse")

#cost_intercept<-c(1e-06,1,10,100,1000,0000)

fish <-
  create_fish_az(
    scientific_name = "Pagellus bogaraveo",
    query_fishlife = T,
    mat_mode = "length",
    time_step = 1,
    cv_len = 0,
    sigma_r = 0.00,
    steepness = 0.8,
    r0 = 13972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
    rec_ac = 0,
    adult_movement = adult_movement,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  1,
    price =  price,#14.5*1000, # biomass is in units of metric tons
  )

for(i in 1:N) {

 
  
  fleet <- create_fleet_az(
    fish = fish,
    q = 0.014, # Get this from JABBA output
    cost_intercept = L_cost_B$c_intercept[i],#853.3343,#440.6,
    cost_slope = L_cost_B$c_slope[i], 
    beta = 1.3,
    length_50_sel = 0.000001 * fish$linf,
    initial_effort = 200, # This is something we can take out depending on which equations we are using
    delta = 2,#steepness of selectivity curve 
    mpa_reaction = "leave",#"leave", #"leave"
    ) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)
  
  
   system.time(simple <- sim_fishery_az_L(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = 0,#mpa_scen[i],
                             year_mpa = year_mpa),
    num_patches = num_patches,
    sim_years = sim_years,
    burn_years = burn_years,
    time_step = fish$time_step,
    #est_msy = FALSE,
    random_mpas =TRUE,
    min_size = 0.05,
    mpa_habfactor = 1,
    sprinkler = TRUE,
    keep_burn = TRUE,
    adult_distance = adult_distance,
    juve_adult_distance = juve_adult_distance,
    adult_juve_distance = adult_juve_distance,
    juve_distance = juve_distance,
    shore_dist = shore_dist,
    hab_qual = hab_qual,
    rec_driver = "stochastic",
    estimate_costs = FALSE ,
    constant_L = FALSE,
    L = L_cost_B$L[i]))#constant annual value of effort to be distributed to all patches
  
   
   
  
   # Define fle
  #View(simple)
  #plot_spasm_az(simple, type = "patch", font_size = 12, L=fleet$L)
  an_sum<-simple %>%
    group_by(year) %>%
    summarise(biomass = sum(biomass),
              biomass_caught = sum(biomass_caught),
              effort = sum(effort),
              profits=sum(profits),
            #  mpa = unique(mpa),
              b0 = unique(b0),
              L = unique(L),
              sse = unique(sse)
            #  distance = unique(distance)
    ) %>%
    ungroup() %>%
    mutate(b_ratio = biomass/b0,
         #  cost_slope = fleet$cost_slope,
           f = effort * fleet$q) %>%
        #   cost_intercept = c_intercept[i])
           filter(year==max(year))


L_cost_B$B_ratio[i]<-an_sum$b_ratio
L_cost_B$sse[i]<-an_sum$sse


print(paste0("Done with ",i))
}

write.csv(an_sum,paste0(boxdir,runname,"grid_search.csv"))
write.csv(L_cost_B,paste0(boxdir,runname,"grid_search_results.csv"))


L_cost_B<-L_cost_B %>%
  mutate(rescale_b =B_ratio - 0.3)

c_slope_lab<-c(0.01,0.1,2,3)
names(c_slope_lab)<-c("cost slope = 0.01","cost slope = 0.1", "cost slope = 2", "cost_slope = 3")
ggplot(L_cost_B,aes(x=L,y=c_intercept))+
  geom_tile(aes(fill=B_ratio))+
  #scale_fill_viridis_c("Bratio",n=1000) +
  scale_fill_gradient2(mid= "white", low=muted("blue"),high=muted("red"),"B_ratio",midpoint=0.3) +
  theme_bw() +
  facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("L") +
  ylab("cost intercept")

L_cost_B<-L_cost_B %>%
  filter(sse<2e+13) 
ggplot(L_cost_B,aes(x=L,y=c_intercept))+
  geom_tile(aes(fill=sse))+
  #scale_fill_viridis_c("Bratio",n=1000) +
  scale_fill_gradient2(mid= "white", low=muted("blue"),high=muted("red"),"sse",midpoint=0) +
  theme_bw() +
  facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("L") +
  ylab("cost intercept")


theme_heat <- theme_classic() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())

# basic plot
ggplot(L_cost_B,aes(x=L,y=c_intercept))+
  geom_tile(aes(fill=B_ratio))+
   facet_wrap(~c_slope) + theme_heat +
  ylab("cost intercept") +

# plot with text overlay and viridis color palette
geom_text(aes(label = round(L_cost_B$B_ratio, 4)), 
                 color = "white",size=1.5) +
  scale_fill_viridis() +
  # formatting
 ggtitle("",subtitle = "cost slope")
 #labs(caption = "Source: vcdExtra::Yamaguchi87") +
 # theme(plot.title = element_text(face = "bold")) +
#  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) 
 # theme(plot.caption = element_text(color = "grey68"))
 
down<- L_cost_B %>%
  filter(B_ratio < 0.4 & B_ratio > 0.24) %>%
  filter(sse < 3e13) 

ggplot(down,aes(x=L,y=c_intercept))+
  geom_tile(aes(fillB_ratio))+
  #scale_fill_viridis_c("Bratio",n=1000) +
  scale_fill_gradient2(mid= "white", low=muted("blue"),high=muted("red"),"B_ratio",midpoint=0.3,na.value="grey") +
  theme_bw() +
  facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("L") +
  ylab("cost intercept")

ggplot(down,aes(x=L,y=c_intercept))+
  geom_tile(aes(fill=sse))+
  #scale_fill_viridis_c("Bratio",n=1000) +
  scale_fill_gradient2(mid= "white", low=muted("blue"),high=muted("red"),"sse",midpoint=0) +
  theme_bw() +
  facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("L") +
  ylab("cost intercept")



# ggplot(Lande,aes(x=L,y=total_effort,col=as.factor(c_intercept)))+
#   geom_point() +
#   scale_color_viridis_d("cost_intercept")+
#   theme_bw()

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
  
down<- L_cost_B %>%
  filter(B_ratio < 0.4 & B_ratio > 0.24) %>%
  filter(sse < 3e13)

 
