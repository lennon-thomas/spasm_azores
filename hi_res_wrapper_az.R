library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
library(rgl)
library(surrogate)

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


# Biological functions ----------------------------------------------------


fish <-
  create_fish_az(
    scientific_name = "Pagellus bogaraveo",
    query_fishlife = T,
    mat_mode = "length",
    time_step = 1,
    cv_len = 0,
    sigma_r = 0.00,
    steepness = 0.8,
    r0 = 10972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
    rec_ac = 0,
    adult_movement = 50000,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  1,
    price = 14.5*1000, # biomass is in units of metric tons
    price_cv = 0,
    price_ac = 0,
    price_slope =  0.0001
  )
# Define fleet ------------------------------------------------------------

fleet <- create_fleet_az(
  fish = fish,
  q = 0.014, # Get this from JABBA output
  cost_intercept =50,#853.3343,#440.6,
  #cost_factor = 1, #How many X bigger are capital costs relative to cost of fuel (i.e. how much is distance from shore going to matter)
  # distance_factor<-5, # This should be how much it costs to go each km (~fuel cost/km)   cost_cv =  0,
  cost_ac = 0,
  cost_slope = 2, 
  cost_cv = 0,
  beta = 1.3,
  #This has to be >0 in order for distance from shore to be considered cost but increases costs significantly
  q_cv = 0.00,
  q_ac = 0,
  q_slope = 1e-6,
  #eq_f = .1,
  b_ref_oa = 0.25,#0.25,
  # max_cr_ratio = 0.8, ## this is cost revene ratio- the higher the number the higher the costs.  this is how you change economics.
  fleet_model = "constant effort",
  # sigma_effort = 0.0,
 # length_50_sel = 0.5* fish$linf, #0.5
  length_50_sel = 0.000001 * fish$linf,
  initial_effort = 0.2, # This is something we can take out depending on which equations we are using
  delta = 2,#steepness of selectivity curve # need to figure out wha this  means- borrowed value from spasm
  #  theta = 1e-1,
  # max_perc_change_f = 2,
  effort_allocation = "simple", #"gravity", #'simple',
  mpa_reaction = "concentrate",#"leave", #"leave"
  profit_lags=3,
  L = 		0.000010) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)

#option to fish all ages
fleet$sel_at_age[c(1:2),]<-1
# Simulate Fishery (not working yet bc of distribute fleet function- just working on inside the function for now) -------------------------------------------------------

system.time(simple <- sim_fishery_az(
  fish = fish,
  fleet = fleet,
  manager = create_manager(mpa_size = 0.5,
                           year_mpa = 80),
  num_patches = num_patches,
  sim_years = 100,
  burn_years = 30,
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
  constant_L = FALSE))#constant annual value of effort to be distributed to all patches

#View(simple)
#plot_spasm_az(simple, type = "patch", font_size = 12, L=fleet$L)
sim_sum<-simple %>%
  group_by(year,cell_no) %>%
  summarise(biomass = sum(biomass),
            biomass_caught = sum(biomass_caught),
            effort = unique(effort),
            f = unique(f),
            profits=sum(profits),
            mpa = unique(mpa)
            )

plot_spasm_az(sim_sum, type = "totals", font_size = 12,L=fleet$L)

#plot(area)

hab_qual_map<-all_habitat

hab_qual_map[cell_lookup$cell_no]<-cell_lookup$hab_qual

juve_map<-area

juve_map[unique(juve_adult_distance$from)]<-10
juve_map[juve_map<10]<-NA

plot(hab_qual_map,add=TRUE,alpha = 0.5)

end<-sim_sum %>% filter(year == max(year))  
start<-sim_sum %>% filter(year == burn_years + 1) 

end_effort<-area
end_effort[sim_sum$cell_no]<-end$effort
start_effort<-area
start_effort[sim_sum$cell_no]<-start$effort
total_start<-cellStats(start_effort,"sum")
start_effort<-start_effort/total_start
vms<-raster(paste0(boxdir,runname,"/BLL_prj.tif"))
total_vms<-cellStats(vms,"sum")
vms<-vms/total_vms


par(mfrow=c(1,2)) 
plot(start_effort,main="predicted effort",colNA = "black")
#plot(end_effort)
plot(vms,main="observed effort",colNA="black")
results<-stack(c(start_effort,vms))
## S3 method for class 'RasterStack'
show_landscape(results, xlab = "Lat",
               ylab = "Long", discrete = FALSE, unique_scales = FALSE,
               n_col = NULL, n_row = NULL) +
  theme_nlm( viridis_scale = "C")


plot(hab_qual_map)


vms_cells<-Which(!is.na(vms),cells=TRUE)


