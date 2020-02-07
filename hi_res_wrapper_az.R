library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
library(rgl)
library(surrogate)
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
#runname<-"raster_output_4k"
runname<-"low_res_run"
#area<-raster(paste0(boxdir,runname,"all_habitat.tif"))
area[area==0]<-NA
patches<-Which(!is.na(area),cells=TRUE)
num_patches<-length(patches)

#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
#adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))


cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))

num_patches<-nrow(cell_lookup)

adult_movement<-200
cost_intercept<-400
cost_slope<-10
L<-0
sim_years<-150
year_mpa<-75
burn_years<-10
price<-3000
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
    adult_movement = adult_movement,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  1,
    price =  price,#14.5*1000, # biomass is in units of metric tons
   # price_cv = 0,
  #  price_ac = 0
#    price_slope =  .000000001
  )
# Define fleet ------------------------------------------------------------

fleet <- create_fleet_az(
  fish = fish,
  q = 0.0014, # Get this from JABBA output
  cost_intercept = cost_intercept,#853.3343,#440.6,
  #cost_ac = 0,
  cost_slope = cost_slope, 
  #cost_cv = 0,
  beta = 1.3,
 # q_cv = 0.00,
  #q_ac = 0,
#  q_slope = 1e-6,
 # b_ref_oa = 0.25,#0.25,
  #fleet_model = "constant effort",
  length_50_sel = 0.000001 * fish$linf,
  initial_effort = 200, # This is something we can take out depending on which equations we are using
  delta = 2,#steepness of selectivity curve 
  mpa_reaction = "leave",#"leave", #"leave"
  L = 	L) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)

#option to fish all ages
fleet$sel_at_age[c(1:2),]<-1
# Simulate Fishery (not working yet bc of distribute fleet function- just working on inside the function for now) -------------------------------------------------------

system.time(simple <- sim_fishery_az(
  fish = fish,
  fleet = fleet,
  manager = create_manager(mpa_size = 0.5,
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
  constant_L = TRUE))#constant annual value of effort to be distributed to all patches

#View(simple)
#plot_spasm_az(simple, type = "patch", font_size = 12, L=fleet$L)
sim_sum<-simple %>%
 filter(year>burn_years+5) %>%
  group_by(year,cell_no) %>%
  summarise(biomass = sum(biomass),
            biomass_caught = sum(biomass_caught),
            effort = unique(effort),
            f = unique(f),
            profits=sum(profits),
            mpa = unique(mpa),
            b0 = unique(b0),
            distance = unique(distance),
            ) %>%
  ungroup() %>%
  mutate(b_ratio = biomass/b0)  %>%
  mutate(cost_slope = fleet$cost_slope)
 # filter(!year==burn_years + 1)

plot_spasm_az(sim_sum, type = "totals", font_size = 12,L=fleet$L)



# end wrapper -------------------------------------------------------------


