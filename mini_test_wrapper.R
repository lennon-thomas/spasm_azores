
library(tidyverse)
library(FishLife)
#library(spasm)
library(ggridges)
library(gganimate)
library(raster)

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
source('R/generate-length-to-age.R')
source('R/create_manager.R')
source('R/calc-density-gradient.R')
source('R/generate-timeseries.R')
source('R/grow_and_die.R')
source('R/calculate_recruits.R')
boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/ianla/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"test"


# Read in distance data from rasters and define habitat quality an --------


juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
shore_dist<-read.csv(paste0(boxdir,runname,"/distance_to_shore.csv"))

hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))
num_patches<-length(unique(adult_distance$from))
# Create dataframe that has cell_no, patch_no, whether juve or adult, and habitat quality


cell_lookup<-data.frame(matrix(ncol=4,nrow=num_patches))
colnames(cell_lookup)<-c("patch","cell_no","juve_ad_hab","hab_qual")
cell_lookup[,1]<-c(1:20)
cell_lookup[,2]<-unique(adult_distance$from)
cell_lookup[,3]<-c(rep(0,10),rep(1,10))
cell_lookup[,4]<- hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))# Adult habitat quality


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
    adult_movement = 500,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  1,
    price = 14.5*1000, # biomass is in units of metric tons
    price_cv = 0,
    price_ac = 0,
    price_slope =  0.0001
  )


#Code chunk demonstrating use of funciton: create_fish_az_from_import


# #Using the import function to query FishLife, indentical behavior to `create_fish_az()`
# fish <- create_fish_az_from_import(
#   scientific_name = "Pagellus bogaraveo",
#   query_fishlife = T,
#   mat_mode = "length",
#   time_step = 1,
#   cv_len = 0,
#   sigma_r = 0.00,
#   steepness = 0.8,
#   r0 = 10972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
#   rec_ac = 0,
#   adult_movement = 20,
#   larval_movement = 2000,
#   density_dependence_form = 2,
#   density_movement_modifier =  0.5,
#   price = 14.5*1000, # biomass is in units of metric tons
#   price_cv = 0,
#   price_ac = 0,
#   price_slope =  0.0001,
#   import_life_hist_params = F
#   )
# 
# #Example of using import function to import life history parameters from local file. Note comments on several variables.
# fish <- create_fish_az_from_import(
#   scientific_name = "Pagellus bogaraveo",
#   query_fishlife = F, #set query_fishlife equal to false
#   mat_mode = "length",
#   time_step = 1,
#   cv_len = 0,
#   sigma_r = 0.00,
#   r0 = 10972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
#   rec_ac = 0,
#   larval_movement = 2000,
#   density_dependence_form = 2,
#   density_movement_modifier =  0.5,
#   price = 14.5*1000, # biomass is in units of metric tons
#   price_cv = 0,
#   price_ac = 0,
#   price_slope =  0.0001,
#   import_life_hist_params = T, #set import boolean variable to true
#   life_hist_params_path = "data/blackspot_parameters.csv", #specify location of data file to import
#   #The create_fish_az_from_import function will NOT overwrite variable values defined explicitly in the function with import values by default. Therefore, we must assign the overlapping variables to "NA" to allow for the import values to be used. Example:
#   t0 = NA,
#   min_age = NA,
#   steepness = NA,
#   adult_movement = NA
# )


# Define fleet ------------------------------------------------------------

fleet <- create_fleet_az(
  fish = fish,
  q = 0.014, # Get this from JABBA output
  cost_intercept = 50,#e-02,# 1e-03,#853.3343,#440.6,
  #cost_factor = 1, #How many X bigger are capital costs relative to cost of fuel (i.e. how much is distance from shore going to matter)
  # distance_factor<-5, # This should be how much it costs to go each km (~fuel cost/km)   cost_cv =  0,
  cost_ac = 0,
  cost_slope = 10,#1e-06, 
  cost_cv = 0,
  beta = 1.3,
  #This has to be >0 in order for distance from shore to be considered cost but increases costs significantly
  q_cv = 0.00,
  q_ac = 0,
  q_slope = 0,
  #eq_f = .1,
  b_ref_oa = 0.25,#0.25,
 # max_cr_ratio = 0.8, ## this is cost revene ratio- the higher the number the higher the costs.  this is how you change economics.
  fleet_model = "constant effort",
 # sigma_effort = 0.0,
  length_50_sel = 0.1 * fish$linf,
  initial_effort = 0.2, # This is something we can take out depending on which equations we are using
  delta = 2, # need to figure out wha this  means- borrowed value from spasm
#  theta = 1e-1,
 # max_perc_change_f = 2,
  effort_allocation = "simple", #"gravity", #'simple',
  mpa_reaction = "concentrate",#"leave", #"leave"
  profit_lags=3,
  L = 		0.000010) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)



# Simulate Fishery (not working yet bc of distribute fleet function- just working on inside the function for now) -------------------------------------------------------

system.time(simple <- sim_fishery_az(
  fish = fish,
  fleet = fleet,
  manager = create_manager(mpa_size = 0.3,
                           year_mpa = 100),
  num_patches = 20,
  sim_years = 150,
  burn_years = 1,
  time_step = fish$time_step,
  #est_msy = FALSE,
  random_mpas =TRUE,
  min_size = 0.05,
  mpa_habfactor = 1,
  sprinkler = TRUE,
  keep_burn = FALSE,
  adult_distance = adult_distance,
  juve_adult_distance = juve_adult_distance,
  adult_juve_distance = adult_juve_distance,
  juve_distance = juve_distance,
  shore_dist = shore_dist,
  hab_qual = hab_qual,
  rec_driver = "stochastic",
  estimate_costs = FALSE ,
  constant_L = TRUE))#constant annual value of effort to be distributed to all patches




t<-
simple %>%
  filter(year == max(year)) %>%
  # mpa == TRUE) %>%
  dplyr::group_by(patch)
sum(t$biomass)/t$b0
#plot_spasm_az(simple, type = "patch", font_size = 12, L=fleet$L)

plot_spasm_az(simple, type = "totals", font_size = 12,L=fleet$L)
