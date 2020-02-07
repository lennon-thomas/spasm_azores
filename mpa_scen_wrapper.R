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
cost_intercept<-150#4950321/num_patches/15#40000
cost_slope<-0.1
L<-1e+04#-0.8*14500*6646/1177#500000 #0.00005
sim_years<-100
year_mpa<-75
burn_years<-1
price<-14500
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
      r0 = 13972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
      rec_ac = 0,
      adult_movement = adult_movement,
      larval_movement = 2000,
      density_dependence_form = 2,
      density_movement_modifier =  1,
      price =  price,#14.5*1000, # biomass is in units of metric tons
    )
  # Define fleet ------------------------------------------------------------
  
  fleet <- create_fleet_az(
    fish = fish,
    q = 0.014, # Get this from JABBA output
    cost_intercept = cost_intercept,#853.3343,#440.6,
    cost_slope = cost_slope, 
    beta = 1.3,
    length_50_sel = 0.000001 * fish$linf,
    initial_effort = 200, # This is something we can take out depending on which equations we are using
    delta = 2,#steepness of selectivity curve 
    mpa_reaction = "leave",#"leave", #"leave"
    L = 	L) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)
  
  #option to fish all ages
 fleet$sel_at_age[c(1:2),]<-1

 
 
mpa_scen<-c(0,0.05,0.1,0.15,0.2,0.3,0.4,0.5,0.75,1)

mpa_results<-expand.grid(mpa_scen=mpa_scen,
                            mpa_implement = c("before","after"),
                         year = NA,
                         biomass = NA,
                         Catch = NA,
                         Profit = NA)

 for (i in 1:length(mpa_scen)){

    
  system.time(simple <- sim_fishery_az(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = mpa_scen[i],
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
    constant_L = FALSE))#constant annual value of effort to be distributed to all patches
  
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
              distance = unique(distance)
              ) %>%
    ungroup() %>%
    mutate(b_ratio = biomass/b0)  %>%
    mutate(cost_slope = fleet$cost_slope)
   # filter(!year==burn_years + 1)
  
write.csv(sim_sum,paste0(boxdir,runname,"/sim_sum_mpa_scen.csv"))

fish_equil<-sim_sum[sim_sum$year==70,]
mpa_equil<-sim_sum[sim_sum$year==99,]
  
mpa_results$year[ mpa_results$mpa_implement=="before"]<-70
mpa_results$year[mpa_results$ mpa_implement=="after"]<-99
   
mpa_results$Biomass[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="before"]<-sum(fish_equil$biomass)
mpa_results$Catch[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="before"]<-sum(fish_equil$biomass_caught)
mpa_results$Profits[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="before"]<-sum(fish_equil$profits)
 
mpa_results$Biomass[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="after"]<-sum(mpa_equil$biomass,na.rm = TRUE)
mpa_results$Catch[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="after"]<-sum(mpa_equil$biomass_caught,na.rm = TRUE)
mpa_results$Profits[ mpa_scen==mpa_scen[i] & mpa_results$mpa_implement=="after"]<-sum(mpa_equil$profits, na.rm = TRUE)
 
write.csv(mpa_results,paste0(boxdir,runname,"/mpa_results2.csv"))
print(i)  
  plot_spasm_az(sim_sum, type = "totals", font_size = 12,L=fleet$L)
}  
  plot_spasm_az(simple,type="patch",font_size=12,L=fleet$L)

# end wrapper -------------------------------------------------------------


