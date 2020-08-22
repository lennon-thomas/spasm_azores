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
source('~/GitHub/spasm_azores/R/create_fish_az_from_import.R')

boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"raster_output_4k"
#runname<-"low_res_run"
area<-raster(paste0(boxdir,runname,"all_habitat.tif"))
area[area==0]<-NA
patches<-Which(!is.na(area),cells=TRUE)
num_patches<-length(patches)
condor<-readOGR(dsn=paste0(boxdir,"condor_seamount"),layer="condor_seamount")
condor<-spTransform(condor,crs(area))

area[cell_lookup$cell_no]<-cell_lookup$cell_no

#condor_raster<-rasterize(condor,goraz,filename=paste0(boxdir,"raster_output_4k/condor_raster.tif"))
condor_raster<-raster(paste0(boxdir,"raster_output_4k/condor_raster.tif"))
condor_raster_cells<-Which(condor_raster==1,cells=TRUE)

#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
#adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))


cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))

area[cell_lookup$cell_no]<-cell_lookup$hab_qual


num_patches<-nrow(cell_lookup)

 adult_movement<-200
  cost_intercept<-2000#150#4950321/num_patches/15#40000
  cost_slope<-1
#  L<-1e+04#-0.8*14500*6646/1177#500000 #0.00005
  sim_years<-50
  year_mpa<-100
  burn_years<-5
  price<-14500
  L=0.1*price
  mpasize<- 0
  
  
  L<-seq(0.01,1.2*price,length.out=10)
  cost_intercept_vec<-seq(1000,200000,length.out=10)
  cost_slope_vec<-c(0,100,1000,10000,length.out=4)
  
  L_cost_B<-expand.grid(L,cost_intercept_vec,cost_slope_vec)
  N <- dim(L_cost_B)[1]
  B_ratio=rep(NA,N)
  sse=rep(NA,N)
  L_cost_B<-cbind(L_cost_B,B_ratio,sse)
  names(L_cost_B) <- c('L','c_intercept',"c_slope",'B_ratio',"catch")
# Biological functions ----------------------------------------------------
 
  # fish <-
  #   create_fish_az(
  #     scientific_name = "Pagellus bogaraveo",
  #     query_fishlife = T,
  #     mat_mode = "length",
  #     time_step = 1,
  #     cv_len = 0,
  #     sigma_r = 0.00,
  #     steepness = 0.8,
  #     r0 = 13972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
  #     rec_ac = 0,
  #     adult_movement = adult_movement,
  #     larval_movement = 200,
  #     density_dependence_form = 2,
  #     density_movement_modifier =  1,
  #     price =  price#14.5*1000, # biomass is in units of metric tons
  #   )

  fish <- create_fish_az_from_import(
    scientific_name = "Pagellus bogaraveo",
    query_fishlife = F, #set query_fishlife equal to false
    mat_mode = "length",
    time_step = 1,
    cv_len = 0,
    sigma_r = 0.00,
    r0 = 13972.933/3, #11492=ssb0,33332.74This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
    rec_ac = 0,
    larval_movement = 2000,
    density_dependence_form = 2,
    density_movement_modifier =  0.5,
    price = price,#14.5*1000, # biomass is in units of metric tons
    price_cv = 0,
    price_ac = 0,
    import_life_hist_params = T, #set import boolean variable to true
    life_hist_params_path = "data/blackspot_parameters.csv", #specify location of data file to import
    #The create_fish_az_from_import function will NOT overwrite variable values defined explicitly in the function with import values by default. Therefore, we must assign the overlapping variables to "NA" to allow for the import values to be used. Example:
    t0 = NA,
    min_age = 0,
    steepness = NA,
    adult_movement = NA
  )
  # Define fleet ------------------------------------------------------------

  for(i in 1:N) {  
    
  fleet <- create_fleet_az(
    fish = fish,
    q = 0.014, # Get this from JABBA output
    cost_intercept =L_cost_B$c_intercept[i],#853.3343,#440.6,
    cost_slope = L_cost_B$c_slope[i], 
    beta = 1.3,
    length_50_sel = 0.000001 * fish$linf,
    initial_effort = 200, # This is something we can take out depending on which equations we are using
    delta = 2,#steepness of selectivity curve 
    mpa_reaction = "leave",#"leave", #"leave"
    profit_lags=10) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)
  
  #option to fish all ages
 fleet$sel_at_age[c(1:4),]<-1


    
  system.time(simple <- sim_fishery_az(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = mpasize,#mpa_scen[i],
                             year_mpa = year_mpa),
    num_patches = num_patches,
    sim_years = sim_years,
    burn_years = burn_years,
    time_step = fish$time_step,
    #est_msy = FALSE,
    random_mpas =TRUE,
    min_size = 1,
    mpa_habfactor = 1,
    keep_burn = TRUE,
    adult_distance = adult_distance,
    juve_adult_distance = juve_adult_distance,
    adult_juve_distance = adult_juve_distance,
    juve_distance = juve_distance,
    shore_dist = shore_dist,
    hab_qual = hab_qual,
    rec_driver = "stochastic",
    constant_L = FALSE,
    L = L_cost_B$L[i],
    condor_mpa_cells=condor_raster_cells)) #constant annual value of effort to be distributed to all patches

an_sum<-simple %>%
  group_by(year) %>%
  summarise(biomass = sum(biomass),
            biomass_caught = sum(biomass_caught),
            f = (sum(biomass_caught))/(sum(biomass)),
            profits=sum(profits),
            #  mpa = unique(mpa),
            b0 = unique(b0),
            L = unique(L),
            sse = unique(sse)
            #  distance = unique(distance)
  ) %>%
  ungroup() %>%
  mutate(b_ratio = biomass/b0
         #  cost_slope = fleet$cost_slope,
  ) %>%
  #   cost_intercept = c_intercept[i])
  filter(year==max(year))


L_cost_B$B_ratio[i]<-an_sum$b_ratio
L_cost_B$catch[i]<-an_sum$biomass_caught


print(paste0("Done with ",i))
  } 
  write.csv(an_sum,paste0(boxdir,runname,"grid_search2.csv"))
  write.csv(L_cost_B,paste0(boxdir,runname,"grid_search_results2.csv"))
  L_cost_B<-read.csv(paste0(boxdir,runname,"grid_search_results.csv") 
  #View(simple)
#   #plot_spasm_az(simple, type = "patch", font_size = 12, L=fleet$L)
  # sim_sum<-simple %>%
  # # filter(year>20) %>%
  #   group_by(year,cell_no) %>%
  #   summarise(biomass = sum(biomass),
  #             ssb = sum(ssb),
  #             biomass_caught = sum(biomass_caught,na.rm=TRUE),
  #           #  effort = unique(effort),
  #             f = unique(f),
  #             profits=sum(profits,na.rm = TRUE),
  #             mpa = unique(mpa),
  #             b0 = unique(b0),
  #             distance = unique(distance)
  #           #  L= unique(L)
  #             ) %>%
  #   ungroup() %>%
  #   mutate(b_ratio = biomass/b0)  %>%
  #   mutate(cost_slope = fleet$cost_slope)
   # filter(!year==burn_years + 1)
  # write.csv(sim_sum,paste0(boxdir,runname,"/sim_sum.csv"))

  # plot_spasm_az(sim_sum, type = "totals", font_size = 12)
# test<-unique(cell_lookup$cell_no)[1:10]
# short<-simple %>%
#   filter(cell_no %in% test)
#   
#  plot_spasm_az(short,type="patch",font_size=12)


 L_cost_B<-L_cost_B %>%
   filter(L<15000) %>%
   filter(c_slope!=10) %>%
   filter(c_slope!=3)
 
 
 
 c_slope_lab<-c("b_i=1","b_i = 100")
 
 names(c_slope_lab)<-(c(1,101))
 
 ggplot(L_cost_B,aes(x=L,y=c_intercept))+
   geom_tile(aes(fill=B_ratio))+
   #scale_fill_viridis_c("Bratio",n=1000) +
   scale_fill_gradient2(mid= "white", low="blue",high="red","B_ratio",midpoint=0.3) +
   theme_bw() +
   facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
   scale_x_continuous(expand=c(0,0)) +
   scale_y_continuous(expand=c(0,0)) +
   xlab(expression(italic("L"))) +
   ylab(expression(italic(alpha)))
 
   ylab(expression(Blah[d1]))
 
 ggplot(L_cost_B,aes(x=L,y=c_intercept))+
   geom_tile(aes(fill=catch))+
   #scale_fill_viridis_c("Bratio",n=1000) +
   scale_fill_gradient2(mid= "white", low="blue",high="red","Catch (mt)",midpoint=500) +
   theme_bw() +
   facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
   scale_x_continuous(expand=c(0,0)) +
   scale_y_continuous(expand=c(0,0)) +
   xlab(expression(italic("L"))) +
   ylab(expression(italic(alpha)))
 
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
 
 # end wrapper -------------------------------------------------------------


