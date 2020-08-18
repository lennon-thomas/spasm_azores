library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
library(rgdal)
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

runname<-"raster_output_4k"
#runname<-"low_res_run"


adult_movement<-200
#cost_intercept <-5000
#cost_intercept_hl<-10000 #150#4950321/num_patches/15#40000
#cost_intercept_bll<-20000 
#cost_slope<-100
sim_years<-5
year_mpa<-30
burn_years<-5
price<-14500
#price_bll<-10000
#L=500#1e+04#2500#0.2*price
size_mpa<-0 #percent of EEZ

area<-raster(paste0(boxdir,runname,"all_habitat.tif"))
area[area==0]<-NA

#patches<-Which(!is.na(area),cells=TRUE)
#num_patches<-length(patches)

#condor<-readOGR(dsn=paste0(boxdir,"condor_seamount"),layer="condor_seamount")
#condor<-spTransform(condor,crs(area))
#condor_raster<-rasterize(condor,goraz,filename=paste0(boxdir,"raster_output_4k/condor_raster.tif"))

condor_raster<-raster(paste0(boxdir,"raster_output_4k/condor_raster.tif"))
condor_raster_cells<-Which(condor_raster==1,cells=TRUE)

#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))



vms_df<-read.csv(paste0(boxdir,runname,"/vms_df.csv")) %>%
  arrange(desc(effort_value,na.rm=TRUE))

vms_complete<-na.omit(vms_df)

# How many cells are equal to 15%
top_fifteen<-round(nrow(vms_complete)*.15)

top_cells<-vms_df$cell_no[1:top_fifteen]

vms_df<-vms_df %>%
  mutate(top_fifteen=ifelse(cell_no %in% top_cells,1,0))

#adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
#juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))

cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))

area[cell_lookup$cell_no]<-cell_lookup$hab_qual

num_patches<-nrow(cell_lookup)

# convert distance to shore to nautical miles and define where fishing occurs in each fleet
cell_lookup<-cell_lookup %>%
  mutate(distance_miles= distance*0.539957) %>%
  mutate(handline = ifelse(distance_miles < 6,1,0)) %>%
  mutate(bottom_ll = ifelse(distance_miles >=6,1,0))

cell_lookup<-left_join(cell_lookup,vms_df,by="cell_no") %>%
  arrange(desc(effort_value))

top_fifteen<-round(nrow(cell_lookup)*.15)

top_cells<-cell_lookup$cell_no[1:top_fifteen]

cell_lookup<-cell_lookup %>%
  mutate(top_fifteen=ifelse(cell_no %in% top_cells,1,0))


`# Grid Search -------------------------------------------------------------
`

L<-seq(0.99,0.99*price,length.out = 40)
cost_intercept_vec<-seq(0.0001,170000,length.out=40)
cost_slope_vec<-c(5)

L_cost_B<-expand.grid(L,cost_intercept_vec,cost_slope_vec)
N <- dim(L_cost_B)[1]
B_ratio=rep(NA,N)
sse=rep(NA,N)
L_cost_B<-cbind(L_cost_B,B_ratio,sse)
names(L_cost_B) <- c('L','c_intercept',"c_slope",'B_ratio',"catch")



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
      r0 = 23972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
      rec_ac = 0,
      adult_movement = adult_movement,
      larval_movement = 200,
      density_dependence_form = 2,
      density_movement_modifier =  1,
      price =  price#14.5*1000, # biomass is in units of metric tons
    )

  # fish <- create_fish_az_from_import(
  #   scientific_name = "Pagellus bogaraveo",
  #   query_fishlife = F, #set query_fishlife equal to false
  #   mat_mode = "length",
  #   time_step = 1,
  #   cv_len = 0,
  #   sigma_r = 0.00,
  #   r0 = 13972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
  #   rec_ac = 0,
  #   larval_movement = 2000,
  #   density_dependence_form = 2,
  #   density_movement_modifier =  1,
  #  # price_hl = price_hl,#14.5*1000, # biomass is in units of metric tons
  # #  price_bll = price_bll,
  #  price = 14500,
  #  price_cv = 0,
  #   price_ac = 0,
  #   import_life_hist_params = T, #set import boolean variable to true
  #   life_hist_params_path = "data/blackspot_parameters.csv", #specify location of data file to import
  #   #The create_fish_az_from_import function will NOT overwrite variable values defined explicitly in the function with import values by default. Therefore, we must assign the overlapping variables to "NA" to allow for the import values to be used. Example:
  # #  t0 = NA,
  #   min_age = 0,
  #   steepness = 0.8,
  #  adult_movement = adult_movement
  # )
  # # Define fleet ------------------------------------------------------------
  
mpa_cells<-round(num_patches*size_mpa)

handline_cells<-cell_lookup %>%
           filter(handline==1)

handline_mpa<-handline_cells$patch[1:mpa_cells]

#mpa_cells<-round(num_patches*0.15)

bll_cells<-cell_lookup %>%
  filter(bottom_ll==1)

bll_mpa<-bll_cells$patch[1:mpa_cells]

both<-c(handline_mpa,bll_mpa)

for(i in 1:N) {  

  fleet <- create_fleet_az(
    fish = fish,
    q = 0.014, # Get this from JABBA output
    #cost_intercept_hl = cost_intercept_hl,#853.3343,#440.6,
    #cost_intercept_bll = cost_intercept_bll,
    cost_intercept = L_cost_B$c_intercept[i],
    cost_slope =  L_cost_B$c_slope[i], 
    beta = 1.3,
    length_50_sel = 0.000001 * fish$linf,  #What is minimum size they are being caught?
    initial_effort = 200, # This is something we can take out depending on which equations we are using
    delta = 2,#steepness of selectivity curve 
    mpa_reaction = "leave",#"leave", #"leave"
    profit_lags=0,
    L=L_cost_B$L[i]) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)
  
  #option to fish all ages
   # fleet$sel_at_age[c(1:4),]<-0 #1


    
  system.time(simple <- sim_fishery_az(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = size_mpa,
                             year_mpa = year_mpa,
                             mpa_locations =both),
    num_patches = num_patches,
    sim_years = sim_years,
    burn_years = burn_years,
    time_step = fish$time_step,
    random_mpas =FALSE,
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
    L = fleet$L,
    condor_mpa_cells=condor_raster_cells))#
 
  
  an_sum<-simple %>%
    group_by(year) %>%
    summarise(biomass = sum(biomass),
              biomass_caught = sum(biomass_caught),
              f = (sum(biomass_caught))/(sum(biomass)),
              profits=sum(profits,na.rm=TRUE),
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
write.csv(an_sum,paste0(boxdir,runname,"grid_search3.csv"))
write.csv(L_cost_B,paste0(boxdir,runname,"grid_search_results3.csv"))
L_cost_B<-read.csv(paste0(boxdir,runname,"grid_search_results3.csv"))
  
L_cost_B<-L_cost_B %>%
  filter(L<13500) %>%
  filter(c_intercept<130000)
 
ggplot(L_cost_B,aes(x=L,y=c_intercept))+
  geom_tile(aes(fill=B_ratio))+
  #scale_fill_viridis_c("Bratio",n=1000) +
  scale_fill_gradient2(mid= "white", low="red",high="green","B_ratio",midpoint=0.7) +
  theme_bw() +
  #   facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab(expression(italic("L"))) +
  ylab(expression(italic(alpha)))
       
       ggplot(L_cost_B,aes(x=L,y=c_intercept))+
         geom_tile(aes(fill=catch))+
         #scale_fill_viridis_c("Bratio",n=1000) +
         scale_fill_gradient2(mid= "white", low="blue",high="red","Catch (mt)",midpoint=600) +
         theme_bw() +
    #     facet_wrap(~c_slope, labeller = labeller(c_slope= c_slope_lab) ) +
         scale_x_continuous(expand=c(0,0)) +
         scale_y_continuous(expand=c(0,0)) +
         xlab(expression(italic("L"))) +
         ylab(expression(italic(alpha)))
       
  
  
  
#   sim_sum <- simple %>%
#     filter(year > 10) %>%
#     group_by(year, cell_no) %>%
#     summarise(
#       biomass = sum(biomass),
#       biomass_caught = sum(biomass_caught, na.rm = TRUE),
#       f = unique(f),
#       profits = sum(profits, na.rm = TRUE),
#       mpa = unique(mpa),
#       b0 = unique(b0),
#       distance = unique(distance),
#       ssb = sum(ssb),
#       handline = unique(handline),
#       bottom_ll = unique(bottom_ll)
#     ) %>%
#     ungroup() %>%
#     mutate(b_ratio = biomass / b0) %>%
#     mutate(cost_slope = fleet$cost_slope) 
# 
#   
#  
#   
#  write.csv(sim_sum,paste0(boxdir,runname,"/base_mpa_sim_sum.csv"))
#  #base<-read.csv(paste0(boxdir,runname,"/base_mpa_sim_sum.csv"))
#   small <- sim_sum %>%
#     filter(year > 60)
#   
#   plot_spasm_az(small,
#                 type = "totals",
#                 font_size = 12,
#                 mpasize = size_mpa)
#   
#  
#   sim_sum<-sim_sum %>%
#     mutate(fleet_no=ifelse(handline==1,1,2))
#    
#   sim_sum$fleet_no<-as.factor(sim_sum$fleet_no)
#   
#   
#  cell_count<-sim_sum %>%
#     filter(year == 40) %>%
#     group_by(fleet_no) %>%
#     summarise(total_cells= length(mpa))
#    
#     
#   mpa_count<-sim_sum %>%
#     filter(year == 40) %>%
#     filter(mpa==TRUE) %>%
#     group_by(fleet_no) %>%
#     summarise(total_cells= length(mpa))  
#   
#  plot_spasm_az(sim_sum,type="totals",font_size=12)
#  
#  
#  
#  ggplot(sim_sum,aes(x=year,y=biomass_caught)) +
#    geom_line()+
#    ylab("Catch")+
#    xlab("Year")+
#    scale_y_continuous(limits=c(0,800)) +
#    theme_bw() +
#    geom_vline(aes(30),col = "red",linetype=2)
#  
#  
#  
#  
#  
#  
#  
#  
#  
#  plot_spasm_az(handline_sum,type="totals",font_size=12)
#  plot_spasm_az(bottom_ll_sum,type="totals",font_size=12)
#  pre<-sim_sum %>%
#    filter(year==50) 
#  
#  pre_df<-as.data.frame(cbind(pre,long_lat))
#  
#  post<-sim_sum %>%
#    filter(year==max(year)) 
#  
#  post_df<-as.data.frame(cbind(post,long_lat))
# # end wrapper -------------------------------------------------------------
#  base_plot<-ggplot() +
#    
#    geom_tile(pre, mapping=aes(x=x, y=y, fill=f),col="black",size=0.005) +
#    scale_fill_viridis(direction=-1)+
#    # scale_fill_viridis("Biomass")+
#  # scale_fill_gradientn("f",colours = pal) +
#  #  facet_wrap(~L) +
#    geom_sf(data=world, fill="grey80", lwd=0.05, col="white",size=1) +
#    # geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
#    labs(x="", y="", title="") +
#  # my_theme+ # xlim(c(-239710.8,868289)) +
#    geom_sf(data=land,fill="black") + 
#    #  geom_sf(data=condor,col="red",lwd=0.5,fill=NA) +
#    coord_sf(xlim = c(-100000,808289), ylim = c(4008070,4428070), expand = TRUE) 
#  ggsave(base_plot,filename=paste0(boxdir,runname,"/Figures/base_f.png"),dpi=600)  
