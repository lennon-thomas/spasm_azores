library(tidyverse)
library(FishLife)
library(spasm)
library(ggridges)
library(gganimate)
library(raster)
library(rgdal)
library(landscapetools)
library(ggpubr)

source('~/GitHub/spasm_azores/R/find_L_az.R')
source('R/sim_fishery_az.R')
source('R/plot-spasm_az.R')
source('R/plot_fleet.R')
source('R/plot_annual.R')
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



adult_movement<-200
cost_intercept <-11624.14
cost_slope<-10
sim_years<-40
year_mpa<-30
burn_years<-5
price<-14500
L=	5940.00#1e+04#2500#0.2*price
size_mpa<-.15 #percent of EEZ

area<-raster(paste0(boxdir,runname,"all_habitat.tif"))
area[area==0]<-NA


condor_raster<-raster(paste0(boxdir,"raster_output_4k/condor_raster.tif"))
condor_raster_cells<-Which(condor_raster==1,cells=TRUE)


adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))


# Identify 15% of cells with highest and lowest fishing effort ------------


vms_df<-read.csv(paste0(boxdir,runname,"/vms_df.csv")) %>%
  arrange(desc(effort_value,na.rm=TRUE))

#vms_complete<-na.omit(vms_df)

# How many cells are equal to 15%
#top_fifteen<-round(nrow(vms_complete)*.15)

#top_cells<-vms_complete$cell_no[1:no_mpa_cells]

#vms_low<-vms_df %>% #read.csv(paste0(boxdir,runname,"/vms_df.csv")) %>%
 # dplyr::arrange(effort_value)

#vms_low<-na.omit(vms_low)

#low_cells<-vms_low$cell_no[1:no_mpa_cells]

#vms_df<-vms_df %>%
 # mutate(top_cells=ifelse(cell_no %in% top_cells,1,0),
  #       low_cells = ifelse(cell_no %in% low_cells,1,0))


cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))

area[cell_lookup$cell_no]<-cell_lookup$hab_qual

num_patches<-nrow(cell_lookup)

no_mpa_cells<-round((num_patches*size_mpa),0)

handline_cells<-read.csv(paste0(boxdir,runname,"/handline_cells.csv"))

handline_cell_vec<-handline_cells$cell

# convert distance to shore to nautical miles and define where fishing occurs in each fleet
cell_lookup<-cell_lookup %>%
  mutate(distance_miles= distance*0.539957) %>%
  mutate(handline = ifelse(cell_no %in% handline_cell_vec,1,0)) %>%
  mutate(bottom_ll = ifelse(cell_no %in% handline_cell_vec,0,1)) %>%
  mutate(condor=ifelse(cell_no %in% condor_raster_cells,1,0))

cell_lookup<-left_join(cell_lookup,vms_df,by="cell_no")
cell_lookup<-cell_lookup %>%  
dplyr::arrange(desc(effort_value)) %>%
  mutate(top_cells=NA,
         low_cells=NA)

cell_lookup$top_cells[1:no_mpa_cells]<-1
cell_lookup$top_cells[is.na(cell_lookup$top_cells)]<-0

cell_lookup$low_cells[(nrow(cell_lookup)-no_mpa_cells):(nrow(cell_lookup)-1)]<-1
cell_lookup$low_cells[is.na(cell_lookup$low_cells)]<-0
# top_fifteen<-round(nrow(cell_lookup)*.15)
# 
# top_cells<-cell_lookup$cell_no[1:top_fifteen]
# 
# cell_lookup<-cell_lookup %>%
#   mutate(top_fifteen=ifelse(cell_no %in% top_cells,1,0),
#          bottom_fifteen = ifelse(cell_no %in% low_cells,1,0))

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
      r0 = 16972.933, #This should correspond to give us the K from best Jabba run during burn years. Still need to create function to solve for this.
      rec_ac = 0,
      adult_movement = adult_movement,
      larval_movement = 200,
      density_dependence_form = 2,
      density_movement_modifier =  1,
      price =  price#14.5*1000, # biomass is in units of metric tons
    )


 
# Create Fleets ------------------------------------------------------------


  fleet <- create_fleet_az(
    fish = fish,
    q = 0.014, # Get this from JABBA output
    #cost_intercept_hl = cost_intercept_hl,#853.3343,#440.6,
    #cost_intercept_bll = cost_intercept_bll,
    cost_intercept =cost_intercept,
    cost_slope = cost_slope, 
    beta = 1.3,
    length_50_sel = 0.000001 * fish$linf,  #What is minimum size they are being caught?
    initial_effort = 200, # This is something we can take out depending on which equations we are using
    delta = 2,#steepness of selectivity curve 
  #  mpa_reaction = "leave",#"leave", #"leave"
    profit_lags=0,
    L=L) # This is how sensitive fleet is to changes in profit. Do the respond on annual basis vs. 5 year average.)
  
  #option to fish all ages
   # fleet$sel_at_age[c(1:4),]<-0 #1

# # Define MPA type ------------------------------------------------------------

# Number of MPA cells 


handline_cells<-cell_lookup %>%
  filter(handline==1)

# Set MPA to handline only
handline_mpa<-handline_cells$patch[1:no_mpa_cells]


# Set MPA to bottomlongline only
bll_cells<-cell_lookup %>%
  filter(bottom_ll==1) %>%
  select(patch)

bll_mpa<-sample(bll_cells$patch)#bll_cells$patch[1:no_mpa_cells]
bll_mpa<-bll_mpa[c(1:no_mpa_cells)]
# Set MPA to bottomlongline and handline

mpa_cells_split<-round((num_patches*size_mpa)/2,0)
handline_mpa2<-sample(handline_cells$patch)
handline_mpa2<-handline_mpa2[c(1:mpa_cells_split)]

bll_cells<-bll_cells %>%
  arrange(patch)
bll_mpa2<-sample(as.vector(bll_cells$patch))
bll_mpa2<-bll_mpa2[c(1:(mpa_cells_split+1))]
both_mpa<-c(handline_mpa2,bll_mpa2)


# Set MPA to areas with highest vms effort
hi_effort_cells<-cell_lookup %>%
  filter(top_cells==1)

hi_effort_mpa<-hi_effort_cells$patch

# Set MPA to areas with lowest vms effort
lo_effort_cells<-cell_lookup %>%
  filter(low_cells==1)

lo_effort_mpa<-lo_effort_cells$patch




mpa_cells<-cbind(hi_effort_mpa,lo_effort_mpa,bll_mpa,handline_mpa,both_mpa)
mpa_scen<-c("High_effort_mpa","Low_effort_MPA","Bottom_longline_MPA","Handline_MPA","Both MPA")

for( i in 1:length(mpa_scen)) {   
  system.time(simple <- sim_fishery_az(
    fish = fish,
    fleet = fleet,
    manager = create_manager(mpa_size = size_mpa,
                             year_mpa = year_mpa,
                             mpa_locations=mpa_cells[,i]), #choices: handline_mpa,bll_mpa,both_mpa,hi_effort_mpa,lo_effort_mpa
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
 
  sim_sum <- simple %>%
   filter(year > 10) %>%
    group_by(year, cell_no) %>%
    summarise(
      biomass = sum(biomass),
      biomass_caught = sum(biomass_caught, na.rm = TRUE),
      f = unique(f),
      profits = sum(profits, na.rm = TRUE),
      mpa = unique(mpa),
      b0 = unique(b0),
      distance = unique(distance),
      ssb = sum(ssb),
      handline = unique(handline),
      bottom_ll = unique(bottom_ll)
    ) %>%
    ungroup() %>%
    mutate(b_ratio = biomass / b0) %>%
    mutate(cost_slope = fleet$cost_slope,
           mpa_scen=mpa_scen[i]) 

  
 write.csv(sim_sum,paste0("results/",mpa_scen[i],"_results.csv"))
}
 
result_files = list.files(path="results", pattern="*.csv", full.names=TRUE)
result_files

all_results  <- sapply(result_files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

sim_sum<-all_results %>%
    mutate(fleet_no=ifelse(handline==1,1,2))
   
  sim_sum$fleet_no<-as.factor(sim_sum$fleet_no)
  
  
 # cell_count<-sim_sum %>%
 #    filter(year == 40) %>%
 #    group_by(fleet_no) %>%
 #    summarise(total_cells= length(mpa))
 #   
 #    
 #  mpa_count<-sim_sum %>%
 #    filter(year == 40) %>%
 #    filter(mpa==TRUE) %>%
 #    group_by(fleet_no) %>%
 #    summarise(total_cells= length(mpa))  

rel_prof_year<-year_mpa-1  

pre_mpa_profit<-sim_sum %>%
  group_by(year,fleet_no,mpa_scen) %>%
  summarise(annual_profit_final=sum(profits,na.rm = TRUE)) %>%
  filter(year==rel_prof_year) %>%
  ungroup()%>%
  select(-c(year))


sim_sum<-  left_join(sim_sum,pre_mpa_profit,by = c("fleet_no","mpa_scen"))
  
sim_sum<-sim_sum %>%
  group_by (year,fleet_no,mpa_scen) %>%
#  mutate(relative_profit_hl = pre_mpa_profit$annual_profit[1],
 #        relative_profit_bll = pre_mpa_profit$annual_profit[10]) %>%
  filter(year>=year_mpa-3) %>%
  mutate(years=year-year_mpa)

levels(sim_sum$fleet_no) <-c("Handline","Bottom longline")
 
annuals_fleet<-sim_sum%>%
  group_by(years,fleet_no,mpa_scen) %>%
  summarise(
    f = sum(f)/num_patches,
    Profits = sum(profits,na.rm=TRUE),
    Biomass = sum(biomass,na.rm=TRUE),
    Catch = sum(biomass_caught,na.rm=TRUE),
    B0 =unique(b0),
    relative_profit_hl = unique(annual_profit_final),
    relative_profit_bll = unique(annual_profit_final)                          
  ) %>%
  ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / (Catch/Biomass),
         B_ratio = Biomass/B0,
         rel_profit = ifelse(fleet_no==1,Profits/relative_profit_hl,Profits/relative_profit_bll)) %>%
  dplyr:: select(-c(B0,Biomass,'Profit Per Unit Effort',B_ratio,f,Catch,Profits,relative_profit_hl,relative_profit_bll)) %>%
  gather(metric, value,-c(years,fleet_no,mpa_scen))


plot_fleet(annuals_fleet,font_size=12) 
ggsave("results/fleet_results.png")
annuals<-sim_sum%>%
  group_by(years,mpa_scen) %>%
  summarise(
    f = sum(f)/num_patches,
    Profits = sum(profits,na.rm=TRUE),
    Biomass = sum(biomass,na.rm=TRUE),
    Catch = sum(biomass_caught,na.rm=TRUE),
    B0 =unique(b0),
  #  relative_profit_hl = unique(annual_profit_final),
   # relative_profit_bll = unique(annual_profit_final)                          
  ) %>%
  ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / (Catch/Biomass),
         B_ratio = Biomass/B0) %>%
        # rel_profit = ifelse(fleet_no==1,Profits/relative_profit_hl,Profits/relative_profit_bll)) %>%
  dplyr:: select(-c(B0,Biomass,'Profit Per Unit Effort',f,Profits)) %>%
  gather(metric, value,-c(years,mpa_scen))

plot_annual(annuals)

ggsave("results/biomass_catch.png")



# Plot MPAs ---------------------------------------------------------------
library(rgeos)
 library(rnaturalearth)
 library(tidyverse)
 library(raster)
 library(rgdal)
 library(wesanderson)
 library(sf)
 goraz=raster(paste0(boxdir,"raster_output_4k/goraz_mdl_prj.tif")) 

 EEZ=raster(paste0(boxdir,"raster_output_4k/azores_EEZ_proj_4k.tif"))
  
 condor<-st_read(paste0(boxdir,"condor_seamount/condor_seamount.shp"))
 EEZ_shape<-st_read("/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/raw_sp/azores_EEZ.shp")
 EEZ=raster(paste0(boxdir,"raster_output_4k/azores_EEZ_proj_4k.tif"))
 goraz=raster(paste0(boxdir,"raster_output_4k/goraz_mdl_prj.tif"))
 land=st_read(paste0(boxdir,"tmp_sp/land_shape/azores_islands.shp"))
 
# boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/Data/SIGMAR"
 library(maptools)
 library(PBSmapping)
 six<-readOGR(dsn="/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/Data/SIGMAR/six_nm/",layer="6NM to coastline")
 six_line<-st_read("/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/Data/SIGMAR/six_nm/6NM to coastline.shp")
 
 six_line<-st_transform(six_line,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))
 
 three_line<-st_read("/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/Data/SIGMAR/three_nm/3NM to coastline.shp")
 
 three_line<-st_transform(three_line,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))
 juv=raster(paste0(boxdir,"raster_output_4k/juv_proj.tif"))
 
 
 EEZ_shape<-st_transform(EEZ_shape,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))
 six<-spTransform(six,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))

 three<-readOGR(dsn="/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/Data/SIGMAR/three_nm/",layer="3NM to coastline")
 
 s<-SpatialLines2PolySet(six)
 six<- PolySet2SpatialPolygons(s, close_polys=TRUE)
 three<-spTransform(three,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))
 
 t<-SpatialLines2PolySet(three)
 three<- PolySet2SpatialPolygons(t, close_polys=TRUE)
 goraz<-mask(goraz,EEZ,updatevalue=NA,inverse=FALSE)
 
 
 
 world <- rnaturalearth::ne_countries(scale="small", type = "countries", sovereignty="Portugal",returnclass = "sf") %>% 
   sf::st_transform(crs(area))
 
 land<-land %>%
   sf::st_transform(crs(goraz))
 
 condor<-st_transform(condor,crs(area))
 my_theme <- theme(axis.text=element_blank(),
                   axis.title=element_blank(),
                   legend.position = "right",
                   legend.text=element_text(size=14),
                   legend.title=element_text(size=16),
                   plot.title=element_text(size=9),
                   # panel.grid.major = element_blank(), 
                   panel.grid.minor = element_line(color = "white"),
                   panel.background = element_rect(color = "white",fill="white"),
                   axis.line = element_line(colour = "white"), axis.ticks  = element_line(colour = "white"),
                   panel.grid.major = element_line(size = 0.7, colour = 'white'))
 
 
 zoom_out<-coord_sf(xlim = c(-239710.8, 1068289), ylim = c(3718070, 4778070 ), expand = TRUE)
 
 goraz<-mask(goraz,EEZ,updatevalue=NA,inverse=FALSE)
 
 #sim_sum<-read.csv(paste0(boxdir,runname,"/base_mpa_sim_sum.csv")) 

 
 cell_no<-unique(sim_sum$cell_no)
 
 long_lat<-xyFromCell(goraz,(cell_no))
 long_lat<-as.data.frame(cbind(cell_no,long_lat))
 names(long_lat)<-c("cell_no","long","lat")
 sim_sum<-sim_sum %>%
   left_join(long_lat,by="cell_no")
 
 before_sim<-sim_sum %>%
   filter(year==year_mpa-1) %>%
   mutate(time="before")
 
 after_sim<-sim_sum %>%
   filter(year==max(year)) %>%
   mutate(time="after")
 
 
 mpa_df<-sim_sum %>%
   filter(year==max(year)) %>%
   filter(mpa==TRUE)
 
 before_after<-rbind(before_sim,after_sim)
 
 mpa_names<-c("7.5 % of handline and bottom longline area"," 15% of bottom longline","15% of handline area","15% highest of fishing effort area","15% of lowest fishing effort area")
 mpa_scenario = c("Both MPA","Bottom_longline_MPA", "Handline_MPA","High_effort_mpa","Low_effort_MPA")  
 
 plot_list<-list() 
 for( i in 1:length(mpa_names)) {
 
   dataframe<-after_sim %>%
    filter(mpa_scen==mpa_scenario[i])
 
after<-ggplot() +
   geom_tile(dataframe, mapping=aes(x=long, y=lat, fill=mpa),size=0.05) +

   scale_fill_manual("",labels=c("Fishing Area","MPA"),values=c("lightblue","red")) +
   #  scale_fill_viridis(direction=-1)+
   #facet_wrap(~time,scales="fixed")+
   #  scale_fill_gradientn("SSB",colours=rev(rainbow(50)))+
   # geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="wheat") +
   # geom_tile(mpa_df,mapping=aes(x=x,y=y,fill=mpa),fill="red",show.legend = TRUE) +
 #  geom_sf(data=world, fill="white", lwd=0.05, col="black",size=1) +
  # geom_sf(data=EEZ_shape, fill="NA", lwd=0.25, col="black") +
   geom_sf(data = six_line,  fill="white", lwd=0.25, col="black",linetype=2) +
   geom_sf(data = three_line, fill="white", lwd=0.25, col="black",linetype=3) +
    geom_sf(data=condor,col="red",lwd=0.5,fill="red") +
  # facet_wrap(~mpa_scen,nrow=3,ncol=2) +
   labs(x="", y="", title=paste0("")) +
   theme_bw() + my_theme+ 
   theme(plot.title=element_text(face="bold",size=8)) +
   geom_sf(data=land,fill="black") + 
   coord_sf(xlim = c(-90000,708289), ylim = c(4058070,4428070), expand = TRUE) +
   ggtitle(mpa_names[i])

  plot_list[[i]] = after   

 #ggsave(after,paste0("results/mpa_plots_",mpa_names[i],".png"))
 }
 
# p <- grid.arrange(grobs=plot_list,ncol=2)
 plot1<-plot_list[[1]]
 plot2<-plot_list[[2]]
 plot3<-plot_list[[3]]
 plot4<-plot_list[[4]]
 plot5<-plot_list[[5]]
 ggarrange(plot1,plot2,plot3,plot4,plot5,common.legend = TRUE,nrow=3,ncol=2,legend="right")
 ggsave("results/mpa_plot.png")
 ## Create Refine handline/bottomLL values
 cell_long_lat<-xyFromCell(goraz,cell_lookup$cell_no)
 
 cell_lookup<-cbind(cell_lookup,cell_long_lat)
 
 fleet_df<-cell_lookup %>% 
   mutate(fleet_no=ifelse(handline==1,1,2)) %>%
   select(x,y,fleet_no)

 fleet_df$fleet_no<-as.factor(fleet_df$fleet_no)
   
 ggplot() +
   geom_tile(fleet_df, mapping=aes(x=x, y=y, fill=fleet_no),col="grey80",size=0.05) +
  
  # facet_wrap(~time,scales="fixed")+
   #  scale_fill_gradientn("SSB",colours=rev(rainbow(50)))+
   # geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="wheat") +
   # geom_tile(mpa_df,mapping=aes(x=x,y=y,fill=mpa),fill="red",show.legend = TRUE) +
  # geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
  # geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
   geom_sf(data = six_line,  fill="white", lwd=0.45, col="black",linetype=2) +
   geom_sf(data = three_line, fill="white", lwd=0.45, col="black",linetype=3) +
   scale_fill_viridis_d("Fleet",labels=c("Handline","Bottom longline"),direction =-1) +
   scale_linetype_manual(labels=c("2 mboundary, 6 m boundary")) +
   geom_sf(data=condor,fill=c("red"),lwd=0.5,color=NA,legend=TRUE) +
  # scale_color_manual(values = c("red","white"),"",label="MPA") +
   geom_sf(data = six_line,  fill="white", lwd=0.25, col="black",linetype=2) +
   geom_sf(data = three_line, fill="white", lwd=0.25, col="black",linetype=3) +
   labs(x="", y="", title=paste0("")) +
   theme_bw() + my_theme+
   geom_sf(data=land,fill="black") + 
   coord_sf(xlim = c(-90000,708289), ylim = c(4058070,4428070), expand = TRUE)
 
 fleet_raster<-rasterFromXYZ(fleet_df,res=c(4000,4000),crs="+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs")
 plot(fleet_raster,col=c("red","blue"))
 
 
 
 condor_cells<-raster::extract(x=goraz,y=condor,na.rm=TRUE,cellnumbers=TRUE)
 condor_cells<-condor_cells$cell
 boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
 
 runname<-"raster_output_4k"
 
 write.csv(condor_cells,paste0(boxdir,runname,"/condor_cells.csv")) 
 

 adult<-goraz
 adult[!is.na(adult)]<-1
 
 all_habitat<-stack(adult,juv)
 
 all_habitat<-stackApply(all_habitat,c(1,1), fun=sum,na.rm=TRUE)
 
 all_habitat[all_habitat==0]<-NA
 
 all_cells<-Which(!is.na(all_habitat),cells = TRUE) 
 
 
three_nm_cells<-raster::extract(x=all_habitat,y=three,na.rm=TRUE,cellnumbers=TRUE, buffer=-1)
six_nm_cells<-raster::extract(x=all_habitat,y=six,na.rm=TRUE,cellnumbers=TRUE,buffer=-2000)




george<-as.data.frame(six_nm_cells[[1]])%>%
  mutate(island="george")
santa_maria<-as.data.frame(six_nm_cells[[2]])%>%
  mutate(island="santa maria")
terceria<-as.data.frame(three_nm_cells[[3]]) %>%
  mutate(island="terceria")

faial<-as.data.frame(six_nm_cells[[1]]) %>%
  mutate(island="faial and pico")
gracioso<-as.data.frame(six_nm_cells[[4]]) %>%
  mutate(island="gracioso")

corvo<-as.data.frame(six_nm_cells[[5]])%>%
  mutate(island="corvo")
sao_miguel<-as.data.frame(three_nm_cells[[8]]) %>%
  mutate(island="corvo")

handline_cells<-rbind(george,santa_maria,terceria,faial,gracioso,corvo,sao_miguel)

write.csv(handline_cells,paste0(boxdir,runname,"/handline_cells.csv"))

# end wrapper -------------------------------------------------------------
 base_plot<-ggplot() +
   
   geom_tile(pre, mapping=aes(x=x, y=y, fill=f),col="black",size=0.005) +
   scale_fill_viridis(direction=-1)+
   # scale_fill_viridis("Biomass")+
 # scale_fill_gradientn("f",colours = pal) +
 #  facet_wrap(~L) +
   geom_sf(data=world, fill="grey80", lwd=0.05, col="white",size=1) +
   # geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
   labs(x="", y="", title="") +
 # my_theme+ # xlim(c(-239710.8,868289)) +
   geom_sf(data=land,fill="black") + 
   #  geom_sf(data=condor,col="red",lwd=0.5,fill=NA) +
   coord_sf(xlim = c(-100000,808289), ylim = c(4008070,4428070), expand = TRUE) 
 ggsave(base_plot,filename=paste0(boxdir,runname,"/Figures/base_f.png"),dpi=600)  
