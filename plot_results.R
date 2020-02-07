boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"raster_output_4k"
library(tidyverse)
library(raster)



boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname<-"raster_output_4k"

mpa_results<-read.csv(paste0(boxdir,runname,"/mpa_results.csv"))

area<-raster(paste0(boxdir,runname,"all_habitat.tif"))

sim_sum<-read.csv(paste0(boxdir,runname,"/sim_sum.csv"))

area[unique(sim_sum$cell_no)]<-unique(sim_sum$cell_no)
# MPA coverage ------------------------------------------------------------


mpa_results<-mpa_results %>%
  select(mpa_scen,mpa_implement,Biomass,Catch,Profits) %>%
  # group_by(mpa_scen,mpa_implement) %>%
  gather(key="attribute",value="Value",Biomass,Catch,Profits) %>%
  spread(mpa_implement,Value) %>%
  #  group_by(mpa_scen,Fishery attribute) %>%
  mutate(MPA_effect = (after-before)/before *100)



ggplot(mpa_results)+
  geom_line(aes(x=mpa_scen,y=MPA_effect,color=attribute),size=2) +
  theme_bw() +
  #facet_wrap(~attribute)+
  geom_hline(yintercept=0,col="red") +
  xlab("MPA Scenario") +
  ylab("MPA Effect")+
  scale_color_viridis_d("",end=0.7,direction=-1)


# mpa placement -----------------------------------------------------------



# Before After MPA --------------------------------------------------------


#variable<-c("B_ratio","Catch","Effort","f","Profit Per Unit Effort","Profits")
land = raster(paste0(boxdir, "raster_output_4k/land_proj.tif"))



year_seq = seq(from = min(sim_sum$year),
               to = max(sim_sum$year),
               by = 10)

cell_no<-unique(sim_sum$cell_no)
effort_map<-area
t<-xyFromCell(effort_map,cell_no)
colnames(t)<-c("Long","Lat")
cell_lookup<-cbind(cell_lookup,t)

sim_sum<-merge(sim_sum,cell_lookup)

fish_equil_yr<-mpa_year-1
mpa_equil_yr<-sim_year

biomass_map<-area
effort_map[]<-NA
biomass_map[]<-NA


data<-sim_sum %>%
  filter(year == fish_equil_yr | mpa_equil_yr) 


ggplot(data=data,aes(x=x,y=y,fill = total_effort)) +
  geom_raster() +
  theme_bw() +
  facet_wrap (~mpa)



fish_data<-data %>%
  filter(mpa== FALSE)

total_effort<-sum(data$effort,na.rm = TRUE)
total_biomass<-sum(data$biomass,na.rm = TRUE)

effort_map[data$cell_no]<-data$effort
biomass_map[data$cell_no]<-data$biomass

par(mfrow=c(1,2))   
plot(effort_map,zlim=c(0,14),main=paste0("Total effort = ",total_effort, "year = ",year_seq[i]))
plot(biomass_map,zlim=c(0,50),main=paste0("Total biomass (mt)  = ",total_biomass, "year = ",year_seq[i]))







# create_gif --------------------------------------------------------------



for (i  in 1:length(year_seq)){
  data<-sim_sum %>%
    filter(year == year_seq[i])
  
  
  total_effort<-sum(data$effort,na.rm = TRUE)
  total_biomass<-sum(data$biomass,na.rm = TRUE)
  
  effort_map[data$cell_no]<-data$effort
  biomass_map[data$cell_no]<-data$biomass
  
  par(mfrow=c(1,2))   
  plot(effort_map,zlim=c(0,14),main=paste0("Total effort = ",total_effort, "year = ",year_seq[i]))
  plot(biomass_map,zlim=c(0,50),main=paste0("Total biomass (mt)  = ",total_biomass, "year = ",year_seq[i]))
  
}





library(viridis) # nice color palette
library(ggplot2) # plotting
library(ggmap) # ggplot functionality for maps
library(dplyr) # use for fixing up data
library(readr) # reading in data/csv
library(RColorBrewer) # for color palettes
library(purrr) # for mapping over a function
library(magick)


land = raster(paste0(boxdir, "raster_output_4k/land_proj.tif"))
land_cells<-Which(land==-1,cells=TRUE)
land_xy<-as.data.frame(xyFromCell(land,land_cells))
land_xy<-land_xy %>% mutate(land=-1)


mpa<-sim_sum %>%
  filter(mpa==TRUE)

mpa_xy<-as.data.frame(xyFromCell(biomass_map,unique(mpa$cell_no)))
mpa_xy<-mpa_xy %>%
  mutate(mpa = -100)

#land_cells<-cbind(land_cells,land_xy)
breaks1<-(c(0.0,10,20,30,40,50))
breaks2<-c(0:14)
az<- ggplot() +  geom_raster(data=land_xy, aes(x=x,y=y,fill=land),fill="grey") +theme_bw() 

variable = "effort"   

ndwi_map <- function(Yr){
  

az +  
 geom_raster(data=sim_sum[sim_sum$year==Yr,], 
                  aes(x=x, y=y, fill=effort),
                  show.legend=T, pch=21, size=4.8, color="gray30")+ 
    theme_bw() + ylab("Latitude") + xlab("Longitude") +
    theme(axis.text.x = element_text(angle = 60, vjust=0.15, size=8),
          legend.position=c(1,1),legend.justification=c(1,1),
          legend.direction="vertical",legend.text=element_text(size=8),
          legend.title=element_text(size=8, face="bold"),
          legend.box="horizontal", panel.background = element_blank(),
          legend.box.just = c("top"), 
          legend.background = element_rect(fill=alpha('white', 0.6), colour = "gray30")) +
    scale_fill_viridis(name="Effort", limits=range(breaks2), 
                       breaks=breaks2, option = "D", direction = -1) 
  #  geom_raster(data=mpa_xy,aes(x=x,y=y,fill=mpa),fill = "red",alpha=0.5) +
    facet_wrap(~year, ncol = 1)
  print(paste0("saving plot ", Yr))
  ggsave(filename = paste0(boxdir,runname,"/fig_output/effort",Yr,".png"),
         width = 8,height=8,dpi = 150)
  
}

seq(from = min(sim_sum$year), to=max(sim_sum$year), by=1) %>% 
  map_df(ndwi_map)

# Step 2: List those Plots, Read them in, and then make animation
list.files(path = paste0(boxdir,runname,"/fig_output/"), pattern = "effort*", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write(paste0(boxdir,runname,"/fig_output/effort.gif")) # write to current dir

mpa_results<-read.csv(paste0(boxdir,runname,"/mpa_results.csv"))






year_seq = seq(from = start_year,
               to = max(year),
               by = 20)




one_year<- sim_sum %>%
  filter(year==year_mpa-1 | year == max(year)) 

MPA_effect<-one_year %>%
  group_by(cell_no,year) %>%
  summarise(effort = sum(effort,na.rm = TRUE),
            distance = unique(distance)) %>%
  mutate(year=  as.factor(year))

sim_sum$year<-as.factor(sim_sum$year)

ggplot(data=MPA_effect,aes(x=distance,y=effort,color=year)) +
  geom_point()+
  theme_bw() +
  scale_color_discrete("year at equilibrium",labels=c("Before MPA","After MPA"))





#variable<-c("B_ratio","Catch","Effort","f","Profit Per Unit Effort","Profits")

land <- crop(land, extent)
extent <- c(-239710.8 , 8e+05, 3718070 , 4500000)

area <- crop(area, extent)
# map_plots <-
#   function(area,
#            land,
#            sim_sum,
#            cell_lookup,
#            variable,
#            num_patches) {
year <- unique(sim_sum$year)
start_year <- min(year)
effort_map <- area

year_seq = seq(from = start_year,
               to = max(year),
               by = 20)

year = start_year
#  for (i in 1:length(year_seq)) {
annual <- sim_sum %>%
  filter(year==year_mpa-1 | year == max(year))  %>%
  group_by(year, cell_no) %>%
  summarise(
    Effort = sum(effort, na.rm = TRUE),
    Profits = sum(profits),
    Biomass = sum(biomass),
    Catch = sum(biomass_caught),
    B0 = unique(b0),
    MPA = unique(mpa)
  ) %>%
  mutate(B_ratio = Biomass / B0) %>%
  ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / Effort,
         f = Catch / Biomass) %>%
  dplyr::select(-c(B0, Biomass)) %>%
  gather(metric, value, -c(year, cell_no))

before_Effort_df <- annual %>%
  filter(metric == "Effort" & year == year_mpa-1)

after_Effort_df <- annual %>%
  filter(metric == "Effort" & year == max(year))

MPA_df <- annual %>%
  filter(metric == "MPA")


area[!is.na(area)] <- NA
before_map <- area
before_map[before_Effort_df$cell_no] <- before_Effort_df$value
before_map[before_map == 0] <- NA

after_map <- area
after_map[after_Effort_df$cell_no] <- after_Effort_df$value
#  after_map[after_map == 0] <- NA


mpa_map <- area

mpa_map[MPA_df$cell_no] <-
  MPA_df$value#ifelse(MPA_df$value==0,NA,1)
mpa_map[mpa_map == 0] <- NA
cuts=c(0,0.0001,0.001,0.01,0.1,0.2,0.4,0.6,0.8,1,1.2) #set breaks
pal <- colorRampPalette(c("blue","orange","red"))    
par(mfrow=c(1,2)) 
plot(before_map, breaks=cuts,
     main = "Before MPA", col=pal(20))

plot(land, add = TRUE, col = "black")

plot(after_map, col=rainbow(n=1000),
     main = paste0("After MPA"))

plot(mpa_map,
     col = "red",
     add = TRUE,breaks=cuts,col=pal(70))
plot(land, add = TRUE, col = "black")



}
# show_landscape(effort_map, discrete = FALSE)#, xlab = "Lat",
#                ylab = "Long", discrete = FALSE, unique_scales = FALSE,
#                n_col = NULL, n_row = NULL)






juve_map[unique(juve_adult_distance$from)]<-10
juve_map[juve_map<10]<-NA

plot(hab_qual_map,add=TRUE,alpha = 0.5)

end<-sim_sum %>% filter(year == max(year))  
start<-sim_sum %>% filter(year == 30 + 1) 

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
plot(start_effort,main="start year effort",colNA = "black")
#plot(end_effort)
plot(end_effort,main="end year effort",colNA="black")
results<-stack(c(start_effort,end_effort))
## S3 method for class 'RasterStack'
show_landscape(results, xlab = "Lat",
               ylab = "Long", discrete = FALSE, unique_scales = FALSE,
               n_col = NULL, n_row = NULL) +
  theme_nlm( viridis_scale = "C")


plot(hab_qual_map)


vms_cells<-Which(!is.na(vms),cells=TRUE)

