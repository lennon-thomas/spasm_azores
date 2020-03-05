library(rgeos)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)
library(wesanderson)



boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#
runname<-"raster_output_4k"
condor<-st_read(paste0(boxdir,"condor_seamount/condor_seamount.shp"))
EEZ_shape<-st_read("/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/raw_sp/azores_EEZ.shp")
EEZ=raster(paste0(boxdir,"raster_output_4k/azores_EEZ_proj_4k.tif"))
BLL=raster(paste0(boxdir,"raster_output_4k/BLL_prj.tif"))
goraz=raster(paste0(boxdir,"raster_output_4k/goraz_mdl_prj.tif"))
land=st_read(paste0(boxdir,"tmp_sp/land_shape/azores_islands.shp"))#raster(paste0(boxdir,"raster_output_4k/land_proj.tif"))
juv=raster(paste0(boxdir,"raster_output_4k/juv_proj.tif"))

condor_raster<-rasterize(condor,goraz,filename=paste0(boxdir,"raster_output_4k/condor_raster.tif"))
condor_raster_cells<-Which(condor_raster==1,cells=TRUE)
world <- rnaturalearth::ne_countries(scale="small", type = "countries", sovereignty="Portugal",returnclass = "sf") %>% 
  sf::st_transform(crs(area))

land<-land %>%
  sf::st_transform(crs(goraz))

condor<-st_transform(condor,crs(area))
my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.position = "right",
                  legend.text=element_text(size=16),
                  legend.title=element_text(size=20),
                  plot.title=element_text(size=9),
                 # panel.grid.major = element_blank(), 
                  panel.grid.minor = element_line(color = "white"),
               panel.background = element_rect(color = "white",fill="white"),
                  axis.line = element_line(colour = "white"), axis.ticks  = element_line(colour = "white"),
                 panel.grid.major = element_line(size = 0.7, colour = 'white'))



goraz<-mask(goraz,EEZ,updatevalue=NA,inverse=FALSE)
#land_df<-as.data.frame(land, xy=T) %>% 
 # setNames(c("x", "y", "value")) %>% 
  #mutate(value=ifelse(value==0, NA, value)) %>% 
  #filter(!is.na(value))

goraz_df <- as.data.frame(goraz, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))


zoom_out<-coord_sf(xlim = c(-239710.8, 1068289), ylim = c(3718070, 4778070 ), expand = TRUE)
# ylim(c( 3718070,4678070))

h_map<-ggplot() +
  geom_tile(goraz_df, mapping=aes(x=x, y=y, fill=value),col="grey80",size=0.05) +
 # scale_fill_viridis(direction=-1)+
  scale_fill_gradientn("Predicted relative\n abundance",colours=rev(terrain.colors(500)))+
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white",size=1) +
  geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title="") +
 my_theme+ # xlim(c(-239710.8,868289)) +
  geom_sf(data=land,fill="black") + 
  coord_sf(xlim = c(-239710.8, 1068289), ylim = c(3718070, 4778070 ), expand = TRUE)
 # ylim(c( 3718070,4678070))
  coord_sf(xlim = c(-90000,708289), ylim = c(4058070,4428070), expand = TRUE)

ggsave(h_map, filename=paste0(boxdir,runname,"/Figures/","habiat_map.png"),dpi=600)

goraz[goraz< 0.01]<-NA
total<-cellStats(goraz,stat="sum")   
goraz[]<-goraz[]/total

goraz_df <- as.data.frame(goraz, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))
pal <- wes_palette("Zissou1", 21, type = "continuous")
h_map2<-ggplot() +

  geom_tile(goraz_df, mapping=aes(x=x, y=y, fill=value),col="black",size=0.05) +
  # scale_fill_viridis(direction=-1)+
  scale_fill_gradientn("Relative adult \n habitat quality",colours=pal)+
  geom_sf(data=world, fill="grey80", lwd=0.05, col="white",size=1) +
 # geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title="") +
  my_theme+ # xlim(c(-239710.8,868289)) +
  geom_sf(data=land,fill="black") + 
coord_sf(xlim = c(-100000,808289), ylim = c(4008070,4428070), expand = TRUE)

ggsave(h_map2, filename=paste0(boxdir,runname,"/Figures/","habiat_map.png"),dpi=600)


cell_lookup<-read.csv(paste0(boxdir,runname,"/cell_lookup.csv"))




juve_df <- as.data.frame(juv, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))

j_map<-ggplot() +
  geom_tile(juve_df, mapping=aes(x=x, y=y, fill=value),col="grey80",size=0.05,fill="red") +
  # scale_fill_viridis(direction=-1)+
#  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
 # geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title="") +
  theme_bw() + my_theme+
  geom_sf(data=land,fill="black") + 
  coord_sf(xlim = c(-100000,808289), ylim = c(4008070,4428070), expand = TRUE)

ggsave(h_map, filename=paste0(boxdir,runname,"/Figures/","habiat_map.png"),dpi=600)


sim_sum-read.csv(paste0(boxdir,runname,"/sim_sum.csv")) %>%
  #filter(year==mpayear-2) %>%
  filter(f!=0)
long_lat<-xyFromCell(goraz,unique(sim_sum$cell_no))

sim_sum<-cbind(sim_sum,long_lat)

before_sim<-sim_sum %>%
  filter(year==mpayear-2) %>%
  mutate(time="before")

after_sim<-sim_sum %>%
  filter(year==max(year)) %>%
  mutate(time="after")

before_after<-rbind(before_sim,after_sim)

condor_df <- as.data.frame(condor_raster, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))

mpa_df<-sim_sum %>%
  filter(year==40) %>%
  filter(mpa==TRUE)

after<-ggplot() +
  geom_tile(sim_sum, mapping=aes(x=x, y=y, fill=f),col="grey80",size=0.05) +
  scale_fill_viridis(direction=-1)+
  #  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="wheat") +
 # geom_tile(mpa_df,mapping=aes(x=x,y=y,fill=mpa),fill="red",show.legend = TRUE) +
  #scale_fill_continuous("MPA")+
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
   geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  geom_sf(data=condor,col="red",lwd=0.5,fill=NA) +
  labs(x="", y="", title=paste0("After MPA")) +
  theme_bw() + my_theme+
  xlim(c(-199710.8,868289)) +
  ylim(c( 3718070,4678070))

par(mfrow=c(1,2)) 
grid.arrange(before,after,nrow=1)


sim_sum$distance[sim_sum$distance==20]<-1
dist<-ggplot() +


  #  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="black") +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
  geom_sf(data=EEZ_shape, fill="white", lwd=0.55, col="black") +
  geom_tile(sim_sum, mapping=aes(x=x, y=y, fill=distance),col="grey80",size=0.05) +
  scale_fill_viridis_c("km",direction=-1)+
 # scale_fill_gradientn("km",colors = pal)+
  labs(x="", y="", title=paste0("Distance from shore")) +
  theme_bw() + my_theme+
  coord_sf(xlim = c(14000,788289), ylim = c(4058070,4478070), expand = TRUE)

ggsave(dist, filename=paste0(boxdir,runname,"/Figures/","distance_map.png"),dpi=600)
