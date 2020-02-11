library(rgeos)
library(rnaturalearth)
library(tidyverse)
library(raster)
library(rgdal)



boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#
runname<-"raster_output_4k"

EEZ_shape<-st_read("/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/raw_sp/azores_EEZ.shp")
EEZ=raster(paste0(boxdir,"raster_output_4k/azores_EEZ_proj_4k.tif"))
BLL=raster(paste0(boxdir,"raster_output_4k/BLL_prj.tif"))
goraz=raster(paste0(boxdir,"raster_output_4k/goraz_mdl_prj.tif"))
land=raster(paste0(boxdir,"raster_output_4k/land_proj.tif"))
juv=raster(paste0(boxdir,"raster_output_4k/juv_proj.tif"))

world <- rnaturalearth::ne_countries(scale="small", type = "countries", sovereignty="Portugal",returnclass = "sf") %>% 
  sf::st_transform(crs(area))

my_theme <- theme(axis.text=element_blank(),
                  axis.title=element_blank(),
                  legend.position = "right",
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  plot.title=element_text(size=9),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

goraz_df <- as.data.frame(goraz, xy=T) %>% 
  setNames(c("x", "y", "value")) %>% 
  mutate(value=ifelse(value==0, NA, value)) %>% 
  filter(!is.na(value))

h_map<-ggplot() +
  geom_tile(goraz_df, mapping=aes(x=x, y=y, fill=value),col="grey80",size=0.05) +
 # scale_fill_viridis(direction=-1)+
  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
  geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title="") +
  theme_bw() + my_theme+
  xlim(c(-239710.8,868289)) +
  ylim(c( 3718070,4678070))

ggsave(h_map, filename=paste0(boxdir,runname,"/Figures/","habiat_map.png"),dpi=600)
       plotdir, "figure_aquacast_methods_costs.png"), 
       width=6.5, height=3, units="in", dpi=600)

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
  xlim(c(-239710.8,868289)) +
  ylim(c( 3718070,4678070))

ggsave(h_map, filename=paste0(boxdir,runname,"/Figures/","habiat_map.png"),dpi=600)


sim_sum<-read.csv(paste0(boxdir,runname,"/sim_sum.csv")) %>%
  filter(year==21) %>%
  filter(f!=0)

long_lat<-xyFromCell(goraz,unique(sim_sum$cell_no))

sim_sum<-cbind(sim_sum,long_lat)

ggplot() +
  geom_tile(sim_sum, mapping=aes(x=x, y=y, fill=f),col="grey80",size=0.05) +
  scale_fill_viridis(direction=-1)+
  #  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="white") +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
   geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title=paste0("cost_slope= ",fleet$cost_slope)) +
  theme_bw() + my_theme+
  xlim(c(-199710.8,868289)) +
  ylim(c( 3718070,4678070))



dist<-ggplot() +
  geom_tile(sim_sum, mapping=aes(x=x, y=y, fill=distance),col="grey80",size=0.05) +
  scale_fill_viridis("Distance from shore (km)",direction=-1)+
  #  scale_fill_gradientn("Habitat quality",colours=rev(terrain.colors(50)))+
  geom_tile(land_df,mapping=aes(x=x,y=y,fill=value),fill="grey") +
  geom_sf(data=world, fill="grey80", lwd=0.05, col="black",size=1) +
  geom_sf(data=EEZ_shape, fill="while", lwd=0.25, col="black") +
  labs(x="", y="", title=paste0("",fleet$cost_slope)) +
  theme_bw() + my_theme+
  xlim(c(-199710.8,868289)) +
  ylim(c( 3718070,4678070))
ggsave(dist, filename=paste0(boxdir,runname,"/Figures/","distance_map.png"),dpi=600)
