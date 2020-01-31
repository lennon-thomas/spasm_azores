#11/11

#Calculate distance from shore to calculate fishing costs

library(raster)
library(rgdal)
library(tidyverse)
library(gdata)
boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#
runname<-"low_res_run"

dir.create(paste0(boxdir,runname))

habitat<-raster(paste0(boxdir,"/", runname, "/all_habitat.tif"))
habitat[habitat==2]<-NA
all_cell_no<-Which(!is.na(habitat),cells=TRUE)

habitat_no<-habitat

for (i in 1: length(all_cell_no)){
  habitat_no[all_cell_no[i]]<-all_cell_no[i]
}
habitat_no[is.na(habitat_no)]<-0
land<-raster(paste0(boxdir,"habitat/land_lo.tif"))
land[is.na(land)]<-0
land[land==-1]<-NA

habitat_no<-mask(habitat_no,land)

dist_shore<-gridDistance(habitat_no,origin=NA)

distance<-dist_shore[all_cell_no]
distance<-distance/1000
dist_matrix <- matrix(data = 0, nrow = length(all_cell_no), ncol = 2 )
colnames(dist_matrix)<-c("cell no", "distance")
dist_matrix[,1]<-all_cell_no
dist_matrix[,2]<-distance

write.csv(dist_matrix,paste0(boxdir,runname,"/distance_to_shore.csv"))

