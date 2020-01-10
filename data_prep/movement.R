# 10/28/19

#Calculate Distance between cells in the Azores in km. Calculates between juvenile and adult habitat cells
#adult to juvenile and adult to adult
library(raster)
library(rgdal)
library(tidyverse)
library(gdata)
boxdir<-"/Users/lennonthomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#
runname<-"low_res_run"

dir.create(paste0(boxdir,runname))


adult<-raster(paste0(boxdir,"habitat/low_res_habitat.tif"))

land<-raster(paste0(boxdir,"habitat/land_lo.tif"))

land[is.na(land)]<--2
land[land==-1]<-NA

adult<-mask(adult,land)

adult[is.na(adult)]<--1

adult[adult==0]<-NA
adult[adult==2]<-NA
adult[adult==-1]<-NA
effort_lo<-raster(paste0(boxdir,"habitat/effort_lo.tif"))

effort_lo<-mask(effort_lo,adult)
effort_lo<-mask(effort_lo,land)
mean_effort<-cellStats(effort_lo,"mean")
# Create new raster of adult habitat where cell values represent cell numbers
all_cell_no<-Which(!is.na(adult),cells=TRUE)
juve_cell_no<-Which(adult==2,cells=TRUE)
ad_cell_no<-Which(adult==1,cells=TRUE)
adult_no<-adult
effort_cell_no<-Which(!is.na(effort_lo),cells=TRUE)
## make all cells effort cells overlap
missing<-Which(is.na(effort_lo),cells=TRUE)
missing_e<-dplyr::intersect(missing,ad_cell_no)
effort_lo[misshing_e]<-mean_effort
effort_cell_no<-Which(!is.na(effort_lo),cells=TRUE)
writeRaster(effort_lo,paste0(boxdir,"/",runname,"/effort_lo.tif"),overwrite=TRUE)
#adult_no[adult_no==2]<-NA
for (i in 1: length(all_cell_no)){
  adult_no[all_cell_no[i]]<-all_cell_no[i]
}

# Calculate distance between all adult cell combiations

dmatrix <- matrix(data = 0, nrow = length(all_cell_no), ncol = length(all_cell_no) )

for(i in 1:length(all_cell_no)){
  c<-adult_no[all_cell_no[i]]
  ## Add omit=land to not go through land cells
  d<-gridDistance(adult_no,origin=c,omit=-1)
  d<-d[ad_cell_no]
  distance_calc<-as.vector(d)
  dmatrix[i,]<-distance_calc
  print(i)
}
#write.csv(dmatrix,paste0(boxdir,runname,"/adult_distance_matrix.csv"))

d_vector<-unmatrix(dmatrix,byrow=TRUE)

dframe<-expand.grid(from = all_cell_no, to = all_cell_no) %>%
  as.data.frame() %>%
  mutate(dist=d_vector/1000)

juve_adult_df<-dframe[which(dframe$from %in% juve_cell_no), ]
juve_adult_df<-juve_adult_df[which(juve_adult_df$to %in% ad_cell_no), ]

write.csv(juve_adult_df,paste0(boxdir,"/",runname,"/juve_adult_distance.csv"))

adult_df<-dframe[which(dframe$from %in% ad_cell_no), ]
adult_df<-adult_df[which(adult_df$to %in% ad_cell_no), ]

write.csv(adult_df,paste0(boxdir,"/",runname,"/adult_adult_distance.csv"))

adult_juve_df<-dframe[which(dframe$from %in% ad_cell_no), ]
adult_juve_df<-adult_juve_df[which(adult_juve_df$to %in% juve_cell_no), ]

write.csv(adult_juve_df,paste0(boxdir,"/",runname,"/adult_juve_distance.csv"))
