#adding a folder to the working directory to store adjusted rasters
# output_dir=paste(wd,"/raster_output",sep="");dir.create(output_dir)
output_dir=paste(boxdir,"/raster_output_42km2",sep="");dir.create(output_dir)


#Making a list of the rasters you want to adjust
# rs_list=list.files(path = "output/",pattern = "*.tif$")
rs_list=list.files(path = paste0(boxdir,"tmp_sp/"),pattern = "*.tif$")

#Loading in the raster representing your desired spatial extent
# study_area=raster(x = "output/azores_EEZ_proj.tif")
study_area= bsb_lo #raster(x = "output_4k/azores_EEZ_proj_4k.tif")
proj4string(study_area)<-CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs") # putting the raster into a projected coordiante system of Azores_Central_1948_UTM_Zone_26N

# creating a list file extentions where you'll be storing the adjusted rasters
outfiles <- paste(output_dir, rs_list,sep="/")

# change extensions to tif
extension(outfiles) <- 'tif'

#raster adjustment lopp
for(i in 1:length(rs_list)) {
  r <-raster(paste(boxdir,"/tmp_sp/",rs_list[i],sep="")) # load in the rastter from list rs_list
  r<-projectRaster(r,study_area,CRS("+proj=utm +zone=26 +ellps=intl +towgs84=-104,167,-38,0,0,0,0 +units=m +no_defs"))
  #proj4string(r)<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # putting the raster into a projected coordiante system of Azores_Central_1948_UTM_Zone_26N
  rc <- crop(r, study_area,snap="in") #crop it and snap it to the extent of the study region
  resample=resample(rc,study_area) # resample to the same resolution as the study region
  resample <- writeRaster(resample, outfiles[i],format="GTiff",overwrite=TRUE) #write out a new raster file
  print(paste("resample ",i," done!",sep="")) #let me know its done
}


#make a list of the new rasters
rs_list=list.files(path ="raster_output_4k/",pattern = "*.tif$")

#stack them so I can see them
rs_stack_study_region=raster::stack(paste(wd,"/raster_output_4k/",rs_list,sep=""))
names(rs_stack_study_region)
dim(rs_stack_study_region)


library(raster)
library(rgdal)
library(tidyverse)
library(gdata)
library(doParallel)
library(foreach)
library(raster)
library(snow)
library(spgwr)
library(maptools)
library(GWmodel)
library(e1071)
library(sp)
boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#
runname<-"raster_output_4k"

dir.create(paste0(boxdir,runname))

#testing them individually
goraz[goraz< 0.01]<-NA
#rs_stack_study_region_combined=stack(EEZ,BLL,goraz,land,juv)

EEZ=raster(paste0(boxdir,"raster_output_4k/azores_EEZ_proj_4k.tif"))
BLL=raster(paste0(boxdir,"raster_output_4k/BLL_prj.tif"))
goraz=raster(paste0(boxdir,"raster_output_4k/goraz_mdl_prj.tif"))
land=raster(paste0(boxdir,"raster_output_4k/land_proj.tif"))
juv=raster(paste0(boxdir,"raster_output_4k/juv_proj.tif"))

area(BLL)

all<-stack(EEZ,BLL,goraz,land,juv)
show_landscape(all, xlab = "Lat",
               ylab = "Long", discrete = FALSE, unique_scales = FALSE,
               n_col = NULL, n_row = NULL) +
  theme_nlm( viridis_scale = "C")


adult<-goraz
adult[!is.na(adult)]<-1

all_habitat<-stack(adult,juv)

all_habitat<-stackApply(all_habitat,c(1,1), fun=sum,na.rm=TRUE)

all_habitat[all_habitat==0]<-NA
writeRaster(all_habitat,paste0(boxdir,runname,"all_habitat.tif"))
all_cells<-Which(!is.na(all_habitat),cells = TRUE)

num_patches<-length(all_cells)

both_habitat_cell<-Which(all_habitat==3,cells=TRUE)

juv_cell<-Which(all_habitat==2,cells=TRUE)

juv_cell<-rbind(c(juv_cell,both_habitat_cell))

adult_cell<-Which(all_habitat==1,cells = TRUE)

adult_cell<-rbind(c(adult_cell,both_habitat_cell))

hab_qual <- goraz



total<-cellStats(hab_qual,"sum")

hab_qual <- hab_qual/total



hab_qual_vec<-hab_qual[all_cells]
#mean_qual<-mean(hab_qual_vec)
#is.na(hab_qual_vec)<-mean_qual
#total<-cellStats(hab_qual,"sum")

#hab_qual <- hab_qual/total


dist<-all_habitat

dist[is.na(dist)]<--1
land[is.na(land)]<-0
land[land==-1]<-NA

dist<-mask(dist,land)

dist_shore<-gridDistance(dist,origin=NA)
distance<-dist_shore[all_cells]
distance<-distance/1000
dist_matrix <- matrix(data = 0, nrow = length(all_cells), ncol = 2 )
colnames(dist_matrix)<-c("cell_no", "distance")
dist_matrix[,1]<-all_cells
dist_matrix[,2]<-distance

dist_layer<-all_habitat
dist_layer[all_cells]<-distance

write.csv(dist_matrix,paste0(boxdir,runname,"/distance_to_shore.csv"))


cell_lookup<-data.frame(matrix(ncol=7,nrow=num_patches))

colnames(cell_lookup)<-c("patch","cell_no","juve_ad_hab","hab_qual","distance","adult","juve")
cell_lookup[,1]<-c(1:num_patches)
cell_lookup[,2]<-all_cells
#0 =adult
cell_lookup[,3]<-ifelse(cell_lookup$cell_no %in% juv_cell,1,0)
cell_lookup[,4]<- hab_qual_vec#hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))# Adult habitat quality
cell_lookup[,5]<-distance
cell_lookup[,6]<-ifelse(cell_lookup$cell_no %in% adult_cell, TRUE, FALSE)
cell_lookup[,7]<-ifelse(cell_lookup$cell_no %in% juv_cell, TRUE, FALSE)

cell_lookup$distance[cell_lookup$distance==0]<-20

write.csv(cell_lookup,paste0(boxdir,runname,"/cell_lookup.csv"))

dmatrix <- matrix(data = 0, nrow = length(all_cells), ncol = length(all_cells) )

no<-as.vector(c(1:86655))

cell_ras<-all_habitat
cell_ras<-setValues(cell_ras,no)
cell_ras<-mask(cell_ras,land,maskvalue = -1,updatevalue=-1)

all_habitat[is.na(all_habitat)]<-0
land[is.na(land)]<--1
all_habitat<-mask(all_habitat,land,maskvalue = -1,updatevalue=-1)
all_habitat[is.na(all_habitat)]<--1

for(i in 1:length(all_cells)){
  c<-cell_ras[all_cells[i]]
  ## Add omit=land to not go through land cells
  d<-gridDistance(cell_ras,origin=c,omit=-1)
  d<-d[all_cells]
  distance_calc<-as.vector(d)
  dmatrix[i,]<-distance_calc
  print(i)
}
#write.csv(dmatrix,paste0(boxdir,runname,"/adult_distance_matrix.csv"))

d_vector<-unmatrix(dmatrix,byrow=TRUE)

dframe<-expand.grid(from = all_cells, to = all_cells) %>%
  as.data.frame() %>%
  mutate(dist=d_vector/1000)

juve_adult_df<-dframe[which(dframe$from %in% juv_cell), ]
juve_adult_df<-juve_adult_df[which(juve_adult_df$to %in% adult_cell), ]

write.csv(juve_adult_df,paste0(boxdir,"/",runname,"/juve_adult_distance.csv"))

dframe[,3]<-ifelse(dframe$from %in% juv_cell,NA,dframe$dist)
dframe[,3]<-ifelse(dframe$to %in% juv_cell,NA,dframe$dist)
#dframe<-dframe$dist[dframe$to %in% juve_cell_no]<-NA
#dframe[whi
write.csv(dframe,paste0(boxdir,"/",runname,"/adult_adult_distance.csv"))



