library(raster)
library(rgdal)
library(tidyverse)
library(gdata)
boxdir<-"/Users/lennonthomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"

habitat_layer<-"tmp_sp/"

bsb<-raster(paste0(boxdir,habitat_layer,"bsb.tif"))
cell<-Which(!is.na(bsb),cells=TRUE)
bsb_lo<-aggregate(bsb,fact=9,fun=mean)
cell_lo<-Which(!is.na(bsb_lo),cells=TRUE)

land<-raster(paste0(boxdir,habitat_layer,"land.tif"))
fishing_effort<-raster(paste0(boxdir,habitat_layer,"BLL.tif"))
fishing_proj<-projectRaster(fishing_effort,bsb)


depth<-raster(paste0(boxdir,"raw_sp/az_depth.tif"))
depth[depth==0]<-0
depth[depth>0]<-NA
depth<-calc(depth,fun=function(x){x*-1})
juvenile<-depth
juvenile[juvenile<=50]<-1
juvenile[juvenile>50]<-0
juve_cell<-Which(juvenile==1,cells=TRUE)

all_cells<-union(cell,juve_cell)

all_data<-matrix(data = 0, nrow = length(all_cells), ncol =  9 )
all_data<-as.data.frame(all_data)
colnames(all_data)<-c("cell_no","long","lat","avg_depth_m","adult","pred_adult_abundance","juve","shore_distance_km","fishing_effort_hours")

long_lat<-xyFromCell(bsb,all_cells)

all_data[,1]<-all_cells

all_data[,c(2:3)]<-long_lat

all_data[,4]<-as.vector(depth[all_cells])

#all_data[,5]<-as.vector(bsb[all_cells])
all_data[,6]<-as.vector(bsb[all_cells])

all_data[,7]<-as.vector(juvenile[all_cells])

dist<-gridDistance(land,origin=-1)

dist<-dist/1000 #convert m to km

all_data[,8]<-dist[all_cells]

all_data[,9]<-fishing_proj[all_cells]

all_data<- all_data %>%
  dplyr::mutate(juve=ifelse(juve == 1 & all_data$shore_distance_km>24,0,juve))

all_data[,5]<-ifelse(!is.na(all_data$pred_adult_abundance),1,0)

all_data<-all_data[!all_data$shore_distance_km==0,]
# Fixing a few cells here where rasters didn't line up correctly
all_data$avg_depth_m[is.na(all_data$avg_depth_m)]<-0
all_data$juve[is.na(all_data$juve)]<-1
#all_data$pred_abundance[is.na(all_data$pred_abundance)]<-0

all_data<-all_data[!(all_data$adult==0 & all_data$juve==0),]

write.csv(all_data,paste0(boxdir,"az_patch_data_1k.csv"))








fit<-lm(all_data$fishing_effort ~ all_data$avg_depth + all_data$pred_abundance + factor(all_data$juve) + all_data$shore_distance_km)

all_data[all_data==0]<-NA

all_data_wide<-all_data[,c(1,4:5,7:8)] %>%
  mutate(log_fishing_effort=log(fishing_effort),
         log_shore_distance=log(shore_distance_km)) %>%
  gather()

ggplot(gather(all_data_wide), aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = 'free_x')

p_data<-all_data [,c(1,4:5,7:8)]%>%
  gather("key","value",-cell_no,-fishing_effort)

ggplot(data = p_data,aes(x=value,y=fishing_effort)) +
         geom_point()+
         facet_wrap(~key,scales = 'free_x',nrow=3,ncol=1) +
  theme_bw()

p_data<-all_data[4]











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

#testing them individually
EEZ=raster("raster_output_4k/azores_EEZ_proj_4k.tif")
BLL=raster("raster_output_4k/BLL_prj.tif")
goraz=raster("raster_output_4k/goraz_mdl_prj.tif")
land=raster("raster_output_4k/land_proj.tif")
juv=raster("raster_output_4k/juv_proj.tif")

rs_stack_study_region_combined=stack(EEZ,BLL,goraz,land,juv)





eez<-raster(paste0(boxdir,habitat_layer,"azoresEEZproj.tif"))
land<-raster(paste0(boxdir,habitat_layer,"land_proj.tif"))

juve<-raster(paste0(boxdir,habitat_layer,"juve_proj.tif"))
effort<-raster(paste0(boxdir,habitat_layer,"bll_proj.tif"))
