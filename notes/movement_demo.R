library(tidyverse)
source('R/move_fish_az.R')
boxdir<-"/Users/lennonrosethomas/Box Sync/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/iladner/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
#boxdir <- "C:/Users/ianla/Box/SFG Centralized Resources/Projects/BPC/Azores/data/bsb_model/"
runname <- "test"

juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
adult_distance<-read.csv(paste0(boxdir,runname,"/adult_adult_distance.csv"))
juve_adult_distance<-read.csv(paste0(boxdir,runname,"/juve_adult_distance.csv"))
adult_juve_distance<-read.csv(paste0(boxdir,runname,"/adult_juve_distance.csv"))
juve_distance<-read.csv(paste0(boxdir,runname,"/juve_distance.csv"))
shore_dist<-read.csv(paste0(boxdir,runname,"/distance_to_shore.csv"))

hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))
num_patches<-length(unique(adult_distance$from))
# Create dataframe that has cell_no, patch_no, whether juve or adult, and habitat quality


cell_lookup<-data.frame(matrix(ncol=4,nrow=num_patches))
colnames(cell_lookup)<-c("patch","cell_no","juve_ad_hab","hab_qual")
cell_lookup[,1]<-c(1:20)
cell_lookup[,2]<-unique(adult_distance$from)
cell_lookup[,3]<-c(rep(0,10),rep(1,10))
cell_lookup[,4]<- hab_qual<-c(.5,.05,.05,.05,0.05,0.05,0.05,0.05,0.05,.1,rep(0,10))# Adult habitat quality



### read in example pop taken from year two of sim_fishery_az
pop <- read_csv("data/pop_example.csv")
ssb0 <- read_csv("data/example_ssb0.csv")$value

#assign movement vairable for the time being. eventually witll change the value of the density_movement_modifier
adult_movement <- 500
density_movement_modifier <- 0.5


##################### density independent movement
adult_move_grid_independent <- adult_distance %>%
  left_join(cell_lookup, by = c("to" = "cell_no")) %>%
  dplyr::mutate(movement = ifelse(is.na(dist), NA, ifelse(
    is.finite(dnorm(dist, 0, adult_movement)),
    dnorm(dist, 0, adult_movement),
    1
  ))) %>%
  group_by(from) %>%
  dplyr::mutate(prob_move = movement / sum(movement, na.rm = TRUE))

adult_move_matrix_independent <- adult_move_grid_independent %>%
  ungroup() %>%
  dplyr::select(from, to, prob_move) %>%
  spread(to, prob_move) %>%
  dplyr::select(-from) %>%
  #  uncount(10) %>%
  as.matrix()

##################### density dependent movement
slope <-
  adult_movement - (adult_movement * density_movement_modifier)
# closer to 1 density dependence has less of an affect
# 
how_crowded <- pop %>%
  group_by(patch) %>%
  summarise(ssb = sum(ssb, na.rm = TRUE)) %>%
  dplyr::arrange(patch) %>%
  mutate(depletion = ssb / ssb0) %>%
  mutate(move_rate = pmin(
    adult_movement,
    slope * depletion + (adult_movement * density_movement_modifier)
  )) %>%
  dplyr::select(patch, move_rate)

how_crowded <- left_join(how_crowded, cell_lookup) %>%
  dplyr::select(cell_no, move_rate)

adult_distance[is.na(adult_distance)] <- 0

adult_move_grid_dependent <- adult_distance %>%
  left_join(how_crowded, by = c("from" = "cell_no")) %>%
  dplyr::mutate(movement = ifelse(is.na(dist), NA, ifelse(
    is.finite(dnorm(dist, 0, move_rate)),
    dnorm(dist, 0, move_rate),
    1
  )))  %>%
  group_by(from) %>%
  dplyr::mutate(prob_move = movement / sum(movement))

juve_cell_no  <-
  cell_lookup[cell_lookup$juve_ad_hab == 1, "cell_no"]

adult_move_grid_dependent[adult_move_grid_dependent$from %in% juve_cell_no |
                  adult_move_grid_dependent$to %in% juve_cell_no, "prob_move"] <- 0

#number of mature age classes
adult_move_matrix_dependent <- adult_move_grid_dependent %>%
  ungroup() %>%
  dplyr::select(from, to, prob_move) %>%
  spread(to, prob_move) %>%
  dplyr::select(-from) %>%
    as.matrix()


#apply move fish function

pop_with_independent_movement <- pop

#something is not working in application of move_fish_az
pop_with_independent_movement[pop_with_independent_movement$age > 4,]<-
  move_fish_az(
    here_pop = pop_with_independent_movement %>% filter(age > 4),
    fish = NA,
    num_patches = 20,
    move_matrix = adult_move_matrix_independent
  )

#To Do: 

#-implement loop/purr to do the same calculations as above with different values of density_movement_modifier
#-plot
