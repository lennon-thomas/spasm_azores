# JABBA model, updated

# Load packages
library(ggplot2)
library(coda)
library(rjags)
library(R2jags)
library(fitdistrplus)
library(reshape)
library(JABBA)
library(tidyverse)

# Load source files
source(file.path(here::here(), "scripts", "start_project.R"))
project_results_path<-file.path(project_path,"site-based-work/azores/bio-economic-model/bsb-assessment/jabba_results")


# Read in data 
#catch in tonnes from ICES. 2021. Working Group on the Biology and Assessment of Deep-sea Fisheries Resources (WGDEEP).
#ICES Scientific Reports. 3:47. 944 pp. http://doi.org/10.17895/ices.pub.8108 and DRAM data

#fi_index=fishery-independent index from ICES. 2021. Working Group on the Biology and Assessment of Deep-sea Fisheries Resources (WGDEEP).
#ICES Scientific Reports. 3:47. 944 pp. http://doi.org/10.17895/ices.pub.8108 and 

#fish days= dram data using average days per vessel size

# cpue_year= calculated using DRAM effort and catch data
#stand_cpue= standardized cpue from bottom longline from ICES. 2017. Report of the Working Group on the Biology and Assessment of Deep-sea Fisheries Resources (WGDEEP), 24 April–1 May 2017, Copenhagen, Denmark. ICES CM 2017/ACOM:14. 702 pp.
dat <- read.csv(file.path(project_table_path, "jabba_input.csv")) %>%
  mutate(log_cpue = log(cpue_year),
         log_catch = log(catch),
         log_effort = log(fish_days),
         log_fi_index = log(fi_index),
         log_stand_cpue = log(stand_cpue))

# check data distribution
#hist(log(dat$cpue_year))
#hist(log(dat$catch))
#hist(log(dat$fish_days))


# hyperstability_cpue <- dat %>% 
#   dplyr::select(season, cpue_hauls_reported) %>% 
#   mutate(adj_cpue = cpue_hauls_reported*(1-0.02)^(as.numeric(season) - as.numeric(min(dat$season))), 
#          log_adj_cpue = log(adj_cpue)) %>% 
#   left_join(cpue_weight %>% 
#               dplyr::select(season, cpue_kg_hauls) %>% 
#               mutate(adj_kg_cpue = cpue_kg_hauls*(1-0.02)^(as.numeric(season) - as.numeric(min(cpue_weight$season))), 
#                      log_adj_kg_cpue = log(adj_kg_cpue)))

catch_df <- dat[,c("Year", "log_catch")]
  
cpue_df <- dat[,c("Year", "new_stand_cpue")]
#241 254
# Model options
psi <- seq(0.001, 7, 0.1)
K.dist <- "range"
K.prior <- c((max(dat$catch))*2,(max(dat$catch)) * 5) #chose low value bc habitat is limited
 psi.dist <- "lognormal"
r.dist <- "lognormal"
r.prior <- c(0.36, 0.82) #range from FishBase
fixed.obsE <- 0.2
proc.dev.all <- FALSE
BmsyK <- 0.7
Plim=0
#m=4
model.type <- "Pella"

catch.metric <- catch_df$log_catch
#paste0(ifelse(grepl("log", colnames(catch_df)[2]), "log ", ""), 
 #                      ifelse(grepl("weight|kg", colnames(catch_df)[2]), "kg", "count"))
cpue.metric <-cpue_df$new_stand_cpue
  #paste0(ifelse(grepl("log", colnames(cpue_df)[2]), "log ", ""), 
 #                     ifelse(grepl("weight|kg", colnames(cpue_df)[2]), "kg/", "count/"), 
  #                    ifelse(grepl("haul", colnames(cpue_df)[2]), "haul", "trip"), 
   #                   ifelse(grepl("adj", colnames(cpue_df)[2]), " (hyperstability adj)", ""))
notes <- "new stand'cpue and log catch 5xwider k range. shape =0.6"




for(i in 1:length(psi)) {
  
  ## Create folder output
  get_folder <- list.dirs(file.path(project_results_path), 
                          recursive = FALSE, full.names = FALSE)
  
  if(length(get_folder) == 0) { 
    new_num = "01"
    new_folder <- paste0("01_", Sys.Date())
  } else { 
    new_num <- str_pad(max(as.numeric(t(data.frame(get_folder %>% 
                                                     str_split(., pattern = "_")))[,1]), na.rm = T) + 1, 2, pad = 0)
    new_folder <- paste0(new_num, "_", Sys.Date())
  new}
  
  dir.create(file.path(project_results_path, new_folder))

  # Compile JABBA model
  jabba_input <- build_jabba(catch = catch_df, 
                             cpue = cpue_df, 
                             K.dist = K.dist, 
                             K.prior = K.prior, 
                             psi.dist = psi.dist,
                             psi.prior = c(psi[i], 0.25),
                             r.dist = r.dist,
                             r.prior = r.prior,
                             fixed.obsE = fixed.obsE,
                             proc.dev.all = proc.dev.all,
                             BmsyK = BmsyK,
                             model.type = model.type)
  
  jabba_output <- fit_jabba(jabba_input)
  
  ## Save log results
  run_log_temp <- data.frame("model_run" = as.numeric(new_num), 
                             "date" = as.character(Sys.Date()), 
                             "catch.metric" = catch.metric, 
                             "cpue.metric" = cpue.metric,
                             "K.dist" = K.dist, 
                             "K.prior" = paste(c(K.prior, 0.25), collapse = ", "),
                             "psi.dist" = psi.dist,
                             "psi.prior" = paste(c(psi[i], 0.25), collapse = ", "),
                             "r.dist" = r.dist,
                             "r.prior" = paste(r.prior, collapse = ", "),
                             "fixed.obsE" = fixed.obsE,
                             "proc.dev.all" = proc.dev.all,
                             "BmsyK" = BmsyK,
                             "model.type" = model.type, 
                             "RMSE" = jabba_output$stats$Value[which(jabba_output$stats$Stastistic == "RMSE")], 
                             "DIC" = jabba_output$stats$Value[which(jabba_output$stats$Stastistic == "DIC")], 
                             "notes" = notes
  )
  
  if(length(list.files(file.path(project_results_path
                                ), 
                       "run_log")) > 0) { 
    run_log <- read.csv(file.path(project_results_path,"run_log.csv"))
    run_log <- bind_rows(run_log, run_log_temp)
    write.csv(run_log, file.path(project_results_path, "run_log.csv"), 
              row.names = FALSE)
  } else { 
    write.csv(run_log_temp, file.path(project_results_path, "run_log.csv"), 
              row.names = FALSE)
  } 
  
  # Save plots
  ## KOBE plot
  png(file.path(project_results_path, new_folder, "kobe_plot.png"))
  jbplot_kobe(jabba_output)
  dev.off()
  
  ## Summary plot
  png(file.path(project_results_path, new_folder, "summary_plot.png"))
  jbplot_summary(jabba_output)
  dev.off()
  
  ## Bayesian p values - should be between 0.2-0.8
  png(file.path(project_results_path, new_folder, "bayesian_ppc.png"))
  jbplot_PPC(jabba_output)
  dev.off() 
  
 
  
 

  
  
  # Save output
  write.csv(jabba_output$estimates, file.path(project_results_path, new_folder, "jabba_estimates.csv"))
  
  # Save output 
  saveRDS(jabba_output, file = file.path(project_results_path, new_folder, "jabba_results.rds"))
} 

# Show best estimate for PSI
log <- read.csv(file.path(project_results_path, "run_log.csv")) %>% 
  separate(psi.prior, into = c("psi", "estimated_psi"), sep = ", ") %>% 
  mutate(psi = as.numeric(psi),
         estimated_psi = as.numeric(estimated_psi))

ggplot2::ggplot(log, mapping = aes(x = psi, y = estimated_psi)) + 
  ggplot2::geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  xlim(c(0,2)) + 
  ylim(c(0,2))


