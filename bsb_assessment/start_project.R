#A script to be sourced at the beginning of each working scripts to load common libraries, working paths, and helpful general lookup tables/datasets 

# Working paths 
sys_path <- ifelse(Sys.info()["sysname"]=="Windows", "G:/Shared drives/", "~/Google Drive/Shared drives/")
# Path to our emLab's data folder
emlab_data_path <- paste0(sys_path,"emlab/data")
# Path to this project's folder
project_path <- paste0(sys_path,"emlab-bpc/blue-prosperity-coalition")
#Path to project's data folder
project_data_path <- file.path(project_path,"data/Azores/bsb_model/bsb-data")
# Path to project's output table folder
project_table_path <- file.path(project_path, "site-based-work/azores/bio-economic-model/bsb-assessment/tables")
# Path to project's output figure folder
project_figure_path <- file.path(project_path, "site-based-work/azores/bio-economic-model/bsb-assessment/figures")

