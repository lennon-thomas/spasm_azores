assign_mpa_cells <- function(df, metric, directionality, prop_mpa){

#returns a list of cell numbers based upon user input  
  
#VARIABLE DEFINITION    
#--------------------------------------------------------------------------  
  #df is the lookup table containing the patch cell numbers, habitat quality, and distance to shore
  #metric is either "distance" or "habitat"
  #directionality indicates whether you want to close the the patches at the "highest" or "lowest" value of that metric
  #i.e. "distance" and "lowest" would close the patches closest to shore. "habitat" and "highest" would close the patches with the highest habitat quality
  #percent_mpa is the proportion of cells that you would like to assign to MPAs from 0 (no MPA) to 1 (all MPA)
#--------------------------------------------------------------------------  
  
  #checks to make sure supplied variables fall within acceptable spaces
  
  if(!metric %in% c("distance","habitat")){
    print("Variable `metric` must either be 'distance' or 'habitat'.")
    return(NULL)
  }
  if(!directionality %in% c("highest","lowest")){
    print("Variable `directionality` must be either 'highest' or 'lowest'.")
    return(NULL)
  }
  if(prop_mpa < 0 | prop_mpa > 1){
    print("Variable `prop_mpa` must be between 0 and 1, inclusive.")
    return(NULL)
  }
#-------------------------------------------------------------------------- 
  
  #handle case where prop_MPA = 0
  #return empty character vector
  if(prop_mpa == 0){
  return(as.character())
  }
#--------------------------------------------------------------------------   
  
  #match `metric` to appropriate column name
  
  sort_column <- ifelse(metric == "distance", "distance", "hab_qual") #assumes column names of cell_lookup table
  
  num_mpa_patches <- round(nrow(df) * prop_mpa,0)
  
  if(directionality == "highest"){
    mpa_cell_list <- df %>% 
    arrange(desc(!!ensym(sort_column))) %>% 
      slice(1:num_mpa_patches) %>% 
      dplyr::pull(cell_no)
  }else if(directionality == "lowest"){
    mpa_cell_list <- df %>% 
      arrange(!!ensym(sort_column)) %>% 
      slice(1:num_mpa_patches) %>% 
      dplyr::pull(cell_no) 
  }else{
    print("Something went wrong, and I'm not entirely sure how we got here.")
  }
#-------------------------------------------------------------------------- 
  return(mpa_cell_list)
}

#DO NOT RUN
#Example Usage

#mpa_locations <- assign_mpa_cells(cell_lookup, metric = "distance", directionality = "lowest", prop_mpa = 0.1)
#pop %>% 
#mutate(mpa = ifelse(cell_no %in% mpa_locations, T,F))

#The above would assign the 10% of cells closest to shore as MPAs.




