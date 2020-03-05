#' create_fish
#'
#' creates a fish list object with all the life history goodies
#'
#' @param common_name
#' @param scientific_name
#' @param linf
#' @param vbk
#' @param t0
#' @param max_age
#' @param weight_a
#' @param weight_b
#' @param length_50_mature
#' @param length_95_mature
#' @param age_50_mature
#' @param age_95_mature
#' @param age_mature
#' @param length_mature
#' @param m
#' @param steepness
#' @param density_dependence_form
#' @param adult_movement
#' @param larval_movement
#' @param query_fishlife
#' @param r0
#' @param cv_len
#' @param length_units
#' @param min_age
#' @param time_step
#' @param weight_units
#' @param delta_mature
#' @param price
#' @param sigma_r
#' @param rec_ac
#' @param cores
#' @param mat_mode
#' @param price_ac
#' @param price_cv
#' @param import_life_hist_params
#' @param life_hist_params_path
#'
#' @return a fish list object
#' @export
#'
#' @examples
#' \dontrun{
#' white_seabass = create_fish(scientific_name = "Atractoscion nobilis", query_fishlife = T)
#'}
create_fish_az_from_import <- function(common_name = 'white seabass',
                           scientific_name = "Atractoscion nobilis",
                           linf = NA,
                           vbk = NA,
                           t0 = -0.1, #differs from imported value. Would have to explicitly list it as NA in function call to get overwritten
                           cv_len = 0.1,
                           length_units = 'cm',
                           min_age = 0, #same as above
                           max_age = NA,
                           time_step = 1,
                           weight_a = NA,
                           weight_b = NA,
                           weight_units = 'kg',
                           length_50_mature = NA,
                           length_95_mature = NA,
                           delta_mature = .1,
                           age_50_mature = NA,
                           age_95_mature = NA,
                           age_mature = NA,
                           length_mature = NA,
                           m = NA,
                           steepness = 0.8, #same as above
                           r0 = 10000,
                           density_dependence_form = 1,
                           adult_movement = 2, #same as above
                           larval_movement = 2,
                           query_fishlife = T,
                           price = 1,
                           price_cv = 0,
                           price_ac = 0,
                           price_slope = 0,
                           sigma_r = 0,
                           rec_ac = 0,
                           cores = 4,
                           mat_mode = "age",
                           default_wb = 2.8,
                           tune_weight = FALSE,
                           density_movement_modifier = 1,
                           linf_buffer = 1.2,
                           import_life_hist_params = FALSE,
                           life_hist_params_path = NA) {
  
  
  
  
  
  fish <- list()
  # check to see if user wants to import life hitory paramters from a local file --------------------
  
  if(import_life_hist_params == T & !is.na(life_hist_params_path)){
    life_hist_params <- read_csv(life_hist_params_path) %>% 
      dplyr::select(parameter, value) %>% 
      mutate(value = as.numeric(value)) %>% 
      filter(!is.na(value)) %>% 
      pivot_wider(names_from = parameter, values_from = value)
    
    #Assign imported values to funciton variables. Defaults to only overriding a variable defined in the function call with an imported value if the function varible is NA. This mimics the behaviour precedent of querying FishLife that is implemented below.
    
    if(is.na(linf)){
      linf <- life_hist_params$linf
    }
    if(is.na(vbk)){
      vbk <- life_hist_params$vbk
    }
    if(is.na(t0)){
      t0 <- life_hist_params$t0
    }
    if(is.na(min_age)){
      min_age <- life_hist_params$min_age
    }
    if(is.na(max_age)){
      max_age <- life_hist_params$max_age
    }
    if(is.na(weight_a)){
      weight_a <- life_hist_params$weight_a
    }
    if(is.na(weight_b)){
      weight_b <- life_hist_params$weight_b
    }
    if(is.na(length_50_mature)){
      length_50_mature <- life_hist_params$length_50_mature
    }
    
    #what to do with length_"selected" variables?
    
    if(is.na(m)){
      m <- life_hist_params$m
    }
    if(is.na(steepness)){
      steepness <- life_hist_params$steepness
    }
    if(is.na(adult_movement)){
      adult_movement <- life_hist_params$adult_movement
    }
      } else if (is.na(scientific_name) == F & query_fishlife == T) {
    # check fishbase -------------
    
    genus_species <- stringr::str_split(scientific_name, " ", simplify = T) %>%
      as.data.frame() %>%
      set_names(c("genus", "species"))
    
    fish_life <- genus_species %>%
      dplyr::mutate(life_traits = pmap(list(Genus = genus, Species = species), safely(Get_traits_az)))
    
    fish_life <- fish_life %>%
      dplyr::mutate(fish_life_worked = purrr::map(life_traits, 'error') %>% map_lgl(is.null)) %>%
      dplyr::filter(fish_life_worked) %>%
      dplyr::mutate(life_traits = purrr::map(life_traits, 'result')) %>%
      tidyr::unnest(cols=c(life_traits)) %>%
      dplyr::mutate(taxa = glue::glue('{genus} {species}')) %>%
      rlang::set_names(tolower)
    
    
    if (weight_units == "kg"){
      fish_life$winfinity <- fish_life$winfinity / 1000
    }
    
    if (tune_weight == T){
      weight_stan <- "
      data {
      real winf;
      real linf;
      }
      parameters {
      real<lower = 0> wa;
      real<lower = 2.7, upper = 3.2> wb;
      real<lower = 0, upper = 1> sigma;
      }
      transformed parameters{
      real w_hat;
      w_hat = wa*linf^wb;
      
      }
      model {
      winf ~ normal(w_hat, sigma);
      wb ~ normal(3,.1);
      }
      "
      
      weight_fit <-
        rstan::stan(
          model_code = weight_stan,
          data = list(winf = fish_life$winfinity*2, linf = fish_life$loo),
          verbose = F,
          cores = cores
        )
      
      weight_fit <- broom::tidy(weight_fit) %>%
        dplyr::select(term, estimate) %>%
        tidyr::spread(term, estimate)
    } else{
      weight_fit <- dplyr::data_frame(wa = fish_life$winfinity / (fish_life$loo ^ default_wb),
                                      wb = default_wb)
    }
    # process lengths ---------------------------------------------------------
    
    if (is.na(linf)) {
      linf <- fish_life$loo
    }
    
    if (is.na(vbk)) {
      vbk <- fish_life$k
      
    }
    
    if (is.na(weight_a)) {
      weight_a <- weight_fit$wa
      
    }
    if (is.na(weight_b)) {
      weight_b <- weight_fit$wb
      
    }
    
    if (is.na(max_age)){
      
      max_age <- ceiling(fish_life$tmax)
      
    }
    
    if (is.na(age_mature)){
      
      age_mature <- round(fish_life$tm,0)
      
    }
    
    if (is.na(length_mature)){
      
      length_mature <- fish_life$lm
      
    }
    
    if (is.na(m)){
      
      m <- fish_life$m
      
    }
    
} #close fishlife query
  
  # max_age <- ((-log(0.01)/m)) %>% floor()
  
  # if (is.na(vbk)){
  #
  #   vbk <- m / (lhi_groups$mean_m_v_k[lhi_groups$type == lhi_type])
  #
  # }
  # if (is.na(weight_a)){
  #
  #   weight_a <-lhi_groups$mean_wa[lhi_groups$type == lhi_type]
  #
  #   weight_b <-lhi_groups$mean_wb[lhi_groups$type == lhi_type]
  #
  # }
  
  #calculations begin regardless of Fish Life, user entry, or import
  
  length_at_age <- linf * (1 - exp(-vbk * (seq(min_age,max_age, by = time_step) - t0)))
  
  # process weight
  
  weight_at_age <- (weight_a) * length_at_age ^ weight_b
  
  lmat_to_linf_ratio <- length_mature / linf
  
  #
  
  length_at_age_key <- generate_length_at_age_key(
    min_age = min_age,
    max_age = max_age,
    cv = cv_len,
    linf = linf,
    k = vbk,
    t0 = t0,
    time_step = time_step,
    linf_buffer = linf_buffer
  ) %>%
    dplyr::ungroup() %>%
    dplyr::select(age, length_bin, p_bin) %>%
    tidyr::spread(length_bin, p_bin) %>%
    dplyr::select(-age)
  
  # process maturity
  if ((is.na(age_50_mature) |
       is.na(age_95_mature)) & is.na(age_mature) == F) {
    age_50_mature <- round(age_mature,0)
    
    age_95_mature <-  age_50_mature + delta_mature
    
    maturity_at_age <-
      ((1 / (1 + exp(-log(
        19
      ) * ((seq(min_age,max_age, by = time_step) - age_50_mature) / (age_95_mature - age_50_mature)
      )))))
    
  } else if (is.na(age_mature) | mat_mode == "length") {
    if (is.na(length_mature)){
      if(import_life_hist_params){
        length_mature <- length_50_mature
      }else{
        length_mature <-  linf * lmat_to_linf_ratio
      }
    }
    
    length_bins <- as.numeric(colnames(length_at_age_key))
    
    mat_at_bin <- ((1 / (1 + exp(-log(
      19
    ) * ((length_bins - length_mature) / (delta_mature)
    )))))
    
    p_mat_at_age <- (as.matrix(length_at_age_key) %*% mat_at_bin)
    
    mat_at_age <- dplyr::data_frame(age = seq(min_age,max_age, by = time_step),mean_mat_at_age = p_mat_at_age)
    
    age_mature <- mat_at_age$age[mat_at_age$mean_mat_at_age >= 0.5][1]
    
    age_50_mature <- age_mature
    
    age_95_mature <-  mat_at_age$age[mat_at_age$mean_mat_at_age >= 0.95][1]
    
    maturity_at_age <- mat_at_age$mean_mat_at_age
  }
  
  
  if (is.na(length_50_mature)){
    
    length_50_mature <- length_mature
  }
  if(is.na(length_95_mature)){
    length_95_mature <- length_50_mature + delta_mature
    
  }

  ssb_at_age <-  maturity_at_age * weight_at_age
  
  fish <- list(mget(ls()))
  
  fish <- fish[[1]]
  
  #
  # fish$scientific_name <- scientific_name
  # fish$common_name <- common_name
  # fish$ssb_at_age <- fish$maturity_at_age * fish$weight_at_age
  # fish$linf <- linf
  # fish$vbk  <-  vbk
  # fish$t0 <-  t0
  # fish$cv_len <- cv_len
  # fish$max_age <-  max_age
  # fish$min_age <- min_age
  # fish$weight_a <-  weight_a
  # fish$weight_b <-  weight_b
  # fish$length_50_mature <-  length_50_mature
  # fish$length_95_mature <-  length_95_mature
  # fish$age_50_mature <-  age_50_mature
  # fish$age_95_mature <-  age_95_mature
  # fish$delta_mature <- delta_mature
  # fish$age_mature <-  age_mature
  # fish$length_mature <-  length_mature
  # fish$m <-  m
  # fish$steepness <- steepness
  # fish$r0 <- r0
  # fish$density_dependence_form = density_dependence_form
  # fish$adult_movement <-  adult_movement
  # fish$larval_movement <-  larval_movement
  # fish$lmat_to_linf_ratio <-  lmat_to_linf_ratio
  # fish$length_units <-  length_units
  # fish$weight_units <-  weight_units
  # fish$price <- price
  # fish$sigma_r <- sigma_r
  # fish$rec_ac <- rec_ac
  # fish$time_step <- time_step
  
  return(fish)
  }