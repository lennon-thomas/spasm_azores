#' \code{sim_fishery} simulates an age structured spatially explicit
#' model forward with fleets etc.
#'
#' @param fish
#' @param fleet
#' @param manager
#' @param num_patches
#' @param sim_years
#' @param ...
#' @param burn_years number of years without fishing to estimate population's unfished equlibrium
#' @param crashed_pop minimum population in patch before going to 0 assuming a population crash
#'
#' @return a pop object with population and catch trajectories
#' @export
#'
#' @examples
#' \dontrun{
#' sim_fishery(fish = fish, fleet = fleet,...)
#' }
#'
new_sim_fishery<-
  function(fish,
           fleet,
           manager,
           num_patches, #10,
           sim_years, #1,
           burn_years=10,
           crashed_pop = 1e-3,
           random_mpas,
           enviro = NA,
           enviro_strength = 1,
           rec_driver,
           est_msy,
           time_step,
           max_window = 10,
           min_size = 1,
           mpa_habfactor = 1,
           sprinkler=FALSE,
           keep_burn,
           tune_costs,
           adult_distance,
           juve_adult_distance,
           adult_juve_distance,
           juve_distance,
           shore_dist,
           hab_qual,
           effort_c,
           estimate_costs) {
# # #
   fish = fish
   fleet = fleet
   manager = create_manager(mpa_size = 0, year_mpa = 100)
   num_patches = 20
   sim_years = 20
   burn_years = 1
   time_step = fish$time_step
   #est_msy = FALSE,
   random_mpas =TRUE
   min_size = 0.05
   mpa_habfactor = 1
   sprinkler = TRUE
   keep_burn = TRUE
   adult_distance = adult_distance
   juve_adult_distance = juve_adult_distance
   adult_juve_distance = adult_juve_distance
   juve_distance = juve_distance
   shore_dist = shore_dist
   rec_driver = "stochastic"
   estimate_costs = TRUE
 #  tune_costs = FALSE
   hab_qual = hab_qual
 fuel_eff = 0.68 # km/L back of envelope calculation
 fuel_price = 1.4 # euro/L price of fuel (found of top internet search)
  perc_op_cost = 0.5



  # What is this doing
    if (sprinkler == FALSE & mpa_habfactor == 1){
      burn_years <- 1
    }


# Set up- Create empty dateframes -----------------------------------------

  sim_years <- burn_years + sim_years

   # Creates empty data frame to keep track of all things in each patch,
    pop<-
        expand.grid(
        year = 1:sim_years,
        patch = 1:num_patches,
        age = seq(fish$min_age, fish$max_age, fish$time_step)
      ) %>%
      dplyr::mutate(
        numbers = NA,
        biomass = NA,
        ssb = NA,
        numbers_caught = NA,
        profits = NA,
        effort = 0,
        f = 0,
        mpa = F,
        cost = NA
      ) %>%
      dplyr::as_data_frame() %>%
      dplyr::arrange(year, patch, age)

    pop<-left_join(pop,cell_lookup)

   # Creates vector of effort and F per year
    effort <- vector(mode = "double", length = sim_years)

    f <- vector(mode = "double", length = sim_years)

    cost<-vector(mode = "double", length = sim_years)
# Recruitment driver ----------------------------------------------------


    if (rec_driver == "stochastic") {
      rec_devs <-
        rnorm(sim_years,
              mean = 0,
              sd = fish$sigma_r)

      ## autocorrelated recruitment deviations
      for (t in 2:length(rec_devs)) {
        rec_devs[t] <-
          rec_devs[t - 1] * fish$rec_ac + sqrt(1 - fish$rec_ac ^ 2) * rec_devs[t]
      }
      # environmental variability driving recruitmet deviations
    } else if (rec_driver == "environment") {
      if (length(enviro) != sim_years) {
        stop("environment must be same length as sim_years")
      }

      rec_devs <-
        rnorm(sim_years,
              mean = enviro_strength * enviro,
              sd = fish$sigma_r)
    }
### end recruitment driver


# Define MPAs -------------------------------------------------------------
### This determines which cells will be MPAs. MPAs can be created randomly, or a vector of patch numbers can be supplied by the user
    prop_mpas <- round(num_patches * manager$mpa_size)

    if (random_mpas == T & prop_mpas > 0) {

      ms <- min(prop_mpas, max(1, min_size * num_patches))

      cwidth <- num_patches / ms

      atemp <- tibble(patch = 1:num_patches) %>%
        dplyr::mutate(cluster = cut(patch, pmax(2,round(cwidth))))

      btemp <-
        sampling::cluster(atemp,
                          cluster = "cluster",
                          pmin(n_distinct(atemp$cluster),ceiling(prop_mpas / ms)),
                          method = "srswor")

      ctemp <- sampling::getdata(atemp, btemp) %>%
        sample_n(pmin(prop_mpas, nrow(.)))

      mpa_locations <- ctemp$patch

    } else {
      mpa_locations <-
        (1:num_patches)[0:prop_mpas] #weird zero is in case prop_mpas is zero
    }

    if (!all(is.na(manager$mpa_locations))){

      if (prop_mpas > 0){
        warning("overwriting MPA size with specific MPA locations")
      }

      mpa_locations <- manager$mpa_locations

      if (max(mpa_locations) > num_patches){
        stop("invalid MPA location supplied, make sure MPAs fit inside number of patches")
      }
    }


# Define habitat for each patch and distance matrices ---------------------

# This should be a vector indicating which cells are juvenile habitat [1] and which cells are adult habitat [0]
    habitat <- cell_lookup$juve_ad_hab

    juve_cells<-cell_lookup[cell_lookup$juve_ad_hab==1,"patch"]

    ad_cells<-cell_lookup[cell_lookup$juve_ad_hab==0,"patch"]



# Create at-age lookup key ------------------------------------------------

    n0_at_age <-#only count the number of patches that have juvenile habitat
      (fish$r0 / length(juve_cells)) * exp(-fish$m * seq(fish$min_age, fish$max_age, fish$time_step))

    n0_at_age[fish$max_age + 1] <-
      n0_at_age[fish$max_age + 1] / (1 - exp(-fish$m))

    b0_at_age <- n0_at_age * fish$weight_at_age

    ssb0_at_age <- n0_at_age * fish$ssb_at_age

# Generate timeseries of cost, price, and q -------------------------------

    price_series <-
      generate_timeseries(
        fish$price,
        fish$price_cv,
        fish$price_ac,
        fish$price_slope,
        time = sim_years
      )

    q <-
      generate_timeseries(
        fleet$q,
        fleet$q_cv,
        fleet$q_ac,
        fleet$q_slope,
        time = sim_years
      )

    if (length(q) == 1) {
      q <- rep(q, sim_years)
    }
    if (length(price_series) == 1) {
      price_series <- rep(price_series, sim_years)
    }



    cost_series <-
      generate_timeseries(
        fleet$cost,
        fleet$cost_cv,
        fleet$cost_ac,
        fleet$cost_slope,
        time = sim_years
      )


    price_frame <-
      dplyr::data_frame(year = 1:sim_years, price = price_series)

    cost_frame <- dplyr::data_frame(year = 1:sim_years, cost = cost_series)

# Join new cost and  pop frame --------------------------------------------

    pop <- pop %>%
      dplyr::select(-cost) %>%
      dplyr::left_join(cost_frame, by = "year") %>%
      dplyr::left_join(price_frame, by = "year")



# Distribute R0 and BO by patch -------------------------------------------

    # Distribute R0 evenly throughout juvenile patches
    pop$numbers[pop$year == 1 & pop$juve_ad_hab == 1 & pop$age<=fish$age_mature] <- rep(n0_at_age[c(1:(fish$age_mature+1))], length(juve_cells))

    # Calculate how many three year olds present in juvenile habitat and move to adult habitat according to habitat quality of adult cells

     mat_age_pop = pop %>% filter(year == 1, age == fish$age_mature)

     total_mat_age_no<-sum(mat_age_pop$numbers,na.rm=TRUE)

     mat_age_hab_vec<-total_mat_age_no*cell_lookup$hab_qual

     age_mature <- fish$age_mature


  # At the end of this loop there should be numbers at all age for year 1 and age 4+ in adult habitat and ages <4 in juvenile habitat
  # This creates a vector of number of adults in each adult patch. ages 0-3 should be 0 in adult habitat

     for (i in 1:length(ad_cells)) {
       n0_at_age_patch <-
         append (rep(0, age_mature), (mat_age_hab_vec[i] * exp(
           -fish$m * seq(fish$min_age, fish$max_age - age_mature, fish$time_step)
         )))

       n0_at_age_patch[fish$max_age + 1] <-
         n0_at_age_patch[fish$max_age + 1] / (1 - exp(-fish$m))

       b0_at_age <- n0_at_age_patch * fish$weight_at_age

       ssb0_at_age <- n0_at_age_patch * fish$ssb_at_age

       pop$numbers[pop$year == 1 &
                     pop$patch == i & pop$juve_ad_hab == 0] <- (n0_at_age_patch)
     }
     pop$numbers[pop$year == 1  &
                   pop$juve_ad_hab == 1 & pop$age == age_mature] <- 0

# Add distance to shore to cost -------------------------------------------
# distance is in kilometers
     distance_to_shore <- shore_dist[cell_lookup$cell_no, ] %>%
       dplyr::select(c(2, 3)) %>%
       mutate(patch = seq(1:num_patches))

     colnames(distance_to_shore) <- c("cell_no", "distance", "patch")
distance_to_shore$distance[distance_to_shore$distance==0]<-20

     cost_frame <-
         expand.grid(year = 1:sim_years, patch = 1:num_patches) %>%
         dplyr::as_data_frame() %>%
         dplyr::left_join(cost_frame, by = "year") %>%
         dplyr::left_join(distance_to_shore, by = "patch") %>%



       pop <- pop %>%
         dplyr::select(-cost) %>%
         dplyr::left_join(cost_frame, by = c("patch", "year"))





# Calculate biomass and ssb for each age class ----------------------------

     pop <- pop %>%
       dplyr::left_join(
         dplyr::data_frame(
           age = seq(fish$min_age, fish$max_age, fish$time_step),
           ssb_at_age = fish$ssb_at_age,
           weight_at_age = fish$weight_at_age
         ),
         by = "age"
       )

     y <- 1

     model_phase <- "burn"


 # Calculate Movement ------------------------------------------------------

# This is movement without density. See 'movement' script for distance calc of  _distance files


     adult_move_grid <- adult_distance %>%
       left_join(cell_lookup, by = c("to" = "cell_no")) %>%
       dplyr::mutate(movement = ifelse(is.na(dist), NA, ifelse(
         is.finite(dnorm(dist, 0, fish$adult_movement)),
         dnorm(dist, 0, fish$adult_movement),
         1
       ))) %>%
       group_by(from) %>%
       dplyr::mutate(prob_move = movement / sum(movement, na.rm = TRUE))


     adult_move_matrix <- adult_move_grid %>%
       ungroup() %>%
       dplyr::select(from, to, prob_move) %>%
       spread(to, prob_move) %>%
       dplyr::select(-from) %>%
       as.matrix()

     # I don't think adult to juvenile movement (i.e. larval dispersal, matters anymore).
     # Keeping just in case we want t o try different recruitment

      adult_juve_move_grid <- adult_juve_distance %>%
       dplyr::mutate(movement = ifelse(is.na(dist), NA, ifelse(
         is.finite(dnorm(dist, 0, fish$adult_movement)),
         dnorm(dist, 0, fish$adult_movement),
         1
       )))  %>%
       group_by(from) %>%
       dplyr::mutate(prob_move = movement / sum(movement, na.rm = TRUE))


     adult_juve_move_matrix <- adult_juve_move_grid %>%
       ungroup() %>%
       dplyr::select(from, to, prob_move) %>%
       spread(to, prob_move) %>%
       dplyr::select(-from) %>%
       as.matrix()


# Start looping through years ---------------------------------------------

     for (y in 1:(sim_years - 1)) {


       now_year <- pop$year == y


   # Include density-dependent movement --------------------------------------

       # This loop uses depletion rate in each cell to incorporate density dependent  movement in movement matrices
       # Density movement modifier is a parameter that indicates how much density dependence affects movement. Must be between 0 and 1 (?).
       # Density dependent adult movement does not occur during burn years

       if (fish$density_movement_modifier < 1 & y > burn_years) {
         slope <-
           fish$adult_movement - (fish$adult_movement * fish$density_movement_modifier)


         how_crowded <- pop %>%
           filter(now_year) %>%
           group_by(patch) %>%
           summarise(ssb = sum(ssb, na.rm = TRUE)) %>%
           dplyr::arrange(patch) %>%
           mutate(depletion = ssb / fish$ssb0) %>%
           mutate(move_rate = pmin(
             fish$adult_movement,
             slope * depletion + (fish$adult_movement * fish$density_movement_modifier)
           )) %>%
              dplyr::select(patch, move_rate)

         how_crowded <- left_join(how_crowded, cell_lookup) %>%
           dplyr::select(cell_no, move_rate)

         adult_distance[is.na(adult_distance)] <- 0

         adult_move_grid <- adult_distance %>%
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

         adult_move_grid[adult_move_grid$from %in% juve_cell_no |
                           adult_juve_move_grid$to %in% juve_cell_no, "prob_move"] <- 0

         adult_move_matrix <- adult_move_grid %>%
           ungroup() %>%
           dplyr::select(from, to, prob_move) %>%
           spread(to, prob_move) %>%
           dplyr::select(-from) %>%
           as.matrix()

         juve_adult_move_grid <- juve_adult_distance %>%
           left_join(how_crowded, by = c("from" = "cell_no")) %>%
           dplyr::mutate (movement = ifelse(is.na(dist), NA, ifelse(
             is.finite(dnorm(dist, 0, move_rate)),
             dnorm(dist, 0, move_rate),
             1
           )))  %>%
           group_by(from) %>%
           dplyr::mutate(prob_move = movement / sum(movement, na.rm = TRUE))

         juve_adult_move_matrix <- juve_adult_move_grid %>%
           ungroup() %>%
           dplyr::select(from, to, prob_move) %>%
           spread(to, prob_move) %>%
           dplyr::select(-from) %>%
           as.matrix()

       }
 # Move different age classes ----------------------------------------------

       # Age mat (4) just sum all individuals at age mat and move them from juvenile to adult habitat based on adult habitat quality

       # Move 4 year olds (age at mat) from juvenile to adult habitat. # of adults per patch is different.


       total_no <-
         sum(pop %>% filter(year == y, age == fish$age_mature) %>% dplyr::select((numbers)))

       total_bio<- sum(pop %>% filter(year == y, age == fish$age_mature) %>% dplyr::select((biomass)))
       total_ssb<- sum(pop %>% filter(year == y, age == fish$age_mature) %>% dplyr::select((ssb)))


       pop[now_year &
             pop$age == (fish$age_mature), ] <- pop[now_year &
                                                      pop$age == fish$age_mature, ] %>%
         group_by(age) %>%
         mutate(numbers = total_no * cell_lookup$hab_qual,
                biomass = total_bio * cell_lookup$hab_qual,
                ssb = total_ssb * cell_lookup$hab_qual) %>%
         ungroup ()


       adult_move_matrix[is.na(adult_move_matrix)] <- 0
       pop$numbers[is.na(pop$numbers)] <- 0

       pop[now_year &
             pop$age > (round(fish$age_mature, 0)),] <-
         move_fish(
           here_pop = pop %>% filter(year == y, age > fish$age_mature),
           fish = fish,
           num_patches = num_patches,
           move_matrix = adult_move_matrix
         )

# Add MPA -----------------------------------------------------------------

       # make a column indicating what year which patches become MPAs
       if ((y - burn_years) == manager$year_mpa) {
         pop$mpa[pop$patch %in% mpa_locations & pop$year >= y] <- T

         # Calculates effort within an MPA prior to implementation
         if (fleet$mpa_reaction == "leave") {
           mpa_effort <-
             sum(pop$effort[pop$patch %in% mpa_locations &
                              pop$year == (y - 1) & pop$age == 0])

           effort[y - 1] <-   effort[y - 1] - mpa_effort


         }

       }

# Adjust fleet ------------------------------------------------------------

       if (y > (burn_years)) {

          # This is K calculated from burn years (no fishing)
        b0 <- sum(pop$biomass[pop$year == burn_years])


         # This is where total effort is calculated. 'determine effort' was a different previous function used here before
         effort[y] <- determine_effort_az(
           fleet = fleet,
           fish = fish,
           pops = pop[pop$year == y,],
           boxdir = boxdir
         )
        }

     }
     
     dev_pro<-optimize(find_L, interval = c(10000.00000,100000.00000),)
      
     
     pop[now_year, "effort"] <-
        distribute_fleet2(
          pops = pop %>% filter(year == y),
          year = y,
          fish = fish,
          burn_years = burn_years,
          effort = effort[y],
          fleet = fleet,
          num_patches = num_patches,
          mpa = mpa
        )

      pop[now_year, "f"] <-
        pop[now_year, "effort"] * q[y]


# Growth and Mortality ----------------------------------------------------
      pop$numbers[is.na(pop$numbers)]<-0
      pop$f[is.na(pop$f)]<-0

       pop[pop$year == (y + 1), "numbers"]  <-
        pop[now_year, ] %>%
        dplyr::group_by(patch) %>%
        dplyr::mutate(numbers = grow_and_die(
          numbers = numbers,
          f = f,
          mpa = mpa,
          fish = fish,
          fleet = fleet,
          y = y
        )$survivors) %>%
        ungroup() %>%
        {
          .$numbers
        }

      pop[now_year, "numbers_caught"] <-
        pop[now_year, ] %>%
        group_by(patch) %>%
        dplyr::mutate(
          numbers_caught = grow_and_die(
            numbers = numbers,
            f = f,
            mpa = mpa,
            fish = fish,
            fleet = fleet,
            y = y
          )$caught
        ) %>%
        ungroup() %>%
        {
          .$numbers_caught
        }

      pop <- pop %>%
        dplyr::mutate(patch_age_costs = ((cost) * (effort)) / fish$max_age) %>% # divide costs up among each age class
        dplyr::mutate(
          ssb = numbers * ssb_at_age,
          biomass = numbers * weight_at_age,
          biomass_caught = numbers_caught * weight_at_age,
          profits = biomass_caught * price - patch_age_costs
        )


# Spawn -------------------------------------------------------------------

 # Spawn and calculate R0 for each juvenile patch next year

      #Model phase is 'burn here so it skips calculating recruits and jumps down to calculate ssb0
      adult_juve_move_matrix[is.na(adult_juve_move_matrix)]<-0
# Don't need move matrix if re
       pop$numbers[pop$year == (y + 1) &
                    pop$age == fish$min_age] <-
        calculate_recruits(
          pop = pop[pop$year == y, ],
          fish = fish,
          num_patches = num_patches,
          phase = model_phase,
          move_matrix = adult_juve_move_matrix, # This should be larval dispersal matrix. Only used in one of the recruitment assumption options
          patch_habitat = cell_lookup$juve_ad_hab
        )


      if (y == burn_years) {
        fish$ssb0 <- pop %>%
          filter(year == burn_years) %>%
          group_by(patch) %>%
          summarise(ssb = sum(ssb,na.rm=TRUE)) %>%
          ungroup() %>%
          {
            (.$ssb)
          }

        model_phase <- "recruit"

        effort[y + 1] <- fleet$initial_effort
      }

# End of one year ---------------------------------------------------------


    }
    rec_mat <-
      dplyr::data_frame(year = 1:sim_years, rec_dev = rec_devs) # Data fraom of recruitment deviates


    og <- burn_years
    if (keep_burn == TRUE) {
      burn_years <- -99
    }

    pop <- pop %>%
      dplyr::left_join(rec_mat, by = "year") %>%
      dplyr::filter(year > burn_years, year < max(year)) %>%
      dplyr::mutate(
        burn = year <= og,
        eventual_mpa = patch %in% mpa_locations,
        b0 = b0
      )

    return(pop)
  }
