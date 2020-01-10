
#'
#' @param pops
#' @param effort
#' @param fleet
#'
#' @return a redistributed vector of effort in space
#' @export
#'
#' @examples
#' \dontrun
#'
#' #p_summary
distribute_fleet_az <-
  function(L,
           pops = pop %>% filter(year == y),
           cell_lookup = cell_lookup,
           year = y,
           fish = fish,
           burn_years = burn_years,
           total_effort = effort * 1e+06,#effort[y],
           fleet = fleet,
           num_patches = num_patches,
           mpa = mpa,
           total_profit = 0.1,
           beta = 1.3
           )
    {

# New approach from # 1/7/20 meeting with Ren -----------------------------

  # Step 1 assume we know total effort (# of hooks) (from Table 13.4.2. in @ICES2018) and calculate total average cost ($/hook)
  # assuming we know profit
    total_biomass<-sum(pops$biomass)

profit_guess<-c(0.000001,1,1000,100000,1e+06)
profit_cost<-as.data.frame(profit_guess) %>%
  mutate(total_avg_cost = 0)

for (i in 1:length(profit_guess)){
# This is in units of avg/$ hook across all patches
    profit_cost$total_avg_cost [i] <-(profit_guess[i]-(fish$price * fleet$q * total_effort * total_biomass))/-total_effort^1.3
}

plot(profit_cost)

total_avg_cost <-(total_profit-(fish$price * fleet$q * total_effort * total_biomass))/-total_effort^1.3

cost_revenue<-profit_cost$total_avg_cost*total_effort/(fish$price * fleet$q * total_effort * total_biomass)

 # total_profit_check = fish$price * fleet$q * total_effort * total_biomass - total_avg_cost * total_effort ^ 1.3

 # total_profit_check - total_profit # should be zero
}
# Calculate the average cost/hook for each patch- scale costs based on distance to shore

patch_distance<-pops %>%
  group_by(patch) %>%
  summarise(distance = sum(unique(distance)))

total_distance = sum(patch_distance$distance)

patch_distance<- patch_distance %>%
  mutate()

# Patch costs will be different for each patch and constant patch over time.
pops <- pops  %>%
  mutate (alpha = total_avg_cost * num_patches / total_distance,
          patch_cost = distance * alpha)

#  mean(pops$patch_cost) -total_avg_cost

#  plot(pops$distance, pops$patch_cost)

# Calculate effort for each patch assuming that profits across patches are equal

patch_summary <-
  pops %>%
  group_by(patch) %>%
  summarise(
    patch_biomass = sum(biomass, na.rm = TRUE),
    patch_cost = unique(patch_cost),
    patch_profit = total_profit / num_patches
  ) %>%
  ungroup() %>%
  #mutate (e_patch = ((fish$price * fleet$q * patch_biomass - patch_profit) / (patch_cost)) ^1.3)
  mutate (e_patch = ((fish$price * fleet$q * patch_biomass - patch_profit) / ( beta + patch_cost)) ^(10 / 3))


return ()



# Old SSE way from 12/20 mtg with CC --------------------------------------


#   patch_summary<-
#     pops %>%
#       group_by(patch) %>%
#       summarise(patch_biomass = sum(biomass, na.rm = TRUE),
#                        patch_cost = unique(cost) / 1000) %>%
#       ungroup() %>%
#       mutate (e_patch = ((fish$price * fleet$q * patch_biomass - L) / (beta * patch_cost)) ^(10 / 3))
#       # e_patch2 = exp(log((fish$price * fleet$q * patch_biomass- L)/(beta*patch_cost))/0.3),
#       # profit = fish$price * fleet$q * patch_biomass - beta * patch_cost *  e_guess ^ (beta - 1), # This should equal whatever we specify as L
#       # e_guess_pos = ifelse(e_guess < 0, 0, e_guess)) #get rid of any patches where effort was zero
#
# estimated_total <- sum(patch_summary$e_patch)
#
# e_diff <-  (log(effort)- log(estimated_total))^2
#
# return (e_diff)
#   }
#
#
#
# profit_dist<-rnorm(100,2,1)
#
# test<-p_summary(L=0)
#
# t<-optim(
#            par = c(.01,.1),
#            fn = p_summary,
#            pops = pops,
#            year = y,
#            fish = fish,
#            effort = effort[y],
#            fleet = fleet,
#            num_patches = num_patches,
#            mpa = mpa,
#            beta = 1.3)
#


# solve_L(L=1)#,patch_summary = patch_summary,efforts=effort[y])
#
# test<-p_summary(L=t$par[1],
#                 pops = pop %>% filter(year == y),
#                 year = y,
#                 fish = fish,
#                 burn_years = burn_years,
#                 effort = effort[y],
#                 fleet = fleet,
#                 num_patches = num_patches,
#                 mpa = mpa)
#
# L_answer <- t$par[1]
#       L,
#       pops = pop %>% filter(year == y),
#       year = y,
#       fish = fish,
#       burn_years = burn_years,
#       effort = effort[y],
#       fleet = fleet,
#       num_patches = num_patches,
#       mpa = mpa))
