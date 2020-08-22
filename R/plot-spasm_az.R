#' Plot Spasm
#'
#' @param sim a simulation produced by sim_fishery
#' @param type the type of plot to produce, one of patch,totals, and doughnut
#' @param font_size base font size for plots
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_spasm(sim, type = "patch")
#'
#' }
#'
#'
plot_spasm_az <- function(sim, type = "patch", font_size = 14, L=fleet$L, cost_intercept = fleet$cost_intercept, mpasize = size_mpa){

mpayear <- year_mpa#sim$year[which(sim$mpa == TRUE)[1]]

mpasize <- mpasize#mean(sim$mpa[sim$year > mpayear])

mpasize <- ifelse(is.na(mpasize), 0, mpasize)

# ptheme <-  hrbrthemes::theme_ipsum(
#   base_size = font_size,
#   axis_title_size = font_size,
#   strip_text_size = font_size + 2
# )

if (type == "patch"){
  out <- sim %>%
    group_by(year, patch) %>%
    summarise(
      F = sum(f)/15,
      Profits = sum(profits),
      Biomass = sum(biomass)
    ) %>%
    ungroup() %>%
    mutate(`Profit Per Unit Effort` = Profits /F) %>%
    gather(metric, value,-year, -patch) %>%
    ggplot(aes(year, value, color = factor(patch))) +
    geom_vline(aes(xintercept = mpayear),
               linetype = 2,
               color = "red") +
    geom_line(show.legend = F, size = 1.5) +
    facet_wrap( ~ metric, scales = "free_y") +
    labs(x = "Year",  y = "", caption = "Each line/color represents a patch. Vertical line shows year MPA put in place",
         title = paste("MPA Size:",scales::percent(mpasize),"        L= ",L, "  cost_intercept = ",cost_intercept)) +
    theme_bw()

}

if (type == "by_fleet"){

profit_plot <- sim %>%
  group_by(years,fleet_no) %>%
  summarise(
    f = sum(f)/num_patches,
    Profits = sum(profits,na.rm=TRUE),
    Biomass = sum(biomass,na.rm=TRUE),
    Catch = sum(biomass_caught,na.rm=TRUE),
    B0 =unique(b0),
    relative_profit_hl = unique(relative_profit_hl),
    relative_profit_bll = unique(relative_profit_bll)                          
  ) %>%
 ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / (Catch/Biomass),
         B_ratio = Biomass/B0,
         rel_profit = ifelse(fleet_no==1,Profits/relative_profit_hl,Profits/relative_profit_bll)) %>%
dplyr:: select(-c(B0,Biomass,'Profit Per Unit Effort',B_ratio,f,Catch,Profits,relative_profit_hl,relative_profit_bll)) %>%
  gather(metric, value,-c(years,fleet_no)) %>%

  ggplot(aes(years, value)) +
  theme_bw() +
  geom_vline(aes(xintercept = 0),
             linetype = 2,
             color = "red") +
  geom_line(aes(col = fleet_no), show.legend = T, size = 1.5) +
  scale_color_discrete("Fleet", labels = c("Handline", "Bottom longline")) +
  # facet_wrap( ~ metric, scales = "free_y") +
  labs(x = "Year (relative to MPA implementation)",  y = "Relative Profit") + # caption = "Vertical line shows year MPA put in place",
  #  title = "Profit") +#paste("MPA Size:",scales::percent(mpasize))) +
  theme_bw() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), limits = c(0, NA))



out<-profit_plot
}

if (type == "totals"){
b_plot <- sim %>%
  group_by(year) %>%
  summarise(
    f = sum(f)/num_patches,
    Profits = sum(profits,na.rm=TRUE),
    Biomass = sum(biomass,na.rm=TRUE),
    Catch = sum(biomass_caught,na.rm=TRUE),
    B0 =unique(b0),
  ) %>%
  ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / (Catch/Biomass),
         #    f = Catch/Biomass,
         B_ratio = Biomass/B0,
         years = c(-1:16)) %>%
  # f2=Effort*fleet$q) %>%
  dplyr:: select(-c(B0,Biomass,'Profit Per Unit Effort',year)) %>%
  gather(metric, value,-c(years)) %>%
  filter(metric == "B_ratio") %>%
  
  ggplot(aes(years, value)) +
  theme_bw() +
  geom_vline(aes(xintercept = 0),
             linetype = 2,
             color = "red") +
  geom_line( size = 1.5) +
  #scale_color_discrete("Fleet",labels=c("Handline","Bottom longline")) +
 # facet_wrap( ~ metric, scales = "free_y", nrow = 2) +
  labs(x = "",  y = "B/Bmsy",# caption = "Vertical line shows year MPA put in place",
       title = paste("MPA Size:",scales::percent(size_mpa))) +
  theme_bw()+
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)),limits=c(0,NA))

catch_plot <- sim %>%
  group_by(year) %>%
  summarise(
    f = sum(f) / num_patches,
    Profits = sum(profits, na.rm = TRUE),
    Biomass = sum(biomass, na.rm = TRUE),
    Catch = sum(biomass_caught, na.rm = TRUE),
    B0 = unique(b0),
  ) %>%
  ungroup() %>%
  mutate(`Profit Per Unit Effort` = Profits / (Catch / Biomass),
         #    f = Catch/Biomass,
         B_ratio = Biomass / B0,
         years = c(-1:16))  %>%
  # f2=Effort*fleet$q) %>%
  dplyr::select(-c(B0, Biomass, 'Profit Per Unit Effort',year)) %>%
  gather(metric, value, -c(years)) %>%
  filter(metric == "Catch") %>%
  
  ggplot(aes(years, value)) +
  theme_bw() +
  geom_vline(aes(xintercept = 0),
             linetype = 2,
             color = "red") +
  geom_line( size = 1.5) +
 # geom_line(aes(col = fleet_no), show.legend = T, size = 1.5) +
  #scale_color_discrete("Fleet", labels = c("Handline", "Bottom longline")) +
  #    facet_wrap( ~ metric, scales = "free_y", nrow = 2) +
  labs(
    x = "Year (relative to MPA implementation)",
    y = "Catch (MT)") +
  # caption = "Vertical line shows year MPA put in place",
  #  title = paste("Catch") +
  theme_bw() +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), limits =
                       c(0, NA))


out<-ggarrange(b_plot,catch_plot,nrow=2)
}
if (type == "doughnut"){

  out <- sim %>%
    group_by(year, patch) %>%
    summarise(
      Effort = sum(effort),
      Profits = sum(profits),
      Biomass = sum(biomass),
      mpa = unique(eventual_mpa)
    ) %>%
    ungroup() %>%
    mutate(`Profit Per Unit Effort` = Profits / Effort) %>%
    gather(metric, value,-year,-patch,-mpa) %>%
    group_by(metric) %>%
    mutate(svalue = value / max(value, na.rm = TRUE)) %>%
    ungroup()

  out <- out %>%
    # filter(year == max(year)) %>%
    ggplot(aes(patch,svalue, fill = mpa)) +
    geom_col(alpha = 0.75,
             color = "transparent",
             width = 1) +
    facet_wrap(~metric) +
    coord_polar() +
    gganimate::transition_time(year) +
    gganimate::ease_aes('linear') +
    labs(title = 'Year: {frame_time}',x = "'",  y = "Relative Value") +
    theme_bw()
}

return(out)

}
