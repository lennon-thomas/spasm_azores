plot_fleet <- function(sim,
                       font_size = 14,
                       L=fleet$L, 
                       cost_intercept = fleet$cost_intercept, 
                       mpasize = size_mpa){
  
  mpayear <- year_mpa#sim$year[which(sim$mpa == TRUE)[1]]
  
  mpasize <- mpasize#mean(sim$mpa[sim$year > mpayear])
  
  mpasize <- ifelse(is.na(mpasize), 0, mpasize)
  
 profit_plot<- ggplot(sim,aes(years, value)) +
    theme_bw() +
    geom_vline(aes(xintercept = 0),
               linetype = 2,
               color = "red") +
    geom_line(aes(col = mpa_scen,linetype=fleet_no), show.legend = T, size = 1.5) +
    scale_color_discrete("MPA_scenario", labels = mpa_scen) +
    scale_linetype_discrete("Fleet",labels = c("Handline","Bottom longline")) +
    labs(x = "Year (relative to MPA implementation)",  y = "Relative Profit",   caption = "Vertical line shows year MPA put in place",
      title =paste("MPA Size:",scales::percent(mpasize))) +
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), limits = c(0, NA))
  
  
  p<-profit_plot
  return(p)
}

  