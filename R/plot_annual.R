plot_annual <- function(sim,
                       font_size = 14,
                       L=fleet$L, 
                       cost_intercept = fleet$cost_intercept, 
                       mpasize = size_mpa){
  
  mpayear <- year_mpa#sim$year[which(sim$mpa == TRUE)[1]]
  
  mpasize <- mpasize#mean(sim$mpa[sim$year > mpayear])
  
  mpasize <- ifelse(is.na(mpasize), 0, mpasize)
  
  biomass<-sim %>%
    filter(metric == "B_ratio")
  
 biomass_plot<- ggplot(biomass,aes(years, value)) +
    theme_bw() +
    geom_vline(aes(xintercept = 0),
               linetype = 2,
               color = "red") +
    geom_line(aes(col=mpa_scen),show.legend = TRUE, size = 1.5) +
   scale_color_discrete("MPA scenario", labels = mpa_scen) +
    # facet_wrap( ~ metric, scales = "free_y") +
    labs(x = "Year (relative to MPA implementation)",  y = "B-ratio", # caption = "Vertical line shows year MPA put in place",
     title = paste("MPA Size:",scales::percent(mpasize))) +
    theme_bw() +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), limits = c(0, NA))
  
  
 catch<-sim %>%
   filter(metric == "Catch")
 
catch_plot<- ggplot(catch,aes(years, value)) +
   theme_bw() +
   geom_vline(aes(xintercept = 0),
              linetype = 2,
              color = "red") +
   geom_line(aes(col=mpa_scen),size = 1.5,show.legend = TRUE) +
   scale_color_discrete("MPA scenario", labels = mpa_scen) +
   # facet_wrap( ~ metric, scales = "free_y") +
   labs(x = "Year (relative to MPA implementation)",  y = "Catch (t)", caption = "Vertical line shows year MPA put in place") +
   theme_bw() +
   scale_y_continuous(expand = expand_scale(mult = c(0, 0.1)), limits = c(0, NA))
 
 
  out<-ggarrange(biomass_plot,catch_plot,nrow=2,legend="right",common.legend = TRUE)
  return(out)
}