library(tidyr)
library(reshape2)
library(ggpubr)
# Summarize data for catch, cpue, and effort

# Load source files
source(file.path(here::here(), "scripts", "start_project.R"))

# Read in data 
dat <- read.csv(file.path(project_data_path, "vessels_bsb_per_year.csv"))  %>%
  mutate(Vessel.LOA..cm.= ifelse(Vessel.LOA..cm.=="#N/A",NA,Vessel.LOA..cm.))

# 300 rows have either 0 or NA. 0 will be changed to 100 and NA will be changed to the average length
dat<- transform(dat, Vessel.LOA..cm.= as.numeric(Vessel.LOA..cm.)) 

dat$Vessel.LOA..cm.[(dat$Vessel.LOA..cm==0)]<-100  

mean_length<-round(mean(dat$Vessel.LOA..cm.,na.rm=TRUE),0)

dat$Vessel.LOA..cm.<-replace_na(dat$Vessel.LOA..cm.,mean_length) #ifelse(is.na(dat$Vessel.LOA..cm.),mean_length,dat$Vessel.LOA..cm.) #[((dat$Vessel.LOA..cm.=NA))]<-mean_length

by_year<-dat %>%
  group_by (Year) %>%
  summarise(total_weight=sum(Weight))


by_vessel<-dat %>%
  mutate(Vessel.l.m=Vessel.LOA..cm./100) 


# assign number of days at sea based on vessel length using average days per length category from 
#'Landing per boat per year_SBR_boat_information_average days at sea_Bruno.xlxs'
#after running this the range of fish_days in the dataframe should be between 2 and 10
by_vessel<-by_vessel %>%
  mutate(fish_days= ifelse (Vessel.l.m<=9,2,
                            ifelse(Vessel.l.m>9 & Vessel.l.m<=12,2.5,##,#,
                                   ifelse(Vessel.l.m >12 & Vessel.l.m <=14,3.5,
                                       ifelse(Vessel.l.m>14 & Vessel.l.m<=24,6.5,
                                           ifelse(Vessel.l.m>24,10,-100)))))) 


by_vessel<- by_vessel %>% 
 group_by(Year, Vessel.name)%>%
  summarise(total_weight=sum(Weight),
            fish_days=sum(fish_days)) %>%
  mutate(cpue=total_weight/fish_days)

 
 final_df<-by_vessel %>%
   group_by(Year) %>%
   summarise (catch=sum(total_weight*.001),
              fish_days=sum(fish_days),
              cpue_year=median(cpue))
 
 #write.csv(final_df,paste0(project_table_path,"/jabba_input.csv")) don't overwrite because new data was entered by hand

 
 #df <- melt(final_df ,  id.vars ='Year', variable.name = 'series') 
 #levels(df$series) <- c("Catch (kg)", "Effort (Fishing Days)",  
  #                       "CPUE (Catch/Effort)")


catch_plot<- ggplot(final_df,aes(Year,catch)) +
  geom_line(size=1.2) +
  scale_y_continuous(name="Catch (kg)")+#,limits=c(0,1200))+
  scale_x_continuous("Year")+
   theme_bw() 
  #facet_wrap(~series,scales="free",nrow=3,ncol=1,strip.position = "left")
   
effort_plot<- ggplot(final_df,aes(Year,fish_days)) +
  geom_line(size=1.2) +
  scale_y_continuous(name="Effort (fishing days)")+#limits=(c(0,2500)))+
  scale_x_continuous("Year")+
  theme_bw() 

cpue_plot<- ggplot(final_df,aes(Year,cpue_year)) +
  geom_line(size=1.2) +
  scale_y_continuous(name="CPUE (catch/effort)")+#,limits=(c(0,100)))+
  scale_x_continuous("Year")+
  theme_bw() 

ce_plots<-ggarrange(catch_plot,effort_plot,cpue_plot,nrow=3)  

png(paste0(project_figure_path,"/jabba_input_data_2.png"))

print(ce_plots)
dev.off()


ggsave(catch_plot,paste0(project_figure_path,"/catch_effort_cpue_plot.png"),device="png")
       
