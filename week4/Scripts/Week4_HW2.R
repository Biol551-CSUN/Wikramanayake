###Week 4 Homework2###
###Shanelle A. Wikramanayake###
###2021-02-17#####

###load packages and data###
library(tidyverse)
library(here)
library(PNWColors)


###load data####
ChemData <- read.csv(here("week4", "data", "chemicaldata_maunalua.csv"))
glimpse(ChemData)

#Summary stats using pivot wider
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>% #remove all Na's from dataframe
  separate(col = Tide_time, #column of concern is selected, Time_time here
           into = c("Tide", "Time"),  #separate into Tide and Time columns
           sep = "_", #currently separated by an _ 
           remove = FALSE) %>% #to keep original column if you want it
  select(Site, Tide, Time, Salinity, Phosphate, Silicate, NN, pH, percent_sgd) %>% 
  pivot_longer(cols = Salinity: percent_sgd, 
               names_to = "Variables", 
               values_to = "Values")%>%
  group_by(Variables) %>%
  summarise(mean_vals = mean(Values, na.rm = TRUE), #means
            var_vals = var(Values, na.rm = TRUE), #variances
            sd_vals = sd(Values, na.rm = TRUE)) %>% #standard deviations
  pivot_wider(names_from = Variables,
              values_from = mean_vals, var_vals, sd_vals)
view(ChemData_clean) %>% 
  write_csv(here("week4", "Outputs", "Summary_lab.csv"))
 
 #graph using long data
(ChemData_Long <-ChemData %>%
  filter(complete.cases(.)) %>% #remove all Na's from dataframe
  separate(col = Tide_time, #column of concern is selected, Time_time here
           into = c("Tide", "Time"),  #separate into Tide and Time columns
           sep = "_", #currently separated by an _ 
           remove = FALSE) %>% #to keep original column if you want it
  select(Site, Season,Salinity, Phosphate, Silicate, NN, pH, percent_sgd)%>% #relevant variables selected
    filter(Season == "SPRING") %>% #filtering for data only collected in Spring
  pivot_longer(cols = Salinity: pH,#considering columns lainity - pH
               names_to = "Variables", #call them variables
               values_to = "Values") %>% #call them values
    ggplot(aes(x = percent_sgd,#%submarine groundwater discharge
               y = Values, #chemical attributes on y axis
               color = Site))+ #colour by different sites
    scale_colour_viridis_d()+ #set colour scheme
    scale_x_continuous(limits = c(0,30))+ #submarine groundwater extends only upto about 30%
    geom_point()+ #plotting points
    facet_wrap(~Variables, scales = "free")+ #facetwrap with a graph for each variable vs submarine groundwater %
    labs(title = "Change in chemical variables as relative submarine groundwater discharge in the water increases", #title
        subtitle = "Chemical variables: Nitrates and Nitriles,pH, phosphate, salinity, and silicate", #subtitle
        caption = "Silbiger Lab", #caption for data source
        x = "Relative Submarine Groundwater discharge (%)")+
    theme_bw()+ #adding bw theme to show important gridlines
    theme(axis.title= element_text(size = 15), #increasing title text size
         axis.text = element_text(size = 12))) + #increasing axis text size
   ggsave(here("week4", "Outputs", "Chem_W4.png")) #saving to your output folder
 



