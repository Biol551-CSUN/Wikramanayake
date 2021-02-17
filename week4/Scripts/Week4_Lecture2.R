####Data wrangling with dplyR_pt2###
####Shanelle Wikramanayake###
#### 2021-02-17####

###Load Libraries###
library(tidyverse)
library(here)

###load data####
ChemData <- read.csv(here("week4", "data", "chemicaldata_maunalua.csv"))
glimpse(ChemData)

ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>% #remove all Na's from dataframe
  separate(col = Tide_time, #column of concern is selected, Time_time here
           into = c("Tide", "Time"),  #separate into Tide and Time columns
           sep = "_", #currently separated by an _ 
           remove = FALSE) %>% #to keep original column if you want it 
  unite(col = "Site_Zone", #Unite to add a new column that's a combo. The name of the NEW column, 
        c(Site, Zone), #columns to unite
        sep = ".", #separate it by a .
        remove = FALSE) #keep zone and site within df

view(ChemData_clean)

ChemData_long <- ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #the cols you want to pivot. This says select the temp to the percentage sgd col
   names_to = "Variables", # the names of the new columns with all the column names
   values_to = "Values") #names of the new column with all the values
view(ChemData_long)

ChemData_long %>% 
  group_by(Variables, Site) %>%  #group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), #calculate mean of all variables
            Param_vars = var(Values, na.rm = TRUE)) #calc variance of all variables 

#mean variance and sd by site zone and tide
ChemData_long %>% 
  group_by(Variables, Site, Zone, Tide) %>% #specifying the site, zone and tide to group by 
  summarise(Param_means = mean(Values, na.rm = TRUE), #means
            Param_vars = var(Values, na.rm = TRUE), #variances
            Param_SDs = sd(Values, na.rm = TRUE)) #standard deviations

ChemData_long %>% #you can use facet wrap easily with long data, 
  ggplot(aes(x = Site, 
             y = Values))+
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free") #scales = free allows different scales

ChemData_wide <- ChemData_long %>% #convert from long to wide data again
  pivot_wider(names_from = Variables, #cols with the names for the new cols
              Values_from = Values) #colums with values


#build an entire pipe

ChemData_clean <-   ChemData %>% 
  filter(complete.cases(.)) %>%  #remove na's
  separate(col = Tide_time, #separate tide_time col
           into = c("Tide", "Time"), #into colus tide and time separately
           sep = "_", #they are separated by a _
           remove = FALSE) %>% #don't remove tide_time
  pivot_longer(cols = Temp_in: percent_sgd, #cols you wantto pivot
               names_to = "Variables", #new cols with all the variables
               values_to = "Values") %>% #new cols with all teh values
  group_by(Variables, Site, Time) %>% #variables you want to calc summary stats
  summarise(mean_vals = mean(Values, na.rm= TRUE)) %>% #calculate means for Site, Time in variables
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% 
  write_csv(here("week4", "Outputs", "Summary.csv"))

view(ChemData_clean)
###notes###
#wide datasets: one obs per row
#long data: one unique measurement per row. Useful to summarize data 
#for table making = gt, dt 
#independent project info:May 19th and 9am, presentations are 10 mins on 10th and 12th
#Tell a story with your data
#final project shoudlbe in a standalone github repository 
#Three different outputs from your data, publication ready
#Data set by March 8th (your data, or tidy tuesday)



