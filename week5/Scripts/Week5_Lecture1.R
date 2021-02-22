####Practice joins (Data from Becker and Silbiger (2020)) ###
####Shanelle Wikramanayake###
#### 2021-02-22####


####Load libraries###
library(tidyverse)
library(here)

####load data###
#Environmental data from each site 
EnvirData <- read.csv(here("week5", "Data", "site.characteristics.data.csv")) #in long format 

#thermal performance data
TPCData <- read.csv(here("week5", "Data", "Topt_data.csv")) #in wide format 

#view the data
glimpse(EnvirData) 
glimpse(TPCData) 

view(EnvirData)
view(TPCData)

#pivot env data to wider
EnviroDat_wide <- EnvirData %>% 
  pivot_wider(names_from = parameter.measured, 
              values_from = values) %>% 
  arrange(site.letter)  #sort data by site.letter in alphabetical order

view(EnviroDat_wide)

#left join to join the datasets
FullData <- left_join(TPCData, EnviroDat_wide) %>% #the data on the left is the base,it already knows to use site.letter to join
  relocate(where(is.numeric), .after = where(is.character)) #arrange column such that all the numeric data after character data

view(FullData)


#calculate mean and variance of all collected TPC and env data by site 
FullData_long <- FullData %>% 
  pivot_longer(E:substrate.cover, 
               values_to = "values", 
               names_to = "measurements") %>% 
  group_by(site.letter, measurements) %>% 
  summarise(means = mean(values), 
            vars = var(values))
view(FullData_long)


#dyi tibble
T1 <- tibble(site.ID = c("A", "B", "C", "D"), #make a tibble with a column for site ID's 
             Temperature = c(14.1, 16.7, 15.3, 12.8)) #add a column with temps
T1

T2 <- tibble(site.ID = c("A", "B", "D", "E"), #another tibble, no C site, new E site
             pH = c(7.3, 7.8, 8.1, 7.9)) #some values for pH
T2

left_join(T1, T2) #join T2 to T1, NA for site C in pH 
right_join(T1,T2) #join T1 to T2, NA for site C for temp

inner_join(T1,T2) #no C and no E cos missing data
full_join(T1, T2) #all data there, with NA for C and E m missing data

semi_join(T1,T2) #only temp data 
anti_join(T1,T2) #only C temp data. Order matters for semi and anti join



###NOTES###
#When collecting data and using two datasheets, you need to have an ID that is the exact same in both sheets 
#can use arrange to do ascending to descending if its numbers
#left_join(T1, T2)- joins to T1
#right_join(T1, T2) - joins to T2

#inner join only keeps teh rows that are present in both datasets
#full join keeps ALL the data but adds NA's wherever data is missing 
#semi join keeps all the rows from the first data set where there are matching values from the second, but anti join is the opposite
#semi join and anti join is useful to see if you have missing data