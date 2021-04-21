####Working with factors ###
####Shanelle Wikramanayake###
#### 2021-04-21####

### Load libraries ###
library(here)
library(tidyverse)
library(forcats)


tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
income_mean<-tuesdata$income_mean

#default for levels is alphabetical
#Once you convert to a factor it becomes an integer


fruits<-factor(c("Apple", "Grape", "Banana")) #factor function puts them as a factor. 
fruits

test<-c("A", "1", "2") 
as.numeric(test)

test<-factor(test) 
as.numeric(test) #BEWARE: Converts the A to a 3 

#read_csv will make sure to bring everything as characters and not factors so use this

#load data 
glimpse(starwars)

#clean data
starwars %>% #imports as charcter since its forcats
  filter(!is.na(species)) %>% #remove the NAs 
  count(species, sort = TRUE) #counts the number of entries for each species 


star_counts<-starwars %>% #filter for all the species with lots of individuals
  filter(!is.na(species)) %>%
  mutate(species = fct_lump(species, n = 3)) %>% #fct_lump lumps data gives unique data that has more than 3 individuals
  count(species) #counts the individuals
star_counts #comes in alphabetical order bcos its a factors

star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n))+ #fct_reorder and n makes it reorder in ascending order 
  geom_col() #makes a barplot with species on x and counts on y 


star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ #puts n descending 
  geom_col() +
  labs(x = "Species")

#fct_reorder can be used to order by different criteria!

#reorderinf line plots

glimpse(income_mean) #from tidytuesday

total_income<-income_mean %>%
  group_by(year, income_quintile)%>% #group by year and income quantile
  summarise(income_dollars_sum = sum(income_dollars))%>% #summarize and get sum for income
  mutate(income_quintile = factor(income_quintile)) #muting the column into a factor from character

total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+ #legend is inalphabetical order 
  geom_line() #but we want it inquintile cos taht makes most sense



#fact_redorder reorders by 2 factors
total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+ #fct_reorder so that it goes by income quantile and by year  
  geom_line()+
  labs(color = "income quantile")



#reorder levels in a vector
x1 <- factor(c("Jan", "Mar", "Apr", "Dec")) #this does it in alphabetical order
x1

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), 
             levels = c("Jan", "Mar", "Apr", "Dec")) #this specifies the order you want it in
x1

#subsetiing the data with factors

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% #filter for species with more individuals than 3
  count(species, sort = TRUE) %>% #count gives you the number of each entry
  mutate(species = factor(species)) %>% #
  filter(n>3) # only keep species that have more than 3
starwars_clean

levels(starwars_clean$species) #gives you a bunch of Nas

#correct it with droplevels: 
starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% 
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>%  
  filter(n>3)  %>%  
  droplevels() #Drop levels helps drop the levels

levels(starwars_clean$species)


#recode and rename the levels

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% 
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% 
  filter(n>3)  %>%  
  droplevels() %>%  
  mutate(species = fct_recode(species, "Humanoid" = "Human")) #fct_recode using mutate so you rename the human column. 'Newname' = "oldname"
starwars_clean


