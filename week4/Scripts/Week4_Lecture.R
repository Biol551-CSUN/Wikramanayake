####Data wrangling with dplyR###
####Shanelle Wikramanayake###
#### 2021-02-15####


####Load libraries###
library(palmerpenguins)
library(tidyverse)
library(here)

glimpse(penguins)

head(penguins)


filter(.data = penguins, sex =="female") #filtering for all female penguins - if words, they are in quotes


#filtering out penguins in the year 2008 and penguins that have a body mass greater than 5000
filter(.data = penguins, year =="2008", body_mass_g > 5000)

#filter with multiple conditions
filter(.data = penguins, sex == "female", body_mass_g > 4000) #Female penguins THAT ARE ALSO greater than 4000g

###Boolean operators###
filter(.data = penguins, sex == "female" & body_mass_g > 4000) #using and for teh same thing above

#1)penguins that were collected in either 2008 or 2009, 2) not from the island dream, 3) species adelie and gentoo

filter(.data = penguins, 
       year == 2008 | 2009) #1)

filter(.data = penguins, 
       island != "Dream")#2)

filter(.data = penguins, 
       species == "Adelie" & species =="Gentoo") #3)

filter(.data = penguins, 
       species %in% c("Adelie", "Gentoo"))#alternative to 3

###using mutate to add new columns
data2 <- mutate(.data = penguins, 
               body_mass_kg = body_mass_g/1000) #cretaing column for body mass in kg using body mass in g 

view(data2)

data2 <- mutate(.data = penguins, 
               body_mass_kg = body_mass_g/1000, 
               bill_length_depth = bill_length_mm/bill_depth_mm) #adds 2 new columns, BM in kg and Bill length.depth ration

view(data2)

###ifelse function
data2 <- mutate(.data = penguins, 
                after_2008 = ifelse(year>2008, "After 2008", "Before 2008")) #groups into smaller categories using a particular variables: sorting into two categories based on 2008

view(data2)

##1) create new column to add flipper length and body mass together
#2)Use mutate and ifelse to create a new column where male and female are capitalized
 data2 <- mutate(.data = penguins, 
                 flipper_length_body_mass = flipper_length_mm + body_mass_g ) #1)
view(data2)


data2 <- mutate(.data = penguins, 
                Sex = ifelse(sex %in% c("male", "female"), "Male", "Female")) #2)
view(data2)


data2 <- mutate(.data = penguins, 
                Sex_cap = ifelse(sex == "male", "Male", "Female")) #2) alternative (first statement is always a true and the second is a fales)

view(data2)

####the pipe %>% :adds multiple levels of functions in a line


penguins %>% #start with penguins dataframe -you don't use it in subsequent lines
  filter(sex == "female") %>% #select for females
  mutate(log_mass = log(body_mass_g)) %>% #calculate log biomass
  select(Species = species, island, sex, log_mass) #using select to only concentrate on selected columns, also renaming species column


###summarize 

penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE)) #mean for flipper length. Add na.rm to remove na's.


penguins %>%
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE), #mean for flipper length. Add na.rm to remove na's.
              min_flipper = min(flipper_length_mm, na.rm = TRUE)) #create new column for min flipper length 

###group_buy

penguins %>%
  group_by(island) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm =TRUE), 
            max_bill_length = max(bill_length_mm, na.rm = TRUE)) #calculating the mean and max bill length BY ISLAND



penguins %>%
  group_by(island, sex) %>% #group by island AND sex
  summarise(mean_bill_length = mean(bill_length_mm, na.rm =TRUE), 
            max_bill_length = max(bill_length_mm, na.rm = TRUE)) #calculating the mean and max bill length BY ISLAND


penguins %>%
  group_by(island, sex, species) %>% #group by island AND sex AND species
  summarise(mean_bill_length = mean(bill_length_mm, na.rm =TRUE), 
            max_bill_length = max(bill_length_mm, na.rm = TRUE)) #calculating the mean and max bill length BY ISLAND

###removing NA's
#drop_na()
penguins %>%
  drop_na(sex) #drops na's in a particular categories: removing rows with Na's in the sex column


penguins %>%
  drop_na(sex) %>% #dropping na's in sex
  group_by(island, sex) %>% #for island and sex categories
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE)) #creating mean bill length


###pipe into ggplot

penguins %>%
  drop_na(sex) %>% #piped into plot so you don't  need the data = penguins command 
  ggplot(aes(x = sex, 
             y = flipper_length_mm)) +
  geom_boxplot()


#####NOTES#####
#Look up : mutate_if(), mutate_at(), mutate_all() 

#Look up: complete.cases