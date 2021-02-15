###Week 4 Homework###
###Shanelle A. Wikramanayake###
###2021-02-15#####

###load packages and data###
library(tidyverse)
library(here)
library(palmerpenguins)
library(PNWColors)

view(penguins)

#1. mean and variance of body mass by species, island and sex

penguins %>%
  drop_na(species, island, sex) %>% #drop na's from categories
  group_by(species, island, sex) %>% #group by species, island, sex
  summarise(mean_body_mass = mean(body_mass_g, na.rm = TRUE), var_body_mass = var(body_mass_g, na.rm = TRUE)) #columns for mean and variance of body mass for each species, sex and island

#2. filter out (exclude) male penguins, then calculate log body mass, then select only columns for species, island, sex and log bodymass, then use these data to make any plot
penguins %>%
  filter(sex == "female") %>% #select for females and drop males
  mutate(log_body_mass = log(body_mass_g)) %>% #calculate log body mass and add new column
  select(species, island, sex, log_body_mass) %>% #select species, island, sex, log body mass
  ggplot(aes(x = species, 
             y = log_body_mass))+ #set aesthetics+
  geom_boxplot(aes(colour= island), #adding a boxplot with colours separated by island
               fill = "grey", #adding grey fill to bring out colours within boxplot
               alpha = 0.3, #lightening grey colour 
               outlier.shape = NA)+ #getting rid of outlier points 
  geom_jitter(aes(colour= island), 
              size = 3, 
              alpha = 0.7)+ #adding jitter points separated by island same colours as boxplot
  scale_color_manual(values = pnw_palette("Moth"))+
  coord_flip()+ #switching x and y axes
  labs(title = "Log Body Mass of Three Species of Penguins on Three different Islands", 
       subtitle = "Species: Adelie, Chinstrap, gentoo; Islands: Biscoe, Dream, Targersen",
       caption = "Source: Palmer Station LTER/palmerpenguins package",
       x = "Species",
       y = "Log of Body Mass (g)",
       colour = "Island")+ #Adding relevant text
  theme_classic()+ #adding bw theme to show important gridlines
  theme(axis.title= element_text(size = 15), 
        axis.text = element_text(size = 12))
  


