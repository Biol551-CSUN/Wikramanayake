####Week 12 Lab###
####Shanelle Wikramanayake###
#### 2021-04-21####

### Load libraries ###
library(here)
library(tidyverse)
library(forcats)
library(ggmosaic)
library(PNWColors)

#import data
intertidal <- read_csv(here("week12", "Data", "intertidaldata.csv"))

view(intertidal)

pal = pnw_palette("Starfish", 3, type = "discrete")

itdata_cover2 <- intertidal %>%
    select("Site", "Quadrat", "Mussels", "Algae", "Anemone") %>% 
    mutate(Quadrat = recode(Quadrat, "Mid  1" = "Mid", "Low  ." = "Low")) %>% #correcting typos in mid and low
    mutate(Quadrat = factor(Quadrat, levels = c("Low", "Mid", "High"))) %>%  #make it a factor and order them low - high
    rename(Tide_Height = Quadrat) %>% #this makes more sense to me
    group_by(Site, Tide_Height) %>% 
    summarise(Mussels = sum(Mussels), 
              Algae = sum(Algae), 
              Anemone = sum(Anemone)) %>% 
  pivot_longer(cols = Mussels:Anemone, 
               names_to = "Organism", 
               values_to = "Percent_cover") %>% 
  filter(!is.na(Percent_cover)) %>% 
  ggplot()+
  geom_mosaic(aes(x = product(Tide_Height), fill= Organism, weight = Percent_cover), colour = "grey", size = 1)+
  scale_fill_manual(values = pal)+
  labs(x = "Tide Height")+
  theme(axis.title = element_text(size = 17), 
        axis.text = element_text(size = 15))

itdata_cover2
  
