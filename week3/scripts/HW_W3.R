
###Plotting penguin data###
###2021-02-10###
###Created by: Shanelle Wikramanayake ###



###Load Libraries###
library(palmerpenguins)
library(tidyverse)
library(here)
library(ghibli)




###Load data###

glimpse(penguins)

penguin <- drop_na(penguins) #dropping na's

view(penguins)


 
###
ggplot(data = penguin,
       aes(x = island,
           y = body_mass_g)
       )+
  geom_boxplot(fill = "grey90",
               outlier.shape = NA) +
  geom_jitter(data = penguin,
              aes(x = island,
                  y = body_mass_g,
                  colour = sex), 
              alpha = 0.5, 
              position = position_jitterdodge(dodge = 0.5, jitter.width = 0.9)
              ) +
  labs(title = "Penguin body mass by sex on three different islands", 
       subtitle = "Islands: Biscoe, Dream, Targersen", 
       caption = "Source: Palmer Station LTER/palmerpenguins package",
       x = "Islands", 
       y = "Body mass (g)", 
       colour = "sex") +
  scale_colour_ghibli_d("MarnieMedium1",
                        labels = c("Female", "Male")
                        ) +
  theme_classic() +
  theme(axis.title= element_text(size = 11))


ggsave(here("week3","Outputs","penguinHW1.png"), 
       width = 7, height = 5)