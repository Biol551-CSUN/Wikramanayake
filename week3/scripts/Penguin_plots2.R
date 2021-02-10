
###Plotting penguin data###
###2021-02-10###
###Created by: Shanelle Wikramanayake ###



###Load Libraries###
library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(ghibli)


###Load data###

glimpse(penguins)



plot1 <- ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, 
                     colour = species), 
       )+

geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Bill depth and length",
       x = "Bill depth (mm)", 
       y = "Bill length (mm)" ) +
  
  scale_colour_ghibli_d("MononokeMedium", direction = -1) +
  theme_bw()+
  theme(axis.title= element_text(size = 13, 
                                 colour = "orange"), #axis text colour 
        panel.background = element_rect(fill = "ivory"))

ggsave(here("week3","Outputs","penguins.png"), 
       width = 7, height = 5) # in inches



###Alternatives###
#coord_polar(c("x")) #make them polar, but that's kind of useless in this context
#coord_flip()
#coord_fixed() to fix the axes 
#scale_x_continuous(breaks = c(14, 17, 21), 
                   #labels = c("Low", "Medium", "High")) #set x to continuous scale from 0-20

