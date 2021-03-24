####Advanced Plotting ###
####Shanelle Wikramanayake###
#### 2021-03-08####

### Load libraries ###
library(tidyverse)
library(here)
library(patchwork)
library(gganimate)
library(ggrepel)
library(magick)
library(palmerpenguins)


###patchwork
p1<-penguins %>% #create plot 1 geom point
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species))+
  geom_point()
p1

p2<-penguins %>% #create plot 2 geom jitter
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species))+
  geom_jitter(width = 0.2)
p2

p1+p2+ #brings plots together
  plot_layout(guides = "collect")+ #combines legends to one and adds to the right
  plot_annotation(tag_levels = "A") #adds subplot levels and specifies what kind of levels
  
p1/p2+ #brings plots together stacked
  plot_layout(guides = "collect")+ 
  plot_annotation(tag_levels = "A")





###ggrepel#
view(mtcars) #view mtcars dataset

ggplot(mtcars, aes(x = wt, #weight
                   y = mpg, #miles per gallon
                   label = rownames(mtcars))) + #takes all the rownames out 
  geom_label_repel() + #adds text labels WITH A BACKGROUND geom_text adds only text labels
  geom_point(color = 'red') 


###gganimate#
#useful for timeseries

penguins %>% #geom_point bowdy mass vs bill dapth, colour by species
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()+
  transition_states(
    year, # Animating BY year- column to animate by 
    transition_length = 2, #How long is it trasnitioning from one to the other
    state_length = 1 #Length of pause between transitions
  )+
  ease_aes("bounce-in-out")+ #cahnge easthetic of transition
  ggtitle('year: {closest_state}')+ #have the title change by year too, teh curly bracket means it's part of the text and function
  anim_save(here("Week8","Outputs","mypengiungif.gif")) #use anim_save to save as a gif instead of ggsave 


###Magick#

penguin<-image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png") #link to penguin pic online
penguin


penguins %>%
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  ggsave(here("week8","Outputs","penguinplot.png"))

penplot<-image_read(here("Week8","Outputs","penguinplot.png")) #plot at the back
out <- image_composite(penplot, penguin, offset = "+70+30") #pen in the front and offset sets teh position and size
out

#do it with a gif
pengif<-image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center")
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
animation
