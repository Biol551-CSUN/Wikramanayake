###Tidy Tuesday 2021-02-23: BLS####
###Created by: Shanelle Wikramanayake###
###2021-03_18##

###Packages###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(ggpubr)
library(jpeg)
library(extrafont)


###Load fonts###
font_import()
loadfonts(device = "win")


###Load Data from old TidyTuesday Challenge###

tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
spotify_songs <- tuesdata$spotify_songs

###MOst popular genres###
#What is the most popular track?###
pops <- spotify_songs %>% 
  select(track_name, track_artist, track_popularity, 
         playlist_genre, danceability, loudness, speechiness) %>% 
  filter(track_popularity == 100)
view(pops)
#Apparently it's dance monkey by Tones and I (why?)

img <- readJPEG(here("week7", "Data", "Dance_Monke.jpg")) #getting cover image of no1 song

Spot_pops <- spotify_songs %>% 
  select(track_name, track_artist, track_popularity, 
         playlist_genre, danceability, loudness, speechiness) %>% 
  ggplot(mapping = aes(x = track_name, 
                       y= track_popularity))+
  background_image(img)+ #adding the most popular cover as the plot background because its important that everyone knows
  geom_col(aes(fill= playlist_genre))+
  scale_fill_viridis_d(labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock"))+ #setting legend labels
  annotate("text", x = 7000, y = 650, 
           label = "No. 1 is Dance Monke", 
           size = 15, 
           colour ="green")+
  labs(title = "The boppiest bops", 
       x = "These r song names ^", 
       y = "Popularity")+
  theme_dark()+
  theme(text = element_text(family = "Comic Sans MS", #setting font as comic sans because why not
                            face = "bold"), 
        plot.title = element_text(colour = "light green", #title in light green
                                  size = 18), 
        plot.background = element_rect(fill = "dark blue"), #background is dark blue
        legend.background = element_rect(fill = "blue"), #Distinguish the legend in blue
        legend.key = element_rect("orange"), #axis text in orange
        legend.text = element_text(colour = "magenta", #Legend text in magenta 
                                   size = 13),
        legend.title = element_text(colour = "purple", #legend title in purple
                                    size = 15),
        axis.text = element_text(color = "purple", #axis text in purple
                                 size = 10), 
        axis.text.y = element_text(angle = -90, #Make y axis text horizontal so it's "easier" to read
                                   size = 30),
        axis.title = element_text(colour = "orange", #Axis titles in orange
                                  size = 15))+
  ggsave(here("week7", "Outputs", "Spotify_Pop_bad.png"))
        
Spot_pops    
    


