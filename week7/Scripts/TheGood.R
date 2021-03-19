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
library(RColorBrewer)



###Load fonts###
font_import()
loadfonts(device = "win")


###Load Data from old TidyTuesday Challenge###

tuesdata <- tidytuesdayR::tt_load('2020-01-21') 
spotify_songs <- tuesdata$spotify_songs



Spot_pops_good <- spotify_songs %>% 
  select(track_name, track_artist, track_popularity, 
         playlist_genre, danceability, loudness, speechiness) %>% 
  
  ggplot(aes(x = playlist_genre, #plotting popularity of each genra
             y = track_popularity))+
  geom_violin(aes(fill = playlist_genre,
                  alpha = 0.95,
                  colour = playlist_genre))+
  scale_colour_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  geom_boxplot(aes(),
               width = 0.3,
               size = 1, 
               fill = NA,
               outlier.colour = "black")+
  labs(title = 'Popularity of tracks by genre',
       subtitle = "Distribution of popularity of songs from playlists on Spotify by genre",
       x = "Genre",
       y = "Track Popularity",
       caption = "Source: spotifyr (Tidy Tuesday/Week 4/2020)")+
  scale_x_discrete(limit = c("edm", "latin", "pop", "r&b", "rap", "rock"), 
                   labels = c("EDM", "Latin", "Pop", "R&B", "Rap", "Rock"))+
  theme_bw()+
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        plot.subtitle = element_text(size = 17),
        plot.title = element_text(size = 20, 
                                  face= "bold"),
         
        legend.position = "none")+
  ggsave(here("week7", "Outputs", "Spotify_Pop_good.png"))
  
  

Spot_pops_good

