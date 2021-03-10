####Map making ###
####Shanelle Wikramanayake###
#### 2021-03-08####

### Load libraries ###
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

###Import data###
#population in CA by countu
popdata<-read_csv(here("Week7","Data","CApopdata.csv"))
view(popdata)


#number of seastars at different fieldsites
stars<-read_csv(here("Week7","Data","stars.csv"))
view(stars)

world <- map_data("world") #creates a df of polygons of the world
head(world)

usa <- map_data("usa") #latlong just for USA

states <- map_data("state") #polygons for each state

counties <- map_data("county") #subregion is county

#group: used for plotting. COntrols what points are supposed to be next to each other


#making world map
ggplot()+
  geom_polygon(data = world, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = region), 
               colour = "black")+
  guides(fill = FALSE)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "light blue"))+
  coord_map(projection = "mercator", 
            xlim = c(-180, 180))


#making map of California
CA_data <- states %>% 
  filter(region == "california")
head(CA_data) %>% 
  ggplot()+
  geom_polygon(data = CA_data, 
               aes(x = long, 
                   y = lat, 
                   group = group, 
                   fill = region), 
               colour = "black", 
               fill = "black")+
  guides(fill = FALSE)+
  theme_void()+ #just the map and NOTHING else with it
  theme(panel.background = element_rect(fill = "light blue"))+
  coord_map(projection = "mercator")


#plot the population of CA by county
head(counties) #look at county geo data
head(popdata) #look at county pop data

CApop_county<-popdata %>%
  select("subregion" = County, Population)  %>% #renaming county to subregion in population
  inner_join(counties) %>% #inner join by county
  filter(region == "california") #filter to only california counties

CApop_county
ggplot()+
  geom_polygon(data = CApop_county, 
               aes(x = long, 
                   y = lat, 
                   group = group,
                   fill = Population),
               color = "black")+
  geom_point(data = stars, #adding starsfish data as a layer
             aes(x = long, 
                 y = lat, 
                 size = star_no))+ #size by starfish density
  coord_map()+
  theme_void()+
  scale_fill_gradient(trans = "log10")+ #make it log scale for easy interpretaion
  labs(size = "# stars/m2")+
  ggsave(here("week7", "Outputs", "Starfish_popmap.png"))
  
 


