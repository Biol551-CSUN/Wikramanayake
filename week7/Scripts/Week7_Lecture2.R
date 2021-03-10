####Map making ###
####Shanelle Wikramanayake###
#### 2021-03-08####


###Load Libraries ###
library(ggmap)
library(tidyverse)
library(here)
library(ggsn)


#Register google key :done in console

### Load Data###
ChemData<-read_csv(here("Week7","Data","chemicaldata_maunalua.csv"))
glimpse(ChemData)

Oahu <-  get_map("Oahu") #get coordinates for Oahu

ggmap(Oahu)     #Plots base layer

#Dataframe with lat an dlong 
WP<-data.frame(lon = -157.7621, lat = 21.27427) # centers on Wailupe

Map1<-get_map(WP)# Get base layer

ggmap(Map1)# plot it

Map1 <- get_map(WP, zoom = 17) #zoom to look into area of concern
ggmap(Map1) #get layer of zoomed in map


SMap1 <- get_map(WP, 
                zoom = 17, 
                maptype = "satellite") #Satellite imagery 

ggmap(Map1)


ggmap(Map1)+ #plot a map using lat and long and plot points based on salinity data
  geom_point(data = ChemData, 
             aes(x = Long, 
                 y = Late, 
                 colour = Salinity), 
             size = 2)+
  scale_colour_viridis_c()+
  scalebar(x.min = 1157.766, m.max = -157.758, 
           y.min = 21.2715, y.max = 21.2785, 
           dist = 250, dist_unit = "m", model = "WGS84", 
           box.fill = c("yellow", "white"))

geocode("the white house") #exact lat and long points of locations
