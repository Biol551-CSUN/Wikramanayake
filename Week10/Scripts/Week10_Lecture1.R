####Troubleshooting errors###
####Shanelle Wikramanayake###
#### 2021-04-05####

### Load libraries ###
library(tidyverse)
library(ggplot2)
#library(reprex)
#library(datapasta)
#library(styler)
#library(see)


#Make a reprex to let R community help you

#SOme bugged code 
mpg %>%
  ggplot(aes(x = displ, y = hwy))%>%
  geom_point(aes(color = class))


#Datapasta helps coy the data in 

data <- tibble::tribble(
            ~lat,    ~long, ~star_no,
          33.548, -117.805,      10L,
          35.534, -121.083,       1L,
          39.503, -123.743,      25L,
          32.863,  -117.24,      22L,
           33.46, -117.671,       8L,
          33.548, -117.805,       3L
          )

#need to run basis
#Load only the necessary libraries 
#make sure including the output doesn't mess things up

library(tidyverse)
data <- tibble::tribble(
  ~lat,    ~long, ~star_no,
  33.548, -117.805,      10L,
  35.534, -121.083,       1L,
  39.503, -123.743,      25L,
  32.863,  -117.24,      22L,
  33.46, -117.671,       8L,
  33.548, -117.805,       3L
)

data %>% 
  ggplot(aes(x = lat, y = long, ))+
  geom_point(size = 3)+
  geom_text(aes(label= rownames(star_no), size = 10))+
  theme_dark()+
  labs(x = "Latitude", 
       y = "Longitude")

