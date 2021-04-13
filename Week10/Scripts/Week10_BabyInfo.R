#Load Libraries


library(tidyverse)
library(lubridate)
library(here)

#Load data
 

BabyData2 <- read.csv(here("Week10","Scripts","baby_app", "HatchBabyExport.csv")) %>% 
  filter(Activity == "Weight") %>% 
  select(Baby.Name, Start.Time, Amount) %>% 
  rename(Name = Baby.Name, 
         Start_Time = Start.Time, 
         Weight = Amount) %>% 
  mutate(Start_Time = str_remove(Start_Time, "AM")) %>% 
  mutate(Start_Time = str_remove(Start_Time, "PM")) %>% 
  separate(Start_Time,into = c("Date", "Time"), " ") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(Time = hm(Time)) %>% 
  mutate(Weight = as.numeric(Weight)) %>% 
  ggplot(mapping = aes(x = Date, 
                       y = Weight))+
  geom_line(size = 1.2, 
            colour = "#00CCCC")+
  theme_bw()+
  theme(axis.title = (element_text(size = 15, 
                                   face = "bold")))

BabyData2
  






