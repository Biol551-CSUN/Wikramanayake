####Learning lubridate Lab ###
####Shanelle Wikramanayake###
#### 2021-02-24####


####Load libraries###
library(tidyverse)
library(here)
library(lubridate)

####Load data###
cond <- read.csv(here("week5", "Data", "CondData.csv"))
depth <- read.csv(here("week5", "Data", "DepthData.csv"))

#view the date
view(cond)
view(depth)

cond <- cond %>% 
  mutate(date = ymd_hms(date)) %>% #mutate to ymd_hms format 
  mutate(date = round_date(date, "10 secs")) #round up to 10 minutes
view(cond)

depth <- depth %>% 
  mutate(date = ymd_hms(date)) %>% #mutate to ymd_hms format 
  mutate(date = round_date(date, "10 secs"))#round up to 10 minutes
view(depth)

all_data <- inner_join(cond, depth, by = "date")%>%  #join both datasets based on datetime and lose teh entries with missing data
  select("date","TempInSitu", "Serial", "AbsPressure", "Depth", "SalinityInSitu_1pCal" ) %>% #get rid of old date columns
  mutate(hours = hour(date), #Extract hours
         mins = minute(date))%>% #Extract minutes
  unite("Time", hours:mins, sep = ":") %>% #format time as Hours:minutes
  group_by(Time) %>%  #group by time
  summarize(Date_mean = mean(date), #summarise means for date, depth, temperature, salinity
            Depth_mean = mean(Depth),
            Temp_mean = mean(TempInSitu),
            Sal_mean = mean(SalinityInSitu_1pCal)) %>% 
  ggplot(mapping= aes(x = Sal_mean, #plot salinity by depth 
                      y = Depth_mean)) +
  geom_point(aes(colour = Temp_mean))+ #colour code by temp on a range
  scale_colour_viridis_c()+ #scale colour viridid continuous 
  labs(title = "Average Salinity by Average Depth", #set labels and titles
       caption = "Week 5 data from: Silbiger lab", 
       x = "Mean Salinity (ppt)", 
       y = "Mean Depth (m)", 
       colour = "Average Temperature (°C)")+
  ggsave(here("week5", "Outputs", "Sal_Depth_plot.png"), #save my plot
         height = 5, 
         width = 7)

