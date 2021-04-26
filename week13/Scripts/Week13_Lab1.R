####Lab: Iterative coding###
####Shanelle Wikramanayake###
#### 2021-03-26####

### Load libraries ###
library(here)
library(tidyverse)
library(purrr)

###Using for loop ##################

HWPath <- here("week13", "data", "homework") #specify file path
HWPath

HWFiles <- dir(path = HWPath, pattern = ".csv")
HWFiles


HWData <- data.frame(matrix(nrow = length(HWFiles), ncol = 5)) #create empty df

colnames(HWData) <- c("filename", "mean_temp", "stdev_temp", "mean_light", "stdev_light") #specifiy col names
HWData


raw_data_hw <- read_csv(paste0(HWPath,"/", HWFiles[i])) #Testing path
(raw_data_hw) #it works

mean_temp2 <- mean(raw_data_hw$Temp.C, na.rm = TRUE) #calculate the mean temp
mean_temp2 #it works

stdev_temp2 <- sd(raw_data_hw$Temp.C, na.rm = TRUE) #calculate the standard deviation temp
stdev_temp2 #it works




#now make loop

for (i in 1:length(HWFiles)){
  raw_data_hw <- read_csv(paste0(HWPath,"/", HWFiles[i])) #read in your csvs
  HWData$filename[i] <- HWFiles[i] #add file names to column
  HWData$mean_temp[i] <- mean(raw_data_hw$Temp.C, na.rm = TRUE) #calculate mean temp
  HWData$stdev_temp[i] <- sd(raw_data_hw$Temp.C, na.rm = TRUE) #calculate standard deviation temp
  HWData$mean_light[i] <- mean(raw_data_hw$Intensity.lux, na.rm = TRUE) #calculate mean light
  HWData$stdev_light[i] <- sd(raw_data_hw$Intensity.lux, na.rm = TRUE) #caluclate standard deviation light
  
}
HWData


###Using purrrrrrr##################


HWPath <- here("week13", "data", "homework") #specify file directory
HWPath

HWFiles2 <- dir(path = HWPath, pattern = ".csv", full.names = TRUE) #specify files directory 
HWFiles2


data<-HWFiles2 %>% 
  set_names()%>% 
  map_df(read_csv,.id = "filename2") %>% 
  group_by(filename2) %>% 
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE),
            stdev_temp = sd(Temp.C, na.rm = TRUE),
            mean_light = mean(Intensity.lux, na.rm = TRUE), 
            stdev_light = sd(Intensity.lux, na.rm = TRUE))
data #I think it worked again



