####Iterative coding###
####Shanelle Wikramanayake###
#### 2021-03-26####

### Load libraries ###
library(here)
library(tidyverse)
library(purrr)


###For Loops###
#(index in sequence){command to repeat}

#step 1: just the command code 
print(paste("The year is", 2000))

#step 2: add the iterative index 
years <- c(2015:2021) #create a vector of a sequence of numbers 

for (i in years){ #set up the for loop where i is the index
  print(paste("The year is", i)) #loop over i 
}

#step 3: save output create empty df
year_data <- data.frame(matrix(ncol = 2, nrow = length(year))) #create df where 1 col for year and 1 col for year_name, and rows is the length of your vector

colnames(year_data) <- c("year", "year_name") #add the column names
year_data #empty df of 2 columns 

#step 4: create for loop

for (i in 1:length(year)) {
  year_data$year_name[i] <- paste("The year is", year[i])
}

year_data

#step 5: Add to the year column 
for (i in 1:length(year)) {
  year_data$year_name[i] <- paste("The year is", year[i])
  year_data$year[i] <- years[i]
}

year_data




###Reading in multiple csvs###
testdata <- read_csv(here("week13", "data", "cond_data", "011521_CT316_1pcal.csv"))
view(testdata)

CondPath <- here("week13", "data", "cond_data") #say where the path is 

files <- dir(path = CondPath, pattern = ".csv") #file path and type of file specified
files

cond_data <- data.frame(matrix(nrow = length(files), ncol = 3)) #create empty datafram for all your imports. rows for no. of files, and cols for the 3 variables

colnames(cond_data) <- c("filename", "mean_temp", "mean_sal") #specifiy col names

cond_data

raw_data <- read_csv(paste0(CondPath,"/", files[1])) #the backslash needs to fo between path and file name and paste0 takes the space out

head(raw_data)

mean_temp <- mean(raw_data$Temperature, na.rm = TRUE) #calculate the mean temp
mean_temp


#turn it into a for loop

for (i in 1:length(files)){
  raw_data <- read_csv(paste0(CondPath,"/",files[i])) #reading in your csvs
  cond_data$filename[i] <- files[i] #filling in your file name columns
  cond_data$mean_temp[i] <- mean(raw_data$Temperature, na.rm = TRUE) #calculate mean across each raw data file and put it into temp mean columns
  cond_data$mean_sal[i] <- mean(raw_data$Salinity, na.rm = TRUE) #same as above for salinity
}
glimpse(raw_data)


###Using Purrr###

#map function: looping over a vector, doing something to each element and saving results 

###Method 1: Use a canned function###

#create vector from 1-10
1:10 %>% #pipe to map function, can assign this from her with <-  to save the output 
  map(rnorm, n = 15) %>% #it will spit out 15 randomly selected from a normal distribution TEN TIMES
  map_dbl(mean) #This calculates the mean for each set of numbers. Map_dbl because the output is a vector of type double. Mean is the function we put inside of the map_dbl function


###Method 2: Make a function###
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% 
  map_dbl(mean)


###Method 3: formula###
1:10 %>%
  map(~ rnorm(15, .x)) %>% 
  map_dbl(mean)




####Bring in files using purrr

CondPath<-here("week13", "data", "cond_data") #sepcify file path
files <- dir(path = CondPath,pattern = ".csv")
files


#alternative 
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE) #save with full file name

files


data<-files %>% #vector of files = what we are looping over
  set_names()%>% #this keeps the name of each file that we are reading in 
  map_df(read_csv,.id = "filename") %>%  #map df function to make it a giant dataframe so many rows
  group_by(filename) %>% 
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity, na.rm = TRUE))
data



