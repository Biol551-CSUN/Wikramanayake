####Learning lubridate ###
####Shanelle Wikramanayake###
#### 2021-02-24####


####Load libraries###
library(tidyverse)
library(here)
library(lubridate)

####Load data###
cond <- read.csv(here("week5", "Data", "CondData.csv"))
depth <- read.csv(here("week5", "Data", "DepthData.csv"))


####
#What time is it now? can use it to date and time your script
now()

now(tzone = "EST") #time in the east coast
now(tzone = "GMT")

today() #just gives date

am(now()) #is it morning rn?

leap_year(now()) #is it a leap year?

#dates must be characters, so use quotes 
#these all produce the same results as ISO dates 
ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")

#specify time and date both
ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")


#extract elemets from date or time elements
datetimes<-c("02/24/2021 22:22:20",
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")
datetimes <- mdy_hms(datetimes) #change these to mdyhms format 

month(datetimes, label = TRUE, abbr = FALSE) #extract the months, label makes it abreviate the month name, abbr=FALSE makes the entire thing spell out 

day(datetimes) #extract the day date 
wday(datetimes, label = TRUE) #extract day of the week
hour(datetimes) #hours
minute(datetimes) #mins
second(datetimes) #secs

datetimes + hours(4) #this adds four hours (hourS adds the hours to it, hour(no s) just extracts hours)

round_date(datetimes, "minute") #round date to the nearest minute
round_date(datetimes, "5 mins") #round to nearest 5 mins


#convert date column to a datetime
view(cond)
cond <- cond%>% 
  mutate(DateTime = ymd_hms(date)) 
view(cond)















####Notes###
#mutate(ymd()) change the column date format
#mutate(as.character()) to change column to character