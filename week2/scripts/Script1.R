### This is my first script. i am learning how to import data
### Createrd by Shanelle A.. Wikramanayake
###Created on 2021-02-03
#############################################################


###load libraries########
library(here)



###Read in data##########
Weightdata<- read_csv(here("Week2","data", "weightdata.csv"))


###Data Analysis########

head(Weightdata) #Viewing the data 
tail(Weightdata) #iwin the last few lines
view(Weightdata)
  