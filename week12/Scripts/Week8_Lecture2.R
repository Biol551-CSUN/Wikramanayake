####Advanced Plotting ###
####Shanelle Wikramanayake###
#### 2021-03-24####

### Load libraries ###
library(tidyverse)
library(here)
library(palmerpenguins)
library(PNWColors)



#create dataframe
df <- tibble::tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
head(df)

#Rescale values so that value/min(max-min)
df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE))) #use na.rm = true just in case

#makes a faunction for transformation
rescale01 <- function(x) { #x is argument,  within {} is argument 
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
}
df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

#make function for Farenhiet to celcius
temp_C <- (temp_F - 32) * 5 / 9 #calculation

fahrenheit_to_celsius <- function() {
} #makes it a function

temp_C <- (temp_F - 32) * 5 / 9
} #add our equation intp the bracket

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9 
} #figure out what argument is aka the known value we add

fahrenheit_to_celsius <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
} #decide what to return. we want Celcius value


#test 
fahrenheit_to_celsius(32) #this is correct! (0 C)
fahrenheit_to_celsius(212)



###celsius to K Kelvin is C +273.15
temp_k <- temp_c +273.15 #equation

C_to_K <- function(temp_c) { #define argument which is temp in celcius
  temp_k <- temp_c +273.15
  }


C_to_K <- function(temp_c) { #define output which is temp in K
  temp_k <- temp_c +273.15 
  return(temp_k)
} 

C_to_K(0) #Apparently this works 



###Make plots into a function
pal<-pnw_palette("Lake",3, type = "discrete") #set the palatte, 3 discrete colours 

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, colour = island))+ #set up the plot
  geom_point()+
  geom_smooth(method = "lm")+ #linear model
  scale_color_manual("Island", values=pal)+   #set colours
  theme_bw() 



myplot<-function(data, x, y){ #no libraries in body of function
  
  
  pal<-pnw_palette("Lake",3, type = "discrete") #plot code added
  ggplot(data, aes(x = {{x}}, y = {{y}}, colour = island))+ #generalize the arguments for data, x and y 
    geom_point()+
    geom_smooth(method = "lm")+ 
    scale_color_manual("Island", values=pal)+   
    theme_bw()
}
#no need for a return value since it will already create a plot. 
#If you name the plot then you need to add a return value

myplot(data = penguins, x = body_mass_g, y = bill_length_mm) #aha it works 

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm) #oho it works with flipper length. wonderful

#add a default
myplot<-function(data = penguins, x, y){ #specified data = penguins
  pal<-pnw_palette("Lake",3, type = "discrete")  
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ 
    scale_color_manual("Island", values=pal)+   
    theme_bw()
}

myplot(x = body_mass_g, y = flipper_length_mm)+ #np need to specify penguin data set 
  labs(x = "Body mass (g)", #add layers to your ggplot for labels
       y = "Flipper length (mm)")



###If else
a <- 4
b <- 5

if (a > b) { #question
  f <- 20 #if a > b f is 20
} else { # if not, f is 10
  f <- 10
}

f #f is 10 since a is not > b

myplot<-function(data = penguins, x, y ,lines=TRUE ){ # new argument is lines
  pal<-pnw_palette("Lake",3, type = "discrete")  
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ 
    scale_color_manual("Island", values=pal)+   
    theme_bw()
}


#Setting plotting function so that you can specify if you want lines or not if your data isn't continuous 

myplot<-function(data = penguins, x, y, lines=TRUE ){ #add new argument for lines
  pal<-pnw_palette("Lake",3, type = "discrete") 
  if(lines==TRUE){ #if lines is true then geom_smooth 
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ 
      scale_color_manual("Island", values=pal)+   
      theme_bw()
  }
  else{ #if lines is not true, then no geom smooth
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=pal)+   
      theme_bw()
  }
}

myplot(x = body_mass_g, y = flipper_length_mm) #with lines

myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE) #without lines. INcredible

###NOTES###
#three important things
#1. Name, 2. inputs/argument, 3. place code in body of function (block)
#{{}} curly curly helps us assign variables that are column names in dataframes. How nice
#If else useful if you are using conditions for a variables 