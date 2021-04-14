###Wheatmap 



#load libraries
library(wheatmap)
library(tidyverse)
library(here)

#load data
mtcard_matrix <- as.matrix(mtcars)
head(mtcard_matrix)

#scale data 
mtcars_scaled <- scale(mtcard_matrix)
head(mtcars_scaled)


#cluster data 
heatmap <- both.cluster(mtcars_scaled)
heatmap$mat[,1:4] #select columns

BaseHeatmap <- WHeatmap(heatmap$mat, name = 'hl', 
                        yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize = 11, 
                        xticklabels = TRUE, xticklabel.side = 'b', xticklabel.fontsize = 20, 
                        cmp = CMPar(brewer.name = 'BuPu'))
BaseHeatmap()

#Dendrograms
Dendrogram <- BaseHeatmap + 
  WDendrogram(heatmap$row.clust, 
              LeftOf('h1'), 
              facing='right') 
Dendrogram


#Legend
Legend <- Dendrogram + 
  WLegendV('h1', BottomRightOf('h1', h.pad=.4), 'l1') #add legend
Legend


# Highlight a rectangle 
Highlight <- Legend +
  WRect('h1',c(2,5),c(2,3),col='yellow') #highlight cells
Highlight


#Exercise 1
Exercise1 <- WHeatmap(heatmap$mat, name = 'hl', 
                        yticklabels = TRUE, yticklabel.side = 'b', yticklabel.fontsize = 11, 
                        xticklabels = TRUE, xticklabel.side = 'a', xticklabel.fontsize = 20, #xaxis labels to top
                        cmp = CMPar(brewer.name = 'Spectral'))#olour palatte
Exercise1()

#Exercise 2
Exercise2 <- BaseHeatmap + 
  WDendrogram(heatmap$row.clust, 
              LeftOf('h1'), 
              facing='bottom') 
Exercise2

#Exercise 3

Exercise3 <- Dendrogram + 
  WLegendV('h1', BottomLefttOf('h1', h.pad=.4), 'l1') #add legend
Legend



###########################################################################################################################
#Rinat
install.packages("rinat")
library(rinat)



uid_me<- get_inat_taxon_stats(uid= "shanelle97")
view(uid_me)

  
MyData <- get_inat_obs_user(username = "shanelle97")
view(MyData)

MyData2 <- (get_inat_user_stats(uid = "shanelle97"))
view(MyData2$most_observations)


inat_map(MyData, "world")


############################################################################################################################

#Janitor
install.packages("janitor")
library(janitor)

#Make your own datafram 
a <- data.frame(v1 = c(7, 6, 4, 5),
                v2 = c(NA, NA, NA, NA),
                v3 = c("a", "b", "c", "d"), 
                v4 = c(6, 5, 8, 10))



a_clean<-a %>% 
  remove_empty(c("rows", "cols")) #checks and removes empty cols
a_clean



CoralGrowth <- readr::read_csv('https://raw.githubusercontent.com/Biol551-CSUN/Janitor_Package/main/project/Data/CoralGrowth.csv')

coralgrowth_clean<-CoralGrowth %>% 
  remove_empty(c("rows", "cols"))# remove NA's

#get dupes
coralgrowth_clean %>%
  get_dupes("change_mg_cm2")%>%  #
  arrange(dupe_count)
########################################################################################################################
#plotly 
library(plotly)


tuesdata <- tidytuesdayR::tt_load('2020-02-18')
food_data <- (tuesdata$food_consumption)
food_data


fig <- plot_ly(data = food_data, 
               x = ~co2_emmission, 
               y = ~consumption)
fig


fig <- plot_ly(data = food_data, 
               x = ~co2_emmission, 
               y = ~consumption,
               colour = ~food_category)
fig


fig <- plot_ly(data = food_data, 
               x = ~co2_emmission, 
               y = ~consumption,
               colour = ~food_category,
               colours = "viridis")
fig



fig <- plot_ly(data = food_data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               marker = list(size = 10), #change the marker size
               text = ~paste("CO2 emission:", co2_emmission, #change the hover data labels
                             "<br>Consumption:", consumption, #<br> moves the label to a new line
                             "<br>Food category:", food_category)) %>% #must PIPE to the layout 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', #add a title
         xaxis = list(title = "CO2 Emission"), #change the axes titles
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) #add a legend title
fig

#stacked bar

bar_stacked <- food_data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China")




bar_stacked <- food_data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country, 
          y = ~percent_consumption, 
          color = ~food_category, 
          type = "bar") #bar chart
bar_stacked


#dropdown menu

dropdown <- food_data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") %>%
  add_bars(y = ~co2_emmission, name = "CO2 Emissions", visible = FALSE) %>%
  layout(updatemenus = list(list(y = 0.6, #set horizontal and vertical position of menu
                                 x = -0.2, 
                                 buttons = list(list(method = "restyle", #use "buttons" to add the 2 different menu options
                                                     args = list("visible", list(TRUE, FALSE)), #show plot 1 and hide plot 2
                                                     label = "Consumption"),
                                                list(method = "restyle",
                                                     args = list("visible", list(FALSE, TRUE)), #hide plot 1 show plot 2
                                                     label = "CO2 Emissions")))))
dropdown

#Mapping data
map_codes <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
glimpse(map.codes)


food_data <- map.codes %>% 
  rename(country = COUNTRY) %>% # rename column heading for easier join
  right_join(food_data) %>%  # join to food.data dataframe by 'country'
  select(-GDP..BILLIONS.)
food_data


food_data %>% 
  mutate(na_col = is.na(food_data$CODE)) %>% 
  filter(na_col == TRUE) %>% 
  distinct(country)
food_data


US <- food_data %>% 
  filter(country == "USA") %>% 
  mutate(CODE = 'USA',
         country = 'United States')

food_data <- food_data %>% 
  full_join(US) %>% 
  drop_na()


co2_data <- food_data %>% 
  group_by(country, CODE) %>% 
  summarise(mean_co2 = mean(co2_emmission)) %>% 
  ungroup()
full_data <- food_data %>% 
  select(-consumption) %>% 
  left_join(co2_data)

wide_data <- full_data %>%
  pivot_wider(names_from = food_category, values_from = co2_emmission) %>%
  drop_na()


plot_geo(wide_data) 

plot_geo(wide_data) %>%
  add_trace(locations = ~CODE) 

plot_geo(wide_data) %>% # 
  add_trace(locations = ~CODE, 
            color = ~mean_co2) 
