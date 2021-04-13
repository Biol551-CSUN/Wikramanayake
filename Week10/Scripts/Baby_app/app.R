library(tidyverse)
library(kableExtra)
library(shiny)
library(lubridate)
library(here)
library(shinythemes)

BabyData2 <- read_csv(here("Week10","Scripts","Baby_app", "HatchBabyExport.csv"))

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),

  # Application title
  titlePanel(title = "How Are Baby Blakely and Micah Growing?"),
  img(src = "Cat.jpg", height = 250, width = 800, style = "float:right"),

  # Sidebar with a dropdown menu for baby
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Name",
        label = "Baby:",
        choices = c("Blakely", "Micah"),
        selected = "Micah" 
      ), 
    ),


    # Show a plot of baby weight over time
    mainPanel(
        plotOutput("BabyWeightPlot")
      ), # /mainPanel # /fluidPage
  )
)

# Server logic
server <- function(input, output) {
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
    mutate(Weight = as.numeric(Weight))
  
  r_babies <- reactive({
    BabyData2 %>%
      filter(Name == input$Name & !is.na(Weight))
  })
  
  
  output$BabyWeightPlot <- renderPlot({
    r_babies() %>% 
      ggplot(mapping = aes(x = Date, 
                           y = Weight, 
                           group = 1))+
      geom_line(size = 1.2, 
                colour = "#00CCCC")+
      theme_bw()+
      theme(axis.title = (element_text(size = 20, 
                                       face = "bold")), 
            axis.text = (element_text(size = 17)))
    
  })
}
  

# Run the application
shinyApp(ui = ui, server = server)
