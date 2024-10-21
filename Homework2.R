library(tidyverse)
library(shiny)

# Loads data relevant for app
park_species <- read.csv("https://raw.githubusercontent.com/seengel11/homework2/refs/heads/main/most_visited_nps_species_data.csv") %>% 
  select('ParkName', 'CategoryName', 'SciName', 'CommonNames', 'Nativeness')

## makes variable lists for relevant Input options
parks <- pull(park_species, ParkName) %>% 
  unique() %>% 
  na.omit()

categories <- pull(park_species, CategoryName) %>% 
  unique() %>% 
  na.omit()

nativeness <- pull(park_species, Nativeness) %>% 
  unique() %>% 
  na.omit()

# creates bar graph to be used in app server
create_bar_plot <-function(df) {
  ggplot(df) +
    geom_bar(aes(x = CategoryName, fill = ParkName),
             position = "dodge") +
    labs(x = "Species Category",
         y = "Species Count",
         fill = "Park") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
}

## creates user interface
ui <- fluidPage(
  titlePanel("Species Distribution by Park"),
  selectInput("Park_select", "Select National Parks:", 
              choices = parks, multiple = TRUE),
  selectInput("category_select", "Select Species Category:", 
              choices = categories, selected = categories, multiple = TRUE),
  checkboxGroupInput("nativeness_select", "Select Species Nativeness:", 
                     choices = nativeness, 
                     selected = nativeness),
  plotOutput("bar_plot"),
  dataTableOutput("dt")
)

## creates app server
server <- function(input, output) {
## filters data frame based on user inputs.
  park_data <- reactive({
    park_species %>% 
   filter(ParkName %in% input$Park_select &
          CategoryName %in% input$category_select &
          Nativeness %in% input$nativeness_select)
  })
  
  ##creates bar plot
  output$bar_plot <- renderPlot({
    create_bar_plot(park_data())
  })
  
  ## creates data table
  output$dt <- renderDataTable({
    park_data()  
  })
}
##calls app
shinyApp(ui, server)