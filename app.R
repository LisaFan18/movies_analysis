#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(readr)
library(stringr)

#load and clean data
smovies <- read_csv("./data/movies_shiny.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 1916, max = 2016, 
                  value = c(1956, 2016)),
      sliderInput("scoreInput", "IMDB Score", min = 0, max = 10, 
                  value = c(4.0, 9.0)),
      selectInput("genreInput", "Genre", 
                  choices = c("Drama", "Comedy", "Thriller", "Action", "Romance", "Adventure", "Crime", "Science Fiction", "Horror", "Family", "Fantasy", "Mystery", "Animation", "History", "Music", "War", "Documentary", "Western", "Foreign", "TV Movie")),
      textInput("directorInput", "Director"),
      textInput("actorInput", "Actors"),
      textInput("plotInput", "Plot Keywords")
    ),
    mainPanel(
      dataTableOutput("results")
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  reduced_df <- reactive({
    filter(
      smovies, 
      year >= input$yearInput[1] & year <= input$yearInput[2], 
      score >= input$scoreInput[1] & score <= input$scoreInput[2],
      str_detect(director, input$directorInput),
      str_detect(genres, input$genreInput),
      str_detect(actors, input$actorInput),
      str_detect(plot, input$plotInput)
    ) %>% 
      # create a link
      mutate(detail = sprintf('<a href=%s>Info</a>', imdb_link)) %>% 
      select(-genres,-imdb_link) 
  })
  output$results <- renderDataTable(
    reduced_df(), escape = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

