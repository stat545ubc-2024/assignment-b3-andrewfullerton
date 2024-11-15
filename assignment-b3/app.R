library(shiny)
library(DT)
library(tidyverse)

source("/Users/andrewfullerton/Desktop/Code/assignment-b3-andrewfullerton/data/preprocessing.R")

# Building the UI
ui <- fluidPage(
  titlePanel("Christmas Tunes: Billboard Top 100", "Top Christmas Tunes"),
  
  # All-time popularity interactive plot
  sidebarLayout(
    sidebarPanel(
      h3("Plot Settings"),
      
      # select songs and/or artists you want to view in the plot
      selectInput("songInput", 
                  "Choose Song(s):",
                  choices = c("All Songs", xmas_billboard$song),
                  selected = "All Songs", # default: all songs
                  multiple = TRUE),
      
      # select year range you want the popularity stats pulled from
      sliderInput("yearInput", 
                  "Select Year Range:", 
                  min = 1958, 
                  max = 2017, 
                  c(1958, 2017), 
                  sep = ""),
      
      # select ranking criteria
      radioButtons("rankInput", 
                   "Rank by:",
                   choices = c("Overall Popularity", "Weeks on Chart", "Peak Position"),
                   selected = "Overall Popularity")
      
      # download button (CSV)
    ),
    mainPanel(
      tabsetPanel(
        # Plot tab
        tabPanel("Interactive Plot",
                 h3("Song Popularity Plot"),
                 plotOutput("popularityPlot")),
        # Table tab
        tabPanel("Interactive Table",
                 h3("Top Songs"),
                 DTOutput("popularityTable"))
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)