library(shiny)
library(tidyverse)

# Set up our data
xmas_billboard <- read_csv("data/christmas_billboard_data.csv")

xmas_billboard <- xmas_billboard |>
  mutate(weekid = mdy(weekid)) # @AF TO DO: aggregate by year (i.e. by Christmas season)

# Building the app
ui <- fluidPage(
  titlePanel("Christmas Tunes: Billboard Top 100", "100 Christmas Tunes"),
  
  # Popularity by year interactive plot
  sidebarLayout(
    sidebarPanel(
      # select year range you want to view for
      sliderInput("yearInput", "Year", 1958, 2017, c(1958, 2017), sep = ""),
      
      # toggle between artist/song (requires custom CSS)
      checkboxInput("switchInput", "Show Artist", value = TRUE),
      
      # select ranking criteria
      radioButtons("rankInput", "Rank by:",
                   choices = c("Weeks on Chart", "Peak Position", "Overall Popularity"),
                   selected = "Overall Popularity")
    ),
    mainPanel("results will go here once I do more stuff")
  )
  
  # Your favourite song(s) over the years interactive plot
  
  # Download as CSV
)



server <- function(input, output) {}

shinyApp(ui = ui, server = server)