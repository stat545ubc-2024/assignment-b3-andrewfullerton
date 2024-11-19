library(shiny)
library(DT)
library(tidyverse)

# Building the UI
ui <- fluidPage(
  titlePanel("Star Wars Characters", "Star Wars Characters"),
  
  # All-time popularity interactive plot
  sidebarLayout(
    sidebarPanel(
      h3("Plot Settings"),
      
      # select characters you want to view in the plot
      selectInput("characterInput", 
                  "Choose Character(s):",
                  choices = c("All Characters", starwars$name),
                  selected = "All Characters", # default: all characters
                  multiple = TRUE),
      
      # select metric you want to compare
      radioButtons("metricInput", 
                   "Compare by:",
                   choices = c("Height", "Mass"),
                   selected = "Height")
      
      # download button (CSV)
    ),
    mainPanel(
      tabsetPanel(
        # Plot tab
        tabPanel("Interactive Plot",
                 h3("Compare Star Wars Characters"),
                 plotlyOutput("charactersPlot")),
        # Table tab
        tabPanel("Interactive Table",
                 h3("Compare Star Wars Characters"),
                 DTOutput("charactersTable"))
      )
    )
  )
)

server <- function(input, output) {
  # Filter data based on inputs
  filteredData <- reactive({
    data <- starwars %>% 
      filter(!is.na(height) & !is.na(mass)) # Ensure no NA values in key metrics
    
    # Filter by character selection
    if (!("All Characters" %in% input$characterInput)) {
      data <- data %>% filter(name %in% input$characterInput)
    }
    
    data
  })
  
  # Render the plot
  output$charactersPlot <- renderPlotly({
    metric <- if (input$metricInput == "Height") "height" else "mass"
    metricLabel <- if (input$metricInput == "Height") "Height (cm)" else "Mass (kg)"
    
    filteredData() %>%
      plot_ly(
        x = ~name, 
        y = as.formula(paste0("~", metric)), 
        type = 'bar', 
        name = metricLabel,
        marker = list(color = if (metric == "height") 'blue' else 'green'),
        text = ~paste(
          "<b>", name, "</b>", "<br>",
          "Sex:", sex, "<br>",
          "Species:", species, "<br>",
          "Homeworld:", homeworld, "<br>", 
          if (metric == "height") {
            paste("Height (cm):", height, "<br>Mass (kg):", mass)
          } else {
            paste("Mass (kg):", mass, "<br>Height (cm):", height)
          }
        ),
        hoverinfo = "text"
      ) %>%
      layout(
        title = paste("Comparison of", metricLabel),
        xaxis = list(title = "Character", categoryorder = "total ascending"),
        yaxis = list(title = metricLabel),
        barmode = 'group'
      )
  })
}

shinyApp(ui = ui, server = server)