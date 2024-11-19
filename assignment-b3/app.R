library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Star Wars Characters", "Star Wars Characters"),
  
  # Settings
  sidebarLayout(
    sidebarPanel(
      h3("Settings"),
      
      # Select characters you want to view in the plot
      selectInput("characterInput", 
                  "Choose Character(s):",
                  choices = c("All Characters", starwars$name),
                  selected = "All Characters", # default: all characters
                  multiple = TRUE),
      
      # Select metric you want to compare
      radioButtons("metricInput", 
                   "Compare by:",
                   choices = c("Height", "Mass"),
                   selected = "Height")
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
                 DTOutput("charactersTable"),
                 div(style = "text-align: center; margin-top: 20px;", downloadButton("downloadData", "Download CSV")))
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
        hoverinfo = "text",
        textposition = "none"
      ) %>%
      layout(
        title = paste("Comparison of", metricLabel),
        xaxis = list(title = "Character", categoryorder = "total ascending"),
        yaxis = list(title = metricLabel),
        barmode = 'group'
      )
  })
  
  # Render the table
  output$charactersTable <- renderDT({
    data <- filteredData()
    
    # Adjust table columns based on the selected metric
    metric <- if (input$metricInput == "Height") "height" else "mass"
    
    # Determine the index of the metric column in the table (1-based index)
    metric_index <- if (metric == "height") 2 else 3  # Assuming height is the 2nd column and mass is the 3rd
    
    # Create the table and apply sorting based on the selected metric
    data %>%
      select(name, height, mass, sex, species, homeworld, all_of(metric)) %>%
      datatable(
        options = list(
          pageLength = 10,
          order = list(list(metric_index, 'desc'))  # Automatically sort by metric (descending)
        )
      ) 
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("starwars_data_", Sys.Date(), ".csv", sep = "")  # Create a file name based on the date
    },
    content = function(file) {
      # Apply filtering directly in the content function
      data <- starwars %>% 
        filter(!is.na(height) & !is.na(mass)) # Ensure no NA values in key metrics
      
      # Filter by character selection
      if (!("All Characters" %in% input$characterInput)) {
        data <- data %>% filter(name %in% input$characterInput)
      }
      
      # Write the filtered data to CSV
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)