library(shiny)
library(plotly)
library(DT)
library(tidyverse)

lightsaber_options <- list(
  Red = list(image = "lightsaber_red.png", hex = "#FF0000"),
  Blue = list(image = "lightsaber_blue.png", hex = "#0000FF"),
  Green = list(image = "lightsaber_green.png", hex = "#00FF00")
)

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
                   selected = "Height"),
      
      # Lightsaber color selector
      h4("Choose a Lightsaber Color:"),
      uiOutput("lightsaber_grid")
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
  # Reactive value for selected lightsaber color
  selected_color <- reactiveVal("Red")
  
  # Lightsaber grid UI
  output$lightsaber_grid <- renderUI({
    tagList(
      tags$div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(100px, 1fr)); gap: 10px;",
        lapply(names(lightsaber_options), function(color) {
          tags$div(
            style = "text-align: center;",
            actionButton(
              inputId = paste0("select_", color),
              label = tags$img(
                src = lightsaber_options[[color]]$image,
                height = "60px",
                style = if (color == selected_color()) "border: 3px solid black;" else "border: 1px solid gray;"
              ),
              style = "border: none; background: none; padding: 0;"
            ),
            tags$div(color)
          )
        })
      )
    )
  })
  
  # Update selected lightsaber color
  observeEvent(input$select_Red, { selected_color("Red") })
  observeEvent(input$select_Blue, { selected_color("Blue") })
  observeEvent(input$select_Green, { selected_color("Green") })
  
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
    
    # Get the hex color for the selected lightsaber color
    color_hex <- lightsaber_options[[selected_color()]]$hex
    
    filteredData() %>%
      plot_ly(
        x = ~name, 
        y = as.formula(paste0("~", metric)), 
        type = 'bar', 
        name = metricLabel,
        marker = list(color = color_hex),
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
}

shinyApp(ui = ui, server = server)