library(shiny)
library(plotly)
library(DT)
library(tidyverse)

lightsaber_options <- list(
  Red = list(image = "lightsaber_red.png", hex = "#FF0000"),
  Blue = list(image = "lightsaber_blue.png", hex = "#0000FF"),
  Green = list(image = "lightsaber_green.png", hex = "#00CC00")
)

ui <- fluidPage(
  # Add banner image at the top of the page
  tags$div(
    style = "text-align: center;",
    tags$img(
      src = "starwars_logo.png",    
      height = "25%",  
      width = "25%"    
    )
  ),
  
  tags$div(
    style = "text-align: center; margin-top: 20px;",  # Add margin for spacing
    titlePanel("Compare Star Wars Characters by Height and Weight")
  ),
  
  wellPanel(
    style = "background-color: #f0f0f0; border-radius: 8px;",  # Light gray background and rounded corners
    h4("About this app"),
    p("This Shiny app allows you to explore and compare Star Wars characters by height (cm) and mass (kg). Filter characters, choose the metric to compare, and explore in both graphical and tabular formats."),
    p("Use the options on the left to select characters, adjust the comparison metric, and choose your colour.")
  ),
  
  # Settings
  sidebarLayout(
    sidebarPanel(
      h3("Settings"),
      
      # Select characters you want to view in the plot
      selectInput("characterInput", 
                  "Choose character(s):",
                  choices = c("All Characters", starwars$name),
                  selected = "All Characters", # default: all characters
                  multiple = TRUE),
      
      # Select metric you want to compare
      radioButtons("metricInput", 
                   "Compare by:",
                   choices = c("Height", "Mass"),
                   selected = "Height"),
      
      # Lightsaber color selector
      h4("Choose a colour:"),
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
        style = "display: flex; justify-content: center; gap: 10px;", # Single row layout with spacing
        lapply(names(lightsaber_options), function(color) {
          tags$div(
            style = "text-align: center;",
            actionButton(
              inputId = paste0("select_", color),
              label = tags$img(
                src = lightsaber_options[[color]]$image,
                height = "60px",
                style = "padding: 5px;" # Add padding for spacing, no border
              ),
              style = "border: none; background: none; padding: 0;" # No button border or background
            ),
            tags$div(style = "font-size: small;", color) # Optional: Display color label below each image
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
        hoverlabel = list(
          bgcolor = "rgba(255, 255, 255, 0.8)",  # Set the background color of the hovertext box (white with transparency)
          font = list(color = "black"),  # Set the font color for hovertext
          bordercolor = "black"  # Set the border color of the hovertext box
        ),
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