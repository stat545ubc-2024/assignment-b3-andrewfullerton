library(shiny)
library(plotly)
library(DT)
library(tidyverse)

# Store lightsaber hex values for use in plotting
lightsaber_options <- list(
  Red = list(image = "lightsaber_red.png", hex = "#FF0000"),
  Blue = list(image = "lightsaber_blue.png", hex = "#0000FF"),
  Green = list(image = "lightsaber_green.png", hex = "#00CC00")
)

ui <- fluidPage(
  # Banner image at the top of the page
  tags$div(
    style = "text-align: center;",
    tags$img(
      src = "starwars_logo.png",    
      height = "18%",  
      width = "18%"    
    )
  ),
  
  # Main title
  tags$div(
    style = "text-align: center; margin-top: 20px;",  # Add margin for spacing
    titlePanel("Compare Star Wars Characters by Height and Mass", "Star Wars Characters by Size")
  ),
  
  # About this app bubble
  wellPanel(
    style = "background-color: #f0f0f0; border-radius: 8px;",  # Light gray background and rounded corners
    h4("About this app"),
    p("This Shiny app allows you to explore and compare Star Wars characters by height (cm) and mass (kg). Filter characters, choose the metric to compare, and explore in both graphical and tabular formats."),
    p("Use the options on the left to select characters, adjust the comparison metric, and choose your colour.")
  ),
  
  # Plot/table configuration settings
  sidebarLayout(
    sidebarPanel(
      h3("Settings"),
      
      # Feature 1: Select characters you want to view in the plot.
      # This feature enables the user to hand-pick the characters they want to compare
      # in the plot/table; this feature is flexible in order to allow users to
      # investigate a variety of different comparisons without imposing any unnecessary constraints.
      # For ease of use, users can simply type in a characters name and select them from the
      # drop down list. 
      selectInput("characterInput", 
                  "Choose character(s):",
                  choices = c("All Characters", starwars$name),
                  selected = "All Characters", # Default: all characters
                  multiple = TRUE),
      
      # Feature 2: Select metric you want to compare by.
      # The starwars dataset includes height and mass measures; this feature allows
      # the user to select the metric that they are most interested in. 
      radioButtons("metricInput", 
                   "Compare by:",
                   choices = c("Height", "Mass"),
                   selected = "Height"), # Default: height
      
      # Feature 3: Lightsaber/plot colour selector.
      # Rather than providing a simple colour palette for users to select from,
      # this feature uses lightsaber images as a colour selector as means of staying
      # on-theme with the app and making the user experience more delightful. 
      h4("Choose a colour:"),
      uiOutput("lightsaber_grid")
    ),
    
    # Feature 4: Tab layout.
    # This feature enables the user to explore height/mass comparisons in both
    # a graphical and tabular format without having to deal with separate settings.
    # The characters and metric selected by the user will be dynamically applied
    # to both the plot and the table. 
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
  ),
  # Footer/in-app data source statement
  tags$footer(
    style = "bottom: 0; 
             width: 100%; 
             background-color: #f8f9fa; 
             text-align: center; 
             padding: 10px; 
             border-top: 1px solid #ddd;
             margin-top: 20px;",
    HTML("The data used in this app is sourced from the R package 'dplyr'. It was originally sourced from the Star Wars API (SWAPI) and revised to include gender and sex determinations.")
  )
)

server <- function(input, output) {
  # Reactive value for selected lightsaber color
  selected_color <- reactiveVal("Red") # Default plot colour: red
  
  # Lightsaber/plot colour selector grid UI
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
            tags$div(style = "font-size: small;", color) # Display color label below each image
          )
        })
      )
    )
  })
  
  # Update selected lightsaber colour based on user input
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
    
    # Get the hex color for the selected lightsaber colour
    color_hex <- lightsaber_options[[selected_color()]]$hex
    
    # Plot specifications
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
          "Height (cm):", height, "<br>",
          "Mass (kg):", mass, "<br>"
          ),
        hoverinfo = "text",
        hoverlabel = list(
          bgcolor = "rgba(255, 255, 255, 0.8)",  # Set the background color of the hovertext box (white with transparency)
          font = list(color = "black"),  # Set the font color for hovertext
          bordercolor = "black"  # Set the border color of the hovertext box
        ),
        textposition = "none" # Remove labels from bars in plot
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