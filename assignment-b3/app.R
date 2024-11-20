library(shiny)
library(plotly)
library(DT)
library(tidyverse)

# Store lightsaber hex values and images paths for use in plotting
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
    style = "text-align: center; margin-top: 20px;",
    titlePanel("Compare Star Wars Characters by Height and Mass", "Star Wars Characters by Size")
  ),
  
  # About this app bubble
  wellPanel(
    style = "background-color: #f0f0f0; border-radius: 8px;",
    h4("About this app"),
    p("This Shiny app allows you to explore and compare Star Wars characters by height (cm) and mass (kg). Filter characters, choose the metric to compare, and explore in both graphical and tabular formats."),
    p("Use the options on the left to select characters, adjust the comparison metric, and choose your colour.")
  ),
  
  # Plot/table configuration settings
  sidebarLayout(
    sidebarPanel(
      h3("Settings"),
      
      # Feature 1: Select characters you want to view in the plot.
      # This feature allows the user to hand-pick the characters they want to compare
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
      # This feature allows the user to select the metric that they are most 
      # interested in comparing the characters by. 
      radioButtons("metricInput", 
                   "Compare by:",
                   choices = c("Height", "Mass"),
                   selected = "Height"), # Default: height
      
      # Feature 3: Lightsaber/plot color selector.
      # This feature uses a lightsaber color selector to let users customize
      # the appearance of their plot; this is more on-theme with the app 
      # and makes the user experience more fun/enjoyable than a traditional
      # color selector.
      h4("Choose a colour:"),
      uiOutput("lightsaber_grid")
    ),
    
    # Feature 4: Tab layout.
    # This feature enables the user to explore height/mass comparisons in both
    # a graphical and tabular format without having to deal with separate settings;
    # the characters and metric selected by the user will be dynamically applied
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
  # Initialize reactive value for selected lightsaber color
  selected_color <- reactiveVal("Red")  # Default colour: red
  
  # Reactive metric selection
  selected_metric <- reactive({ 
    if (input$metricInput == "Height") "height" else "mass" 
  }) 
  
  # Reactive metric label
  metric_label <- reactive({ 
    if (input$metricInput == "Height") "Height (cm)" else "Mass (kg)" 
  })
  
  # RENDER LIGHTSABER SELECTOR/plot color selector grid
  output$lightsaber_grid <- renderUI({
    tagList(
      tags$div(
        style = "display: flex; justify-content: center; gap: 10px;", 
        lapply(names(lightsaber_options), function(color) {  # Loop over the lightsaber options
          tags$div(
            style = "text-align: center;",
            actionButton(
              inputId = paste0("select_", color),  # Unique color ID for each button
              label = tags$img(
                src = pluck(lightsaber_options, color, "image"),  # Get image path for the lightsaber
                height = "60px",  # Set the image height
                style = "padding: 5px;"  # Add padding around the image
              ),
              style = "border: none; background: none; padding: 0;"  # Remove border/background/padding
            ),
            tags$div(style = "font-size: small;", color)  # Display the color name below each lightsaber image
          )
        })
      )
    )
  })
  
  # Update selected color based on user input from lightsaber grid
  observeEvent(input$select_Red, { selected_color("Red") })
  observeEvent(input$select_Blue, { selected_color("Blue") })
  observeEvent(input$select_Green, { selected_color("Green") })
  
  # Reactive expression to filter the data based on selected characters and ensure no NA values in key metrics
  filteredData <- reactive({
    # Ensure no NA values in height and mass columns
    data <- starwars %>%
      filter(!is.na(height) & !is.na(mass))
    
    # Filter the data to only include only selected characters
    if (!("All Characters" %in% input$characterInput)) {
      data <- data %>% filter(name %in% input$characterInput)
    }
    
    data  # Return the filtered data
  })
  
  # RENDER THE PLOT with the selected metric and color
  output$charactersPlot <- renderPlotly({
    filteredData() %>%
      plot_ly(
        x = ~name, 
        y = as.formula(paste0("~", selected_metric())),  # Dynamically set the y-axis based on the selected metric
        type = 'bar',
        marker = list(color = pluck(lightsaber_options, selected_color(), "hex")),  # Set the color of the bars to the lightsaber's associated hex
        text = ~paste(
          "<b>", name, "</b>", "<br>",
          "Sex:", sex, "<br>",
          "Species:", species, "<br>",
          "Homeworld:", homeworld, "<br>",
          "Height (cm):", height, "<br>",
          "Mass (kg):", mass, "<br>"
        ), # Character details to be shown in hover text
        hoverinfo = "text",  # Show text information when hovering over the bar
        hoverlabel = list(
          bgcolor = "rgba(255, 255, 255, 0.8)",  # Set background color
          bordercolor = "black" 
        ),
        textposition = "none"  # No text labels directly on the bars (only hover)
      ) %>%
      layout(
        title = paste("Comparison of", metric_label()),  # Set the plot title based on the selected metric
        xaxis = list(title = "Character", categoryorder = "total ascending"),  # Sort characters by metric (ascending)
        yaxis = list(title = metric_label())  # Set y-axis label based on the selected metric
      )
  })
  
  # RENDER THE TABLE sorted by selected metric
  output$charactersTable <- renderDT({
    metric_index <- if (selected_metric() == "height") 2 else 3  # Assuming height is 2nd column and mass is 3rd
    
    filteredData() %>%
      select(name, height, mass, sex, species, homeworld) %>%  # Select/specify order of columns
      datatable(
        options = list(
          pageLength = 10,
          order = list(list(metric_index, 'desc'))  # Sort the table by the selected metric in descending order
        )
      )
  })
}

shinyApp(ui = ui, server = server)