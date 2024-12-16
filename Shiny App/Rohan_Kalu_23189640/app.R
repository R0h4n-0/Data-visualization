library(shiny)
library(ggplot2)
library(plotly)


df_sample <- read.csv('df.csv')
# Define UI for the app
ui <- fluidPage(
  
  # Application title
  titlePanel("Forest Cover Type Visualization"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Select the x and y variables for analysis
      selectInput("xvar", "Select X-axis Variable:",
                  choices = c("Elevation", "Slope", "Aspect", 
                              "Horizontal_Distance_To_Hydrology", 
                              "Vertical_Distance_To_Hydrology", 
                              "Horizontal_Distance_To_Roadways", 
                              "Horizontal_Distance_To_Fire_Points",
                              "Combined_Soil_Type",
                              "Combined_Wilderness_Area",
                              "Total_Hillshade",
                              "Cover_Type")),
      selectInput("yvar", "Select Y-axis Variable:",
                  choices = c("Elevation", "Slope", "Aspect", 
                              "Horizontal_Distance_To_Hydrology", 
                              "Vertical_Distance_To_Hydrology", 
                              "Horizontal_Distance_To_Roadways", 
                              "Horizontal_Distance_To_Fire_Points",
                              "Combined_Soil_Type",
                              "Combined_Wilderness_Area",
                              "Total_Hillshade",
                              "Cover_Type")),
      
      # Input: Select the facet variable for grouping, with "None" as an option
      selectInput("facetvar", "Facet by:",
                  choices = c("None", "Cover_Type", "Combined_Wilderness_Area", "Combined_Soil_Type", "Aspect"),
                  selected = "None"),
      
      # Input: Select the color variable, with "None" option
      selectInput("colorvar", "Color by:",
                  choices = c("None", "Combined_Wilderness_Area", "Cover_Type", "Combined_Color"),
                  selected = "None"),
      
      # Input: Control the point size
      sliderInput("pointsize", "Select Point Size:", 
                  min = 1, max = 5, value = 2),
      
      # Input: Control the point transparency
      sliderInput("alpha", "Select Point Transparency (alpha):", 
                  min = 0.1, max = 1, value = 0.5)
    ),
    
    # Output: Display plot with increased size
    mainPanel(
      plotlyOutput("interactivePlot", height = "650", width = "1000px")  # Adjusting plot size
    )
  )
)

# Define server logic
shiny_server <- function(input, output) {
  
  # Load the dataset
  forest_data <- df_sample
  
  # Create a combined variable for color
  forest_data$Combined_Color <- paste(forest_data$Combined_Wilderness_Area, 
                                      forest_data$Cover_Type, sep = " - ")
  
  # Reactive plot based on selected variables
  output$interactivePlot <- renderPlotly({
    
    # Start building the base scatter plot
    p <- ggplot(forest_data, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(alpha = input$alpha, size = input$pointsize) +
      theme_minimal(base_size = 15) +  # Increase base font size for better readability
      theme(plot.title = element_text(size = 20, face = "bold"),
            legend.title = element_text(size = 8),  # Adjust legend title size
            legend.text = element_text(size = 6)) +  # Adjust legend text size
      labs(title = paste("Scatter Plot of", input$xvar, "vs", input$yvar),
           x = input$xvar, y = input$yvar)
    
    # Add color if a variable other than "None" is selected
    if (input$colorvar != "None") {
      p <- p + aes_string(color = input$colorvar) +
        labs(color = "Color Legend")  # Adjust color legend title
    }
    
    # Apply faceting if a variable other than "None" is selected
    if (input$facetvar != "None") {
      p <- p + facet_wrap(as.formula(paste("~", input$facetvar)), nrow = 2) +
        theme(strip.text = element_text(size = 14))  # Increase facet label size
    }
    
    # Convert ggplot to plotly for interactivity
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = shiny_server)
