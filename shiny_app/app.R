library(shiny)
library(ggplot2)
library(dplyr)
library(rsconnect)

# Load dataset
completed_data <- read.csv("compressed_data.csv-2.gz")

# Define user-friendly variable names
variable_labels <- c(
  pitch_type = "Pitch Type",
  release_speed = "Release Speed",
  release_pos_x = "Release X Position",
  release_pos_z = "Release Z Position",
  events = "Event",
  zone = "Zone",
  bat_speed = "Bat Speed",
  strikes = "Strike Count",
  pfx_x = "Horizontal Movement",
  pfx_z = "Vertical Movement",
  plate_x = "Plate Horizontal",
  plate_z = "Plate Vertical",
  outs_when_up = "Out Count",
  inning = "Inning",
  vz0 = "Velocity Z",
  hit_distance_sc = "Hit Distance",
  launch_speed = "Launch Speed",
  launch_angle = "Launch Angle",
  release_spin_rate = "Release Spin Rate",
  release_extension = "Release Extension",
  release_pos_y = "Pitch Release Position",
  spin_axis = "Spin Axis",
  delta_run_exp = "Change in Run Expectancy",
  swing_length = "Swing Length",
  hyper_speed = "Hyper Speed",
  age_pit = "Pitcher Age",
  age_bat = "Batter Age",
  pitcher_days_since_prev_game = "Pitcher Days Since Last Game",
  batter_days_since_prev_game = "Batter Days Since Last Game",
  api_break_x_arm = "API Break X Arm",
  api_break_x_batter_in = "API Break X Batter In",
  arm_angle = "Arm Angle",
  description_grouped = "Description",
  swing_combined = "Swing Combined",
  swing_efficiency = "Swing Efficiency",
  pitch_combined = "Pitch Combined",
  batter_pitcher_opposite = "Handedness Matchup",
  bat_team_lead = "Batting Team Lead",
  sz_height = "Strike Zone Size",
  event_value = "Value of Batting Outcome",
  batter_score = "Batter Offensive Contribution",
  pitch_number = "Pitch Number",
  cluster = "Cluster"
)

# Identify numeric and categorical variables
numeric_vars <- names(completed_data)[sapply(completed_data, is.numeric)]
categorical_vars <- names(completed_data)[sapply(completed_data, is.character) | sapply(completed_data, is.factor)]

# Convert categorical variables to factors
completed_data[categorical_vars] <- lapply(completed_data[categorical_vars], function(x) {
  x <- as.factor(x)
  levels(x) <- make.names(levels(x))  # Ensures safe factor levels
  return(x)
})

# Define UI
ui <- fluidPage(
  titlePanel("Distributions by Cluster"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Choose a Variable:",
        choices = setNames(names(variable_labels), variable_labels),  # Use friendly names
        selected = "release_speed"
      )
    ),
    mainPanel(
      uiOutput("plot_ui")  # Dynamically update UI
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Debugging: Print selected variable information
  observe({
    print(paste("Selected variable:", input$variable))
    print(str(completed_data[[input$variable]]))  # Debug selected variable
    print(table(completed_data[[input$variable]], useNA = "ifany"))  # Debug counts
  })
  
  # Dynamically update UI based on variable type
  output$plot_ui <- renderUI({
    req(input$variable)  # Ensure input is available
    
    if (input$variable %in% names(completed_data)) {  # Ensure variable is valid
      if (input$variable %in% numeric_vars) {
        plotOutput("histPlot")
      } else if (input$variable %in% categorical_vars) {
        plotOutput("barPlot")
      } else {
        h4("Selected variable is neither numeric nor categorical.")
      }
    } else {
      h4("Invalid selection. Please choose another variable.")
    }
  })
  
  # Histogram for Numeric Variables
  output$histPlot <- renderPlot({
    req(input$variable)  # Ensure a variable is selected
    
    ggplot(completed_data, aes(x = .data[[input$variable]], fill = factor(cluster))) +
      geom_histogram(bins = 30, color = "black", alpha = 0.7, na.rm = TRUE) +  # Prevent NA errors
      facet_wrap(~ cluster) +
      theme_dark() +
      theme(legend.position = "none") +
      labs(
        title = paste(variable_labels[[input$variable]] %||% input$variable, "by Cluster"),
        x = variable_labels[[input$variable]] %||% input$variable,
        y = "Count"
      )
  })
  
  # Bar Plot for Categorical Variables
  output$barPlot <- renderPlot({
    req(input$variable)  # Ensure a variable is selected
    
    # Ensure variable is a factor before plotting
    completed_data[[input$variable]] <- as.factor(completed_data[[input$variable]])
    
    ggplot(completed_data, aes(x = .data[[input$variable]], fill = factor(cluster))) +
      geom_bar(position = "dodge", color = "black", na.rm = TRUE) +  # Prevent NA errors
      theme_dark() +
      labs(
        title = paste(variable_labels[[input$variable]] %||% input$variable, "by Cluster"),
        x = variable_labels[[input$variable]] %||% input$variable,
        y = "Count",
        fill = "Cluster"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)