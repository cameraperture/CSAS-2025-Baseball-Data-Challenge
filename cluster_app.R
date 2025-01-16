library(shiny)
library(ggplot2)
library(dplyr)

completed_data<- read.csv("compressed_data.csv-2.gz")

ui <- fluidPage(
  titlePanel("Distributions by Cluster"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "variable",
        label = "Choose a Variable:",
        choices = names(completed_data),
        selected = "release_speed"
      )
    ),
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

server <- function(input, output) {
  output$histPlot <- renderPlot({
    req(input$variable)  
    
    completed_data |>
      ggplot(aes_string(x = input$variable)) +
      geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      facet_wrap(~ cluster) +
      theme_dark() +
      labs(
        title = paste("Histogram of", input$variable, "by Cluster"),
        x = input$variable,
        y = "Count"
      )
  })
}

shinyApp(ui = ui, server = server)