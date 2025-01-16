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
      geom_histogram(aes(fill = cluster), bins = 30, color = "black", alpha = 0.7) +
      facet_wrap(~ cluster) +
      theme_dark() +
      theme(legend.position = "none") +
      labs(
        title = paste(input$variable, "by Cluster"),
        x = input$variable,
        y = "Count"
      )
  })
}

shinyApp(ui = ui, server = server)