#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

source("../R/script.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Manifesto visualizations, elections 2019"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose the manifesto you wish to look at"),
      selectInput(
        inputId = "selectParty",
        label = "Party",
        choices = c("BJP", "Congress")
      )
    ),
    mainPanel(h1("Word frequency of both the manifestos"),
              textOutput(outputId = "selected_var"),
              plotOutput(outputId = "freqPlot"))
    
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$selected_var <- renderText({
    paste("You have chosen", input$selectParty)
  })
  
  output$freqPlot <- renderPlot({
    freqPlot()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
