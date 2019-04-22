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

ui <- dashboardPage(
  dashboardHeader(title = "Manifesto-viz"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Word frequencies", tabName = "wordFreq"),
      menuItem("tf-idf", tabName = "tfidf"),
      menuItem("Bigrams", tabName = "bigrams"),
      menuItem("Correlated words", tabName = "corWords"),
      menuItem("Topics per party", tabName = "topicWords")
    )
  ),
  dashboardBody(tabItems
                (
                  tabItem
                  (tabName = "wordFreq",
                    fluidRow(box(
                      plotOutput("freqPlot", width = 1000, height = 600)
                    )),
                    fluidRow(box(
                      plotOutput("facetPlot", width = 1000)
                    ))),
                  tabItem(tabName = "tfidf",
                           fluidRow(box(
                             selectInput(
                               inputId = "selectParty",
                               label = "Party",
                               choices = c("BJP", "Congress")
                             ),
                             plotOutput("tfidfPlot")
                           ))),
                  tabItem(tabName = "bigrams",
                          fluidRow(box(
                            selectInput(
                              inputId = "selectPartyBG",
                              label = "Party",
                              choices = c("BJP", "Congress")
                            ),
                            plotOutput("bgPlot")
                          ))),
                  tabItem(tabName = "corWords",
                          fluidRow(box(
                            selectInput(
                              inputId = "selectPartyCor",
                              label = "Party",
                              choices = c("BJP", "Congress")
                            ),
                            plotOutput("corWords")
                          ))),
                  tabItem(tabName = "topicWords",
                          fluidRow(box(
                            selectInput(
                              inputId = "selectPartyTopic",
                              label = "Party",
                              choices = c("BJP", "Congress")
                            ),
                            plotOutput("topicWords")
                          )))
                ))
)


uiFluid <- fluidPage(
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
    mainPanel(
      h1("Word frequency of both the manifestos"),
      textOutput(outputId = "selected_var"),
      plotOutput(outputId = "freqPlot"),
      plotOutput(outputId = "facetPlot"),
      plotOutput(outputId = "tfidfPlot")
    )
    
  )
  
)

server <- function(input, output) {
  output$selected_var <- renderText({
    paste("You have chosen", input$selectParty)
  })
  
  output$freqPlot <- renderPlot({
    freqPlot()
  })
  
  output$facetPlot <- renderPlot({
    wordCount()
  })
  
  output$tfidfPlot <- renderPlot({
    tfidfWords(stringr::str_to_lower(input$selectParty))
  })
  
  output$bgPlot <- renderPlot({
    bgPlotFunc(stringr::str_to_lower(input$selectPartyBG))
  })
  
  output$corWords <- renderPlot({
    corWords(stringr::str_to_lower(input$selectPartyCor))
  })
  
  output$topicWords <- renderPlot({
    topicWords(stringr::str_to_lower(input$selectPartyTopic))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
