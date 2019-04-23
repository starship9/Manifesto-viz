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
library(shinythemes)
library(dashboardthemes)

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
  dashboardBody(
    shinyDashboardThemes(theme = "grey_dark"),
    tabItems
    (
      tabItem
      (tabName = "wordFreq",
        fluidRow(
          box(
            title = "Word frequencies",
            #background = "black",
            width = 12,
            solidHeader = TRUE,
            plotOutput("freqPlot", height = 600)
          )
        ),
        fluidRow(
          box(
            numericInput(
              inputId = "numInput",
              label = "Number of most frequent words",
              value = 10,
              min = 1,
              max = 30
            ),
            title = "Word count per manifesto",
            solidHeader = TRUE,
            width = 12,
            #background = "black",
            plotOutput("facetPlot")
          )
        )),
      tabItem(tabName = "tfidf",
              fluidRow(
                box(
                  numericInput(
                    inputId = "numInputTFIDF",
                    label = "Number of words more specific to a party",
                    value = 10,
                    min = 1,
                    max = 30
                  ),
                  title = "Words more specific per manifesto",
                  #background = "black",
                  width = 12,
                  solidHeader = TRUE,
                  selectInput(
                    inputId = "selectParty",
                    label = "Party",
                    choices = c("BJP", "Congress")
                  ),
                  plotOutput("tfidfPlot")
                )
              )),
      tabItem(tabName = "bigrams",
              fluidRow(
                box(
                  title = "Frequent word pairings",
                  #background = "black",
                  width = 12,
                  solidHeader = TRUE,
                  selectInput(
                    inputId = "selectPartyBG",
                    label = "Party",
                    choices = c("BJP", "Congress")
                  ),
                  plotOutput("bgPlot")
                )
              )),
      tabItem(tabName = "corWords",
              fluidRow(
                box(
                  title = "Correlated words from each manifesto",
                  solidHeader = TRUE,
                  #background = "black",
                  width = 12,
                  selectInput(
                    inputId = "selectPartyCor",
                    label = "Party",
                    choices = c("BJP", "Congress")
                  ),
                  plotOutput("corWords")
                )
              )),
      tabItem(tabName = "topicWords",
              fluidRow(
                box(
                  solidHeader = TRUE,
                  #background = "black",
                  width = 12,
                  title = "Topic modelling of each manifesto",
                  selectInput(
                    inputId = "selectPartyTopic",
                    label = "Party",
                    choices = c("BJP", "Congress")
                  ),
                  plotOutput("topicWords", width = 1000)
                )
              ))
    ))
)

server <- function(input, output) {
  output$selected_var <- renderText({
    paste("You have chosen", input$selectParty)
  })
  
  output$freqPlot <- renderPlot({
    freqPlot()
  })
  
  wordInput <- reactive({
    wordCount(input$numInput)
  })
  
  output$facetPlot <- renderPlot({
    wordInput()
  })
  
  tfidfInput <- reactive({
    tfidfWords(stringr::str_to_lower(input$selectParty), input$numInputTFIDF)
  })
  
  output$tfidfPlot <- renderPlot({
    tfidfInput()
  })
  
  output$bgPlot <- renderPlot({
    bgPlotFunc(stringr::str_to_lower(input$selectPartyBG))
  })
  
  output$corWords <- renderPlot({
    corWords(stringr::str_to_lower(input$selectPartyCor))
  })
  
  topicReactive <- reactive({
    topicWords(stringr::str_to_lower(input$selectPartyTopic))
  })
  
  output$topicWords <- renderPlot({
    topicReactive()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
