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
  skin = "black",
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
                    fluidRow(
                      box(
                        title = "Word frequencies",
                        background = "black",
                        width = 12,
                        solidHeader = TRUE,
                        plotOutput("freqPlot", width = 1000, height = 600)
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Word count per manifesto",
                        solidHeader = TRUE,
                        width = 12,
                        background = "black",
                        plotOutput("facetPlot", width = 1000)
                      )
                    )),
                  tabItem(tabName = "tfidf",
                          fluidRow(
                            box(
                              title = "Words more specific per manifesto",
                              background = "black",
                              width = 12,
                              solidHeader = TRUE,
                              selectInput(
                                inputId = "selectParty",
                                label = "Party",
                                choices = c("BJP", "Congress")
                              ),
                              plotOutput("tfidfPlot", width = 1000)
                            )
                          )),
                  tabItem(tabName = "bigrams",
                          fluidRow(
                            box(
                              title = "Frequent word pairings",
                              background = "black",
                              width = 12,
                              solidHeader = TRUE,
                              selectInput(
                                inputId = "selectPartyBG",
                                label = "Party",
                                choices = c("BJP", "Congress")
                              ),
                              plotOutput("bgPlot", width = 1000)
                            )
                          )),
                  tabItem(tabName = "corWords",
                          fluidRow(
                            box(
                              title = "Correlated words from each manifesto",
                              solidHeader = TRUE,
                              background = "black",
                              width = 12,
                              selectInput(
                                inputId = "selectPartyCor",
                                label = "Party",
                                choices = c("BJP", "Congress")
                              ),
                              plotOutput("corWords", width = 1000)
                            )
                          )),
                  tabItem(tabName = "topicWords",
                          fluidRow(
                            box(
                              solidHeader = TRUE,
                              background = "black",
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
