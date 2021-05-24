
library(shiny)

navbarPage("HELLO!",
           tabPanel("NLP predictor",
                    sidebarLayout(
                        sidebarPanel(
                            radioButtons("topn", "Select the number of predictions",
                                         c("Top 5"="5", "Top 3"="3")
                            )
                        ),
                        mainPanel(
                            textInput(inputId = "stringInput", 
                                      label = "Enter string to guess next word",
                                      value = "", 
                                      width = NULL),
                            tags$br(),
                            verbatimTextOutput("stringOutput"),
                            actionButton("predict", "Predict"),
                        )
                    )
           ),
           tabPanel("More information",
                    mainPanel(
                      tags$h3("The whole code is available in github"),
                      a(href="https://github.com/danielavarelat/Final-Project-Specialization",
                        "Github: danielavarelat/Final-Project-Specialization"),
                      tags$h3("The presentation"),
                      a(href="https://rpubs.com/dvarelat/773162",
                        "Rpubs presentation"),
                      )
                  
           )
                    
)