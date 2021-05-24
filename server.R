#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd(getwd())
gram2 <- readRDS("2gram_Dictionary.rds")
gram3 <- readRDS("3gram_Dictionary.rds")
gram4 <- readRDS("4gram_list.rds")
source("pred.R")

shinyServer(function(input, output) {
    observeEvent( input$predict,{
        p<- return_prediction(input$stringInput)
        if(!is.null(p)){
            t<-top(p, as.integer(input$topn));
            output$stringOutput = renderPrint(t);
        } else {
            output$stringOutput = renderText("No prediction available")
        }
        
    });
})