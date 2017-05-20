
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("source.R")


shinyServer(function(input, output) {

  
 
    
  output$rentprice <- renderText({
    
    x = regression_x[1,]
    x[,3:18]=0
    x[,21:97]=0
    x["area"] = input$area
    
    if (input$section != "d20") {
      x[input$section] = 1
      x[paste0("area", input$section)]= input$area
    }

    if (input$kind != "d40") {x[input$kind] = 1
    }
    
    if (!is.null(input$top)) {x[input$top] = 1}
    
    if (input$kind != "d40"&input$section != "d20") {
      x[paste0(input$kind,input$section)] = 1
      x[paste0("area", input$kind, input$section)]=input$area
    }
    
 
    
    x=as.matrix(x)
    b=coefficient[,2]
    b=as.matrix(b)
    y_hat=round(x%*%b,2)

    paste0("預測租金：",y_hat)
    
  })
  output$plot<- renderPlot({
    
  })


})
