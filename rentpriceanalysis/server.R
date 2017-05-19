
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(leaflet)
library(sandwich)
library(lmtest)
library(quantreg)
library(ggplot2)

shinyServer(function(input, output) {
  
  
  
  output$rentprice <- renderText({
    if(input$address==""){
      print("請輸入地址")
    }else
    {source("source.R")
      x = regression_x_model[1,]
      x[,3:21]=0
      x[,24:79]=0
      x["area"] = input$area
      area2 = input$area^2
      x["area_square"] = area2
      
      if (input$section != "d20") {
        x[input$section] = 1
        x[paste0("area", input$section)]= input$area
        x[paste0("area2", input$section)]= area2
      }
      
      if (input$kind != "d40") {
        x[input$kind] = 1
      }
      
      if (!is.null(input$top)) {x[input$top] = 1}
      
      if (input$kind != "d40"&input$section != "d20") {
        x[paste0(input$kind,input$section)] = 1
        
        
      }
      source("distancevariable.R")
      distance<-distance_output(input$address)
      x["min_station"]<-distance$min_station
      x["min_park"]<-distance$min_park
      print(distance$min_station)
      x=as.matrix(x)
      b=lr_result_coefficient[,2]
      b=as.matrix(b)
      y_hat=round(x%*%b,2)
      
      paste0("預測租金：",y_hat)
    }
  })
  output$qrplot <-  renderPlot({
    if(input$address==""){
      
    }else
    {x = regression_x[1,]
    x[,2:20]=0
    x[,23:78]=0
    x["area"] = input$area
    area2 = input$area^2
    x["area_square"] = area2
    if (input$section != "d20") {
      x[input$section] = 1
      x[paste0("area", input$section)]= input$area
      x[paste0("area2", input$section)]= area2
    }
    if (input$kind != "d40") {
      x[input$kind] = 1
    }
    if (!is.null(input$top)) {x[input$top] = 1}
    if (input$kind != "d40"&input$section != "d20") {
      x[paste0(input$kind,input$section)] = 1
    }
    source("distancevariable.R")
    distance<-distance_output(input$address)
    x["min_station"]<-distance$min_station
    x["min_park"]<-distance$min_park
    x=as.matrix(x)
    regression_output(x)}
  })
  output$quantile<- renderTable({
    if(input$address==""){
      print(data.frame())
    }else
    {x = regression_x[1,]
    x[,2:20]=0
    x[,23:78]=0
    x["area"] = input$area
    area2 = input$area^2
    x["area_square"] = area2
    if (input$section != "d20") {
      x[input$section] = 1
      x[paste0("area", input$section)]= input$area
      x[paste0("area2", input$section)]= area2
    }
    if (input$kind != "d40") {
      x[input$kind] = 1
    }
    if (!is.null(input$top)) {x[input$top] = 1}
    if (input$kind != "d40"&input$section != "d20") {
      x[paste0(input$kind,input$section)] = 1
    }
    source("distancevariable.R")
    distance<-distance_output(input$address)
    x["min_station"]<-distance$min_station
    x["min_park"]<-distance$min_park
    x=as.matrix(x)
    
    predict_quantile <- c(1,x) %*% qr_result_coefficient1
    predict_quantile<- as.data.frame(predict_quantile)
    colnames(predict_quantile)<-c("PR10","PR20","PR30","PR40","中位數","PR60","PR70","PR80","PR90")
    print(predict_quantile)}
    
  })
  
  source("shinylatlngdistance.R",local = T)
  source("mapsource.R",local = T)
  
})
