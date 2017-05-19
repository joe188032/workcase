
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
library(ggvis)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("颱風租屋實價網"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("section", 
                  label = h4("請選擇行政區"), 
                  choices = list("中正區" = "d20","大同區" = "d21", "中山區"="d22",
                                 "松山區" = "d23","大安區" = "d24","萬華區"="d25",
                                 "信義區" = "d26","士林區"="d27","北投區"="d28",
                                 "內湖區" = "d29","南港區"="d210","文山區"="d211"),
                  selected = "d24"
      ),
      
      textInput("address", label = h4("請輸入道路或街名："), 
                value = "羅斯福路四段一號"),
      
      
      
      numericInput("area", 
                   label = h4("請輸入坪數："), 
                   value = 10),
      
      selectInput("kind", label = h4("請選擇房型："), 
                  choices = list("獨立套房" = "d41", "分租套房" = "d42",
                                 "雅房" = "d43","整層住家" = "d40"), 
                  selected = "d40"),
      checkboxGroupInput("top",
                         label = h4("請勾選"),
                         choices = list("頂樓加蓋" = "d5",
                                        "家俱" = "d6",
                                        "電視" = "d7",  
                                        "冰箱" = "d8",
                                        "洗衣機" = "d9"
                         )
      ),
      checkboxGroupInput("Area", label = h2("administrative district"), 
                         choices = list("星巴克"="star","康是美"="cosmed","捷運站"="mrt","郵局"="post"),
                         selected = "mrt"),
      sliderInput("zoom",label = h2("please select a zoom:"),
                  min=13,max=19,value=16),
      sliderInput("radius",label = h2("please select a radius:"),
                  min=100,max=1000,value=500,step=25)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("rentprice",h1 ),
      tableOutput("distance"),
      tableOutput("quantile"),
      plotOutput("qrplot"),
      leafletOutput("mymap", height="600px")
    )
  )
))
