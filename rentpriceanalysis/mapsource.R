library(leaflet)
library(ggvis)
output$mymap <- renderLeaflet({
  if(input$address==""){
    
  }else{
  source("addressfunction.R")
  addresspoint<-address_output(input$address)
  lng<-as.numeric(addresspoint$lng)
  lat<-as.numeric(addresspoint$lat)
  getId = which(stores$tag %in% input$Area)
  markers711 <- leaflet() %>% 
    addTiles() %>%
    setView(lng,lat, zoom = input$zoom)
  markers711
  Icons <- iconList(
    star = makeIcon(iconUrl = "star.png",
                    iconWidth = 18, iconHeight = 18,
                    iconAnchorX = 22, iconAnchorY = 22),
    cosmed = makeIcon(iconUrl = "cosmed.png",
                      iconWidth = 18, iconHeight = 18,
                      iconAnchorX = 22, iconAnchorY = 22),
    mrt = makeIcon(iconUrl = "mrt.png",
                   iconWidth = 18, iconHeight = 18,
                   iconAnchorX = 22, iconAnchorY = 22),
    post = makeIcon(iconUrl = "post.png",
                   iconWidth = 18, iconHeight = 18,
                   iconAnchorX = 22, iconAnchorY = 22)
  )
  if(!is.null(input$Area))
    {addMarkers(markers711, stores$lan[getId], 
               stores$lat[getId], 
               icon = Icons[input$Area])%>%
    addMarkers(lng,lat)%>%
    addCircles(lng,
               lat,
               weight = 1,
               radius = input$radius)}
  else{
    addMarkers(markers711,lng,lat)%>%
      addCircles(lng,
                 lat,
                 weight = 1,
                 radius = input$radius)
  }
  }  
})
