


output$distance<-renderTable({

address_df <- data.frame(lat = 0,lng = 0,loc_type=NA)
root = "http://maps.google.com/maps/api/geocode/"
return.call = "xml"
sensor = "false"
# address_df<-c()
url_gen = paste0(root, return.call, "?address=",input$address, "&sensor=", sensor)
html_code<-read_xml(url_gen)
if(xml_text(xml_find_first(html_code,"//GeocodeResponse//status"))=="OK"){
  lat=xml_find_first(html_code,"//result//geometry//location//lat")%>%xml_text()
  lng=xml_find_first(html_code,"//result//geometry//location//lng")%>%xml_text()
  loc_type=xml_find_first(html_code,"//result//geometry//location_type")%>%xml_text()
  address_df[1,1] <- lat
  address_df[1,2] <- lng
  address_df[1,3] <- loc_type
}else if(xml_text(xml_find_first(html_code,"//GeocodeResponse//status"))=="ZERO_RESULTS"){
  lat= ""
  lng= ""
  loc_type=""
  address_df[1] <- lat
  address_df[2] <- lng
  address_df[3] <- loc_type
}

# ===================算距離=============================
station_location<-read.table("station_location.csv",header = T,sep = ",",fileEncoding = "UTF-8",stringsAsFactors = F)
park<-read.table("park.csv",header = T,sep = ",",fileEncoding = "UTF-8",stringsAsFactors = F)
park_location <- park[,c(1,6,7)] 
address_df$lat <- as.numeric(address_df$lat)
address_df$lng <- as.numeric(address_df$lng)
park_location$ParkName<-as.character(park_location$ParkName)
park_location$Latitude <- as.numeric(park_location$Latitude)
park_location$Longitude <- as.numeric(park_location$Longitude)

distance_of_station <- data.frame(min_station = rep(0,dim(address_df)[1]),
                                  min_station_name = rep(0,dim(address_df)[1]),
                                  min_park = rep(0,dim(address_df)[1]),
                                  min_park_name = rep(0,dim(address_df)[1]))
for(i in 1:length(address_df$lat)){ ######你把起點跟終點改一下，可以繼續往下算最短距離。
  
  if(is.na(address_df[i,1]) == T) next
  
  temp <- rep(0,dim(station_location)[1])
  
  for(j in 1:dim(station_location)[1]){
    
    temp[j] <- distCosine(address_df[i,c(2,1)] , station_location[j,c(4,3)])
    
  }
  
  distance_of_station[i,1] <- min(temp)
  
  
  ################################################################################################
  ######### 超重要！這邊多寫一個邏輯判斷式，是因為相同最膽距離下，可能會同時存在兩個捷運站，
  ######### 例如，給定最短距離是500公尺，剛好第一筆資料跟忠孝復興捷運站跟中山國中站的距離都是500
  ######### 所以這這種情況下，我先把最短捷運站名稱定義成NA，之後再想辦法處理。刪除NA資料時，要避開
  ######### 此欄位，因為此欄位還是存在最短距離的自變數
  ################################################################################################
  
  
  if(length(station_location[ temp == min(temp) ,2]) > 1){
    distance_of_station[i,2] <-NA
  }else{
    distance_of_station[i,2] <- station_location[ temp == min(temp) ,2]
  }
  
  ################################################################################################
  
  temp <- rep(0,dim(park_location)[1])
  
  for(j in 1:dim(park_location)[1]){
    
    temp[j] <- distCosine(address_df[i,c(2,1)] , park_location[j,c(3,2)])
    
  }
  distance_of_station[i,3] <- min(temp)
  
  ###################################################################################################
  #跟前面同理   #####################################################################################
  if(length(park_location[ temp == min(temp) ,1]) > 1){
    distance_of_station[i,4] <-NA
  }else{
    distance_of_station[i,4] <- park_location[ temp == min(temp) ,1]}
  ################################################################################################
  
  print(i)
}
print(distance_of_station)
})