library(tidyr)
library(httr)
library(RCurl)
library(XML)
library(bitops)
library(magrittr)
library(dplyr)
library(geosphere)
library(xml2)
library(rvest)
###########################
######### 抓經緯度 ########
###########################


address_df <- data.frame(lat = rep(0,length(address)), 
                         lng = rep(0, length(address)))

########################### 這邊應該要加一個load(total)的指令

address <- total$address
#######################12/26 改了起點跟終點 #####################
start =10087
end = 10129
################################################################

#tmp_address <- address[start:end]
root = "http://maps.google.com/maps/api/geocode/"
return.call = "xml"
sensor = "false"
#num = start


#for(each_address in tmp_address){
  url_gen = paste0(root, return.call, "?address=", each_address, "&sensor=", sensor)
  html_code<-read_xml(url_gen)
  if(xml_text(xml_find_first(html_code,"//GeocodeResponse//status"))=="OK"){
    lat=xml_find_first(html_code,"//result//geometry//location//lat")%>%xml_text()
    lng=xml_find_first(html_code,"//result//geometry//location//lng")%>%xml_text()
    loc_type=xml_find_first(html_code,"//result//geometry//location_type")%>%xml_text()
    address_df[num,1] <- lat
    address_df[num,2] <- lng
    address_df[num,3] <- loc_type
  }
  
  if(xml_text(xml_find_first(html_code,"//GeocodeResponse//status"))=="ZERO_RESULTS"){
    lat= ""
    lng= ""
    loc_type=""
    address_df[num,1] <- lat
    address_df[num,2] <- lng
    address_df[num,3] <- loc_type
  }
#  else {
#    print(paste0('error happened in row : ', as.character(num)))
#    break
#  }
#  num = num + 1
#  print(num)
#}
  

save(address_df,file = "2016.12.26.RData")
#'total <- cbind(total, address_df)
#pp591<-filter(total,!grepl("~",address))
#qq591<-filter(total,grepl("~",address))

################################################################################################
################################# 算資料與捷運站還有公園的距離 #################################
################################################################################################


#### 這邊應該要加一個load(park)的指令 ###########################(12/26)

#### 我把Fileencoding刪掉了，還有改工作路徑 。###################(12/26)
#### 超重要！！！！！我改了station_location的名字！##############(12/26)
station_location<-read.table(file ="/Users/nowbird/Documents/各類文件/pq/station_latlng.txt",header = T,sep = ",",stringsAsFactors = F)
park_location <- park[,c(1,6,7)]  ####如果我給你了整理好的park_loaction資料，這行就不需要了
#################################################################(12/26)

address_df$lat <- as.numeric(address_df$lat)
address_df$lng <- as.numeric(address_df$lng)
park_location$Latitude <- as.numeric(park_location$Latitude)
park_location$Longitude <- as.numeric(park_location$Longitude)

distance_of_station <- data.frame(min_station = rep(0,dim(address_df)[1]),
                                  min_station_name = rep(0,dim(address_df)[1]),
                                  min_park = rep(0,dim(address_df)[1]),
                                  min_park_name = rep(0,dim(address_df)[1]))


for(i in 1:10129){ ######你把起點跟終點改一下，可以繼續往下算最短距離。
  
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

################# 接著把distance_of_station 還有 address_df併在一起就完成了 #####################

#station_loction<-read.table(file ="/Users/nowbird/Documents/各類文件/pq/station_latlng.txt",header = T,sep = ",",stringsAsFactors = F)


################12/26 下個步驟是要使用 RAAN套件 #################################################

###############12/26 跑回歸前，要先在total資料中，刪掉 NA的欄位 #################################

###############12/26 大筆資料跑回歸，又是另一個關卡了 ###########################################

