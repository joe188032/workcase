

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

address_output<-function(x){
address_df <- data.frame(lat = 0,lng = 0,loc_type=NA)
root = "http://maps.google.com/maps/api/geocode/"
return.call = "xml"
sensor = "false"
# address_df<-c()
url_gen = paste0(root, return.call, "?address=",x, "&sensor=", sensor)
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
print(address_df)
}
addresspoint<-address_output("羅斯福路四段一號")
addresspoint$lat
address_df$lng



