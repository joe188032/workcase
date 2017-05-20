library(jsonlite)
library(plyr)
library(rvest)
library(dplyr)
result<-data.frame()
for(i in (1:10)){
  house<-paste0("https://evertrust.yungching.com.tw/region/%e5%8f%b0%e5%8c%97%e5%b8%82/%e5%85%a7%e6%b9%96%e5%8d%80/",i,"?q=&dt=&d=24&t=1&a=&c=&x=&y=")  
  house<-fromJSON(house)
  df<-house$data$data
  result<-rbind.fill(result,df)
}
#'?ˆ¿å±‹å?‹æ??(type)?¦??? ex.?›»æ¢¯å¤§æ¨“ã€å…¬å¯?..etc
type<-c()
for(i in (1:length(result$id))){
  detail<-paste0("https://rent.591.com.tw/rent-detail-",result$id[i],".html")
  detail<-read_html(detail,encoding = "UTF-8")%>%html_nodes(".attr li")%>%html_text()
  type1<-detail[4]
  type1<-sapply(strsplit(type1,":",fixed = T),"[",2)
  type[i]<-type1
}

model591<-data.frame(result$section_name,result$kind_name,result$area,result$price,result$layout,type)