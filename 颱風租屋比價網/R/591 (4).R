library(jsonlite)
library(plyr)
library(rvest)
library(dplyr)
result<-data.frame()
for(i in (1:299)){
  house<-paste0("https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=0&searchtype=1&region=1&firstRow=",30*(i-1),"&totalRows=8949")  
  house<-fromJSON(house)
  df<-house$data$data
  result<-rbind.fill(result,df)
}
#'房屋型態(type)另抓 ex.電梯大樓、公寓..etc
type<-c()
for(i in (1:length(result$id))){
  detail<-paste0("https://rent.591.com.tw/rent-detail-",result$id[i],".html")
  detail<-read_html(detail,encoding = "UTF-8")%>%html_nodes(".attr li")%>%html_text()
  type1<-detail[4]
  type1<-sapply(strsplit(type1,":",fixed = T),"[",2)
  type[i]<-type1
}

model591<-data.frame(result$section_name,result$kind_name,result$area,result$price,result$layout,type)