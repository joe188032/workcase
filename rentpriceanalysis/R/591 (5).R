df<-data.frame(title=rep(NA,times=8397),price=rep(NA,times=8397),kindname=rep(NA,times=8397),address=rep(NA,times=8397))
n<-1
library(jsonlite)
library(plyr)
result<-data.frame()
for(i in (1:307)){
  house<-paste0("https://rent.591.com.tw/home/search/rsList?is_new_list=1&type=1&kind=0&searchtype=1&region=1&firstRow=",30*(i-1),"&totalRows=9193")  
  house<-fromJSON(house)
  df<-house$data$data
  result<-rbind.fill(result,df)
  }







