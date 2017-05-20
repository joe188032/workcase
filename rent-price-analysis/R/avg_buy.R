library(jsonlite)
library(plyr)
library(rvest)
library(dplyr)
section<- c("u58EB%u6797","u5927%u5B89","u4E2D%u5C71","u4E2D%u6B63","u5927%u540C", "u5167%u6E56",
            "u6587%u5C71", "u5317%u6295", "u677E%u5C71","u4FE1%u7FA9", "u5357%u6E2F", "u842C%u83EF")
result<-c()
for (i in (1:12)){
  house<-paste0("https://price.housefun.com.tw/Ashx/GetDealCaseChart.ashx?period=3&type=2&county=%u53F0%u5317%u5E02&district=%",section[i],"%u5340")
  house<-fromJSON(house)
  #read jason
  df<-house[[2]][,2]%>%as.numeric()%>% mean()
  result[i]<-df
}
write.table(result, "D:/github/project/csv/avg_buy2.csv",sep=",")
house<-("https://price.housefun.com.tw/Ashx/GetDealCaseChart.ashx?period=3&type=2&county=%u53F0%u5317%u5E02&district=%u58EB%u6797%u5340")
house<-fromJSON(house)
#read jason
df<-house[[2]][,2]%>%as.numeric()%>% mean()
result[i]<-df