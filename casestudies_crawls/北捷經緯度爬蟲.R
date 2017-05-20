library(rvest)
url<-"http://web.metro.taipei/c/selectstation2010.asp"
station<-read_html(url)%>%html_nodes("map area")%>%html_attr("href")
station<-head(station,108)

net<-data.frame()
for(i in c(1:108)){
url1<-paste0("http://web.metro.taipei/c/",station[i])
station1<-read_html(url1)%>%html_nodes("a")%>%html_attr("href")
stationname<-read_html(url1)%>%html_nodes("table tr td")%>%html_text()
result<-data.frame(stationname[4],station1[2])
net<-rbind(net,result)
}
net$station1.2.<-as.character(net$station1.2.)
m<-gregexpr("[[:digit:]]+[[:punct:]][[:digit:]]+",net$station1.2.)
m1<-regmatches(net$station1.2., m)
lat<-sapply(m1,"[",2)
lng<-sapply(m1,"[",1)
station_name<-net$stationname.4.
index<-c(1:108)
station_location<-data.frame(index,station_name,lat,lng,stringsAsFactors = T)

write.table(station_location,file = "station_location.csv",sep = ",",row.names = T,col.names = TRUE,fileEncoding = "UTF-8")
