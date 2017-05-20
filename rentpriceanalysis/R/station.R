library(dplyr)
result$price<-as.numeric(gsub(",","",result$price))

arrange(kind_average, kind)



result$price<-as.numeric(gsub(",","",result$price))
station<-mutate(result,mp=price/area)%>%group_by(section, min_station)%>%summarise(mean=mean(mp,na.rm=T))
write.table(station, "D:\\github\\project\\csv\\station.csv", sep=",")
