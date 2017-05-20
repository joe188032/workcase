library(magrittr)
library(httr)
library(rvest)
library(XML)  # readHTMLTable
library(dplyr) # data manipulation & pipe line
library(stringr)

res <- POST(
  "http://rent.yungching.com.tw/Ashx/ShowList.ashx?VID=1250",
  body = "County=%E5%8F%B0%E5%8C%97%E5%B8%82&District=&Rooms=&PriceMin=&PriceMax=&AreaNeeds=&Purpose=&CaseType=&BuildAge=&CaseFloor=&DirFace=&ParkingSpace=&KeyWord=&Group=&ListMode=PhotosAndWords&PageCount=40&CurrentPage=2&CurrentRange=1&Sequence=&SearchMode=1&BuildNo=&BuildingID=&RoadName=&MainShopID=")
res<-POST(
  "http://rent.yungching.com.tw/Ashx/ShowList.ashx?VID=1250",
  body = body,
  add_headers("user-agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.143 Safari/537.36"))
node = htmlParse(content(res,"text", encoding = "UTF-8"))

res_text <- content(res, "parsed", encoding = "UTF-8")%>%html_nodes(".houseul01 li:nth-child(1)")%>%html_text()
html_nodes(res_text,"h2 a")%>%html_attr("href")

df<-as.list(rep(NA,times=136))
n<-1
for(i in (1:136)){
  body<-list(County="台北市",
              ListMode="PhotosAndWords",
              PageCount=40,
              CurrentPage=1,
              CurrentRange=(if (1/10<=n) print(n) else print(n+1)),
              SearchMode=1)
res<-POST("http://rent.yungching.com.tw/Ashx/ShowList.ashx?VID=1250",
          body =body,
          add_headers("user-agent"="Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.143 Safari/537.36"))
        n<-body$CurrentRange    
        res_text <- content(res, "parsed", encoding = "UTF-8") %>%html_text()          
        df[i]<-res_text
  }




