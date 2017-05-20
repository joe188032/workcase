id<-"449664581882455" #黃國昌
access_token<-"EAACEdEose0cBALlzWBwR4yZAaFxmwfd26zxi3Wf6653eZBfH3mgzLZBztC4FV8wGZBPssR9wm6DM7EvR9YL7sUlmddaRmxAfbP41DWVrwHr3PCBJL69vzZCZAcBWZA47LjzP8CZBmPEs9knFZB67bxAFZC6AH5lnqlBIYv2Ty2FoBya6c3U7fjPo2r1SCDSF2OqNcZD"


query<-"46251501064?fields=posts{message,likes}"

url<-sprintf("https://graph.facebook.com/v2.8/%s&access_token=%s",query,access_token)
library(rjson)
# install.packages('RCurl')
library(RCurl)
library(plyr)
options(fileEncoding = 'utf-8')
options(stringsAsFactors = F)

data <- getURL(url)
res<-fromJSON(data, unexpected.escape = "keep")
class(res$posts$data)
all <- do.call('rbind.fill', lapply(res$posts$data, as.data.frame))

nexturl<-res$posts$paging$"next"
nextdata <- getURL(nexturl)
nextres<-fromJSON(nextdata, unexpected.escape = "keep")
nextall <- do.call('rbind.fill', lapply(nextres$data, as.data.frame))
all<-rbind(all,nextall)

while(T){
nexturl<-nextres$paging$"next"
nextdata <- getURL(nexturl)
nextres<-fromJSON(nextdata, unexpected.escape = "keep")
nextall <- do.call('rbind.fill', lapply(nextres$data, as.data.frame))
all<-rbind(all,nextall)
if(is.null(nexturl)){
  break
}
print(nrow(all))
}



#get comments----------------------------------------------------------
id<-all$id[[1]]
id
query<-sprintf("%s?fields=posts.limit(100){id,created_time,message}",id)
url<-sprintf("https://graph.facebook.com/v2.8/%s&access_token=%s",query,access_token)
data <- getURL(url)
cres<-fromJSON(data, unexpected.escape = "keep")

call<-do.call('rbind.fill', lapply(res$posts$data, as.data.frame))


