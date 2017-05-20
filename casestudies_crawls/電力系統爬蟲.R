library(magrittr)
library(httr)
library(rvest)
library(XML)  # readHTMLTable
library(dplyr) # data manipulation & pipe line
library(stringr)
library(plyr)


# target = paste0("N",1)

res = POST("http://140.112.166.97/power/fn2/dataq.aspx",
           body = list(dtype = "h",
                       build= "01A_P1_14",
                       dt1 = "2017/3/8",
                       dt2 = "2017/3/8 12:00:00"))

node = content(res, "parsed", encoding = "big5") %>% html_nodes("table table")
a = html_table(node,header = T)[[1]]

node1 = content(res, "parsed", encoding = "big5") %>% html_nodes("table select option")
index = html_attr(node1,"value")
index = index[-c(1:3)]
text = html_text(node1)
building = text[-c(1:3)]

# powerdata = data.frame()
for(i in c(181:length(index))){
  res = POST("http://140.112.166.97/power/fn2/dataq.aspx",
             body = list(dtype = "h",
                         build= index[i],
                         dt1 = "2013/12/1",
                         dt2 = "2017/3/9 15:00:00"))
  
  node = content(res, "parsed", encoding = "big5") %>% html_nodes("table table")
  power = html_table(node,header = T)[[1]]
  power = cbind(building = rep(building[i],dim(power)[1]),power)
  powerdata = rbind.fill(powerdata,power)
  save(powerdata,file = "powerdata.RData")
}


for(block in c(1:11)){
  for(year in c(98:105)){
   for(month in c(1:12)){
     target = paste0("N",block)
     res = POST("http://140.112.166.97/power/fn3/build.aspx",
                body = list(ctg = target,
                            yr= year,
                            mn= month,
                            ok = "%BDT%A9w"))
     
     node = content(res, "parsed", encoding = "big5") %>% html_nodes("table table")
     a = html_table(node,header = T)[[1]]
     
   } 
  }
}

