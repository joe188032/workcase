rm(list=ls())


## 載入套件
library(sandwich)
library(lmtest)
library(quantreg)
library(ggplot2)
##

## 整理資料
#result <- read.table("/Users/nowbird/Documents/各類文件/pq/result.txt",
#                     header = T, sep = ",",
#                     fileEncoding = "BIG5",
#                     stringsAsFactors = F)
#data <- cbind(model591, result[1:8887,23:26])


#real <- cbind(result[8888:dim(result)[1],1:22],matrix(0,ncol = 3))
#colnames(real)[23:25] <- c("d7","d8","d9") 
#real <- cbind(real,result[8888:dim(result)[1],23:26])

data <- rbind(total1,real)

data <- data[data$min_station != "0",]
data$price <- gsub( "," , "" , data$price )

regression_y <- as.numeric(data[,4])

x_index <- c(3,7:25, 26,28)
x_names <- c("area","d21","d22","d23","d24","d25","d26","d27","d28","d29","d210",
             "d211","d41","d42","d43","d5","d6","d7","d8","d9","min_station","min_park")
regression_x <-  matrix(0, nrow = dim(data)[1], ncol = length(x_index))

colnames(regression_x) <- x_names
n <- 1

#### 填入資料於x中

for( i in x_index){
  regression_x[,n] <- data[,i]
  regression_x[,n] <- as.numeric(regression_x[,n])
  n <- n+1
}

regression_x <- as.matrix(regression_x)

######## 以上是整理資料 ############

##### d41 獨立
##### d42 分租
##### d43 雅房 #### 整層是都是0
##### d5 頂加
##### d6 傢俱



##### Generate Interaction terms
######  坪數交乘地區 #######（一定要）

for(i in 1:11){
  temp <- as.matrix(regression_x[,1] * regression_x[,(1+i)], nrow = length(regression_y), ncol = 1)
  colnames(temp) <- paste0("aread2",i)
  regression_x <- cbind(regression_x, temp)
}


###### 房型交乘地區 ########(先別加)

for(i in 1:3){
  for(j in 1:11){
    temp <- as.matrix(regression_x[,i+12] * regression_x[,(j+1)],nrow = length(regression_y), ncol = 1)
    colnames(temp) <- paste0("d4",i,"d2",j)
    regression_x <- cbind(regression_x, temp)
  }
}


#####(坪數交乘房型交乘地區)############(沒加)####
for(i in 31:63){
  temp <- as.matrix(regression_x[,1] * regression_x[,(i)],nrow = length(regression_y), ncol = 1)
  colnames(temp) <- paste0("area",colnames(regression_x)[i])
  regression_x <- cbind(regression_x, temp)
}
##################################################




####製造坪數平方項################################(加)

area_square <- regression_x[,1]^(2)
regression_x <- cbind(regression_x, area_square)

####製造坪數平方與地區交乘項 (加)
temp <- diag(as.vector(area_square), dim(regression_x)[1])
tempp <- temp %*% regression_x[,2:12]

new_name <- rep(0,11) 

for(i in 1:11){
  new_name[i] <- paste0("area2","d2",i)
}
colnames(tempp) <- new_name

regression_x <- cbind(regression_x, tempp)
#######加入各地區平均房價 #####

#average_house_price <- matrix(0,nrow = dim(data)[1],ncol = 1)

#for(i in 1:dim(data)[1]){
#  if(data$section[i] == "士林區"){
#    average_house_price[i,] <- 57.6083
#  }else if(data$section[i] == "大安區"){
#    average_house_price[i,] <- 85.3625
#  }else if(data$section[i] == "中山區"){
#    average_house_price[i,] <- 67.1583
#  }else if(data$section[i] == "中正區"){
#    average_house_price[i,] <- 77.2458
#  }else if(data$section[i] == "大同區"){
#    average_house_price[i,] <- 56.4875
#  }else if(data$section[i] == "內湖區"){
#    average_house_price[i,] <- 55.3583
#  }else if(data$section[i] == "文山區"){
#    average_house_price[i,] <- 46.875
#  }else if(data$section[i] == "北投區"){
#    average_house_price[i,] <- 49.625
#  }else if(data$section[i] == "松山區"){
#    average_house_price[i,] <- 71.729
#  }else if(data$section[i] == "信義區"){
#    average_house_price[i,] <- 74.083
#  }else if(data$section[i] == "南港區"){
#    average_house_price[i,] <- 56.766
#  }else{
#    average_house_price[i,] <- 49.45
#  }    
#}

#regression_x <- cbind(regression_x, average_house_price)

#colnames(regression_x)[dim(regression_x)[2]] <- "average_house_price"


#####加入家電的虛擬變數，實價登錄網的資料全設為1


#regression_y <- log(regression_y)

#### 簡單回歸表現（培宣文亞部分）
regression_x_model<- regression_x[,-1]
lr_result <- lm(regression_y ~ regression_x_model)
summary(result1)
#### Comment:#
####

#### 測異質變異 ################################
test_heteroskedastic<-bptest(regression_y ~ regression_x)
print(test_heteroskedastic)

#######################################################################################
# Remark: bptest默認的解釋變數為原自變數。
# Comment: 存在異質變異，雖然單項只有坪數顯著，但是我猜因為放入太多自變數，所以總檢定
#          還是顯著。
######################################################################################


#### 使用HC estimator 
result2 <- coeftest(result1, df = Inf, vcov = vcovHC(result1, type = "HC2"))
#### 結論跟同質變異下的簡單回歸一致。


#### Quantile regression #####（培宣文亞部分）
regression_x_model<- regression_x_model[,-1]
qr_result <- rq(regression_y~regression_x_model,tau=seq(0.1,0.9,by=0.1))
summary(qr_result)
#############################


########## Output ############
y<- regression_x[1,]
x<- as.vector(y)
regression_output <- function(x, lr_result, qr_result){
  predict_mean <- c(1, x) %*% lr_result[[1]]
  predict_quantile <- c(1,x) %*% qr_result[[1]]
  temp <- density(predict_quantile)
  dis_data  <- data.frame(x = temp$x, y = temp$y)
  sample_sd <- sd(dis_data$x)
  plott <- qplot(x,y,data=dis_data,geom="line")+
    geom_ribbon(data=subset(dis_data,x>0 & x<quantile(dis_data$x,0.2)),aes(ymax=y),ymin=0,
                fill="#CCFFFF",colour=NA,alpha=0.5) + 
    geom_ribbon(data=subset(dis_data,x>quantile(dis_data$x,0.2) & x<quantile(dis_data$x,0.3)),aes(ymax=y),ymin=0,
                fill="#99CCCC",colour=NA,alpha=0.5) +
    geom_ribbon(data=subset(dis_data,x>quantile(dis_data$x,0.3) & x<quantile(dis_data$x,0.4)),aes(ymax=y),ymin=0,
                fill="#669999",colour=NA,alpha=0.5) +  
    geom_ribbon(data=subset(dis_data,x>quantile(dis_data$x,0.4) & x<quantile(dis_data$x,0.6)),aes(ymax=y),ymin=0,
                fill="#336666",colour=NA,alpha=0.5) +  
    geom_ribbon(data=subset(dis_data,x>quantile(dis_data$x,0.6) & x<quantile(dis_data$x,0.7)),aes(ymax=y),ymin=0,
                fill="#669999",colour=NA,alpha=0.5) +
    geom_ribbon(data=subset(dis_data,x>quantile(dis_data$x,0.7) & x<quantile(dis_data$x,0.8)),aes(ymax=y),ymin=0,
                fill="#99CCCC",colour=NA,alpha=0.5) +
    geom_ribbon(data=subset(dis_data, x>quantile(dis_data$x,0.8) & x<quantile(dis_data$x,1)),aes(ymax=y),ymin=0,
                fill="#CCFFFF",colour=NA,alpha=0.5)
    return(list(mean = predict_mean, sd = sample_sd, quantile = predict_quantile ,plot = plott))
}

