rm(list=ls())

result <- read.table("D:\\PecuClass\\final_project\\regression_price\\result.txt", header = T, sep = ",", fileEncoding = "Big-5", stringsAsFactors = F)
result


data <- result[result$min_station != "0",]

data$price <- gsub( "," , "" , data$price )

regression_y <- as.numeric(data[,4])

x_index <- c(3,7:23,25)
x_names <- c("area","d21","d22","d23","d24","d25","d26","d27","d28","d29","d210",
             "d211","d41","d42","d43","d5","d6","min_station","min_park")

regression_x <- as.data.frame( matrix(0, nrow = dim(data)[1], ncol = length(x_index)))
colnames(regression_x) <- x_names



n <- 1

for( i in x_index){
  regression_x[,n] <- data[,i]
  regression_x[,n] <- as.numeric(regression_x[,n])
  n <- n+1
}

regression_x <- as.matrix(regression_x)


##### d41 獨立
##### d42 分租
##### d43 雅房 #### 整層是都是0
##### d5 頂加
##### d6 傢俱



##### Generate Interaction terms

for(i in 1:11){
  temp <- as.matrix(regression_x[,1] * regression_x[,(1+i)],nrow = length(regression_y), ncol = 1)
  colnames(temp) <- paste0("aread2",i)
  regression_x <- cbind(regression_x, temp)
}

for(i in 1:3){
  for(j in 1:11){
    temp <- as.matrix(regression_x[,i+12] * regression_x[,(j+1)],nrow = length(regression_y), ncol = 1)
    colnames(temp) <- paste0("d4",i,"d2",j)
    regression_x <- cbind(regression_x, temp)
  }
}

for(i in 31:63){
  temp <- as.matrix(regression_x[,1] * regression_x[,(i)],nrow = length(regression_y), ncol = 1)
  colnames(temp) <- paste0("area",colnames(regression_x)[i])
  regression_x <- cbind(regression_x, temp)
}




result1 <- lm(regression_y ~ regression_x)
summary(result1)
regression_x<-cbind(1,regression_x)
write.csv(regression_x,file = "regression_x.csv",row.names = F)
View(regression_x)
y_hat = regression_x%*%result1$coefficients
colnames(y_hat)[1]<-"y_hat"
coefficient = as.data.frame(result1$coefficients)
colnames(coefficient)[1]<-"coefficient"
write.table(coefficient,"D:\\PecuClass\\final_project\\regression_price\\coefficient.csv",sep=",")

######################
#data[,c(3,7:23,25)]##
######################

x1 <- rnorm(20,0,1)
x2 <- rnorm(20,0,1)

x <- cbind(x1,x2)

y <- rnorm(20,5,2)

test <- lm(y~x)

test[1]
