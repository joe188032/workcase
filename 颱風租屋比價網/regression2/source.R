lr_result_coefficient = read.table("lr_result_coefficient.csv",header = T,sep = ",",fileEncoding = "UTF-8")

# write.table(regression_x,file = "regression_x.csv",sep = ",",row.names =T,col.names = TRUE,fileEncoding = "UTF-8")
# qr_result_coefficient = read.table("qr_result_coefficient.csv",header = T,sep = ",",fileEncoding = "UTF-8")
regression_x = read.table("regression_x.csv",header = T,sep = ",",fileEncoding = "UTF-8")
qr_result_coefficient1<-read.table("qr_result_coefficient1.csv",header = T,sep = ",",fileEncoding = "UTF-8")
lr_result_coefficient1<-read.table("lr_result_coefficient1.csv",header = T,sep = ",",fileEncoding = "UTF-8")
qr_result_coefficient1<-as.matrix(qr_result_coefficient1)
lr_result_coefficient1<-as.matrix(lr_result_coefficient1)
# load("lr_result.RData")
# load("qr_result.RData")
regression_x_model<-cbind(1,regression_x)

regression_output <- function(x){
  predict_mean <- c(1, x) %*% lr_result_coefficient1
  predict_quantile <- c(1,x) %*% qr_result_coefficient1
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
  # return(list(mean = predict_mean, sd = sample_sd, quantile = predict_quantile ,plot = plott))
  return(plott)
}


