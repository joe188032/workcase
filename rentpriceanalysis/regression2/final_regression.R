library(sandwich)
library(lmtest)
library(quantreg)
library(ggplot2)

regression_x_model<-cbind(1,regression_x)


qr_result_coefficient = qr_result$coefficients
lr_result_coefficient = lr_result$coefficients

lr_result_coefficient = as.data.frame(lr_result$coefficients)
colnames(lr_result_coefficient)[1]<-"coefficient"
write.table(lr_result_coefficient,"D:\\PecuClass\\final_project\\regression_price\\lr_result_coefficient.csv",sep=",")

qr_result_coefficient = as.data.frame(qr_result$coefficients)
colnames(qr_result_coefficient)[1]<-"coefficient"
write.table(qr_result_coefficient,"D:\\PecuClass\\final_project\\regression_price\\qr_result_coefficient.csv",sep=",")
