library(dplyr)
library(readr)
library(e1071)
library(miscTools)
trainset = read_csv("trainset-2.csv")
colnames(trainset)
str(trainset)
usedcol = c("vehicleType","gearbox","fuelType","brand","notRepairedDamage")
trainset[usedcol] = lapply(trainset[usedcol],function(x){as.factor(x)})


with(trainset,plot(powerPS,price,type = "p"))
trainset1 =trainset[trainset$powerPS<=500,]  #powerPS is not a good explanatory.
with(trainset,plot(kilometer,price,type = "p"))  #kilometer is not a good explanatory.
with(trainset,plot(ad_exist_time,price))
with(trainset,plot(monthOfRegistration,price))

#以下為可用變數
with(trainset,plot(gearbox,price,type = "h"))
with(trainset,plot(brand,price))
with(trainset,plot(notRepairedDamage,price))
with(trainset,plot(fuelType,price))
year = 2017 - trainset$yearOfRegistration
with(trainset,plot(year,price))

#看每個類別的資料量比較
barplot(table(trainset$vehicleType))
barplot(table(trainset$gearbox))
barplot(table(trainset$brand))
barplot(table(trainset$notRepairedDamage))
barplot(table(trainset$fuelType))
sort(table(trainset$brand),T)
sort(table(trainset$gearbox),T)
sort(table(trainset$vehicleType),T)
sort(table(trainset$notRepairedDamage),T)
sort(table(trainset$fuelType),T)





dta = data.frame(price = as.numeric(trainset$price),
                 gearbox = as.factor(trainset$gearbox),
                 brand = as.factor(trainset$brand),
                 notRepairedDamage = as.factor(trainset$notRepairedDamage),
                 fuelType = as.factor(trainset$fuelType),
                 vehicleType = as.factor(trainset$vehicleType),
                 year = (2017-trainset$yearOfRegistration))

#將較少資料量的類別"內定"為trainset的一部分，以求訓練過程得以顧及到少數類別
sebrand = names(table(trainset$brand))[table(trainset$brand)<200]
senot = "ja"
sefuel = names(table(trainset$fuelType))[table(trainset$fuelType)<120]
segearbox = "automatik"
sevetype = names(table(trainset$vehicleType))[table(trainset$vehicleType)<500]

dta1 = dta[dta$brand %in% sebrand|dta$fuelType %in% sefuel,]
dta2 = dta[-as.numeric(row.names(dta1)),]

set.seed(123)
num = sample(nrow(dta2),3500-1538)
dta3 = dta2[num,]
dta4 = dta2[-num,]

train = rbind(dta1,dta3)
test = dta4

#SVR
tune(svm,price~.,data = train,ranges = list(epsilon = seq(0,0.2,0.01), cost = 2^(-1:8),gamma = 2^(-5:5)))
model1 = svm(price~.,data = train,gamma=0.125,cost=1,epsilon=0.1,kernel="radial",
             type="eps")
pred = predict(model1,test)
r2 = 1 - sum((test$price-pred)^2)/sum((test$price-mean(train$price))^2)
r2 = rSquared(test$price,test$price-pred)
#random forest
levels(train$brand)
train$brand = factor(train$brand,levels = levels(testdta$brand))
test$brand = factor(test$brand,levels = levels(testdta$brand))
testdta$fuelType = factor(testdta$fuelType,levels = levels(train$fuelType))


tune.randomForest(price~.,data = train,ntree = seq(50,1000,by=20))
rf = randomForest(price~.,data = train)
pred2 = predict(rf,test)
r2 = rSquared(test$price,test$price-predict(rf,test))
r2 = 1 - sum((test$price-pred2)^2)/sum((test$price-mean(train$price))^2)

importance(rf)
finaltest = read_csv("testset.csv")
testdta = data.frame(gearbox = as.factor(finaltest$gearbox),
                     brand = as.factor(finaltest$brand),
                     notRepairedDamage = as.factor(finaltest$notRepairedDamage),
                     fuelType = as.factor(finaltest$fuelType),
                     vehicleType = as.factor(finaltest$vehicleType),
                     year = (2017-finaltest$yearOfRegistration))


levels(testdta$brand)
levels(testdta$fuelType)
pred3 = predict(rf,testdta)
levels(testdta$gearbox)
levels(train$gearbox)

#當兩筆資料的資料結構完全相同時，rf的prediction才能work，故在此將
#train的資料和最後上傳的test資料的factor變為相同的樣子
train$brand = factor(train$brand,levels = levels(testdta$brand))
test$brand = factor(test$brand,levels = levels(testdta$brand))
testdta$fuelType = factor(testdta$fuelType,levels = levels(train$fuelType))

sapply(colnames(testdta),check)

submit = data.frame(id = finaltest$id,predict = pred3)
write.table(submit,file = "submit.csv",sep = ",",col.names = T,row.names = F)


