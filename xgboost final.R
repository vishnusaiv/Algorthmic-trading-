xgdata <- read.csv("xgboostdata.csv")
str(xgdata)
## To get the Date and close column
xgdata_final <- xgdata[,c(6,7)]
head(xgdata_final)
dim(xgdata_final)
##Splitting train and test
set.seed(123)
splitdata <- sample(1:nrow(xgdata_final), size = 0.8*nrow(xgdata_final))
traindata <- xgdata_final[splitdata,]
testdata <- xgdata_final[-splitdata,]
dim(traindata)
dim(testdata)
library(xgboost)
xgb <- xgboost(data = data.matrix(traindata$Volume..BTC.),eta=0.5,max_depth=30,subsample=0.5,
               label = traindata$Close,booster="gblinear",eval_metric="rmse",objective = "reg:linear",
               nrounds=100)
summary(xgb)
testdata$Prediction<-predict(xgb,data.matrix(testdata$Volume..BTC.))
predict(xgb,data.matrix(3))
View(testdata)
write.csv(testdata,"testdata.csv")
RMSE = function(m, o){ sqrt(mean((m - o)^2))}
RMSE_Value<-RMSE(testdata$Close,testdata$Prediction)
RMSE_Value
##traindata prediction
traindata$Prediction<-predict(xgb,data.matrix(traindata[,]))
View(testdata)
write.csv(traindata,"traindata.csv")
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE_Value<-RMSE(traindata$Close,traindata$Prediction)
RMSE_Value
##Plotting between actual and predicted
library(ggplot2)


ggplot(testdata, aes(Prediction, Volume..BTC.) ) +
  xlab("Predicted")+
  ylab("actual")+
  geom_line()
##
plot(testdata, aes(Prediction, Volume..BTC.) )
ggplot(testdata, aes(Prediction, Close) ) +
  xlab("Predicted")+
  ylab("actual")

attach(traindata)
plot(Prediction, Close, main="Prediction vs Actual", 
     xlab="Prediction ", ylab="Actual ", pch=19)

pairs(~Prediction+Close,data=testdata, 
      main="Simple Scatterplot Matrix")
library(car)
scatterplot.matrix(~Prediction|Close, data=testdata,
                   main="Three Cylinder Options")

# 3D Scatterplot
library(scatterplot3d)
attach(testdata)
scatterplot3d(Prediction,Close, main="3D Scatterplot")

s3d <-scatterplot3d(Prediction,Close, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")



library(car) 
scatterplot(Prediction ~ Close | cyl, data=testdata, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))


plot(Prediction, Close, main="Prediction vs Actual", 
     xlab="Prediction ", ylab="Actual ")
