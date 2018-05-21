library(quantmod)
library(TTR)
library(caret)
library(corrplot)
library(pROC)
library(FSelector)

set.seed(5)

df_Stock<- read.csv("bitcoinpricedata.csv")

price=df_Stock$Close-df_Stock$Open

class=ifelse(price>0,"UP","DOWN")

forceIndex=(df_Stock$Close -df_Stock$Open)*(df_Stock$Volume..BTC.)

forceIndex=c(NA,head(forceIndex,-1));

willR5= WPR(df_Stock[,c("High","Low","Close")],n=5)
willR5=c(NA,head(willR5,-1))
willR5

willR10= WPR(df_Stock[,c("High","Low","Close")],n=10)
willR10=c(NA,head(willR10,-1))

willR15= WPR(df_Stock[,c("High","Low","Close")],n=15)
willR15=c(NA,head(willR15,-1))


RSI5  = RSI(df_Stock$Close, n = 5,maType="WMA") ;
RSI5 = c(NA,head(RSI5,-1)) ;
RSI10 = RSI(df_Stock$Close, n = 10,maType="WMA") ;
RSI10 = c(NA,head(RSI10,-1)) ;
RSI15 = RSI(df_Stock$Close, n = 15,maType="WMA") ;
RSI15 = c(NA,head(RSI15,-1)) ;

# Price change Indicators (ROC and Momentum)
ROC5 = ROC(df_Stock$Close, n = 5,type ="discrete")*100 ; 
ROC5 = c(NA,head(ROC5,-1)) ;
ROC10 = ROC(df_Stock$Close, n = 10,type ="discrete")*100 ;
ROC10 = c(NA,head(ROC10,-1)) ;

MOM5 = momentum(df_Stock$Close, n = 5, na.pad = TRUE) ;
MOM5 = c(NA,head(MOM5,-1)) ;
MOM10 = momentum(df_Stock$Close, n = 10, na.pad = TRUE) ; 
MOM10 = c(NA,head(MOM10,-1)) ;


# Volatility signal Indicator (ATR)
ATR5 = ATR(df_Stock[,c("High","Low","Close")], n = 5, maType="WMA")[,1] ;
ATR5 = c(NA,head(ATR5,-1)) ;
ATR10 = ATR(df_Stock[,c("High","Low","Close")], n = 10, maType="WMA")[,1];
ATR10 = c(NA,head(ATR10,-1)) ;


## Combining all the Indicators and the Class into one dataframe
dataset = data.frame(class,forceIndex,willR5,willR10,willR15,RSI5,RSI10,RSI15,ROC5,
                     ROC10,MOM5,MOM10,ATR5,ATR10)
dataset = na.omit(dataset)

dataset

## Understanding the dataset using descriptive statistics
print(head(dataset),5)
dim(dataset)
y = dataset$class
cbind(freq=table(y), percentage=prop.table(table(y))*100)

summary(dataset)

##  Visualizing the dataset using a correlation matrix
correlations = cor(dataset[,c(2:13)])
print(head(correlations))
corrplot(correlations, method="circle")

## Selecting features using the random.forest.importance function from the FSelector package
set.seed(5)
weights = random.forest.importance(class~., dataset, importance.type = 1)
print(weights)
weights


set.seed(5)
subset = cutoff.k(weights, 10)
print(subset)

## Creating a dataframe using the selected features
dataset_rf = data.frame(class,forceIndex,willR5,willR10,RSI5,RSI10,RSI15,ROC5,ROC10,MOM5)
dataset_rf = na.omit(dataset_rf)

# Resampling method used - 10-fold cross validation 
# with "Accuracy" as the model evaluation metric.
trainControl = trainControl(method="cv", number=10)
trainControl
metric = "Accuracy"

## Trying four different Classification algorithms
# k-Nearest Neighbors (KNN)
set.seed(5)


fit.knn = train(class~., data=dataset_rf, method="knn", 
                metric=metric, preProc=c("range"),trControl=trainControl)

fit.knn
# Classification and Regression Trees (CART)
set.seed(5)
fit.cart = train(class~., data=dataset_rf, method="rpart", 
                 metric=metric,preProc=c("range"),trControl=trainControl)

fit.cart
# Naive Bayes (NB)
set.seed(5)
fit.nb = train(class~., data=dataset_rf, method="nb", 
               metric=metric, preProc=c("range"),trControl=trainControl)

fit.nb

# Support Vector Machine with Radial Basis Function (SVM)
set.seed(5)
fit.svm = train(class~., data=dataset_rf, method="svmRadial", 
                metric=metric,preProc=c("range"),trControl=trainControl)

fit.svm
## Evaluating the algorithms using the "Accuracy" metric
results = resamples(list(KNN=fit.knn,CART=fit.cart, SVM=fit.svm))
summary(results)
dotplot(results)

## Tuning the shortlisted algorithm (KNN algorithm)
set.seed(5)
grid = expand.grid(.k=seq(1,10,by=1))
fit.knn = train(class~., data=dataset_rf, method="knn", metric=metric, tuneGrid=grid,
                preProc=c("range"), trControl=trainControl)
print(fit.knn)






