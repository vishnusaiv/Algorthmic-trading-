##XGBOOST
install.packages("xgboost")
library(xgboost)
xgboostdata=read.csv("xgboostdata.csv")
xgboostdata
library(quantmod);
library(TTR);
library(xgboost);

df = as.data.frame(read.csv("xgboostdata.csv"))
colnames(df) = c("Date","Time","Close","High", "Low", "Open","Volume")
df
rsi = RSI(df$Close, n=14, maType="WMA")
adx = data.frame(ADX(df[,c("High","Low","Close")]))
sar = SAR(df[,c("High","Low")], accel = c(0.02, 0.2))
trend = df$Close - sar
trend
rsi = c(NA,head(rsi,-1)) 
adx$ADX = c(NA,head(adx$ADX,-1)) 
trend = c(NA,head(trend,-1))
trend
price = df$Close-df$Open
class = ifelse(price > 0,1,0)
model_df = data.frame(class,rsi,adx$ADX,trend)
model = matrix(c(class,rsi,adx$ADX,trend), nrow=length(class))
model = na.omit(model)
colnames(model) = c("class","rsi","adx","trend")
train_size = 2/3
breakpoint = nrow(model) * train_size

training_data = model[1:breakpoint,]
test_data = model[(breakpoint+1):nrow(model),]

# Split data training and test data into X and Y
X_train = training_data[,2:4] ; Y_train = training_data[,1]
class(X_train)[1]; class(Y_train)

X_test = test_data[,2:4] ; Y_test = test_data[,1]
class(X_test)[1]; class(Y_test)

X_train
X_test
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
xgModel = xgboost(data = dtrain, nround = 5, objective = "binary:logistic")
dtrain = xgb.DMatrix(data = X_train, label = Y_train)
cv = xgb.cv(data = dtrain, nround = 10, nfold = 5, objective = "binary:logistic")
print(cv)

preds = predict(xgModel, X_test)

# Determine the size of the prediction vector
print(length(preds))

# Limit display of predictions to the first 6
print(head(preds))
prediction = as.numeric(preds > 0.5)
print(head(prediction))
error_value = mean(as.numeric(preds > 0.5) != Y_test)
print(paste("test-error=", error_value))
importance_matrix = xgb.importance(model = xgModel)
print(importance_matrix)

library(DiagrammeR)
xgb.plot.tree(model = xgModel)

# View only the first tree in the XGBoost model
xgb.plot.tree(model = xgModel, n_first_tree = 1)
