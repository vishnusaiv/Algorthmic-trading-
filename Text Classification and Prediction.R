install.packages("twitteR")
library(stringr)
library(twitteR)
library(xlsx)
library(plyr)
library(ggplot2)
library("tm")
library("SnowballC")
library(syuzhet)
library("caTools")
library("rpart")
library("rpart.plot")
library("ROCR")
library("randomForest")
library("rtweet")
library("tidytext")
library(stringr)

library(lubridate)
library(scales)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(reshape2)
library(dplyr)
library(twitteR)

consumer_key<- "bPLOx8pgxBCnKHC9IghmwP6Dc"
consumer_secret <- "tJfaC0hxuWWh6klMJwF1wLn7uFKATmJPqk3ikmeA7JbpyqWJeA"
access_token <- "954932356271362049-jO0llWDdxYPAWseHtwKlq50ENuvE2Yi"
access_secret <- "3Y0lhj9CQFbiGoVUdeffUJREHyrBAPLXz0Zu6VBzwiSq8"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")

neg = c(neg, 'wtf')

score.sentiment = function(tweets, pos.words, neg.words)
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
  
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweet, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    
   
    
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

#hashtags <- c("#bitcoin", "#ethereum","#ripple")

#needle <- paste(hashtags, collapse = " OR ")

#tweets <- searchTwitter('bitcoin+price', n =1000,since='2018-04-01',until ='2018-04-20')

#tweets<-searchTwitteR("\"bitcoin price\"", n = 1000)

#tweets<-userTimeline('bitcoin price', n=1000,includeRts=TRUE)


#tw = twitteR::searchTwitter('#bitcoin + price', n = 1000,since='2018-04-01',until ='2018-04-20')

tweets<-userTimeline('bitcoin', n=2500, maxID=NULL, sinceID=NULL, includeRts=TRUE)


alltweets <- twListToDF(tweets)

Tweettimestamp<- (alltweets$created)

TweetDate<-as.Date(as.POSIXct(Tweettimestamp))

Tweets.text = laply(tweets,function(t)t$getText())# gets text from Tweets

Tweets.text=do.call("rbind", lapply(Tweets.text, as.character))

#To get price Related tweets alone
library(quanteda)
library(foreach)
library(data.table)
corpus <- Corpus(VectorSource(tw1))
phrase <- c("price")

find.phrases = foreach(i = 1:length(phrase)) %do% {
  kwic(Tweets.text, phrase[i])
}

find.df = rbindlist(find.phrases)

find.df

PriceData=find.df

write.csv(PriceData,"PriceData.csv")

analysis = score.sentiment(Tweets.text, pos, neg) # calls sentiment function

analysis$TweetDate <-as.Date(as.POSIXct(Tweettimestamp))

analysis$Tweettimestamp<- Tweettimestamp


analysis



#a new variable in our data set called Negative.

tweets$Negative <- as.numeric(analysis$score<= -1)

table(tweets$Negative)

#Add one more variable for the positive tweets, tweet for that average sentiment score is __grater than of equal to 1

tweets$Positive <- as.numeric(analysis$score>=1)
table(tweets$Positive)

str(tweets)

table(tweets$Positive)

numneg = sum(tweets$Negative)
numneg
numpos = sum(tweets$Positive)
numpos


meanscore = tapply(analysis$score,analysis$TweetDate,mean)

Score<-meanscore

write.csv(Score,"Score.csv")

# calls sentiment function

hist(analysis$score,col ="yellow", main ="Score of tweets",ylab = "Count of tweets")

count(analysis$score)

write.csv(analysis,"myResults.csv")

write.csv(PriceData,"PriceData.csv")

qplot(analysis$score,xlab = "Score of tweets")

tweets <- read.csv("myResults.csv", stringsAsFactors = FALSE)
str(tweets)

#a new variable in our data set called Negative.

tweets$Negative <- as.factor(analysis$score <= -1)

table(tweets$Negative)

#Add one more variable for the positive tweets, tweet for that average sentiment score is __grater than of equal to 1

tweets$Positive <- as.factor(analys$score>=1)

str(tweets)

table(tweets$Positive)

#CREATING A CORPUS

corpus <- Corpus(VectorSource(Tweets.text))

corpus

#To inspect the first tweet in our corpus

corpus[[1]]

corpus <- tm_map(corpus,tolower)

corpus[[1]]

#converts corpus to a Plain Text Document

#corpus <- tm_map(corpus, PlainTextDocument)

#Removing punctuation

corpus <- tm_map(corpus, removePunctuation)

#Stop Words

stopwords("english")[1:10]

#Removing stop words
corpus <- tm_map(corpus, removeWords,stopwords("english"))

corpus[[1]]

corpus <- tm_map(corpus, stemDocument)

corpus[[1]]

#Create a Document Term Matrix

#corpus <- Corpus(VectorSource(corpus))

DTM <- DocumentTermMatrix(corpus)

DTM

inspect(DTM[1000:1005, 505:515])
freq <- findFreqTerms(DTM, lowfreq = 20)

freq

#Remove sparse terms
sparse_DTM <- removeSparseTerms(DTM, 0.995)

sparse_DTM

#Convert the DTM to a data frame

tweetsSparse <- as.data.frame(as.matrix(sparse_DTM))

tweetsSparse


colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

colnames

#Add the dependent variable

tweetsSparse$Negative <- tweets$Negative

tweetsSparse$Positive <- tweets$Positive

#Split data in training/testing sets

set.seed(123)

splitNegative <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparseNegative <- subset(tweetsSparse, splitNegative == TRUE)
testSparseNegative <- subset(tweetsSparse, splitNegative == FALSE)

#Split data based on the positive variable

splitPositive <- sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparsePositive <- subset(tweetsSparse, splitPositive == TRUE)
testSparsePositive <- subset(tweetsSparse, splitPositive == FALSE)

#PREDICTING SENTIMENT

tweetCARTNegative <- rpart(Negative ~ . , data = trainSparseNegative, method = "class")

prp(tweetCARTNegative)

tweetCARTPositive <- rpart(Positive ~ . , data = trainSparsePositive, method = "class")

prp(tweetCARTPositive)

#Prediction for the negative sentiment:

predictCARTNegative <- predict(tweetCARTNegative, newdata = testSparseNegative, type = "class")

#Prediction for the positive sentiment:
predictCARTPositive <- predict(tweetCARTPositive, newdata = testSparsePositive, type = "class")

#confusion matrix:

cmat_CARTNegative <- table(testSparseNegative$Negative, predictCARTNegative)
cmat_CARTNegative 
#Overall Accuracy for negative sentiment
accu_CART <- (cmat_CARTNegative[1,1] + cmat_CARTNegative[2,2])/sum(cmat_CARTNegative)
accu_CART


cmat_CARTPositive <- table(testSparsePositive$Positive, predictCARTPositive)
cmat_CARTPositive

accu_CARTP <- (cmat_CARTPositive[1,1] + cmat_CARTPositive[2,2])/sum(cmat_CARTPositive)
accu_CARTP

#BASELINE MODEL

cmat_baseline <- table(testSparseNegative$Negative)
cmat_baseline

accu_baseline <- max(cmat_baseline)/sum(cmat_baseline)
accu_baseline

cmat_baselineP <- table(testSparsePositive$Positive)
cmat_baselineP

accu_baselineP <- max(cmat_baselineP)/sum(cmat_baselineP)
accu_baselineP


#Random Forrest for negative sentiment 

  set.seed(123)
tweetRFN <- randomForest(Negative ~ . , data = trainSparseNegative)

tweetRFN

#random forest for positive sentiment  

set.seed(123)
tweetRFP <- randomForest(Positive ~ . , data = trainSparsePositive)

tweetRFP

#Out-of-Sample predictions for negative:
predictRFN <- predict(tweetRFN, newdata = testSparseNegative)

#And then compute the Out-of-Sample predictions for positive:

predictRFP <- predict(tweetRFP, newdata = testSparsePositive)

#compute the confusion matrix for negative:
cmat_RFN <- table(testSparseNegative$Negative, predictRFN)
cmat_RFN 

accu_RFN <- (cmat_RFN[1,1] + cmat_RFN[2,2])/sum(cmat_RFN)
accu_RFN

#compute the confusion matrix for positive:

cmat_RFP <- table(testSparsePositive$Positive, predictRFP)
cmat_RFP 

accu_RFP <- (cmat_RFP[1,1] + cmat_RFP[2,2])/sum(cmat_RFP)
accu_RFP


#Logistic Regression

#negative
tweetLogN <- glm(Negative ~ . , data = trainSparseNegative, family = "binomial")

#Positive

tweetLogP <- glm(Positive ~ . , data = trainSparsePositive, family = "binomial")


tweetLog_predict_testN <- predict(tweetLogN, type = "response", newdata = testSparseNegative)

tweetLog_predict_testP <- predict(tweetLogP, type = "response", newdata = testSparsePositive)

cmat_logRegrN <- table(testSparseNegative$Negative, tweetLog_predict_testN > 0.5)
cmat_logRegrN

accu_logRegrN <- (cmat_logRegrN[1,1] + cmat_logRegrN[2,2])/sum(cmat_logRegrN)
accu_logRegrN

cmat_logRegrP <- table(testSparsePositive$Positive, tweetLog_predict_testP > 0.5)
cmat_logRegrP

accu_logRegrP <- (cmat_logRegrP[1,1] + cmat_logRegrP[2,2])/sum(cmat_logRegrP)
accu_logRegrP


# XGB style matrices
#library(xgboost)
#dtrain <- xgb.DMatrix(train_matrix, label = target)
#dtest <- xgb.DMatrix(test_matrix)

#params <- list(eval_metric = "mlogloss",
#               objective = "multi:softprob",
#               num_class = 3, 
#               eta = 0.1,
#               gamma = 0.01,
#               max_depth = 6,
#               min_child_weight = 1,
#               subsample = 0.7,
#               colsample_bytree = 0.7)

#mod <- xgboost(params = params, 
#               data = dtrain,
#               nrounds = 500)

