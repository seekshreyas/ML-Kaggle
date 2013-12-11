## Kaggle Classifier
## ===================
## Shreyas <shreyas@ischool.berkeley.edu>
setwd("~/Documents/_Berkeley/sem3/stat154MachineLearning/ML-Kaggle")
train_data <- read.table('data/zip_train.txt')
test_data <- read.table('data/test_data.txt')

head(train_data)
summary(train_data)
boxplot(train_data)

names(train_data)

library(rpart)

rpartModel.train.data<-rpart(paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")),data=train_data)
quartz("Plot Tree Model on HandWritten Data")
plot(rpartModel.train.data)
text(rpartModel.train.data)

rpartModel.more.train.Data<-rpart(paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")),data=train_data)                                                                                                                                                                
quartz("Plot Tree based model for handwritten data")
plot(rpartModel.more.train.Data)
text(rpartModel.more.train.Data,pretty=TRUE)
table(train_data$V1)


rpartModel.train.pred.data<-rpart(V1~.,data=train_data)
quartz("Tree based model for Train model using SA Code")
plot(rpartModel.train.pred.data)
text(rpartModel.train.pred.data,pretty=TRUE)



# Tree Based Prediction
# ----------------------
myRpartPredictions.Proba.Pic<-predict(rpartModel.train.pred.data,newdata=train_data[,2:257])
my.digit.threshold=0.5

pic.digit<-(myRpartPredictions.Proba.Pic>my.digit.threshold)

actual.digit<-train_data[,1]

agreement.Vector<-(pic.digit==actual.digit)
length.Test.Vector<-length(actual.digit)
misClassif<-1-sum(agreement.Vector)/length.Test.Vector
misClassif
# [1] 0.7329584
accuracy <- sum(agreement.Vector)/length.Test.Vector
accuracy
# [1] 0.2670416


# Bagging
library(ipred)
baggedTree<-bagging(V1~.,data=train_data)
myBaggingPredictions.Proba.Spam<-predict(baggedTree,newdata=train_data[,2:257])
my.digit.threshold=0.5





