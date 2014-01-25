## Kaggle Classifier
## ===================

setwd("~/Documents/_Berkeley/sem3/stat154MachineLearning/ML-Kaggle")
train_data <- read.table('data/zip_train.txt')
test_data <- read.table('data/test_data.txt')

head(train_data)
summary(train_data)
boxplot(train_data)

names(train_data)


#TRAINING WITH QDA

tr_data <- read.table('data/zip_train.txt')

tr_input <- tr_data[,-1]
tr_output <- tr_data[,1]

trj_data <- tr_data
trj_data[, -1] <- apply(tr_data[, -1], 2, jitter)

trj_input <- trj_data[,-1]
trj_output <- trj_data[,1]

library(rpart)
library(ipred)
library(randomForest)
# rpartModel.tree<-rpart(formula = paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")), data=train_data)







rpartModel.tree <- rpart(formula = paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")), data = trj_data)
quartz("Plot Tree Model on HandWritten Data")
plot(rpartModel.tree)
text(rpartModel.tree)

baggedTree<-bagging(V1~.,data=train_data)
rfModel2<-randomForest(tr_input,as.factor(tr_output),ntrees=1000)




#TESTING ON TRAINING DATA
# tr_result <- predict(rpartModel.tree, tr_input)
# tr_result <- predict(baggedTree, tr_input)
tr_result <- predict(rfModel2, tr_input)

l <- length(tr_result)
error_sum <- 0
current_error <- 0
for (i in 1:l){
  
  o <- tr_output[i]
  r <- tr_result[i]
  
  if (o == r){
    current_error <- 0
  } else {
    current_error <- 1
  }
  
  error_sum <- error_sum + current_error
  
}

error_rate = error_sum / l
tr_accuracy = 1 - error_rate

tr_accuracy

#GENERATING RESPONSE ON TEST DATA

te_data <- read.table('data/test_data.txt')


#te_result <- predict(rpartModel.tree, te_data)
#te_result <- predict(baggedTree, te_data)
te_result <- predict(rfModel2, te_data)

te_result

#write.csv(te_result, file="output/baggedTree.csv")
#write.csv(te_result, file="output/rpartTreeModel.csv")
write.csv(te_result, file="output/randomForest.csv")





