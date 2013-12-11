## Kaggle Classifier
## ===================
## Shreyas <shreyas@ischool.berkeley.edu>
setwd("~/Documents/_Berkeley/sem3/stat154MachineLearning/ML-Kaggle")
train_data <- read.table('data/zip_train.txt') 

head(train_data)
summary(train_data)
boxplot(train_data)