####train data ####
train_data <- read.table('data/zip_train.txt') #read the entire training data


#this is for splitting the data set into training data of individual digits.

train_data_segmented <- list()

for(i in seq(0,9))
{
  train_data_segmented = c(train_data_segmented,list(which(train_data[,1]==i)))
}

#need to make more changes here., currently not working
#idea is to split data set by the class label and then split each into 90:10 ratio
#and use this split to do cross validation/ train error 
#and then use the final model on the actual test data

# other option would be to use the individual data sets available for each categoy and use them 

# for(i in seq(1,length(train_data_segmented)))
# {
#   no_of_rows <- as.numeric(length(train_data_segmented[[i]]))
#   no_of_train <- as.integer(no_of_rows*0.9)
#   no_test <-no_of_rows - no_of_train
#   train_indices <- train_data_segmented[[i]][1:no_of_train]
#   a =train_data[train_indices,]
#   train_x <- c(train_x, a)   
# }
train_y <- train_data[,1]

train_x <- train_data[,-1]


#####Test data#############

test_data <- read.table("zip.test", sep =" ") # read the test data


#select the first column of test data which denotes the class labels and save them in a list
test_y <- test_data[,1]
test_x <- test_data[,-1]


###############knn#########
library("FNN")


#function to run knn and get the predictions and error
predict_and_error <- function(k)
{
  
  #run knn on train set ; so specify test =null  
  result_train <- knn.reg(train_x,test=NULL,train_y,k = k)
  #error - PRESS gives the sum of res. squares
  train_error <- (result_train$PRESS)/length(train_y)
  
  #test - do the same for test
  result_test <- knn.reg(train_x,test_x,train_y,k =k)
  
  #get the prefictions
  predictions_test <-result_test$pred
  
  res <- 0
  #   
  #   #get the sum of squares of residuals
  #   for(i in 1:length(predictions_test))
  #   {
  #     res <- res + ( (test_y[i] - predictions_test[i]) ^2)
  #   }
  #   
  #   
  #   #get the error
  #   reg_error2 <- res/length(test_y)
  #   res <- 0
  
  #get the misclassification error.
  res <- mean(predictions_test != test_y)
  
  #return it
  #instead of printing it. so that we can plot it later. 
  # TO DO - later.
  print(k)
  print (reg_error1)
  print (reg_error2)
  print("##########")
  
}

#apply method on this list which denotes k values
kvals <- c(1,3,5,7,15)
sapply(kvals,predict_and_error)
