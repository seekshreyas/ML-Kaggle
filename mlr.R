require(MASS)
require(nnet)



#TRAINING WITH MULTINOMIAL LOGISTIC REGRESSION

tr_data <- read.table('/Users/rob/Documents/ML-Kaggle/data/zip_train.txt')

tr_input <- tr_data[,-1]
tr_output <- tr_data[,1]

mlr_model <- multinom(formula = paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")), data = tr_data, MaxNWts = 2581)



#TESTING ON TRAINING DATA

tr_result <- predict(mlr_model, tr_input)

l <- length(result)
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

te_data <- read.table('/Users/rob/Documents/ML-Kaggle/data/test_data.txt')

te_result <- predict(mlr_model, te_data)

write.csv(te_result, file="/Users/rob/Documents/ML-Kaggle/data/mlr_results.csv")