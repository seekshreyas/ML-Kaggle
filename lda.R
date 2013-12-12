require(MASS)



#TRAINING WITH LDA

tr_data <- read.table('data/zip_train.txt')

tr_input <- tr_data[,-1]
tr_output <- tr_data[,1]

lda_model <- lda(formula = paste("V1 ~", paste("V",2:257,sep="",collapse=" + ")), data = tr_data, x = tr_input, grouping = tr_output)



#TESTING ON TRAINING DATA

tr_result <- predict(lda_model, tr_input)$class

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

te_result <- predict(qda_model, te_data)$class

write.csv(te_result, file="data/qda_results.csv")