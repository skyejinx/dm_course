# SVM, Logit & KNN with caret
# use credit data from AER package
#install.packages("AER")

setwd(PATH)
# load packages
library(AER)
library(caret)
# svm
library(kernlab)
# ROC curve 
library(pROC)	

library(mlbench)
data(BostonHousing)

str(BostonHousing)
summary(BostonHousing)

# exploratory analysis
library(ggplot2)


# factors to numeric
cols = c(7, 8)  
for (col in cols) {
  BostonHousing[[col]] = as.numeric(BostonHousing[[col]])
}

# remove share variable
BostonHousing$share = NULL

library(caret)
trainInd = createDataPartition(BostonHousing$crim,
                               p=.75, 
                               list = FALSE)

## Models
trainData = BostonHousing[trainInd,]
trainX = trainData[, names(trainData) != "crim"]
testData = BostonHousing[-trainInd,]
testX = testData[, names(testData) != "crim"]

# setting the resampling method
ctrl = trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=2,		    # do 3 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)


########################### SVM ###########################
svmFit = train(x=trainX,
                y = trainData$card,
                method = "svmRadial",   # Radial kernel
                tuneLength = 5,					# try 9 different parameters
                preProc = c("center","scale"),  # Center and scale data
                metric ="ROC",
                trControl = ctrl)

# the resulting model
svmFit$finalModel
plot(svmFit)

# predict on the test data
testSVM = predict(svmFit, newdata = testX)
testSVMProb = predict(svmFit, newdata= testX, type = "prob")

# analyze model results
confusionMatrix(data = testSVM, testData$card)

# ROC
svmROC <- roc(predictor = testSVMProb$yes,
              response = testData$card,
              levels = rev(levels(testData$card)))
plot(svmROC, type = "S", col = "red")



library(nnet)
########################### neural network ###########################
# setting the resampling method
set.seed(100)

# training nnet model
nnetFit = train(x=trainX,
                y = trainData$card,
                method = "nnet",   
                tuneGrid=expand.grid(.decay = 0.5, .size = 3),  #3 vs 30
                preProc = c("center","scale"),  # Center and scale data
                metric ="ROC",
                trControl = ctrl)

# the resulting model
nnetFit$finalModel
plot(nnetFit)

# predict on the test data
testNNET = predict(nnetFit, newdata = testX)
testNNETProb = predict(nnetFit, newdata= testX, type = "prob")
trainNNET= predict(nnetFit, newdata = trainX)

# analyze model results
confusionMatrix(data = testNNET, testData$card)

#compare training error vs test error
confusionMatrix(data = testNNET, testData$card)$overall[1]
confusionMatrix(data = trainNNET, trainData$card)$overall[1]


# ROC
nnetROC <- roc(predictor = testNNETProb$yes,
              response = testData$card,
              levels = rev(levels(testData$card)))
plot(nnetROC, type = "S", col = "red")

#plot both svm roc and nnet roc
plot(nnetROC, type = "S", col = "red")
plot(svmROC, add = TRUE, col = "blue")
legend("bottomright", legend=c("nnet", "svm"),
       lty=1:1, cex=0.8,col=c("red", "blue"))


###############################################################

# logistic regression
lmFit = train(card ~ ., 
             data = trainData, 
             method = "glm", # linear model
             family = "binomial",
             maxit = 100,
             preProcess = c("center","scale"),
             metric ="ROC",
             trControl = ctrl)
# warnings ~ linearly separated data

# resulting model
lmFit$finalModel

#predict on the test data
testLM = predict(lmFit, newdata = testData)
testLMProb = predict(lmFit, newdata = testData, type = "prob")

# analyze model results
confusionMatrix(data = testLM, test$card)
# ROC
lmROC <- roc(predictor = testLMProb$yes,
             response = testData$card,
             levels = rev(levels(testData$card)))

plot(lmROC, type = "S", col = "blue")
# add ROC from SVM
plot(svmROC, add = TRUE, col = "red")

# KNN
knnFit <- train(card ~ ., 
                data = trainData, 
                method = "knn", 
                trControl = ctrl, 
                metric ="ROC",
                preProcess = c("center","scale"), 
                tuneLength = 9)

# resulting model
knnFit$finalModel
plot(knnFit)

#predict
testKNN = predict(knnFit, newdata = testData)
testKNNProb = predict(knnFit, newdata=testData, type = "prob")

# analyze the results
confusionMatrix(data = testKNN, testData$card)

# ROC
knnROC = roc(predictor = testKNNProb$yes,
             response = testData$card,
             levels = rev(levels(testData$card)))

plot(knnROC, type = "S", col = "green")
plot(svmROC, add = TRUE, col = "red")
plot(lmROC, add = TRUE, col = "blue")

