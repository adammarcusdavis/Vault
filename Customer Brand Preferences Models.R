install.packages("readr")
library(readr)
library(dplyr)
library(caret)
# Read CSV into R
ExistingData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T3/original data productattributes/existingproductattributes2017.csv", header=TRUE, sep=",")
ExistingData %>% select(-c('ShippingWeight','ProductDepth','ProductWidth','ProductHeight','ProfitMargin'))
str(ExistingData)
# dummify the data
#ExistingData$ProductType <- ifelse(ExistingData$ProductType == "PC", 1, 0)
#ExistingData$ProductType <- ifelse(ExistingData$ProductType == "f", 1, 0)
# dummify the data
newDataFrame <- dummyVars(" ~ .", data = ExistingData)
readyData <- data.frame(predict(newDataFrame, newdata = ExistingData))
readyData$BestSellersRank <- NULL
corrData <- cor(readyData)
corrData
#Correlation
install.packages("corrplot")
library(corrplot)
corrplot(corrData)
#Predictions
set.seed(123)
trainSize<-round(nrow(readyData)*0.7) 
testSize<-nrow(readyData)-trainSize 
trainSize
testSize
training_indices<-sample(seq_len(nrow(readyData)),size =trainSize)
trainSet<-readyData[training_indices,]
testSet<-readyData[-training_indices,]  
Model1<-lm(Volume~ Price+Recommendproduct+PositiveServiceReview+NegativeServiceReview+ProductTypeAccessories+ProductTypeDisplay+ProductTypeExtendedWarranty+ProductTypeGameConsole+ProductTypeLaptop+ProductTypeNetbook+ProductTypePC+ProductTypePrinter+ProductTypePrinterSupplies+ProductTypeSmartphone+ProductTypeSoftware+ProductTypeTablet+x5StarReviews+x4StarReviews+x3StarReviews+x2StarReviews+x1StarReviews,trainSet)
summary(Model1)
#predictions
PredictionsName <- predict(Model1,testSet)
PredictionsName
#end of work on historical data set
#beginning of work on newer data set 

#Load Historical Data & Newer Data  
ExistingData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T3/original data productattributes/existingproductattributes2017.csv", header=TRUE, sep=",")
NewData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T3/original data productattributes/newproductattributes2017.csv", header=TRUE, sep=",")
ExistingData = na.omit(ExistingData)
#Model 1 of 3: RandomForest
#load library and set seed
library(caret)
set.seed(123)
#create a 20% sample of the data
#NewData <- NewData[sample(nrow(NewData), floor(0.2 * nrow(NewData))), ]
#NewData <- NewData[sample(1:nrow(NewData), 7,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(ExistingData$Volume, p = 0.80, list = FALSE)
training <- ExistingData[inTraining,]
testing <- ExistingData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
rfFit2 <- train(Volume~., data = ExistingData, method = "rf", trControl=fitControl, tuneLength = 2)
#training results
rfFit2

#Predictions
PredictionsName <- predict(rfFit2,NewData)
PredictionsName
Output = NewData
Output$volume <- PredictionsName

write.csv(Output, file="rfoutput.csv", row.names = TRUE)

#Model 2 of 3: GradientBoosting
#load library and set seed
library(caret)
set.seed(123)
#create a 20% sample of the data
#NewData <- NewData[sample(nrow(NewData), floor(0.2 * nrow(NewData))), ]
#NewData <- NewData[sample(1:nrow(NewData), 7,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(ExistingData$Volume, p = 0.80, list = FALSE)
training <- ExistingData[inTraining,]
testing <- ExistingData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
gb <- train(Volume~., data = ExistingData, method = "gbm", trControl=fitControl, tuneLength = 2)
#training results
gb

#Predictions
PredictionsName <- predict(gb,NewData)
PredictionsName
Output = NewData
Output$volume <- PredictionsName

write.csv(Output, file="gboutput.csv", row.names = TRUE)



#Model 3 of 3: SupportVectorMachine
#load library and set seed
library(caret)
set.seed(123)
#create a 20% sample of the data
#NewData <- NewData[sample(nrow(NewData), floor(0.2 * nrow(NewData))), ]
#NewData <- NewData[sample(1:nrow(NewData), 7,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(ExistingData$Volume, p = 0.80, list = FALSE)
training <- ExistingData[inTraining,]
testing <- ExistingData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
svm <- train(Volume~., data = ExistingData, method = "svmRadial", trControl=fitControl, tuneLength = 2)
#training results
svm

#Predictions
PredictionsName <- predict(svm,NewData)
PredictionsName
Output = NewData
Output$volume <- PredictionsName

write.csv(Output, file="svmoutput.csv", row.names = TRUE)




