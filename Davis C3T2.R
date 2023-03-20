install.packages("caret", dependencies = c("Depends", "Suggests"))
Yes
Yes
library(caret)
library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,p = .75,list = FALSE)
str(inTrain)

training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
nrow(training)
nrow(testing)

plsFit <- train(Class ~ .,data = training,method = "pls",preProc = c("center", "scale"))

ctrl <- trainControl(method = "repeatedcv", repeats = 3,classProbs = TRUE, summaryFunction = twoClassSummary)
plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))
plsFit

plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)

plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

plot(plsFit)

confusionMatrix(data = plsClasses, testing$Class)

rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,data = training,method = "rda",tuneGrid = rdaGrid,trControl = ctrl,metric = "ROC")
rdaFit

rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

diffs <- diff(resamps)
summary(diffs)

xyplot(resamps, what = "BlandAltman")

##End of tutorial 

##First Example: caret model - Automatic Tuning Grid Random Forest

#load library and set seed
library(caret)
set.seed(998)
# Read CSV into R
WholeYearData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T2/WholeYear.csv", header=TRUE, sep=",")
#create a 20% sample of the data
WholeYearData <- WholeYearData[sample(1:nrow(WholeYearData), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYearData$SolarRad, p = .75, list = FALSE)
training <- WholeYearData[inTraining,]
testing <- WholeYearData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Random Forest Regression model with a tuneLenght = 1 (trains with 1 mtry value for RandomForest)
rfFit1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 1)
#training results
rfFit1


##Second Example: caret model - Manual Grid Random Forest 
#load library and set seed
library(caret)
set.seed(998)
#create a 20% sample of the data
WholeYearData <- WholeYearData[sample(1:nrow(WholeYearData), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYearData$SolarRad, p = .75, list = FALSE)
training <- WholeYearData[inTraining,]
testing <- WholeYearData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model with a tuneLenght = 2 (trains with 2 mtry values for RandomForest)
rfFit2 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)
#training results
rfFit2


##Third Example: caret model - Random Search Random Forest  
#load library and set seed
library(caret)
set.seed(998)
#create a 20% sample of the data
WholeYearData <- WholeYearData[sample(1:nrow(WholeYearData), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYearData$SolarRad, p = .75, list = FALSE)
training <- WholeYearData[inTraining,]
testing <- WholeYearData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#dataframe for manual tuning of mtry
rfGrid <- expand.grid(mtry=c(1,2,3))
#train Random Forest Regression model
#note the system time wrapper. system.time()
#this is used to measure process execution time 
system.time(rfFitm1 <- train(SolarRad~., data = training, method = "rf", trControl=fitControl, tuneGrid=rfGrid))
#training results
rfFitm1


##Fourth Example: caret model - Random Search Random Forest 
#load library and set seed
library(caret)
set.seed(998)
#create a 20% sample of the data
WholeYearData <- WholeYearData[sample(1:nrow(WholeYearData), 7000,replace=FALSE),]
#define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYearData$SolarRad, p = .75, list = FALSE)
training <- WholeYearData[inTraining,]
testing <- WholeYearData[-inTraining,]
#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')
#train Random Forest Regression model
rfFitr2 <- train(SolarRad~., data = training, method = "rf", trControl=rfitControl)
#training results
rfFit2

##Fifth Example: caret model - Automatic Grid Linear Model
library(caret)
set.seed(998)
#create a 20% sample of the data
WholeYearData <- WholeYearData[sample(1:nrow(WholeYearData), 7000,replace=FALSE),]
# define an 75%/25% train/test split of the dataset
inTraining <- createDataPartition(WholeYearData$SolarRad, p = .75, list = FALSE)
training <- WholeYearData[inTraining,]
testing <- WholeYearData[-inTraining,]
#10 fold cross validation
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)
#train Linear Regression model
LMFit1 <- train(SolarRad~., data = training, method = "lm", trControl=fitControl)
#check the results
LMFit1


#Next Steps with Survey Data
# Read CSV into R
SurveyData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T2/SurveyData/CompleteResponses.csv", header=TRUE, sep=",")
library(caret)
str(SurveyData)
#Preprocessing: convert values to factors 
SurveyData$brand=as.factor(SurveyData$brand)

#Models 1: RandomForest
inTraining <- createDataPartition(SurveyData$brand, p = .75, list = FALSE)
training <- SurveyData[inTraining,]
testing <- SurveyData[-inTraining,]
#10 fold cross validation
rfitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')
#train Random Forest Regression model
rfFitr2 <- train(brand~., data = training, method = "rf", trControl=rfitControl)
#training results
rfFitr2

#First Model
var_imp <- varImp(rfFitr2)
var_imp

#Install C5 Library 
install.packages("C50")
library(C50)

install.packages("C50", repos="http://R-Forge.R-project.org")
install.packages("inum")


#Model 2: C5
inTraining <- createDataPartition(SurveyData$brand, p = .75, list = FALSE)
training <- SurveyData[inTraining,]
testing <- SurveyData[-inTraining,]
#10 fold cross validation
C5fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, search = 'random')
#train Random Forest Regression model
C5Fitr2 <- train(brand~., data = training, method = "C5.0", trControl=rfitControl)
#training results
C5Fitr2

#Load Complete Data Set & Incomplete Data Set 
IncompleteData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T2/SurveyData/SurveyIncomplete.csv", header=TRUE, sep=",")
SurveyData <- read.csv("/Users/adamdavis/Desktop/Data Analytics Program/P3/P3T2/SurveyData/CompleteResponses.csv", header=TRUE, sep=",")

#Predictions
set.seed(123)
trainSize<-round(nrow(SurveyData)*0.7) 
testSize<-nrow(SurveyData)-trainSize 
trainSize
testSize
training_indices<-sample(seq_len(nrow(SurveyData)),size =trainSize)
trainSet<-SurveyData[training_indices,]
testSet<-SurveyData[-training_indices,]  
Model1<-lm(brand~ salary+age+zipcode+credit,trainSet)
summary(Model1)
#predictions
PredictionsName <- predict(Model1,testSet)
PredictionsIncomplete <- predict(Model1, IncompleteData)
postResample(PredictionsIncomplete,IncompleteData$brand)
postResample(PredictionsName,testSet$brand)
#Summary
summary(PredictionsIncomplete)
summary(Model1)