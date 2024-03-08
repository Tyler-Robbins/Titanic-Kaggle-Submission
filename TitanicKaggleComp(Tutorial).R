library(randomForest)
setwd("C:/Users/tyman/Desktop/Datasets/Titanic")

trainData <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
testData <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#combine the two data files to clean them:

#Differentiate between the two datasets when combined by making a new
#column called IsTrainSet

trainData$IsTrainSet <- TRUE
testData$IsTrainSet <- FALSE

#To combine we have to ensure all headers have the same names and ensure both
#data sets have the same amount of columns.
ncol(trainData)
ncol(testData)
names(trainData)
names(testData)

#We can see that the only difference in headers is that testData does not have
#the Survived column

#Even though Survived does not exist within testData R will automatically make
#a new column and fill it with whatever value
testData$Survived <- NA


#An rbind will combine the two tables via their rows hence why we had to match columns
#and header names.

combinedData <- rbind(trainData, testData)


#We can check to ensure the two sets are properly in there using table():
table(combinedData$IsTrainSet)

#Now we deal with null values via data cleaning

#By finding the null values in this method of cleaning them we replace the null values
#with the mode (most repeated value) which in embarked is S.
combinedData[combinedData$Embarked=='', "Embarked"] <- 'S'
table(combinedData$Embarked)


#Now we must do the same for other null values in other variables.

ageMedian <- median(combinedData$Age, na.rm = TRUE)

combinedData[is.na(combinedData$Age), "Age"] <- ageMedian

table(is.na(combinedData$Age))

fareMedian <- median(combinedData$Fare, na.rm = TRUE)

combinedData[is.na(combinedData$Fare), "Fare"] <- fareMedian

table(is.na(combinedData$Fare))


#Categorical Casting
combinedData$Pclass <- as.factor(combinedData$Pclass)
combinedData$Sex <- as.factor(combinedData$Sex)
combinedData$Embarked <- as.factor(combinedData$Embarked)

str(combinedData)




#Now not all of the na values are gone but apparently this is good enough to build a predictive model!
#First step to do that is to split the cleaned data.

trainData <- combinedData[combinedData$IsTrainSet == TRUE,]
testData <- combinedData[combinedData$IsTrainSet == FALSE,]

#Turn Survived into a factor. Not entirely sure what factoring does, but apparently its important
trainData$Survived <- as.factor(trainData$Survived)


#We can now use a predictive model in this case RandomForest

survivedEquation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

survivedFormula <- as.formula(survivedEquation)

titanicModel <- randomForest(formula = survivedFormula, data = trainData, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(trainData))

survivedFeatures <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanicModel, newdata = testData)

PassengerId <- testData$PassengerId
outputDF <- as.data.frame(PassengerId)
outputDF$Survived <- Survived
tail(outputDF)


write.csv(outputDF, file="kaggle_submission.csv", row.names = FALSE)
