########################################################################
########################################################################
# Course Project
#https://rpubs.com/cheyu/pmlProject
#https://github.com/daniellillja/Practical-Machine-Learning-08
#https://yoke2.github.io/PMLCourseProject/pmlreport.html
#http://rstudio-pubs-static.s3.amazonaws.com/204733_799e533a7ae348b480a6fff023d59954.html

library(caret)
setwd(
  "D:/pentaho/design-tools/aggregation-designer/Aggregation Designer.app/Contents/MacOS/WinSCP/Extensions/MySQL/"
)

trainRaw <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),na.strings=c('#DIV/0', '', 'NA') ,stringsAsFactors = F)
testRaw <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),na.strings=c('#DIV/0', '', 'NA') ,stringsAsFactors = F)
sum(complete.cases(trainRaw))




trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0] 
testRaw <- testRaw[, colSums(is.na(testRaw)) == 0] 
classe <- trainRaw$classe

trainRemove <- grepl("^X|timestamp|window", names(trainRaw))
trainRaw <- trainRaw[, !trainRemove]
trainCleaned <- trainRaw[, sapply(trainRaw, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testRaw))
testRaw <- testRaw[, !testRemove]
testCleaned <- testRaw[, sapply(testRaw, is.numeric)]

set.seed(96780)
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]
control <- trainControl(method = "cv", number = 10)


fit_rf <- train(classe ~ ., data = trainData, method = "rf", 
                   trControl = control, metric="Kappa")
print(fit_rf, digits = 4)

xgb.save(fit_rf, "fit_rf.rda")

setwd("C:/Users/nelson.pedernera/Desktop/BigData/DataScience/MVII - Practical Machine Learning/04/Course Project/")
load("fit_rf.rda")

(predict(fit_rf, testCleaned))
# [1] B A B A A E D B A A B C B A E E A B B B
# Levels: A B C D E

##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
