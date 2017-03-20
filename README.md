Practical-Machine-Learning-CourseCapstone
=============================

Capstone Project: Peer-graded Assignment: Prediction Assignment Writeup

### Summary
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the  goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. It may be used any of the other variables to predict with.

****

### Import the data
Firstly, The necessary R packages are loaded and then, the training and testing data set are downloaded from the web locations.
library(caret)
```r
setwd(
  "D:/pentaho/design-tools/aggregation-designer/Aggregation Designer.app/Contents/MacOS/WinSCP/Extensions/MySQL/"
)
trainRaw <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),na.strings=c('#DIV/0', '', 'NA') ,stringsAsFactors = F)
testRaw <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),na.strings=c('#DIV/0', '', 'NA') ,stringsAsFactors = F)
```
#### Data preparation
All the columns that do not add value to the training are deleted.
Also, the "timestamp"", "Window"" and other columns are deleted since they do not are valid for the training

```r
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
```


#### Training
The training method was Random Forest (Other method perfomance were very poor).

```r
set.seed(96780)
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]
control <- trainControl(method = "cv", number = 10)


fit_rf <- train(classe ~ ., data = trainData, method = "rf", 
                   trControl = control, metric="Kappa")
```

***
### Model accuracy
The Random Forest accuracy seems to be pretty high in the training data set.

```r
print(fit_rf, digits = 4)
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## Summary of sample sizes: 12363, 12362, 12364, 12361, 12363, 12365, ... 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa 
##    2    0.9904    0.9878
##   27    0.9917    0.9895
##   52    0.9849    0.9809
## 
## Kappa was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27. 
```

#### Prediction
Finally, the prediction is obtained from the test data set.

```r
(predict(fit_rf, testCleaned))
## [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
