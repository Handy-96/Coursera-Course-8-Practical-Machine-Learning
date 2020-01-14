Title :"Practical Machine Learning"
Name :"Handy Gouwardy"
Date :"January 14,2020"
Output : html_document
---

#### Background
  Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it.


The data consists of a Training data and a Test data (to be used to validate the selected model).

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with.

Note: The dataset used in this project is a courtesy of "Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements"

#### Download Data

Training data was obtained from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

Testing data was obtained from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#### Set working directory
```{r set working directory}
setwd("D:/Handy/Coursera/Modul 8")
```

#### Load the dataset
```{r loading data}
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rattle)
set.seed(12345)
training <- read.csv("D:\\Handy\\Coursera\\Modul 8\\pml-training.csv")
testing <- read.csv("D:\\Handy\\Coursera\\Modul 8\\pml-testing.csv")
```

#### Data Cleaning
```{r data cleansing}
features <- names(testing[,colSums(is.na(testing)) == 0])[8:59]
trainclasse <- training[,c(features,"classe")]
testproblem <- testing[,c(features,"problem_id")]
```

#### Partitioning the data
```{r partitioning data}
inTrain <- createDataPartition(trainclasse$classe, p=0.7, list = FALSE)
myTraining <- trainclasse[inTrain,]
myTesting <- trainclasse[-inTrain,]
```


#### Decision Tree Prediction
```{r predict}
set.seed(12345)
DTmodel <- rpart(classe ~ ., data = myTraining, method = "class")
fancyRpartPlot(DTmodel)
DTpredict <- predict(DTmodel, myTesting, type = "class")
confusionMatrix(DTpredict, myTesting$classe)
```

#### Random Forest Prediction
```{r forest prediction}
RFmodel <- randomForest(classe ~ ., data = myTraining)
RFpredict <- predict(RFmodel, myTesting, type = "class")
confusionMatrix(RFpredict, myTesting$classe)
```

#### Since the random forest model's accuracy was 99.7%, the out of sample error is 0.003.

#### We will use the random forest model to submit our predictions.
```{r}
FinalPredict <- predict(RFmodel, testing, type = "class")
FinalPredict
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(FinalPredict)
```