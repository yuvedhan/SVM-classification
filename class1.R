## data: https://s3.amazonaws.com/capitalbikeshare-data/index.html

library(e1071)
library(rpart)
library(lattice)
library(ggplot2)
library(caret)

##settingup the working directory
setwd("C:/Users/Yuvedhan/Desktop/bike share data")   
# getwd()


## reading the .csv file as a data frame into the variable 'bike'
bikes <- read.csv("C:/Users/Yuvedhan/Desktop/Project/New Folder/2016-Q1-Trips-History-Data.csv", stringsAsFactors=FALSE)


## Since there are only two outcomes for the Memeber Type
## Registered is treated as 0
## And casual is treated as 1
bikes$Member.Type <- gsub("Registered", 1, bikes$Member.Type)
bikes$Member.Type <- gsub("Casual", 0, bikes$Member.Type)

## Subsetting all the Zeros
zeros <- subset.data.frame(bikes, bikes$Member.Type == 0)

## Subsetting only the predictor and the Response
zeros_x <- subset.data.frame(zeros, select = c ("Duration", "Member.Type"))

## randomly selecting 10000 data points
zeros_random <- zeros_x[sample(nrow(zeros_x), 10000), ]

## Subsetting all the ones
ones <- subset.data.frame(bikes, bikes$Member.Type == 1)

## Subsetting only the predictor and the Response
ones_x <- subset.data.frame(ones, select = c ("Duration", "Member.Type"))


## randomly selecting 10000 data points
ones_random <- ones_x[sample(nrow(ones_x), 10000), ]

## Now the data is balanced with equal numbers of zeros and ones

## Appending the two data frames 
total <- rbind.data.frame(zeros_random, ones_random)

## Splitting the data into Training data and Test data
## 75% of the sample size
smp_size <- floor(0.75 * nrow(total))

## set the seed to make your partition reproductible
set.seed(123)
train_t <- sample(seq_len(nrow(total)), size = smp_size)

train <- total[train_t, ]
test <- total[-train_t, ]

train_y <- as.data.frame((train$Member.Type))
train_x <- as.data.frame(subset(train, select = c("Duration")))
train_x <- as.data.frame(as.numeric(unlist(train_x)))
train_y <- as.data.frame(as.numeric(unlist(train_y)))


test_y <- as.data.frame((test$Member.Type))
test_x <- as.data.frame(subset(test, select = c("Duration")))
train_x <- as.data.frame(as.numeric(unlist(train_x)))
train_y <- as.data.frame(as.numeric(unlist(train_y)))

## Fitting the model using the training data
model <- svm (train_x, train_y)

summary(model)

## Predicting the model using the Test Data
prediction <- predict(model, test_x)
round = round(prediction - 1)


## Calculating the acuracy
sum((round == test_y)*1)/dim(test_y)

      
## Creating confusion matrix
confusionMatrix(round, unlist(test_y), positive = NULL,
                dnn = c("Prediction", "Reference"), prevalence = NULL,
                mode = "sens_spec")

############################################################################################
