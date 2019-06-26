setwd("/home/lim/Documents/Coursera R/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

library(dplyr)
library("data.table")

#extract mean and standard deviations from test set
xtest <- read.table("test/X_test.txt")
colnames(xtest) <- read.table("features.txt")[,2]
xtest <- xtest[grep("mean\\(\\)|std", colnames(xtest))]
subject_test <- read.table("test/subject_test.txt")
colnames(subject_test) <- "subject"

ytest <- read.table("test/y_test.txt", col.names = "label")
test <- cbind(xtest, ytest)
test <- cbind(test, subject_test)


#extract mean and standard deviations from training set
xtrain <- read.table("train/X_train.txt")
colnames(xtrain) <- read.table("features.txt")[,2]
xtrain <- xtrain[grep("mean\\(\\)|std", colnames(xtrain))]

subject_train <- read.table("train/subject_train.txt")
colnames(subject_train) <- "subject"

ytrain <- read.table("train/y_train.txt", col.names = "label")
train <- cbind(xtrain, ytrain)
train <- cbind(train, subject_train)


#combine test and train set
Combined_Data <- rbind(test, train)

#function for labeling the data set with descriptive activity names
label_maker <- function(x) {
  if(x==1) {x <- "Walking" }
  else if(x==2) {x <- "Walking_Upstairs" }
  else if(x==3) {x <- "Walking_Downstairs"}
  else if(x==4) {x <- "Sitting"}
  else if(x==5) {x <- "Standing"} 
  else if(x==6) {x <- "Laying"}
}

#create labels in data set
Combined_Data$Descriptive_Label <- sapply(Combined_Data$label, label_maker)


#Creating average by subject and activity
Average_data <- data.table(Combined_Data)
Average_data <- setDT(Average_data)[, lapply(.SD, mean), by = list(Descriptive_Label, label, subject)]
Average_data <- arrange(Average_data, label, subject)
