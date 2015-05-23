### ########################## 
### Author: Parveen Sharma
### Date: May 23, 2015
### Program Assignment
### Coursera: Getting and Cleaning Data
### ##########################

### Instructions
# The data linked to from the course website represent data collected from the 
# accelerometers from the Samsung Galaxy S smartphone.
# A full description is available at the site where the data was obtained: 
# 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### ##########################


library(reshape2)
library(sqldf)
library(lubridate)
library(dplyr)
library(tidyr)
library(plyr)

require(reshape2)
require(sqldf)
require(lubridate)
require(dplyr)
require(tidyr)
require(plyr)

# working directory - wth unzipped data files
setwd("C:/Users/Parveen/Desktop/r-prog/Getting_and_Cleaning_Data/week3/UCI HAR Dataset/")

#step01 read data

collateData <- function(act_num, act_lbl, act_who){
  # Provide data location for following  
  # act_num: Activity Numbers; e.g. "train/X_train.txt"
  # act_lbl: Defines the Activity identifier (and would help connect actual Activity label); e.g. "train/y_train.txt"
  # act_who: Defines who within group of 30 volunteers, performed the above activity ('act_lbl'), and generated numbers ('act_num'); e.g. "train/subject_train.txt"
  
  ### Read commmon data ### Overall Actitivites and their Labels
  labels=read.table("activity_labels.txt", stringsAsFactors=FALSE, header=FALSE, as.is=T)
  colnames(labels) <- c("label","activity")
  
  features=read.table("features.txt", stringsAsFactors=FALSE, header=FALSE, as.is=T)
  colnames(features) <- c("index","name")
  ### -- There are 84 duplicate names
  ### -- Are these even important for this exercise? Don't think so!!
  
  
  # Remaining calculations to be done within the function
  
  activity.num <- read.table(act_num, stringsAsFactors=FALSE, header=FALSE, as.is=TRUE)
  colnames(activity.num) <- features$name 
  ### Read data and deine column names correctly
  
  activity.lbl <- read.table(act_lbl, stringsAsFactors=FALSE, header=FALSE)
  ### Identifies performed activity
  
  activity.who <- read.table(act_who, stringsAsFactors=FALSE, header=FALSE)
  ### Identifies subject who carried out the experiment
  
  activity.id <- cbind(activity.lbl, activity.who)
  colnames(activity.id) <- c("label","subject")
  
  activity.wrk <- cbind(activity.num, activity.id) 
  ### Attach both identifiers to activity data

  activity.all <- merge(activity.wrk, labels, by="label", all.x=TRUE)
  activity.all$what <- "train"
  ### Additional identifier: to help identify training or test dataset
  ### Not required for curent exercise
  
  activity.all$group_var <- paste(activity.all$subject, activity.all$activity,sep="-" )
  ### Additional identifier: for grouping dataset and creating summary statistics
  ### Required for current exercise

  data.frame(activity.all)
}

trainData <- collateData("train/X_train.txt", "train/y_train.txt", "train/subject_train.txt")
testData <- collateData("test/X_test.txt", "test/y_test.txt", "test/subject_test.txt")

allData <- rbind(trainData,testData)

###########

### Identify columns to keep for processing
colList <- colnames(allData)
### [563] "subject"
### [564] "activity"                       
### [565] "what"  
### [566] "group_var"

### identify index of selected columns
cols.mean = grep("mean", colList)
cols.std = grep("std", colList)
cols.com <- c(cols.mean, cols.std)
cols.com = sort(cols.com)

cols.select = c(566,cols.com) ### List of columns to be used for aggregation

allData.use <- allData[, cols.select]

allData.mean <- aggregate(allData.use, list(agg_group = allData.use$group_var), mean)
allData.mean$group_var <- NULL

# library(reshape2) #function::colsplit 
allData.mfinal <- data.frame(allData.mean, colsplit(as.character(allData.mean$agg_group), pattern = "-", names = c("subject","activity")))
allData.final <- allData.mfinal[, c(81,82,2:80)]

## -- for check only
## wrk <- mydata.train %>%
##   filter(subject==11 & activity.name=="LAYING")
## write.csv(wrk,"wrk_check.csv")
## checked - manual - Agggregate seeems to be working fine

allData.final2 <- arrange(allData.final,subject,activity)
## arrange function, plyr package

write.table(allData.final2,"output-submit/tidy_data_submit.txt", col.names=FALSE, row.names=FALSE)
## write output
