## run_analysis.R
##
## Bob George  https://github.com/rwgeorge
###############################################################################
## Getting & Cleaning Data (getdata-012)
## Course Project
## https://github.com/rwgeorge/GettingAndCleaningDataCourseProject.git
###############################################################################
##
## The purpose of this project is to demonstrate your ability to collect, 
## work with, and clean a data set. The goal is to prepare tidy data that can 
## be used for later analysis. You will be graded by your peers on a series of 
## yes/no questions related to the project. You will be required to submit: 
## 1) a tidy data set as described below, 
## 2) a link to a Github repository with your script for performing the 
##    analysis, and 
## 3) a code book that describes the variables, the data, and any 
##    transformations or work that you performed to clean up the data called 
##    CodeBook.md. 
## You should also include a README.md in the repo with your scripts. 
## This repo explains how all of the scripts work and how they are connected.
##
###############################################################################




## 0. Get the data files.
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for 
##    each measurement. 
## 3. Use descriptive activity names to name the activities in the data set.
## 4. Appropriately label the data set with descriptive variable names. 
## 5. From the data set in step 4, create a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)
library(lubridate)

###############################################################################
## 0. Get the data files.
###############################################################################

downloadUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataDir <- "UCI_HAR_Dataset/"
dataFile <- paste0(dataDir, ".zip")

if (!file.exists(dataDir)) {
    if (!file.exists(dataFile)) {
        download.file(downloadUrl, dataFile, method="curl")
        dataFileDate <- today()  
    }
    
    unzip(dataFile)
}


###############################################################################
## 1. Merge the training and the test sets to create one data set.
###############################################################################

## Load Test Data
###############################################################################
testDir <- paste0(dataDir, "test/")
testSignalsDir <- paste0(testDir, "Inertial Signals/")

testSubjectDataFile <- paste0(testDir, "subject_test.txt")
testSubjectData <- tbl_df(read.delim(testSubjectDataFile, header = FALSE, sep = "", as.is = TRUE))

testXDataFile <- paste0(testDir, "X_test.txt")
testXData <- tbl_df(read.delim(testXDataFile, header = FALSE, sep = "", as.is = TRUE))

testYDataFile <- paste0(testDir, "y_test.txt")
testYData <- tbl_df(read.delim(testYDataFile, header = FALSE, sep = "", as.is = TRUE))

## Load Training Data
###############################################################################
trainDir <- paste0(dataDir, "train/")
trainSignalsDir <- paste0(trainDir, "Inertial Signals/")

trainSubjectDataFile <- paste0(trainDir, "subject_train.txt")
trainSubjectData <- tbl_df(read.delim(trainSubjectDataFile, header = FALSE, sep = "", as.is = TRUE))

trainXDataFile <- paste0(trainDir, "X_train.txt")
trainXData <- tbl_df(read.delim(trainXDataFile, header = FALSE, sep = "", as.is = TRUE))

trainYDataFile <- paste0(trainDir, "y_train.txt")
trainYData <- tbl_df(read.delim(trainYDataFile, header = FALSE, sep = "", as.is = TRUE))

## Merge Train and Test Files
###############################################################################
allSubjectData <- bind_rows(testSubjectData, trainSubjectData)
allXData <- bind_rows(testXData, trainXData)
allYData <- bind_rows(testYData, trainYData)

## Merge All Data Into One File
###############################################################################
subjectCol <- select(mutate(allSubjectData, Subject = V1), Subject)
activityCol <- select(mutate(allYData, Activity = V1), Activity)

combinedData <- cbind(subjectCol, activityCol, allXData)


###############################################################################
## 2. Extract only the measurements on the mean and standard deviation for 
##    each measurement.
###############################################################################

## Load Feature Data
###############################################################################
featuresFile <- paste0(dataDir, "features.txt")
featuresData <- tbl_df(read.delim(featuresFile, header = FALSE, sep = "", as.is = TRUE))

## Determine Desired Features
###############################################################################
desiredFeatures <- filter(featuresData, grepl("mean|std", V2))
featuresNumbers <- select(desiredFeatures, V1)$V1

combinedVData <- select(combinedData, V1:V561)

desiredData <- combinedVData
names(desiredData) <- extract_numeric(names(desiredData))
desiredData <- select(desiredData, featuresNumbers)
desiredData <- cbind(subjectCol, activityCol, desiredData)

###############################################################################
## 3. Use descriptive activity names to name the activities in the data set.
###############################################################################

## Load Activities
###############################################################################
activitiesFile <- paste0(dataDir, "activity_labels.txt")
activitiesData <- tbl_df(read.delim(activitiesFile, header = FALSE, sep = "", as.is = TRUE))

desiredData <- select(mutate(merge(desiredData, activitiesData, by.x = "Activity", by.y = "V1", all = TRUE), Activity = V2), -V2)


###############################################################################
## 4. Appropriately label the data set with descriptive variable names.
###############################################################################
numberedColumns <- select(desiredData, -Activity, -Subject)
names(numberedColumns) <- desiredFeatures$V2

desiredData <- cbind(subjectCol, select(desiredData, Activity), numberedColumns)
desiredData <- tbl_df(desiredData)

###############################################################################
## 5. From the data set in step 4, create a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.
###############################################################################
tidyData <- group_by(desiredData, Activity, Subject)
tidyData <- summarise_each(tidyData, funs(mean), 3:81)



# write.table(tidyData, file="tidy_data.txt", row.names = FALSE)


