# =====================================================================================
# Assignment for Getting and Cleaning Data
# Author: Massimo Noro
# =====================================================================================

#Background Info: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Data: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
library(readr)
library(tidyr)
library(dplyr)

# =====================================================================================
#1. Merges the training and the test sets to create one data set. -- Test successful
# =====================================================================================
# Read data
#
# -- Test data
#
X_test <- read_table("Data/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)
# Reads the subject information
subject_test <- read_csv("Data/UCI HAR Dataset/test/subject_test.txt", col_names = FALSE)
# Reads the Activity
Y_test <- read_csv("Data/UCI HAR Dataset/test/y_test.txt", col_names = FALSE)
# Add subject and activity column
X_test$subject <- subject_test$X1
X_test$activity <- Y_test$X1
#
# -- Train data
#
X_train <- read_table("Data/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
# Adds the subject information
subject_train <- read_csv("Data/UCI HAR Dataset/train/subject_train.txt", col_names = FALSE)
# Reads the Activity
Y_train <- read_csv("Data/UCI HAR Dataset/train/y_train.txt", col_names = FALSE)
# Adds subject and activity column
X_train$subject <- subject_train$X1
X_train$activity <- Y_train$X1

#
# Appends the two datasets
#
X <- bind_rows(X_test, X_train)
head(X)

# =====================================================================================
#1.a - Assign measure/feature labels -- Test Successful
# =====================================================================================
features_labels <- read.table("Data/UCI HAR Dataset/features.txt", 
                quote="\"", comment.char="", stringsAsFactors = FALSE)
# Renames the measures/variables from 1 to 561
names(X)[1:561] <- features_labels[,2]

# =====================================================================================
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
# =====================================================================================
#
# select the variables/columns containing mean and std and activity and subject
list_columns <- grepl("mean|std|subject|activity",names(X), ignore.case = TRUE)
# Extracts only those columns
X1 <- X[, list_columns]

# =====================================================================================
#3. Uses descriptive activity names to name the activities in the data set -- Test Successful
# =====================================================================================

# Imports activity Labels
activity_labels <- read_table("Data/UCI HAR Dataset/activity_labels.txt", 
                              col_names = FALSE)
names(activity_labels) <- c("ActivityId", "ActivityLabel")
#
X1$activity <- factor(X1$activity,
                      levels = activity_labels$ActivityId,
                      labels = activity_labels$ActivityLabel)

# 
# =====================================================================================
#4. Appropriately labels the data set with descriptive variable names. -- Test Successful
# reads the description from features.txt and use it 
# =====================================================================================
# Change a copy of the Variable names
list_variables <- names(X1)

list_variables <- sapply(list_variables,function(x){gsub("^f","frequency_",x)})
list_variables <- sapply(list_variables,function(x){gsub("^t","time_",x)})
list_variables <- sapply(list_variables,function(x){gsub("Acc","Accelerometer",x)})
list_variables <- sapply(list_variables,function(x){gsub("Gyro","Gyroscope",x)})
list_variables <- sapply(list_variables,function(x){gsub("Mag","Magnitude",x)})
list_variables <- sapply(list_variables,function(x){gsub("std","StandardDeviation",x)})
list_variables <- sapply(list_variables,function(x){gsub("Freq","Frequency",x)})
list_variables <- sapply(list_variables,function(x){gsub("mean","Mean",x)})
list_variables <- sapply(list_variables,function(x){gsub("tBody","time_Body",x)})
# duplicate
list_variables <- sapply(list_variables,function(x){gsub("BodyBody","Body",x)})

#
# =====================================================================================
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# =====================================================================================
X2 <- X1 %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))

# Write
write.table(X2, file = "tidy_data.txt", row.names = FALSE)
