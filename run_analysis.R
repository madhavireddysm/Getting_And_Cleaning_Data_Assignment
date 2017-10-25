## Getting and Cleaning Data Course Project

## Create one R script called run_analysis.R that does the following.
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## Download data

library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'./UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = getwd())
}

## Read data from all the files of UCI HAR Dataset

activity_labels <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])

features <- read.table('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

x_train <- read.table('./UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

data.train <-  data.frame(subject_train, y_train, x_train)
names(data.train) <- c(c('subject', 'activity'), features)

x_test <- read.table('./UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

data.test <-  data.frame(subject_test, y_test, x_test)
names(data.test) <- c(c('subject', 'activity'), features)

## 1. Merge the training and test sets to create one data set.

data.merge <- rbind(data.train, data.test)

## 2. Extract only the measurements on mean and standard deviation for each measurement.

mean_std <- grep('mean|std', features)
data.sub <- data.merge[,c(1,2,mean_std + 2)]

## 3. Use descriptive activity names to name the activities in the data set.

data.sub$activity <- activity_labels[data.sub$activity]

## 4. Appropriately label the data set with descriptive variable names.

name_new <- names(data.sub)
name_new <- gsub("[(][)]", "", name_new)
name_new <- gsub("^t", "TimeDomain_", name_new)
name_new <- gsub("^f", "FrequencyDomain_", name_new)
name_new <- gsub("Freq$", "Frequency", name_new)
name_new <- gsub("Acc", "Acceleration", name_new)
name_new <- gsub("Gyro", "Gyroscope", name_new)
name_new <- gsub("Mag", "Magnitude", name_new)
name_new <- gsub("-mean-", "_Mean_", name_new)
name_new <- gsub("-std-", "_StandardDeviation_", name_new)
name_new <- gsub("-", "_", name_new)
names(data.sub) <- name_new

## 5.From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)

write.table(x = tidy_data, file = "tidy_data.txt", row.names = FALSE)