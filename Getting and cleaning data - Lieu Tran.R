# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


### Merges the training and the test sets to create one data set.
## Read multiple data files in a folder
# https://stackoverflow.com/questions/3397885/how-do-you-read-in-multiple-txt-files-into-r
# read all files in the train folder
# list_test <- list.files(path = ".", recursive = TRUE,
#                          pattern = "\\.txt$", 
#                          full.names = TRUE)
# library(data.table)
# Read all the files and create a FileName column to store filenames
# data_test <- rbindlist(sapply(list_test, fread, simplify = FALSE),
                        # use.names = TRUE, idcol = "FileName")
#read the features
setwd("/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/UCI HAR Dataset")
features <- read.table("features.txt")
features <- as.factor(features[,2])
features <- c("ID", "Activity_code", as.character(features))

setwd("/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/UCI HAR Dataset/train")
data_train <- read.table("X_train.txt")
act_train <- read.table("y_train.txt") # class of activity fr 1 to 6
id_train <- read.table("subject_train.txt")
data_train <- cbind(id_train, data_train, act_train)
data_train <- data_train[c(1, 563, 2:562)]
colnames(data_train) <- features

setwd("/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/UCI HAR Dataset/test")
data_test <- read.table("X_test.txt")
act_test <- read.table("y_test.txt") # class of activity fr 1 to 6
id_test <- read.table("subject_test.txt")
data_test <- cbind(id_test, data_test, act_test)
data_test <- data_test[c(1, 563, 2:562)]
colnames(data_test) <- features

# 1. Merge the data
data_merge <- rbind(data_train, data_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
library(stringr)

features_ext <- as.factor(c(features[regexpr("mean", features) >-1], features[regexpr("std", features) >-1]))
features_ext <- c("ID", "Activity_code", as.character(levels(features_ext)))
data_ext <- data_merge[, features_ext]
write.table(features_ext, file = "/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/features.txt", row.name=FALSE)

# 3. Uses descriptive activity names to name the activities in the data set
data_ext$activity <- NA
for (i in 1:nrow(data_ext)) {
      data_ext$activity[data_ext$ID == 1] <- "WALKING"
      data_ext$activity[data_ext$ID == 2] <- "WALKING_UPSTAIRS"
      data_ext$activity[data_ext$ID == 3] <- "WALKING_DOWNSTAIRS"
      data_ext$activity[data_ext$ID == 4] <- "SITTING"
      data_ext$activity[data_ext$ID == 5] <- "STANDING"
      data_ext$activity[data_ext$ID == 6] <- "LAYING"
}

data_ext <- subset(data_ext, select = c(1,2,82,3:81))
# 4. Appropriately labels the data set with descriptive variable names.
# setwd("/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/UCI HAR Dataset")
# features <- read.table("features.txt")
# features <- as.factor(features[,2])
# features <- c("ID", as.character(features))
# 
# colnames(data_merge) <- features

# 5. From the data set in step 4, 
# creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# get average of each subject (by ID), then each activity (by activity)
data_ext_2 <- with(data_ext, aggregate(data_ext[,-c(1:3)], by = list(ID, Activity_code), 
                                    FUN = "mean"))
names(data_ext_2)[1:2] <- c("ID", "Activity_code")
write.table(data_ext_2, file = "/Volumes/LIEU TRAN Apr 2018/PROJECTS LIEU TRAN/R/R coursera/R - Getting cleaning data course/R - Project/data_step5.txt", row.name=FALSE)

