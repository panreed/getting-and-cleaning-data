## This script process the data in the UCI HAR Dataset
## Before begin, we assume the current working directory is "UCI HAR Dataset" folder

## run_analysis <- function(){
## This whole analysis can be made into a function, but I am not doing it here
## for now the execution is by running this script in the console directly


## Step 1, load and merge the data sets, along with the subject and activity information
## so here 6 files will be loaded and merged into 3 data tables

setwd("~/R/UCI HAR Dataset/test")
sub_test <- read.table("subject_test.txt")
x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")

## Load test data in the above 4 lines

setwd("~/R/UCI HAR Dataset/train")
sub_train <- read.table("subject_train.txt")
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")

## Load train data in the above 4 lines

subject_data <- rbind(sub_test, sub_train)
x_data <- rbind(x_test, x_train)
y_data <- rbind(y_test, y_train)

## merge test and train data together, use rbind to maintain relative position



## Step 2, extract all variables in the x_data that has to do with mean or std
## information of the variable is contained in the file named "features.txt"

setwd("~/R/UCI HAR Dataset")
feature_data <- read.table("features.txt")
find_mean <- grepl("mean()", feature_data[,2],fixed = TRUE)
find_std <- grepl("std()", feature_data[,2],fixed = TRUE)

## Load feature data into table, and find out the variables corresponding to mean and std

feature_index <- find_mean | find_std
small_data <- x_data[,feature_index]
small_feature <- feature_data[feature_index,]

## small_data is the data correspond to only mean OR std
## small_feature contains the corresponding names of the measurement

## Step 3, add names to the activity in the data set
## basically, add row attributes that contains activity data
## except it is the name of the activity, instead of numerical code 1-6

activity_name <- y_data
  activity_name[activity_name == 1] <- "walking"
  activity_name[activity_name == 2] <- "walking_upstairs"
  activity_name[activity_name == 3] <- "walking_downstairs"
  activity_name[activity_name == 4] <- "sitting"
  activity_name[activity_name == 5] <- "standing"
  activity_name[activity_name == 6] <- "laying"

names(activity_name) <- "activity"

## create an object, with the correct activity name

## will cbind the activity to data set after step 4


## Step 4, add variable names to the column attributes

colnames(small_data) <- small_feature[,2]

final_data <- cbind(activity_name,small_data)

## Here final data has in its first column the names of the activity, 
## and the remaining parts the correct mean and std data with measurement names



## Step 5, create another tide data set, with data being the average of each
## variable for different test object and different activities

names(subject_data) <- "subject"
## activity_data <- y_data
## names(activity_data) <- "activity"

tidy_data <- cbind(subject_data, small_data)
## add a column containing the subject information

## Here I will use a brutal force way to obtain the final data set,
## Since I have having a bit trouble here.

## There are 30 subjects, each performing 6 activities, so the final data set
## will have 180 rows

test_final <- tidy_data[1:180,]
test_final[,1] <- rep(1:30, each = 6)
test_final[,2] <- c("walking","walking_upstairs","walking_downstairs","sitting","standing","laying")

## Here I created a data table called test_final, which has the correct 
## dimension and first two columns for the final tidy data.

## The remaining things will be to subset the tidy data sets with subject and activity and find out
## the column means and pass on the values to the corresponding test_final

for (i in 1:180) { 
    test_final[i, 3:68] <- colMeans( small_data[ subject_data == test_final[i,1] & activity_name == test_final[i,2],])
}

write.table(test_final, file = "tidy_data_final.txt", row.names = FALSE)

## } 
## the above line is the end of the function defined in line 4