# rm(list=ls())
# cat("\f")
# 
# file1 <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile = "./UCI HAR Dataset.zip")
# con1 <- unzip("./UCI HAR Dataset.zip",exdir = ".")

# Uncomment (ctrl+shif+c) rows 1 until 5 if the dataset is not already in your working directory within the folder with the name of "UCI HAR Dataset"

# The file "run_analysis.R" tidies and cleans the dataset "UCI HAR Dataset" as part of the assignment "getting-and-cleaning-data-course-project"
# This code will carry out the following steps:
#         1. Merges the training and the test sets to create one data set.
#         2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#         3. Uses descriptive activity names to name the activities in the data set
#         4. Appropriately labels the data set with descriptive variable names. 
#         5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 
# Read training data
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header = F)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", header = F)
sub_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = F)

# Read test data
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header = F)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", header = F)
sub_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = F)

# Read activity label and feature data
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = F)
features <- read.table("./UCI HAR Dataset/features.txt", header = F)

# 3. Uses descriptive activity names to name the activities in the data set
colnames(act_labels) <- c("activity number","activity name")
colnames(features) <- c("feature number", "feature name")

# Use labels to name the subject data
colnames(sub_test) <- "subject"
colnames(sub_train) <- "subject"

# Assign feature name to columns of x data
colnames(x_test) <- features[,2]
colnames(x_train) <- features[,2]

# Assign activity number as label of y data
colnames(y_test) <- "activity number"
colnames(y_train) <- "activity number"

# Create matrices for training and test data
mat_train <- cbind(sub_train,x_train,y_train)
mat_test <- cbind(sub_test,x_test,y_test)

# Combine training and test data as a matrix
mat_comb <- rbind(mat_train,mat_test)

# 1. Merges the training and the test sets to create one data set.
dataset1 <- merge(act_labels,mat_comb,by.x="activity number",by.y="activity number")

# Sort the merged dataset by subject and adjust row order
dataset1 <- dataset1[order(dataset1$subject),]
dataset1 <- dataset1[,c(c(3,2,1),4:564)]

# Use dplyr library
library(dplyr)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 4. Appropriately labels the data set with descriptive variable names --> this was mainly performed after step 3 (line 31)
dataset1_sub1 <- dataset1[grepl(c("[M,m]ean|[S,s]td"), colnames(dataset1))]
dataset2 <- cbind(dataset1[,1:3],dataset1_sub1)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Get column names of dataset2 with only mean and std rows
c_names <- colnames(dataset2)
# Split the dataset by subject and activity name 
dataset2_split <- split(dataset2,list(dataset2$subject,dataset2$`activity name`))

# Function to calculate the column mean while retaining the first three columns (subject, activity name, activity number)
columnmean <- function(x){
 r1 = x[1,1]
 r2 = x[1,2]
 r3 = x[1,3]
 r4 = colMeans(x[,c_names[4:89]])
 comb <- c(r1,r2,r3,r4)
 names(comb) <- c_names
 return(comb)
}

# Create tidy dataset with means of the numeric columns
dataset_tidy <- t(sapply(dataset2_split,columnmean))

# Write tidy dataset to .txt file
write.table(dataset_tidy,"./dataset_tidy.txt",row.names = F)

# Read in the written tidy dataset
dataset_tidy_read <- read.table("./dataset_tidy.txt", header = T)