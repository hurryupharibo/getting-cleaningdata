# load packages
library(dplyr)

# assign .zip file to variable 'filename'
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"

# download file if file is not found
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# unzip file if folder in .zip file is not found
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# assign dataframes  
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# merge training and testing data into one data set
X <- rbind(X_train, X_test)
Y <- rbind(Y_train, Y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# extract only the measurements on the mean and standard deviation for each measurement.
Tidy <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# use descriptive activity names to name the activities in the data set
Tidy$code <- activities_labels[Tidy$code, 2]

# label the data set with descriptive variable names
names(Tidy)[2] = "activity"
names(Tidy)<-gsub("Acc", "Accelerometer", names(Tidy))
names(Tidy)<-gsub("Gyro", "Gyroscope", names(Tidy))
names(Tidy)<-gsub("BodyBody", "Body", names(Tidy))
names(Tidy)<-gsub("Mag", "Magnitude", names(Tidy))
names(Tidy)<-gsub("^t", "Time", names(Tidy))
names(Tidy)<-gsub("^f", "Frequency", names(Tidy))
names(Tidy)<-gsub("tBody", "TimeBody", names(Tidy))
names(Tidy)<-gsub("-mean()", "Mean", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-std()", "STD", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("-freq()", "Frequency", names(Tidy), ignore.case = TRUE)
names(Tidy)<-gsub("angle", "Angle", names(Tidy))
names(Tidy)<-gsub("gravity", "Gravity", names(Tidy))

# From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
Output <- Tidy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(Output, "Output.txt", row.name=FALSE)

# view output
Output