#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
feat <- read.table("./UCI HAR Dataset/features.txt")
feat <- feat$V2

#Reading in the Training Data as a data frame
trainX <-  read.table("./UCI HAR Dataset/train/X_train.txt")
trainY <-  read.table("./UCI HAR Dataset/train/y_train.txt")
subTrainX <-  read.table("./UCI HAR Dataset/train/subject_train.txt")
colnames(trainX) <- feat
trainX$Subject <- subTrainX[[1]]
trainX$Activity <- trainY[[1]]


#Reading in the Test Data as a data frame
testX <-  read.table("./UCI HAR Dataset/test/X_test.txt")
testY <-  read.table("./UCI HAR Dataset/test/y_test.txt")
subTestX <-  read.table("./UCI HAR Dataset/test/subject_test.txt")
colnames(testX) <- feat
testX$Subject <- subTestX[[1]]
testX$Activity <- testY[[1]]


#1. Combining test and training set into one data set
data <- rbind(trainX,testX)

#2. Extracting out only the mean and std measurements
meanStdRaw <- c(grep("mean\\(\\)",colnames(data)),grep("std\\(\\)",colnames(data)), 562,563)
meanStdIndex <- sort(meanStdRaw)
data <- data[,meanStdIndex]

#3. Using descriptive activity names
library(plyr)
activityNames <- read.table("./UCI HAR Dataset/activity_labels.txt")
data$Activity <- mapvalues(data$Activity, activityNames[[1]], activityNames[[2]])

#4.Appropriately naming column names
colnames(data)
names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))
colnames(data)

#5. Creating a new data set with the avg of each variable for each activity and each subject
library(reshape2)
meltedData <- melt(data,id=c("Activity","Subject"))
newData <- dcast(meltedData,Activity + Subject ~ variable,mean)
str(newData)
