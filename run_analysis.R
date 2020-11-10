library(dplyr)
#Step 1: Merges the training and the test sets to create one data set.
#download a dataset
if(!file.exists("./data")) {dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/dataset.zip")
projectData <- unzip("./data/dataset.zip", exdir = "./data")
#read data
  #read training data
xtrain <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
colnames(xtrain) <- features[,2]
ytrain <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
  #read testing data
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
subjec_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
  #read feature vector
features <- read.table("./data/UCI HAR Dataset/features.txt")
  #read activiry label
activity_label <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
#assigning variable names
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityID"
colnames(subject_train) <- "subjectID"

colnames(xtest) <- features[,2]
colnames(ytest) <- "activityID"
colnames(subjec_test) <- "subjectID"

colnames(activity_label) <- c("activityID", "activityName")
#merging data in one dataset
train <- cbind(ytrain, subject_train, xtrain)
test <- cbind(ytest, subjec_test, xtest)
dataset <- rbind(train, test)
##########################################################################

#Step 2: Extracting only the measurements on the mean and sd for each measurement
#read available value
colNames = colnames(dataset)
#determine column of dataset to keep base on column name
columntokeep <- grepl("activityID|subjectID|mean|std", colNames)
#keep data in these colum
dataset <- dataset[, columntokeep]
##########################################################################
#Step 3: Uses descriptive activity names to name the activities in the data set
dataset$activityID <- factor(dataset$activityID, levels = activity_label[,1], labels = activity_label[,2])
##############################################################################
#Step 4: Appropriately labels the data set with descriptive variable names.
#get the column names
dataCols <- colnames(dataset)
#remove special character
dataCols <- gsub("[\\(\\)-]", "", dataCols)
#expand abreviation and clean up names
dataCols <- gsub("^t", "timeDomain", dataCols)
dataCols <- gsub("^f", "frequencyDomain", dataCols)
dataCols <- gsub("Acc", "Accelerometer", dataCols)
dataCols <- gsub("Gyro", "Gyroscope", dataCols)
dataCols <- gsub("Mag", "Magnitude", dataCols)
dataCols <- gsub("Freq", "Frequency", dataCols)
dataCols <- gsub("mean", "Mean", dataCols)
dataCols <- gsub("std", "StandardDeviation", dataCols)
dataCols <- gsub("BodyBody", "Body", dataCols)
colnames(dataset) <- dataCols
############################################################################
#Step 5:From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#create new tidy set
secondataset <- aggregate(.~ subjectID + activityID, dataset, mean)
secondataset <- secondataset[order(secondataset$subjectID, secondataset$activityID),]
#Output to file "tidy_data.txt"
write.table(secondataset, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
