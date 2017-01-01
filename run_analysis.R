#This is written and executed by ASaeedSH - UMT 
# Course is based upon the Coursera Cleaning Data Course Week 4 Assignment
#R code was to have 5 immediate outcomes 
#1. Merge training and test sets to create one data set
#2. Extract only the mean and standard deviation measurements
#3. Use descriptive activity names to name activities in the data set
#4. Label data set with descriptive variable names
#5. from the outcome of 4, create an independent data saet with average of each variabe for each activity and subject

##########
#the code and data are broken down into the main 1 - 5 outcomes. However prerequisites have to be completed.
#Pre Requisites steps

#First Step is to find out the current directory 
getwd()
list.dirs()
#there is no directory called data so we will create it 
if(!file.exists("./midtermdata")){dir.create("./midtermdata")}
#Create vector which contains the url 
url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download the zip data 
download.file(fileUrl,destfile="./midtermdata/Dataset.zip", mode = "wb")
#Now the data file is downloaded, it is now time to unzip
unzip(zipfile="./midtermdata/Dataset.zip",exdir="./midtermdata")
# lets check the zip file and the new folder that has been unzipped
list.files("D:/rlang/midtermdata")
#define the path where the new folder has been unziped
pathdata = file.path("./midtermdata", "UCI HAR Dataset")
#create a file which has the 28 file names
files = list.files(pathdata, recursive=TRUE)
########
### 1. Output Steps - Here we begin how to create the data set of training and test
#Reading training tables - xtrain / ytrain, subject train
xtrain = read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"),header = FALSE)
#Reading the testing tables
xtest = read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(pathdata, "test", "subject_test.txt"),header = FALSE)
#Read the features data
features = read.table(file.path(pathdata, "features.txt"),header = FALSE)
#Read activity labels data
activityLabels = read.table(file.path(pathdata, "activity_labels.txt"),header = FALSE)
#Create Sanity and Column Values to the Train Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"
#Create Sanity and column values to the test data
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
#Create sanity check for the activity labels value
colnames(activityLabels) <- c('activityId','activityType')
#Merging the train and test data - important outcome of the project
mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)
#Create the main data table merging both table tables - this is the outcome of 1
alldatamerge = rbind(mrg_train, mrg_test)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement
# Need step is to read all the values that are available
colNames = colnames(setAllInOne)
#Need to get a subset of all the mean and standards and the correspondongin activityID and subjectID 
mean_and_std = (grepl("activityId" , colNames) | grepl("subjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))
#A subtset has to be created to get the required dataset
setForMeanAndStd <- setAllInOne[ , mean_and_std == TRUE]

# 3. Use descriptive activity names to name the activities in the data set
setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)

#4. Label the data set with decriptive variable names
# setAllInOne and setForMeanAndStd are the outcomes and solutions 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
# New tidy set has to be created 
secTidySet <- aggregate(. ~subjectId + activityId, setWithActivityNames, mean)
secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
#The last step is to write the ouput to a text file 
write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
