##### PART 1: GET THE DATA
#
#Download the file and put the file in the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")


#Unzip the file
unzip(zipfile="./data/Dataset.zip",exdir="./data")

#Get the list of the files from the folder UCI HAR Dataset. 
path_rf <- file.path("./data" , "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

#The files that will be used to load data are listed as follows:
#    
#test/subject_test.txt
#test/X_test.txt
#test/y_test.txt
#train/subject_train.txt
#train/X_train.txt
#train/y_train.txt


######## PART 2: READ DATA FROM THE FILES AND LOOK AT THEIR PROPERTIES

#Read the Activity files
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ),header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"),header = FALSE)

#Read the Subject files
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"),header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"),header = FALSE)

#Read Features files
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ),header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"),header = FALSE)

#Check  the properties of the previous variables
str(dataActivityTest)
str(dataActivityTrain)
str(dataSubjectTrain)
str(dataSubjectTest)
str(dataFeaturesTest)


######## PART 3: MERGE THE TRAINING AND THE TEST SETS TO CREATE ONE DATA SET
#
#Concatenate the data tables by rows
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

#Set names to variables
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

#Merge columns to get the data frame Data for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)


############# PART 4: EXTRACTS ONLY THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION
############# FOR EACH MEASUREMENT
#
# Subset Name of Features by measurements on the mean and standard deviation
#(that is consider Names of Features with “mean()” or “std()”

subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

#Subset the data frame Data by seleted names of Features
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

#Check the structures of the data frame Data
str(Data)


###### PART 5: USE DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
#
#Read descriptive activity names from “activity_labels.txt”

activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)

# Factorize variable activity in the dataframe using descriptive activity names
Data$activity<-factor(Data$activity);
Data$activity<- factor(Data$activity,labels=as.character(activityLabels$V2))
head(Data$activity,30)


###### PART 6: LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES
#
#Here we will label the names of features will by using descriptive variable names:
#
#prefix t is replaced by time
#Acc is replaced by Accelerometer
#Gyro is replaced by Gyroscope
#prefix f is replaced by frequency
#Mag is replaced by Magnitude
#BodyBody is replaced by Body

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

names(Data)


###### PART 7: CREATE A SECOND, INDEPENDENT TIDY DATA SET WITH THE AVERAGE OF EACH
###  VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT, USING THE DATA FROM THE PREVIOUS STEP

library(plyr)
Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)



