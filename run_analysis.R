
setwd("")


## Downloading data file from the web

if(!file.exists("CourseProject")){dir.create("CourseProject")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file (fileUrl, destfile="CourseProject/running.zip")

unzip("CourseProject/running.zip",exdir = "./CourseProject")


## Merging the training and the test sets to create one data set

setwd("./CourseProject")
list.files()
list.files("./UCI HAR Dataset")

activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
str(activity_labels)
activity_labels

features <- read.table("UCI HAR Dataset/features.txt")
str(features)
head(features,20)
tail(features,20)

list.files("./UCI HAR Dataset/test")

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
dim(subject_test)
str(subject_test)
summary(subject_test)
colnames(subject_test) <- c("subject")

X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
dim(X_test)
str(X_test)
head(X_test)
colnames(X_test) <- features$V2 ## Appropriately labeling the data set with descriptive variable names

y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
dim(y_test)
str(y_test)
head(y_test)
summary(y_test)
colnames(y_test) <- c("activity_labels") ## Using descriptive activity names to name the activities in the data set

for (i in 1:6){
        y_test$activity_labels <- gsub(i,activity_labels[i,2],y_test$activity_labels)
}

tests <- cbind(subject_test,y_test,X_test)
dim(tests)
str(tests)

list.files("./UCI HAR Dataset/train")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
dim(subject_train)
str(subject_train)
summary(subject_train)
colnames(subject_train) <- c("subject")

X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
dim(X_train)
str(X_train)
head(X_train)
colnames(X_train) <- features$V2  ## Appropriately labeling the data set with descriptive variable names

y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
dim(y_train)
str(y_train)
head(y_train)
summary(y_train)
colnames(y_train) <- c("activity_labels") ## Using descriptive activity names to name the activities in the data set

for (i in 1:6){
        y_train$activity_labels <- gsub(i,activity_labels[i,2],y_train$activity_labels)
}

trains <- cbind(subject_train,y_train,X_train)
dim(trains)
str(trains)

dataset <- rbind(tests, trains)
dim(dataset)
str(dataset)

write.table(dataset, "running_dataset.txt", quote=FALSE, sep=" ", row.names=FALSE, col.names=TRUE)


## Extracting only the measurements on the mean and standard deviation for each measurement. 

names(dataset)
Mean_Std_measures <- sort(c(grep("[Mm]ean()",names(dataset)),grep("[Ss]td()",names(dataset)),grep("[Mm]eanFreq()",names(dataset))))

Mean_Std_dataset <- dataset[,c(1:2,Mean_Std_measures)]
dim(Mean_Std_dataset)
str(Mean_Std_dataset)

write.table(Mean_Std_dataset, "running_Mean_Std_dataset.txt", quote=FALSE, sep=" ", row.names=FALSE, col.names=TRUE)


## Creating a second and independent tidy data set with the average of each variable for each activity and each subject.

Mean_Std_dataset$subject <- as.factor(Mean_Std_dataset$subject)
Mean_Std_dataset$activity_labels <- as.factor(Mean_Std_dataset$activity_labels)

Mean_Std_dataset$subject_activity_labels <- paste(Mean_Std_dataset$subject,Mean_Std_dataset$activity_labels, sep=" ")

names(Mean_Std_dataset)

Averaged_dataset <- Mean_Std_dataset[,c("subject","activity_labels","subject_activity_labels")]
dim(Averaged_dataset)
Averaged_dataset <- unique(Averaged_dataset)


for (i in 3:(length(Mean_Std_dataset)-1)){
        
        j <- tapply(Mean_Std_dataset[,i],Mean_Std_dataset$subject_activity_labels, mean)
        j <- as.data.frame(j)
        colnames(j) <- colnames(Mean_Std_dataset)[i]
        j$subject_activity_labels <- rownames(j)

        Averaged_dataset <- merge(Averaged_dataset,j)
}

Averaged_dataset <- Averaged_dataset[,!colnames(Averaged_dataset)%in%c("subject_activity_labels")]
dim(Averaged_dataset)
summary(Averaged_dataset)

write.table(Averaged_dataset, "running_Averaged_dataset.txt", quote=FALSE, sep="\t", row.names=FALSE, col.names=TRUE)

