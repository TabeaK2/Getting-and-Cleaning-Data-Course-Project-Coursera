library(dplyr)
library(tidyr)
library(data.table)
#Download file
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"data.zip")
#Unzip file
unzip("data.zip")
#Read relevant files into R.
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
features<-read.table("./UCI HAR Dataset/features.txt",header=FALSE)
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
#Merge rows of data tables.
X_data<-rbind(X_test,X_train)
Y_data<-rbind(y_test,y_train)
Subject_Data<-rbind(subject_test,subject_train)
#Add column names.
names(Subject_Data)<-c("Subject")
names(Y_data)<-c("Activity")
feature_names<-as.factor(features$V2) #as.factors gets rid of ""
names(X_data)<-feature_names
#Merge all columns
merge<-cbind(Subject_Data,Y_data,X_data)
#Filter columns to only include mean and std.
#Use Grep to get filtered column names
filtered_names<-as.factor(features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)])
selected_names<-c("Subject","Activity",as.character(filtered_names))
#Subset data using select from dplyr to only include selected columns
data_final<-select(merge,all_of(selected_names))
#Add column to data_frame that contains "human readable" Activity descriptions.
#Use dplyr left_join.
data_final_rename<-left_join(data_final,activity_labels,by=c("Activity"="V1"))
#Rename Activity columns
data_final_rename<-rename(data_final_rename,Activity_numeric=Activity,Activity_label=V2)
#Move Activity_label column after Activity_numberic with dplyr
data_final_rename<-relocate(data_final_rename, Activity_label, .after=Activity_numeric)
#Rename all variables to make more readable.
names(data_final_rename)<-gsub("^f","Frequency",names(data_final_rename))
names(data_final_rename)<-gsub("^t","Time",names(data_final_rename))
names(data_final_rename)<-gsub("Acc","Accelerometer",names(data_final_rename))
names(data_final_rename)<-gsub("Gyro","Gyroscope",names(data_final_rename))
names(data_final_rename)<-gsub("Mag","Magnitude",names(data_final_rename))
#Create tidy dataset with mean by subject and activity, using dplyr.
tidy_data <- data_final_rename %>% group_by(Subject, Activity_label) %>% summarize_all(mean)
#Order tidy_data by subject & activity_numeric
tidy_data<-arrange(tidy_data,Subject,Activity_numeric)
#Write tidy data to file.
write.table(tidy_data,file="tidy_data.txt",row.name=FALSE)