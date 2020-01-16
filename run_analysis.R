if (!require("tidyverse")) {
   install.packages("tidyverse")}
library(tidyverse)

data.dir = path.expand("~/Getting_and_Cleaning_Data/data")
dir.create(data.dir, recursive=T)
setwd(data.dir)

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url , destfile = "HARUS.zip" )
unzip("HARUS.zip")

variables <- read.table("UCI HAR Dataset/features.txt" , header = FALSE)
variables <- as.character(variables[,2])  ### vetor with variable names

#find duplicates descriptions in variables
ind_dupl <- duplicated(variables)
# create a vetor with dupicated
v_dupl <- variables[ind_dupl]
# test se in duplicated exist mean ou std part
grep("mean|std", x = v_dupl)
#replacing duplicate descriptions with innocuous descriptions
variables[ind_dupl] <- paste("new_descricao" , seq(1:84) , sep ="")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
activity_labels <- as.character(activity_labels[,2])  ###vetor with activities labels

tmp_train_X <- read.table("./UCI HAR Dataset/train/X_train.txt" , header = FALSE) #measures train
tmp_train_Y <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE) #activities train
tmp_train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE) #subjects 

tmp_test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)  # measures test
tmp_test_Y <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)  # activities test
tmp_test_subjects <- read.table("./UCI HAR Dataset//test/subject_test.txt", header = FALSE)  #subjects 

tmp_train <- data.frame(tmp_train_subjects,tmp_train_Y, tmp_train_X)
tmp_test  <- data.frame(tmp_test_subjects,tmp_test_Y, tmp_test_X)

# include columns names "subject" e "activity"
names(tmp_train) <- c ("subject" , "activity" , variables)
names(tmp_test) <- c ("subject" , "activity" , variables)

# include descriptive activity 
tmp_test$activity <- activity_labels[tmp_test$activity]
tmp_train$activity <- activity_labels[tmp_train$activity]

data_test <- select(tmp_test,  grep("subject|activity|mean()|std()", names(tmp_test), value = TRUE))
data_train <- select(tmp_train,  grep("subject|activity|mean()|std()", names(tmp_train), value = TRUE))

data_full <- rbind(data_test, data_train) 

# Label the dataset with descriptive variables names and other improvements
ori_names <- names(data_full)

# remove "-()"
new_names <- gsub("[[:punct:]]", "", x= ori_names)
# replace :
# prefix t > Time 
# prefix f > Frequence
# std > Standard Deviation
# Acc > Accelometer Measurement 
# Body > Body Movement
# Gravity > Acceleration of Gravity
# Gyro > Gyroscopic measurements
# Jerk > Sudden Movement Acceleration
# Mag > Magnitude of Moviment 
# X,Y,Z > Axis X, Axis Y, Axis Z
new_names <- gsub("^t" , "Time " , new_names)
new_names <- gsub("^f" , "Frequence " , new_names)
new_names <- gsub("std" , "Standard Deviation " , new_names)
new_names <- gsub("Acc" , "Accelometer Measurement  " , new_names)
new_names <- gsub("Body" , "Body Movement " , new_names)
new_names <- gsub("Gravity" , "Acceleration of Gravity " , new_names)
new_names <- gsub("Gyro" , "Gyroscopic Measurements " , new_names)
new_names <- gsub("Jerk" , "Sudden Movement Acceleration " , new_names)
new_names <- gsub("Mag" , "Magnitude of Moviment " , new_names)
new_names <- gsub("X$" , "Axis X" , new_names)
new_names <- gsub("Y$" , "Axis Y" , new_names)
new_names <- gsub("Z$" , "Axis Z" , new_names)
new_names <- gsub("mean" , "Mean " , new_names)
new_names <- gsub("Freq" , "Frequence " , new_names)

names(data_full) <- new_names

#save full dataset
write.csv(data_full ,file = "data_full.csv")

#compress  files
zip(zipfile = data_full.csv)

#group by subject and activity
data_full_group <- group_by(data_full, subject, activity)

# calcule mean group by

data_full_group_avg <- summarise_each(data_full_group, mean)

#Include the expression Average in names of new dataset
old_names <- names(data_full_group_avg)
old_names[3:81] <- paste("Average " , old_names[3:81] , sep = "")  
names(data_full_group_avg) <- old_names

# write the second dataset

write.csv(data_full_group_avg ,file = "data_full_group_avg.csv")

#compress  files
zip(zipfile = "data.zip", files = c("data_full.csv", "data_full_group_avg.csv"))