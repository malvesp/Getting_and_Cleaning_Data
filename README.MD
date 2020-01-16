## Human Activity Recognition Using Smartphones:


Source:

Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)
1 - Smartlab - Non-Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy.
2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living
Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain
activityrecognition '@' smartlab.ws

Project Origins:
To fulfil remote monitoring systems’ requirements Jorge Luis Reyes Ortiz has developed a complete Human Activity Recognition System able to detect and recognize 6 different activities performed by humans in their daily living in online mode using smartphones

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.  

Data were collected using the url source:

 https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Project Modified:
For our study we are only interested in extracting the means and standard deviations of each observation.

Where:
activity_labels.txt contains the encoding of the 6 activities:
1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

__features.txt contains the measured variables (originally 561)__

*During the data manipulation tasks I discovered that there are duplicate variable names in this vector.*
*It was then necessary to rename them*

**subject_(test/train).txt contains the identifier of the persons tested (1 to 30)**

**X_(test/train).txt contains the observations for each variable**

**y_(test/train).txt contains the code of each activity (1 to 6)**

*The modifications made were:* 
*load necesary librarys*

if (!require("tidyverse")) {
   install.packages("tidyverse")}
library(tidyverse)

*create working diretory* 

data.dir = path.expand("~/Getting_and_Cleaning_Data/data")
dir.create(data.dir, recursive=T)
setwd(data.dir)

*getting raw data*

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url , destfile = "HARUS.zip" )
unzip("HARUS.zip")

*manipulating data*

variables <- read.table("UCI HAR Dataset/features.txt" , header = FALSE)
variables <- as.character(variables[,2])  

*find duplicates descriptions in variables*

ind_dupl <- duplicated(variables)

*create a vetor with dupicated*

v_dupl <- variables[ind_dupl]

*test se in duplicated exist mean ou std part*

grep("mean|std", x = v_dupl)

*replacing duplicate descriptions with innocuous descriptions*

variables[ind_dupl] <- paste("new_descricao" , seq(1:84) , sep ="")


activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)

activity_labels <- as.character(activity_labels[,2])  

tmp_train_X <- read.table("./UCI HAR Dataset/train/X_train.txt" , header = FALSE) 

tmp_train_Y <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE) 

tmp_train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE) 


tmp_test_X <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE) 

tmp_test_Y <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE) 

tmp_test_subjects <- read.table("./UCI HAR Dataset//test/subject_test.txt", header = FALSE)  

tmp_train <- data.frame(tmp_train_subjects,tmp_train_Y, tmp_train_X)

tmp_test  <- data.frame(tmp_test_subjects,tmp_test_Y, tmp_test_X)

include columns names "subject" e "activity"

names(tmp_train) <- c ("subject" , "activity" , variables)

names(tmp_test) <- c ("subject" , "activity" , variables)

*include descriptive activity*

tmp_test$activity <- activity_labels[tmp_test$activity]

tmp_train$activity <- activity_labels[tmp_train$activity]


data_test <- select(tmp_test,  grep("subject|activity|mean()|std()", names(tmp_test), value = TRUE))

data_train <- select(tmp_train,  grep("subject|activity|mean()|std()", names(tmp_train), value = TRUE))

*merge the two databases

data_full <- rbind(data_test, data_train) 

*Label the dataset with descriptive variables names and other improvements*

ori_names <- names(data_full)

*remove "-()"*
new_names <- gsub("[[:punct:]]", "", x= ori_names)

*replace :*

 *prefix t > Time* 
 
 *prefix f > Frequence*
 
 *std > Standard Deviation*
 
 *Acc > Accelometer Measurement*
 
 *Body > Body Movement*
 
 *Gravity > Acceleration of Gravity*
 
 *Gyro > Gyroscopic measurements*
 
 *Jerk > Sudden Movement Acceleration*
 
 *Mag > Magnitude of Moviment*
 
 *X,Y,Z > Axis X, Axis Y, Axis Z*
 

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

*save full dataset*

write.csv(data_full ,file = "data_full.csv")

group by subject and activity
data_full_group <- group_by(data_full, subject, activity)

 *calcule mean group by*

data_full_group_avg <- summarise_each(data_full_group, mean)

*Include the expression Average in names of new dataset*

old_names <- names(data_full_group_avg)

old_names[3:81] <- paste("Average " , old_names[3:81] , sep = "")  

names(data_full_group_avg) <- old_names

 *write the second dataset*

write.csv(data_full_group_avg ,file = "data_full_group_avg.csv")

*compress  files*

zip(zipfile = "data.zip", files = c("data_full.csv", "data_full_group_avg.csv"))
