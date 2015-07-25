getwd()
list.files()
# Samsung data must be in your working directory
# That means you should download getdata-projectfiles-UCI HAR Dataset.zip
# into working directory and extract it there 
# so that the directory contains UCI HAR Dataset folder and subfolder
#
# libraries:
library(dplyr)
library(reshape2)

# load features which will serve as column names for x
features <- read.table("./UCI HAR Dataset/features.txt")
dim(features)
# load activity labels
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
head(activities)
# x_test should have 2947 rows of 561 columns
# the 561 columns correspond to various calculations
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
dim(x_test)
# assign as column names to x_test
colnames(x_test) <- features[, 2]
# y_test should have 2947 rows of 1 column
# these rows list the activity code corresponding to the
# rows in x_test
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
dim(y_test)
# load subjects for test (should have 2947 rows of 1 column)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
dim(subject_test)
# bind the x and y test sets together so as to know
# what calculations were performed for which activity
sxy_test <- cbind(subject_test, y_test, x_test)
dim(sxy_test)

# x_train should have 7352 rows of 561 columns
# total observations 10,299
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(x_train)
# assign as column names to x_train
colnames(x_train) <- features[, 2]
# y_train should have 7352 rows with 1 column
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
dim(y_train)
# load subjects for test (should have 7352 rows of 1 column)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
dim(subject_train)
# bind the subject and x and y test sets together so as to know
# what calculations were performed for which activity by what subject
sxy_train <- cbind(subject_train, y_train, x_train)
dim(sxy_train)

# merge train and text (union)
sxy_all <- rbind(sxy_train, sxy_test)
dim(sxy_all)
#first column was set to subject, second to activity via cbind above
colnames(sxy_all)[1:2] <- c("subject", "activity")
names(sxy_all)
# 10299 observations, 563 variables
# Certain column names are duplicated for some reason according to R
# As these are not used here I will remove these columns

# originally from stack overflow
# http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name
# Verify duplicated names
dups <- names(sxy_all)
dups[duplicated(dups)]
# none of duplicated columns are subject, activity, mean, or std columns
sxy_allunq <- sxy_all[ , !duplicated(colnames(sxy_all))]
dim(sxy_allunq) # 479 columns remain 
names(sxy_allunq)
# For the purposes for course project, only look at subject, activity, mean and std columns
sxy_all_meanstd <- select(sxy_allunq, subject, activity, contains("mean"), contains("std"))
#Use descriptive activity names to name the activities in the data set
sxy_all_meanstd_act <- mutate(sxy_all_meanstd, activity_label = activities[sxy_all_meanstd$activity,2])
names(sxy_all_meanstd_act)

## now make this tidy by normalizing the table, i.e., changing the separate
# column names into separate variables for a new variable column
sxy_pre_tidy <- melt(sxy_all_meanstd_act, id=c("subject","activity_label"),measure.vars = names(select(sxy_all_meanstd_act,-activity,-activity_label, -subject)))
sxy_tidy <- group_by(sxy_pre_tidy, subject, activity_label, variable) %>% summarize(average = mean(value))
#output tidy dataset 15480 x 4 
print(sxy_tidy)
# write tidy dataset to file -- commented out, uncommment if you wish to produce the file
# write.table(sxy_tidy, file = "sxy_tidy.txt", row.names=FALSE)