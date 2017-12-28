## Merge training and test data sets
# Read in data (headers and labels)

headers <- read.delim("UCI HAR Dataset/features.txt", header=FALSE, sep="")
activitylbls <- as.list(read.delim("UCI HAR Dataset/activity_labels.txt", header=FALSE, sep=""))

# Read in data (training)

trnsbj <- read.delim("UCI HAR Dataset/train/subject_train.txt", header=FALSE, sep="")
ytrn <- read.delim("UCI HAR Dataset/train/y_train.txt", header=FALSE, sep="")
xtrn <- read.delim("UCI HAR Dataset/train/X_train.txt", header=FALSE, sep="")

# Gets rid of factors from activitylbls, correctly uses ytrn
# as lookup

activity <- levels(droplevels(activitylbls$V2))
acttrn <- activity[ytrn[[1]]]

## Appropriately label the data set with descriptive
## variable names

colnames(xtrn) <- headers[,2]

# Read in data (test)

tstsbj <- read.delim("UCI HAR Dataset/test/subject_test.txt", header=FALSE, sep="")
ytst <- read.delim("UCI HAR Dataset/test/y_test.txt", header=FALSE, sep="")
xtst <- read.delim("UCI HAR Dataset/test/X_test.txt", header=FALSE, sep="")

# Uses ytst as lookup

acttst <- activity[ytst[[1]]]

# Tidies test data with added headers

colnames(xtst) <- headers[,2]

# Combines training and test data into complete dataset

complete <- rbind(xtrn, xtst)

## Extract only measurements on the mean and standard
## deviation for each measurement

values <- complete[,c(grep("mean", colnames(complete)), grep("std",colnames(complete)))]

## Use descriptive activity names to name activities
## within each data set: 

csbj <- append(trnsbj[,1],tstsbj[,1])
cact <- append(acttrn,acttst)
values$SubjectName <- csbj
values$Activity <- cact

## From the data set above, create a second independent
## tidy data set with the average of each variable for
## each activity and each subject

avgs <- values %>% group_by(SubjectName, Activity) %>% summarize_all(mean)
write.table(avgs, file = "DataCleaningExercise.txt", row.names=FALSE)