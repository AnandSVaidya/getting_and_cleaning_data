library(data.table)

downloadAndUnzipFile <- function(url) {
  if(!file.exists("UCI HAR Dataset")) {
    download.file(url,"data.zip","curl")
    unzip("data.zip")
  }
}

loadRequiredFeatures <- function(features) {
  grep(".*mean.*|.*std.*",features[,2])
}

loadRequiredFeaturesNames <- function(features, requiredFeatures) {
  names <- features[requiredFeatures,2]
  names <- gsub('-mean\\(\\)', 'Mean', names)
  names <- gsub('-std\\(\\)', 'Std', names)
  names <- gsub('-meanFreq\\(\\)', 'MeanFreq', names)
  names <- gsub('-', '', names)
  names
}

loadTrainData <- function(featuresRequired) {
  train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresRequired]
  trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt")
  trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  cbind(trainSubjects, trainActivities, train)
}

loadTestData <- function(featuresRequired) {
  test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresRequired]
  testActivites <- read.table("UCI HAR Dataset/test/y_test.txt")
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  cbind(testSubjects, testActivites, test)
}

activityName <- function(activityIndex, activityLabels) {
  activityLabels[activityIndex,2]
}

downloadAndUnzipFile("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")

activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE)
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE, strip.white = TRUE)

# Required features
requiredFeatures <- loadRequiredFeatures(features)

# Required features names
requiredFeaturesNames <- loadRequiredFeaturesNames(features,requiredFeatures)

# Load train data
trainData <- loadTrainData(requiredFeatures)

# Load test data
testData <- loadTestData(requiredFeatures)

# Merge data
completeData <- rbind(trainData, testData)

# Add column names
colnames(completeData) <- c("Subject", "Activity", requiredFeaturesNames)

# Tidy data
completeData <- data.table(completeData)

# Calculate mean by Subject and Activity
tidydata <- completeData[,lapply(.SD,mean), keyby=.(Subject,Activity)]

# Add Activity Names for readability
tidydata <- tidydata[,ActivityName:=activityName(Activity,activityLabels)]

# Re-order columns
setcolorder(tidydata, c("Subject","Activity","ActivityName",colnames(dt)[!(colnames(dt) %in% c("Subject","Activity","ActivityName"))]))

# Write data in txt format
if(file.exists("tidydata.txt")) {
  file.remove("tidydata.txt")
}

fwrite(tidydata, "tidydata.txt", sep = " ")