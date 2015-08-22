require(data.table)
require(dplyr)
require(stringr)

#setwd("REQUIRED MAIN DIRECTORY")

#Train Data
trainDirectory <- "./train"
#Read Tables and convert to Data Tables for faster & efficient calculations
trainSetX <- setDT(read.table(file.path(trainDirectory,"X_train.txt")))  
trainSetActivity <- setDT(read.table(file.path(trainDirectory,"Y_train.txt")))  
trainSetSubjects <- setDT(read.table(file.path(trainDirectory,"subject_train.txt")))
trainSet <- cbind(trainSetSubjects,trainSetActivity,trainSetX)
rm(list = c("trainSetSubjects","trainSetX","trainSetActivity","trainDirectory"))

#Test Data
testDirectory <- "./test"
testSetX <- setDT(read.table(file.path(testDirectory,"X_test.txt")))  
testSetActivity <-setDT(read.table(file.path(testDirectory,"Y_test.txt")))  
testSetSubjects <- setDT(read.table(file.path(testDirectory,"subject_test.txt")))
testSet <- cbind(testSetSubjects,testSetActivity,testSetX)
rm(list = c("testSetSubjects","testSetX","testSetActivity","testDirectory"))

#Merge
set <- rbind(trainSet,testSet)
rm(list = c("testSet","trainSet"))

#Set ColumnNames
colNames <- read.table("./features.txt",colClasses = c("integer","character"))
colNames <- colNames[2]
colNames <- sapply(rbind("SubjectID","Activity",colNames),as.character)
setNames(set,colNames)

#Select only Required Columns
requiredColumns<-c(1:2,grep("(mean\\(|std)",colNames))
set <- set[,requiredColumns,with=FALSE]
set$SubjectID <- factor(set$SubjectID)
set$Activity <- factor(set$Activity)

#Perform avg with 2 factors "SubjectID" and "Activity"
Fin <- aggregate(set[,3:ncol(set),with=FALSE],
                 FUN = mean,
                 by = list(SubjectID = set$SubjectID,Activity = set$Activity))
#Display Activity Factor in readable Format
levels(Fin$Activity) <- c("WALKING",
                          "WALKING_UPSTAIRS",
                          "WALKING_DOWNSTAIRS",
                          "SITTING",
                          "STANDING",
                          "LAYING")

#Output to text file
write.table(Fin,"TidyOutput.txt",row.names = FALSE,col.names = FALSE,quote=FALSE)

#Remove all values except output
rm(list=setdiff(ls(),"Fin"))