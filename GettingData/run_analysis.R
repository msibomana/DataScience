# Getting and Cleaning Data project
# run_analysis.R script 
# Author: MSibomana
#
# usage
#   data4 <- read_data4(datadir) : reads training and test datasets from user provided directory
#                                  and returns one merged dataset.
#   data5 <- make_data5(data4) : Creates a new dataset with averages from data4 of each variable 
#                                for each activity and each subject
#   data5 <- read_data5(datadir) : calls make_data5(read_data4(datadir))
#
#   write_data5(data5, filename): writes data5 to filename in text format without row names
#
#   read_write_data5(datadir, filename): calls write_data5(read_data5(datadir),filename)

library(data.table)
library(dplyr)

read_dataset <- function(datadir,dset,lbl) {
    lname <- paste0(dset,".txt")
    t <- fread(file.path(datadir,dset,paste0("X_",lname)))
    y <- fread(file.path(datadir,dset,paste0("y_",lname)))
    s <- fread(file.path(datadir,dset,paste0("subject_",lname)))
    colnames(t) <- lbl
    #remove duplicated columns
    t <- tbl_df(t)[, !duplicated(colnames(t))]
    roi <- bind_cols(select(t,contains("mean()")), select(t,contains("std()")))
    #rename -std and -mean by Std and Mean and remove punctuation characters
    roiNames <- gsub("-std", "Std", colnames(roi))
    roiNames <- gsub("-mean", "Mean", roiNames)
    roiNames <- gsub("([[:punct:]])","",roiNames)
    res <- bind_cols(s,mutate(y,Dataset=dset),roi)
    colnames(res) <- append(c("Subject", "Activity","Dataset"), roiNames)
    res
}

read_data4 <- function(datadir) {
    lbl <- fread(file.path(datadir,"features.txt"))
    train <- read_dataset(datadir,"train",lbl$V2)
    test <- read_dataset(datadir,"test",lbl$V2)
    data4 <- bind_rows(train, test)
    # convert Subject,Activity and Dataset to factors
    data4$Subject <- as.factor(data4$Subject)
    data4$Activity <- as.factor(data4$Activity)
    data4$Dataset <- as.factor(data4$Dataset)
    data4
}

make_data5 <- function(data4) {
    criteria <-list(Subject=data4$Subject,Activity=data4$Activity,Dataset=data4$Dataset)
    data5 <- aggregate(data4[4:69], by=criteria, mean)
}

read_data5 <- function(datadir) {
    make_data5(read_data4(datadir))
}

write_data5 <- function(data5, fname) {
  write.table(arrange(data5,Subject,Activity),fname,row.names = FALSE)
}

read_write_data5 <- function(datadir, fname) {
  write_data5(read_data5(datadir), fname)
}
