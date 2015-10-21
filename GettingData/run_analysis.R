# Getting and Cleaning Data project
# run_analysis.R script 
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


lbl <- fread(file.path(".","features.txt"))
train <- read_dataset(".","train",lbl$V2)
test <- read_dataset(".","test",lbl$V2)
data4 <- bind_rows(train, test)
criteria <-list(Dataset=as.factor(data4$Dataset),
                Subject=as.factor(data4$Subject),
                Activity=as.factor(data4$Activity))
data5 <- aggregate(data4[4:69], by=criteria, mean)

