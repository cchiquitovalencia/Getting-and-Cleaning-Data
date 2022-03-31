library(tidyverse)
library(data.table)

#========================================================================#
#=== (1) Merges the training and the test sets to create one data set ===#
#========================================================================#

## Read file of features
features <- fread(paste0(getwd(),"/UCI HAR Dataset/features.txt"))

## Read file of activity labels
activity <- fread(paste0(getwd(),"/UCI HAR Dataset/activity_labels.txt"))

## Created function to read data
function_read_data <- function(set, features, activity){
        
        subject <- fread(paste0(getwd(),"/UCI HAR Dataset/",set,"/subject_",set,".txt")) %>%
                rename(subject = V1)
        
        x_set <- fread(paste0(getwd(),"/UCI HAR Dataset/",set,"/X_",set,".txt"))
        
        y_set <- fread(paste0(getwd(),"/UCI HAR Dataset/",set,"/y_",set,".txt"))
        
        y_set <- merge(y_set, activity) %>%
                rename(activity = V2)
        
        colnames(x_set) <- features$V2
        
        data_set <- cbind(subject,
                            x_set,
                            y_set[,2])
        
        return(data_set)
        
}

## Train data
train_set <- function_read_data("train", features, activity)[, from:="train"]

## Test data
test_set <- function_read_data("test", features, activity)[, from:="test"]

## Create data set
data_set <- rbind(train_set, test_set)



#==================================================================================================#
#=== (2) Extracts only the measurements on the mean and standard deviation for each measurement ===#
#==================================================================================================#
extract <- data_set[,str_detect(colnames(data_set), c("mean\\(\\)|std\\(\\)")), with = FALSE]

head(extract)

#==================================================================================#
#=== (3) Uses descriptive activity names to name the activities in the data set ===#
#==================================================================================#
data_set$activity  ## Already done in (1)



#=============================================================================#
#=== (4) Appropriately labels the data set with descriptive variable names ===#
#=============================================================================#
colnames(data_set) <- tolower(names(data_set)) %>%
        gsub(",", "_", .) %>%
        gsub("-","_", .) %>%
        strsplit("\\()") %>%
        sapply(., function(x){paste0(x[1],x[2])}) %>%
        gsub("NA", "", .)

colnames(data_set) 



#================================================================================#
#=== (5) From the data set in step 4, creates a second, independent tidy data ===#
#=== set with the average of each variable for each activity and each subject ===#
#================================================================================#
tidy_data <- data_set %>% 
        as_tibble( .name_repair = "unique") %>% ## some variable names are duplicated
        gather(key = "Variable", value = "Value", -c(subject, activity)) %>%
        group_by(subject, activity, Variable) %>%
        summarise(Average = mean(as.numeric(Value), na.rm = TRUE))

## Write file
# write_csv(tidy_data, file = "tidy_data.csv")
