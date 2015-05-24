#-----------------------------------------------------------------------
# STEP 1 - Merges the training and the test sets to create one data set.
#-----------------------------------------------------------------------



# We will assume the .zip file has already been unzipped in the working directory,
# meaning that all the files are already accessible in a 'UCI HAR Dataset/' folder

# We will also assume that we want ALL THE DATA in this first data set,
#                                  -------------
# including the 'inertial signals' data included in the 9 files in the 2 directories :
# 'UCI HAR Dataset/test/Inertial Signals/'
# 'UCI HAR Dataset/train/Inertial Signals/'

# As a consequence of this choice, the data set we want to create at STEP 1 will have
# the following columns :

# col1 -> the subject id from 'subject_test.txt' and 'subject_train.txt'
# col2 -> the activity id from 'y_test.txt' and 'y_train.txt'
# col3 to col563 -> the 561 features variables from 'X_test.txt' and 'X_train.txt'
# col564 to col1715 -> the 128 raw measures of each of the 9 internal signals file

# ==> we will name these 128 raw variables after the name of the internal signals file
# ==> for example, the 'body_acc_x_test.txt' file will give 128 columns named :
# body_acc_x-1
# body_acc_x-2
# body_acc_x-...
# body_acc_x-128

# We will make a function MakeStep1DataFrame()
# This function has one argument : 'test' or 'train'
# and accordingly reads and binds all the 'test' or 'train' files

MakeStep1DataFrame <- function(TestOrTrain) {
        
        #TestOrTrain can be 'test' or 'train' according to folders we want to read
        
        # we first read and bind the subject, activity and features data
        Subj_path <- paste("UCI HAR Dataset/", TestOrTrain, "/subject_", TestOrTrain, ".txt", sep="")
        Acti_path <- paste("UCI HAR Dataset/", TestOrTrain, "/y_", TestOrTrain, ".txt", sep="")
        Feat_path <- paste("UCI HAR Dataset/", TestOrTrain, "/X_", TestOrTrain, ".txt", sep="")
        Subj <- read.table(Subj_path, header = F)
        Acti <- read.table(Acti_path, header = F)
        Feat <- read.table(Feat_path, header = F)
        data <- data.frame()
        data <- cbind(Subj, Acti, Feat)
        a <- read.table("UCI HAR Dataset/features.txt", header = F)
        names(data) <- c("Subj", "Act", as.vector(a$V2))
        
        # then we bind the raw data from the 9 internal signals file
        
        # this file list WITH full names will be used to read the data of the 9 inter. sign. files
        Sign_filelist_fnY <- list.files(
                paste("UCI HAR Dataset/", TestOrTrain,"/Inertial Signals", sep=""), 
                full.names = T)
        
        # this file list WITHOUT full names will be used to name the 128 columns 'file_name-x'
        Sign_filelist_fnN <- list.files(
                paste("UCI HAR Dataset/", TestOrTrain,"/Inertial Signals", sep=""), 
                full.names = F)
        
        # we read each of the 9 internal signals file, add a label 'filename-xx' to the 128 col
        for (i in (1:length(Sign_filelist_fnY))) {
                file_data <- read.table(Sign_filelist_fnY[i], header = F)
                file_name <- gsub(pattern = paste("_", TestOrTrain, ".txt", sep=""),
                                  replacement = "", x = Sign_filelist_fnN[i])
                names(file_data) <- paste(file_name,"-",1:128,sep = "")
                data <- cbind(data, file_data)
        }
        data
}


# Now we run this function with 'test' and then 'train' argument
# And we bind by rows in a single data frame

test_df <- MakeStep1DataFrame("test")
train_df <- MakeStep1DataFrame("train")
df1 <- rbind(test_df, train_df)

#-----------------------------------------------------------------------
# STEP 2 -  Extracts only the measurements on the mean and standard 
# deviation for each measurement. 
#-----------------------------------------------------------------------

df2 <- df1[, c(1, 2, grep(pattern = "mean", names(df1)), 
              grep(pattern = "std", names(df1)))]


#-----------------------------------------------------------------------
# STEP 3 - Uses descriptive activity names to name the activities in the data set
#-----------------------------------------------------------------------

ActLab <- read.table("UCI HAR Dataset/activity_labels.txt", header = F)
df2$Act <- factor(x = df2$Act, levels = ActLab$V1, labels = ActLab$V2)

#-----------------------------------------------------------------------
# STEP 4 - Appropriately labels the data set with descriptive variable names. 
#-----------------------------------------------------------------------

# ==> already done in step 1

#-----------------------------------------------------------------------
# STEP 5 - From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable
# for each activity and each subject.
#-----------------------------------------------------------------------

# we wil need function from the 'dply' package
library(plyr)

df3 <- ddply(df2, .(Subj, Act), colwise(mean))

write.table(x = df3, file = "step5data.txt", row.name=FALSE, sep = ",")

