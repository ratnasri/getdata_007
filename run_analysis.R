# Date: 20th Sep 2014
# Purpose: To produce tidy data set as per getdata_007 course requirement
# Brief process:
# 1. Download source data and unzip it
# 2. Prepare feature list data frame and activities labels data frames
# 3. For test and train data, get required measurements along with proper labels
# 4. Club the two data frames into one 
# 5. From this data frame, extract the summary data frame and save it as tidy_dataset.txt


#Step 1 - download input data
#Note - These lines are deliberately commented to avoid traffic to the url
    #file_url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    #download.file(file_url,destfile="wk3_source.zip")
    #source_files_list <- unzip("wk3_source.zip")
    setwd('./UCI HAR Dataset')

#Step 2 - Prepare features list and activities labels
    features_original <- read.csv("features.txt",stringsAsFactors=FALSE,
    				sep="-",header=FALSE)
    #Note that both space and - are there as column separators in this file
    #Need to explicitly split one of the separators
    temp_features_list <- matrix(unlist(strsplit(features_original[,1],
    	split=" ")),
	ncol=2, byrow=TRUE)
    features_df <- data.frame(temp_features_list[,1],
    				temp_features_list[,2],
				features_original[,2],
				features_original[,3],
				stringsAsFactors=FALSE)
    colnames(features_df) <- 
    	c("FeatureId", "SignalType", "Operation","Axis")

    #We need only mean and std measuements. Identify required features
    required_features_df <- features_df[features_df$Operation %in% 
    		c("mean()", "std()"),]

    #Need to read features file once again to extract feature labels
    features_original <- read.csv("features.txt", sep=" ",
    		stringsAsFactors=FALSE, header=FALSE)
    colnames(features_original) <- c("FeatureId", "FeatureLabel")
    required_features_df <-  merge (features_original, required_features_df,
    				by="FeatureId",sort=FALSE)

    #Remove () from labels to improve readability
    required_features_df$FeatureLabel <- unlist(lapply(
    		required_features_df$FeatureLabel,
		function(x) {sub("()","",x,fixed=TRUE)}))

    #Cleanup space
    rm(features_original)
    rm(temp_features_list)


    #Perpare activities dataframe
    activity_labels <- read.csv("activity_labels.txt",sep=" ",
    		stringsAsFactors=FALSE, header=FALSE)
    colnames(activity_labels) <- c("activity_code","activity")


#Step 3 - Prepare train and test data frames
    #Process train data
    #Get subjects (volunteers)
    subjects_train_df <- read.csv("train/subject_train.txt",
    	stringsAsFactors=FALSE, header=FALSE)
    colnames(subjects_train_df) <- c("subject_id")

    #Read X_train_data
    #This file has multiple consecutive whitespaces as separators in some places
    #So, use read.table instead of read.csv
    x_train_original <- read.table("train/X_train.txt", 
    	stringsAsFactors=FALSE, header=FALSE)
    #Extract required measurements (columns)
    x_train_df <- x_train_original[,required_features_df$FeatureId]
    colnames(x_train_df) <- required_features_df$FeatureLabel
    #Free up space
    rm(x_train_original)

    #Read y_train_data, which contains activity code
    y_train_original <- read.csv ("train/y_train.txt",
    	stringsAsFactors=FALSE, header=FALSE)
    colnames(y_train_original) <- c("activity_code")
    #Add activity labels
    train_activity_df <- merge(activity_labels, y_train_original,
    				by="activity_code")
    #Free up space
    rm(y_train_original)

    #Consolidate all train data into one data frame
    train_df <- data.frame(subjects_train_df[,1], train_activity_df[,2],
    			x_train_df)

    #Rename non-x_train_df columns
    colnames(train_df)[1:2] <- 
    	c("subject_id","activity")

    #Free up unused objects
    rm(train_activity_df)
    rm(subjects_train_df)
    rm(x_train_df)

    #Follow the same procedure for test data
    #Subjects (volunteers)
    subjects_test_df <- read.csv("test/subject_test.txt",
    	stringsAsFactors=FALSE, header=FALSE)
    colnames(subjects_test_df) <- c("subject_id")

    #Read X_test_data, using read.table
    x_test_original <- read.table("test/X_test.txt",
    	stringsAsFactors=FALSE)
    x_test_df <- x_test_original[,required_features_df$FeatureId]
    colnames(x_test_df) <- required_features_df$FeatureLabel
    #Free up space
    rm(x_test_original)

    #Read Y_test_data, activities
    y_test_original <- read.csv("test/y_test.txt",
    	stringsAsFactors=FALSE, header=FALSE)
    colnames(y_test_original) <- c("activity_code")

    #Set activity labels
    test_activity_df <- merge(activity_labels,y_test_original,
    			by="activity_code")
    rm(y_test_original)

    #Consolidate test data into one data frame
    test_df <- data.frame(subjects_test_df[,1], test_activity_df[,2],
    			x_test_df)

    #Rename mismatching column ames
    colnames(test_df)[1:2] <-
    	c("subject_id", "activity")

    #Step 4 - club test and train data
    complete_df <- rbind(train_df,test_df)

    #Free up space
    rm(train_df)
    rm(test_df)

    #Create final tidy data set
    library(reshape2)

    #reshape data using melt 
    melted_df <- melt(complete_df,id=c("activity","subject_id"))

    #Compute mean
    tidy_df_with_mean <- dcast(melted_df, ... ~ variable, mean)

    #Write into file
    write.csv(tidy_df_with_mean,file="tidy_data.txt", row.names=FALSE)

    #free up space
    rm(complete_df)
    rm(melted_df)
    rm(tidy_df_with_mean)

    
