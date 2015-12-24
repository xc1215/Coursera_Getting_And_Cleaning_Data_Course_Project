main <- function() {
        finalraw <- step1()
        raw_std_mean <- step2(finalraw)
        raw_std_mean_activity <- step3(raw_std_mean,"activity_labels.txt")
        filter_name <- step4(raw_std_mean_activity)
        summary_data <- step5(filter_name)
        write.table(summary_data, "result.txt", sep='\t', row.names=FALSE)
        return (head(summary_data,2))
        
}
step5 <- function(filter_name){
        #step 5 of course project, summarize data per subject and per activity
        #input data frame filter_name is the output of step 4, raw data with only mean and stdev variable, activity in text, variable name is meaningful
        library(plyr)
        summary_data <- ddply(filter_name,c("subject","activity"),numcolwise(mean))
        return(summary_data)
}
step4 <- function(x_subset_activity) {
        #step 4 of course project, replace old column name with meaningful column names
        #input x_subset_activity is the output of step 3, subset of raw file with only mean and stdev variables and activities is in text, not integer:
        old_col_name <- colnames(x_subset_activity)
        new_col_name<-gsub("Acc","Accelerometer",old_col_name)
        new_col_name<-gsub("Gyro","Gyroscope",new_col_name)
        new_col_name<-gsub("Mag","Magnitude",new_col_name)
        new_col_name<-gsub("mean","Mean",new_col_name)
        new_col_name<-gsub("std","Stdev",new_col_name)
        new_col_name<-gsub("\\(|\\)","",new_col_name)
        new_col_name<-gsub("-","",new_col_name)
        new_col_name<-sub("^t","TimeDomain",new_col_name)
        new_col_name<-sub("^f","FrequencyDomain",new_col_name)
        colnames(x_subset_activity) <- new_col_name
        return(x_subset_activity)
}
step3 <- function(x_subset, activityfilename) {
        #step3 of course project, match activity label ( 1 to 6) to activity names, x_subset is the result of step2
        activityfullfilename <- paste("./",activityfilename,sep="")
        activityname<-read.table(activityfullfilename,header=FALSE)
        colnames(activityname) <- c("label","activity")
        mergex <- merge(x_subset,activityname,by.x="label",by.y="label",all=TRUE)
        mergex <- mergex[,2:ncol(mergex)] 
        #mergex <- mergex[,c(1,ncol(mergex),2:ncol(mergex)-1)]
        return(mergex)
        
}
step2 <- function(x) {
        #step 2 of course project, filter out only columns containing "mean()" or "std()" in data frame x.  x is obtained in step 1
        
        col_to_select <- c("subject","label",grep('\\bmean()\\b|\\bstd()\\b',colnames(x),value=TRUE))
        x_subset <- x[,col_to_select]
        return(x_subset)
}
step1 <- function(){
        #step1 of course project, combine raw files of test and train folder:
        testraw <- readraw("test","X_test.txt","y_test.txt","subject_test.txt","features.txt")
        trainraw <- readraw("train","X_train.txt","y_train.txt","subject_train.txt","features.txt")
        finalraw <- rbind(testraw,trainraw)
        return(finalraw)
}
readraw <- function(directory,xfilename, yfilename, subjectfilename,headerfilename){
        #read raw file and create data frame for test or train folder:
        #sample way to call this function: readraw("test","X_test.txt","y_test.txt","subject_test.txt","feature.txt")
        
        #read X file
        x<-data.frame()
        xfullfilename <- paste("./",directory,"/",xfilename,sep="")
        x<-read.table(xfullfilename,header=FALSE)
        
        #read y file
        y<-data.frame()
        yfullfilename <- paste("./",directory,"/",yfilename,sep="")
        y<-read.table(yfullfilename,header=FALSE)
        
        #read subject file
        subject<-data.frame()
        subjectfullfilename <- paste("./",directory,"/",subjectfilename,sep="")
        subject<-read.table(subjectfullfilename,header=FALSE)
        
        #read feature file, this is the column header
        header<-data.frame()
        headerfullfilename <- paste("./",headerfilename,sep="")
        header<-read.table(headerfullfilename,header=FALSE)
        
        #combine subject, y and x file, get column header from feature file.
        final<-cbind(subject,y,x)
        header <- c("subject","label",as.character(header$V2))
        colnames(final) <- header
        
        #return(header)
        return(final)
        
}
