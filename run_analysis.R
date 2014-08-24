root_directory<-getwd()
aux_url<-paste0(root_directory,"/UCI HAR Dataset")

##Reads x_data for train and test
files<-c("train","test")

for (i in 1:length(files)){
    DS<-files[i]
    file_url<-paste0(aux_url,"/",DS,"/X_",DS,".txt")
    
    if(i==1){
        X_data<-read.table(file_url)
    }else{
        X_data<-rbind(X_data,read.table(file_url))  
    }    
}

##Reads features and names Data Set columns

file_url<-paste0(aux_url,"/","features.txt")
features<-read.table(file_url, stringsAsFactors = FALSE)

colnames(X_data)<-features[,2]
rm(features)


##Reads Activity, binds it to Data Set, names it and adds labels

for (i in 1:length(files)){
    DS<-files[i]
    file_url<-paste0(aux_url,"/",DS,"/y_",DS,".txt")
    
    if(i==1){
        y_data<-read.table(file_url)
    }else{
        y_data<-rbind(y_data,read.table(file_url))  
    }
    
}

X_data<-cbind(X_data,y_data)
rm(y_data)
colnames(X_data)[562]<-"Activity"
X_data$Activity<-as.factor(X_data$Activity)

file_url<-paste0(aux_url,"/","activity_labels.txt")
activity_labels<-read.table(file_url)
levels(X_data$Activity)<-activity_labels[,2]
rm(activity_labels)

##Reads Subjects for train and test and binds it to Data Set

for (i in 1:length(files)){
    DS<-files[i]
    file_url<-paste0(aux_url,"/",DS,"/subject_",DS,".txt")
    
    if(i==1){
        subject_data<-read.table(file_url)
    }else{
        subject_data<-rbind(subject_data,read.table(file_url))  
    }
    
}

X_data<-cbind(subject_data,X_data)
colnames(X_data)[1]<-"Subject"
X_data$Subject<-as.factor(X_data$Subject)
rm(subject_data)


##Drops all non Mean / SD variables and creates the Definitive Clean Data Set

Clean_Data_Set_cols<-c(grep("Subject",colnames(X_data)),grep("Activity",colnames(X_data)),grep("std",colnames(X_data)),grep("mean",colnames(X_data)))
Clean_Data_Set_cols<-sort(Clean_Data_Set_cols)

Clean_Data_Set<-X_data[,Clean_Data_Set_cols]

##Creates the second DB with means of variables

Mean_DataSet<-as.data.frame(rep(1:30,each=6))
Mean_DataSet<-cbind(Mean_DataSet,rep(1:6,times=30))
colnames(Mean_DataSet)<-c("Subject","Activity")

Mean_DataSet$Activity<-as.factor(Mean_DataSet$Activity)
levels(Mean_DataSet$Activity)<-levels(Clean_Data_Set$Activity)

ind=list(as.integer(Clean_Data_Set$Subject),as.integer(Clean_Data_Set$Activity))

for(i in 1:ncol(Clean_Data_Set)){
    if(!(colnames(Clean_Data_Set)[i]=="Subject" | colnames(Clean_Data_Set)[i]=="Activity")){
        
        tab_aux<-as.numeric(tapply(Clean_Data_Set[,i],ind,mean))
        
        Mean_DataSet<-cbind(Mean_DataSet,tab_aux)
        colnames(Mean_DataSet)[ncol(Mean_DataSet)]<-colnames(Clean_Data_Set)[i]
    }
}

##Exports final results
write.table(Mean_DataSet,"TidyDataSet_Mean.txt",row.names=FALSE)
