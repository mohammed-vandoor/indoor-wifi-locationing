

# importing data set

trainingdata=read.csv(file ="UJIndoorLoc/trainingData.csv")
validationdata=read.csv(file = "UJIndoorLoc/validationData.csv")

# remove duplicated rows


no_duplicated<-trainingdata[!duplicated(trainingdata),]
no_duplicated_valda<-validationdata[!duplicated(validationdata),]
#filtering the row which are not connected to any wifi

filtered <- no_duplicated[!apply(no_duplicated[,c(1:520)],1,function(z) all(z==100)),]
filtered_valid<-no_duplicated_valda[!apply(no_duplicated_valda[,c(1:520)],1,function(z) all(z==100)),]
# replecing  100 with -101

trainingdata=filtered[1:524]
trainingdata[trainingdata==100]<--110
validationdata=filtered_valid[1:524]
validationdata[validationdata==100]<--110

# write the pre processed csv file

write.csv(trainingdata,file = 'preprocess_data/trainingdata'  )
write.csv(validationdata,file = 'preprocess_data/validationdata')
