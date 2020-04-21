library(lattice)
library(ggplot2)
library(C50)
library(randomForest)
library(dplyr)
library(caret)

# import the data 

trainingdata=read.csv(file = 'preprocess_data/trainingdata')
validationdata=read.csv(file = 'preprocess_data/validationdata')

# remove near zero variance

nzv_cols=nearZeroVar(trainingdata,
                     freqCut = 3500,
                     uniqueCut = .1,
                     saveMetrics = FALSE,
                     names = FALSE,
                     foreach = FALSE,
                     allowParallel = TRUE)
trainingdata=trainingdata[,-nzv_cols]
validationdata=validationdata[,-nzv_cols]

# apply pca

pca<-prcomp(trainingdata[1:388],scale=TRUE)

# find the standerd deviation

pca.variance<-pca$sdev^2
pca.var.percentage<-round(pca.variance/sum(pca.variance)*100,2)
sum(pca.var.percentage[1:155])# gives 95 percentage of variance

# remove the rest of the variables
#keep only 260 colomns 
n=155
train_pca<-as_tibble(pca$x)%>% select(PC1:n)

# remove the colomns for validation dataset
valid<-validationdata[,1:388]
valid_pca<- as_tibble(
  predict(
    pca,
    newdata = valid
  )) %>% select(PC1:n)

# compine the rest of the colomns 

train1<-trainingdata %>% select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID)
filtered<- cbind(train_pca,train1)
valid1<-validationdata %>% select(LONGITUDE,LATITUDE,FLOOR,BUILDINGID)
validation1<-cbind(valid_pca,valid1)

write.csv(filtered,file = 'pca_and_nzv/filtered')
write.csv(validation1,file='pca_and_nzv/validation1')
