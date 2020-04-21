library(lattice)
library(ggplot2)
library(C50)
library(randomForest)
library(dplyr)
library(caret)

# importing data

filtered<-read.csv(file = 'pca_and_nzv/filtered')
validation1<-read.csv(file='pca_and_nzv/validation1')

# predicing model for building id

buildingid<-filtered %>% select(-LONGITUDE,-FLOOR,-LATITUDE)
buildingid$BUILDINGID<-as.factor(buildingid$BUILDINGID)
validation<-validation1%>% select(-LONGITUDE,-FLOOR,-LATITUDE)
validation$BUILDINGID<-as.factor(validation$BUILDINGID)
set.seed(100)
sample_set<-sample_n(buildingid,1500)



# splitting the data set for training and testing

intraining=createDataPartition(sample_set$BUILDINGID,p=.75,list=FALSE)
training=sample_set[intraining,]
testing=sample_set[-intraining,]

#training knn model 
tuneGrid <- expand.grid(.k = c(1: 20))
ctrl <- trainControl(method="cv",number = 10) 
knnFit <- train((BUILDINGID ~ .), data = training, method = "knn", tuneGrid = tuneGrid, trControl = ctrl, tuneLength = 5)

#Output of kNN fit

knnFit 

# test the k-NN model
testing$BUILDINGID<-as.factor(testing$BUILDINGID)
knnPredict <- predict(knnFit,newdata = testing)

# confusion matrix 

confusionMatrix(knnPredict,testing$BUILDINGID)

# Check results on validation dataset
# Apply k-NN model to the validation data
validation$BUILDINGID<-as.factor(validation$BUILDINGID)
knnPredicttest <- predict(knnFit,newdata = validation)

confusionMatrix(knnPredicttest,validation$BUILDINGID)

#training random forest model 
ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.mtry = c(1: 5))
randomFit <- train((BUILDINGID~.),data=training,method = "rf",trControl = ctrl,tuneGrid = tuneGrid)

#Output of random fit
randomFit
# test the random model
randomPredict <- predict(randomFit,newdata = testing)

#  to see RMSE value and other parameter values

confusionMatrix(randomPredict,testing$BUILDINGID)

# Check results on validation dataset
# Apply random model to the validation data
randomPredicttest <- predict(randomFit,newdata = validation)
confusionMatrix(randomPredicttest,validation$BUILDINGID)


# c50 model


c50Fit <- train((BUILDINGID~.),data=training,
                method = "C5.0",
                tuneLength = 2,
                trControl = ctrl)
# test the c5 model
c50Fitpredict <- predict(c50Fit,newdata = testing)


#  to see RMSE value and other parameter values

confusionMatrix(c50Fitpredict,testing$BUILDINGID)

# Check results on validation dataset
# Apply random model to the validation data
c50FitPredicttest <- predict(c50Fit,newdata = validation)
confusionMatrix(c50FitPredicttest,validation$BUILDINGID)


# compairing models 

resamps=resamples(list(c5model=c50Fit,rfmodel=randomFit,knnmodel=knnFit))
summary(resamps)

# predicting floor


floor<-filtered %>% select(-LONGITUDE,-BUILDINGID,-LATITUDE)
floor$FLOOR=as.factor(floor$FLOOR)
validation<-validation1 %>% select(-LONGITUDE,-BUILDINGID,-LATITUDE)
validation$FLOOR=as.factor(validation$FLOOR)
sample_set<-sample_n(floor,1500)

# splitting the data set for training and testing

intraining=createDataPartition(sample_set$FLOOR,p=.75,list=FALSE)
training=sample_set[intraining,]
testing=sample_set[-intraining,]

#training knn model 
tuneGrid <- expand.grid(.k = c(1: 20))
ctrl <- trainControl(method="cv",number = 10) 
knnFit <- train((FLOOR ~ .), data = training, method = "knn",tuneGrid = tuneGrid, trControl = ctrl, tuneLength = 5)

#Output of kNN fit

knnFit 

# test the k-NN model

knnPredict <- predict(knnFit,newdata = testing)

# confusion matrix 

confusionMatrix(knnPredict,testing$FLOOR)

# Check results on validation dataset
# Apply k-NN model to the validation data
knnPredicttest <- predict(knnFit,newdata = validation)

confusionMatrix(knnPredicttest,validation$FLOOR)

#training random forest model 
ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.mtry = c(1: 5))
randomFit <- train((FLOOR~.),data=training,method = "rf",trControl = ctrl,tuneGrid = tuneGrid)
randomFit




#Output of random fit
randomFit
# test the random model
randomPredict <- predict(randomFit,newdata = testing)


##  to see RMSE value and other parameter values

confusionMatrix(randomPredict,testing$FLOOR)

# Check results on validation dataset
# Apply random model to the validation data
randomPredicttest <- predict(randomFit,newdata = validation)
confusionMatrix(randomPredicttest,validation$FLOOR)


# c50 model


c50Fit <- train((FLOOR~.),data=training,
                method = "C5.0",
                tuneLength = 2,
                trControl = ctrl)
# test the c5 model
c50Fitpredict <- predict(c50Fit,newdata = testing)


#  to see RMSE value and other parameter values

confusionMatrix(c50Fitpredict,testing$FLOOR)

# Check results on validation dataset
# Apply random model to the validation data
c50FitPredicttest <- predict(c50Fit,newdata = validation)
confusionMatrix(c50FitPredicttest,validation$FLOOR)


# compairing models 

resamps=resamples(list(c5model=c50Fit,rfmodel=randomFit,knnmodel=knnFit))
summary(resamps)
