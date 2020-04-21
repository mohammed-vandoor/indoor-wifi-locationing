library(lattice)
library(ggplot2)
library(C50)
library(randomForest)
library(dplyr)
library(caret)

# importing data

filtered<-read.csv(file = 'pca_and_nzv/filtered')
validation1<-read.csv(file='pca_and_nzv/validation1')

# predicting latitude

latitude<-filtered %>% select(-LONGITUDE,-FLOOR,-BUILDINGID)
validation<-validation1%>% select(-LONGITUDE,-FLOOR,-BUILDINGID)
set.seed(100)
sample_set<- sample_n(latitude,1000)


# splitting the data set for training and testing

intraining=createDataPartition(sample_set$LATITUDE,p=.75,list=FALSE)
training=sample_set[intraining,]
testing=sample_set[-intraining,]

#training knn model 
ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.k = c(1: 20))
knnFitl <- train((LATITUDE ~ .), data = training, method = "knn", trControl = ctrl, tuneLength = 5)




#Output of kNN fit
knnFitl

# test the k-NN model
knnPredictl <- predict(knnFitl,newdata = testing)

# confusion matrix to see accuracy value and other parameter values

postResample(knnPredictl,testing$LATITUDE)

# Check results on validation dataset
# Apply k-NN model to the validation data
knnPredicttestl <- predict(knnFitl,newdata = validation)
postResample(knnPredicttestl,validation$LATITUDE)

# training linear regression model 
ctrl <- trainControl(method="cv",number = 10) 
lmFitl <- train((LATITUDE ~ .), data = training, method = "lm", trControl = ctrl)




#Output of linear regression model

lmFitl 

# test the lm model

lmPredictl <- predict(lmFitl,newdata = testing)


# post resample matrix to see mae value and other parameter values

postResample(lmPredictl,testing$LATITUDE)

# Check results on validation dataset
# Apply lm model to the validation data

lmPredicttestl <- predict(lmFitl,newdata = validation)
postResample(lmPredicttestl,validation$LATITUDE)

#training random forest model 

ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.mtry = c(1: 5))
randomFitl <- train((LATITUDE~.),data=training,method = "rf",trControl = ctrl,tuneGrid = tuneGrid)
randomFitl

#Output of random fit

randomFitl

# test the random model

randomPredictl <- predict(randomFitl,newdata = testing)

#  to see RMSE value and other parameter values

postResample(randomPredictl,testing$LATITUDE)


# Check results on validation dataset
# Apply random model to the validation data

randomPredicttestl <- predict(randomFitl,newdata = validation)
postResample(randomPredicttestl,validation$LATITUDE)

#compare the models


resamps=resamples(list(lmmodel=lmFitl,rfmodel=randomFitl,knnmodel=knnFitl))
summary(resamps)
# predicting Longitude

longitude<-filtered %>% select(-LATITUDE,-FLOOR,-BUILDINGID)
validation<-validation1%>% select(-LATITUDE,-FLOOR,-BUILDINGID)
set.seed(100)
sample_set<- sample_n(longitude,1000)


# splitting the data set for training and testing

intraining=createDataPartition(sample_set$LONGITUDE,p=.75,list=FALSE)
training=sample_set[intraining,]
testing=sample_set[-intraining,]

#training knn model 
ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.k = c(1: 20))
knnFit <- train((LONGITUDE ~ .), data = training, method = "knn", trControl = ctrl, tuneLength = 5)

#Output of kNN fit

knnFit 

# test the k-NN model
knnPredict <- predict(knnFit,newdata = testing)


# confusion matrix to see accuracy value and other parameter values

postResample(knnPredict,testing$LONGITUDE)

# Check results on validation dataset
# Apply k-NN model to the validation data
knnPredicttest <- predict(knnFit,newdata = validation)
postResample(knnPredicttest,validation$LONGITUDE)[3]


post
# training linear regression model 
ctrl <- trainControl(method="cv",number = 10) 
lmFit <- train((LONGITUDE ~ .), data = training, method = "lm", trControl = ctrl)




#Output of linear regression model

lmFit 

# test the lm model

lmPredict <- predict(lmFit,newdata = testing)


# post resample matrix to see mae value and other parameter values

postResample(lmPredict,testing$LONGITUDE)

# Check results on validation dataset
# Apply lm model to the validation data

lmPredicttest <- predict(lmFit,newdata = validation)
postResample(lmPredicttest,validation$LONGITUDE)

#training random forest model 

ctrl <- trainControl(method="cv",number = 10) 
tuneGrid <- expand.grid(.mtry = c(1: 5))
randomFit <- train((LONGITUDE~.),data=training,method = "rf",trControl = ctrl,tuneGrid = tuneGrid)
randomFit

#Output of random fit

randomFit

# test the random model

randomPredict <- predict(randomFit,newdata = testing)

#  to see RMSE value and other parameter values

postResample(randomPredict,testing$LONGITUDE)

# Check results on validation dataset
# Apply random model to the validation data

randomPredicttest <- predict(randomFit,newdata = validation)
postResample(randomPredicttest,validation$LONGITUDE)

#compare the models


resamps=resamples(list(lmmodel=lmFit,rfmodel=randomFit,knnmodel=knnFit))
summary(resamps)


lat_pred=as.data.frame(randomPredicttestl)
lon_pred=as.data.frame(randomPredicttest)
LONGLAT_PREDICTIONS <- as.data.frame(c(lon_pred, lat_pred))
validationLONGLAT <- validation1 %>% select(LONGITUDE,LATITUDE)
# change column names
colnames(LONGLAT_PREDICTIONS)[1] <- 'LONGITUDE'
colnames(LONGLAT_PREDICTIONS)[2] <- 'LATITUDE'

# Plot real and predicted results
# Training and Validation log in locations
ggplot() +
  geom_point(data = LONGLAT_PREDICTIONS , aes(x = LONGITUDE, y = LATITUDE, colour = "Predictions")) +
  geom_point(data = validation1 , aes(x = LONGITUDE, y = LATITUDE, colour = "Real values")) +
  ggtitle("Log In Locations") 
randomFitl
