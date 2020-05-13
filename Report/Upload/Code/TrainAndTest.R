library(mlbench)
library(caret)
library(ggplot2)
library(corrplot)
library(caretEnsemble)
library(ROCR)

### 2nd Task: Formation of Training and Test Sets
# Load the Data
heart<-read.csv("D:/Projects/heart.csv")

# Data Pre processing: Removing corrupted rows
length(heart$target)
length(heart[heart$ca!=4&heart$thal!=0,]$target)
heart <- heart[heart$ca!=4&heart$thal!=0,]

# Data Pre processing: Convert columns to factors
str(heart)
heart$sex     <- as.factor(heart$sex)
heart$cp      <- as.factor(heart$cp)
heart$fbs     <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang   <- as.factor(heart$exang)
heart$slope   <- as.factor(heart$slope)
heart$thal    <- as.factor(heart$thal)
heart$ca      <- as.factor(heart$ca)
heart$target  <- as.factor(heart$target)

levels(heart$sex)     <- c('F','M')
levels(heart$cp)      <- c('TA','ATA','NAP','AS')
levels(heart$fbs)     <- c('NO','YES')
levels(heart$restecg) <- c('NORM','ABNORM','VH')
levels(heart$exang)   <- c('NO','YES')
levels(heart$slope)   <- c('UP','FLT','DOWN')
levels(heart$thal)    <- c('FIX','NORM','REVDEF')
levels(heart$ca)      <- c('NONE','ONE','TWO','THREE')
levels(heart$target)  <- c('NO', 'YES')

str(heart)

# Seting up training and testing datasets
set.seed(4096)
intrain <- createDataPartition(y=heart$target,p=0.75,list=FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]
dim(training)
dim(testing)

### 3rd Task: Build Train and Test a Bagging type Classifier
# Repeated CV for Bagging type classifier	
set.seed(128)
bagging_control <- trainControl(method="repeatedcv",number=10,repeats=3)

# Repeated CV for Stacking type classifier
set.seed(128)
stacking_control <- trainControl(method="repeatedcv",number=10,repeats=3,savePredictions='final',classProbs=TRUE)

# Random Forest
set.seed(256)
start_time <- Sys.time()
rf <- train(target~., data=training, method="rf", metric="Accuracy", trControl=bagging_control)
end_time <- Sys.time()
end_time - start_time

# Bagged CART
set.seed(256)
start_time <- Sys.time()
treebag <- train(target~.,data=training,method="treebag",metric="Accuracy",trControl=bagging_control)
end_time <- Sys.time()
end_time - start_time

# Summarize Results
bagging_results <- resamples(list(treebag=treebag, rf=rf))
summary(bagging_results)
dotplot(bagging_results)

# Testing Random Forest
rf_pred<-predict(rf,newdata=testing)
confusionMatrix(data=rf_pred,testing$target)

# Testing Bagged CART
treebag_pred<-predict(treebag,newdata=testing)
confusionMatrix(data=treebag_pred,testing$target)

### 4th Task: Build Train and Test a Stacking type Classifier
# Stacking Algorithms
set.seed(512)
stacking_algorithms <- c('rpart', 'knn', 'nb')

# Training 'rpart', 'knn' and 'nb' models in parallel
start_time <- Sys.time()
models <- caretList(target~., data=training, trControl=stacking_control, methodList=stacking_algorithms)
end_time <- Sys.time()
end_time - start_time

results <- resamples(models)
summary(results)
dotplot(results)

# Testing Stacking CART
rpart_pred<-predict(models$rpart,newdata=testing)
confusionMatrix(data=rpart_pred,testing$target)

# Testing Naive Bayes
nb_pred<-predict(models$nb,newdata=testing)
confusionMatrix(data=nb_pred,testing$target)

# Testing K-NN
knn_pred<-predict(models$knn,newdata=testing)
confusionMatrix(data=knn_pred,testing$target)

# Correlation Between Results
modelCor(results)
splom(results)

# Combining the predictions of the classifiers using a simple linear model
set.seed(512)
start_time <- Sys.time()
stack_glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stacking_control)
end_time <- Sys.time()
end_time - start_time

print(stack_glm)

# Testing linear model combined predictors 'rpart', 'knn' and 'nb
stack_glm_pred <- predict(stack_glm,newdata=testing)
confusionMatrix(data=stack_glm_pred,testing$target)

### 5th Task: Measure Performance
# Re-run the predictions with type set to probability
rf_pred_prob        <- predict(rf,newdata=testing,type="prob")
treebag_pred_prob   <- predict(treebag,newdata=testing,type="prob")
rpart_pred_prob     <- predict(models$rpart,newdata=testing,type="prob")
nb_pred_prob        <- predict(models$nb,newdata=testing,type="prob")
knn_pred_prob       <- predict(models$knn,newdata=testing,type="prob")
stack_glm_pred_prob <- predict(stack_glm,newdata=testing,type="prob")

measure_performance <- function(pred_prob)
{
  perf <- prediction(pred_prob,testing$target,label.ordering = c('YES', 'NO'))
  
  perf.prec_rec <- performance(perf,measure="prec",x.measure='rec')
  plot(perf.prec_rec)
  
  perf.acc <- performance(perf,measure="acc")
  plot(perf.acc)
  
  perf.roc = performance(perf,measure="tpr",x.measure="fpr")
  plot(perf.roc)
  
  perf.auc = performance(perf,measure="auc")
  perf.rauc <- perf.auc@y.values
  perf.rauc
}

measure_performance(rf_pred_prob$NO)
measure_performance(treebag_pred_prob$NO)
measure_performance(rpart_pred_prob$NO)
measure_performance(nb_pred_prob$NO)
measure_performance(knn_pred_prob$NO)
measure_performance(stack_glm_pred_prob)
