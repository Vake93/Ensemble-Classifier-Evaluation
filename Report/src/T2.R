library(mlbench)
library(caret)
library(ggplot2)
library(corrplot)
library(caretEnsemble)

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

# Repeated CV for Bagging type classifier	
set.seed(128)
bagging_control <- trainControl(method="repeatedcv",number=10,repeats=3)

# Repeated CV for Stacking type classifier
set.seed(128)
stacking_control <- trainControl(method="repeatedcv",number=10,repeats=3,savePredictions='final',classProbs=TRUE)

# Random Forest
set.seed(256)
rf <- train(target~., data=training, method="rf", metric="Accuracy", trControl=bagging_control)

# Bagged CART
set.seed(256)
treebag <- train(target~.,data=training,method="treebag",metric="Accuracy",trControl=bagging_control)

# Summarize Results
bagging_results <- resamples(list(treebag=treebag, rf=rf))
summary(bagging_results)
dotplot(bagging_results)

# Stacking Algorithms
set.seed(256)
stacking_algorithms <- c('rpart', 'knn', 'nb')

models <- caretList(target~., data=training, trControl=stacking_control, methodList=stacking_algorithms)
results <- resamples(models)
summary(results)
dotplot(results)


# Testing Random Forest
pred<-predict(rf,newdata=testing)
confusionMatrix(data=pred,testing$target)

# Testing Bagged CART
pred<-predict(treebag,newdata=testing)
confusionMatrix(data=pred,testing$target)

# Testing Bagged CART
pred<-predict(treebag,newdata=testing)
confusionMatrix(data=pred,testing$target)
