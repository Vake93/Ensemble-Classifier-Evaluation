library(mlbench)
library(caret)
library(ggplot2)
library(corrplot)

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
str(heart)

# Rank Features By Importance
set.seed(1024)
control <- trainControl(method="repeatedcv",number=10,repeats=3)
model <- train(target~.,data=heart,method="lvq",trControl=control)
importance <- varImp(model,scale=FALSE)
plot(importance)

# Automatic feature selection
set.seed(2048)
control <- rfeControl(functions=rfFuncs,method="cv",number=10)
results <- rfe(x=heart[,1:13],y=heart[,14],sizes=c(1:13),rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))

# Seting up training and testing datasets
set.seed(4096)
intrain <- createDataPartition(y=heart$target,p=0.75,list=FALSE)
training <- heart[intrain,]
testing <- heart[-intrain,]
dim(training)
dim(testing)
