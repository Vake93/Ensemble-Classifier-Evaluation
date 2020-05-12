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

# Random Forest
set.seed(256)
rf <- train(target~., data=training, method="rf", metric="Accuracy", trControl=bagging_control)
rf_pred<-predict(rf,newdata=testing,type='prob')


rf_perf <- prediction(rf_pred$YES,testing$target)

rf_perf.prec_rec <- performance(rf_perf,measure="prec",x.measure='rec')
plot(rf_perf.prec_rec)

rf_perf.acc <- performance(rf_perf,"acc")
plot(rf_perf.acc)
