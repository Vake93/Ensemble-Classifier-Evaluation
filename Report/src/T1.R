library(ggplot2)
library(corrplot)

heart<-read.csv("D:/Projects/heart.csv")
names(heart)
str(heart)
head(heart)

shuffle_rows <- function(df)
{
  rows <- sample(nrow(df))
  df[rows,]
}

heart <- shuffle_rows(heart)
head(heart)
summary(heart)

# Correlation Plot
heart.cor = cor(heart)
corrplot(heart.cor)
heart.cor[14,]

# Age Analysis
range(heart$age)
summary(heart$age)
sd(heart$age)
var(heart$age)
cor(heart$age,heart$target)
chisq.test(heart$age,heart$target)
hist(heart$age,labels=TRUE,main="Histogram of Age",xlab="Age Class",ylab="Frequency",col="purple")
boxplot(heart$age,horizontal=TRUE,col="purple",main="Boxplot of Age")

a <- ggplot(heart,aes(x=age,y=target)) + geom_point() + geom_smooth(color="purple",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("age vs target")

# Sex Analysis
heart_attacks <- heart[heart$target==1,]
length(heart_attacks$sex)
length(heart_attacks[heart_attacks$sex==0,]$sex)
length(heart_attacks[heart_attacks$sex==1,]$sex)

a <- table(heart_attacks$sex)
par(mfrow=c(1,2))
barplot(a,
        col=c("purple","lightblue"),
        xlab="0 = Female and 1 = Male",
        ylab="Count",
        ylim=range(pretty(c(0, a))), #to adjust y-axis scale
        main="Female vs Male")

percentages <- round(a/sum(a)*100)
labels <- paste(c("Female","Male")," ",percentages,"%",sep="")
pie(a,labels=labels,col=c("purple","lightblue"),main="Male vs Female Percentage")

males <- heart[heart$sex==1,]
a <- ggplot(males,aes(x=age,y=target)) + geom_point() + geom_smooth(color="blue",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Males age vs target")

females <- heart[heart$sex==0,]
a <- ggplot(females,aes(x=age,y=target)) + geom_point() + geom_smooth(color="purple",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Females age vs target")

# Chest Pain Analysis
heart_attacks <- heart[heart$target==1,]
heart$cp <- factor(heart$cp)
heart_attacks$cp <- factor(heart_attacks$cp)

a <- table(heart$cp)
barplot(a,
        col=c("red","green","blue","purple"),
        main="Chest Pain - All",
        xlab="Chest Pain Category",
        ylab="Count",
        ylim=range(pretty(c(0, a))) #to adjust y-axis scale
        )

b <- table(heart_attacks$cp)
barplot(b,
        col=c("red","green","blue","purple"),
        main="Chest Pain - Heart Attacks",
        xlab="Chest Pain Category",
        ylab="Count",
        ylim=range(pretty(c(0, b))) #to adjust y-axis scale
        )

typical_angina <- heart[heart$cp==0,]
a <- ggplot(typical_angina,aes(x=age,y=target)) + geom_smooth(color="red",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Typical Angina - age vs target")

atypical_angina <- heart[heart$cp==1,]
a <- ggplot(atypical_angina,aes(x=age,y=target)) + geom_smooth(color="darkgreen",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Atypical Angina - age vs target")

nonanginal_pain <- heart[heart$cp==2,]
a <- ggplot(nonanginal_pain,aes(x=age,y=target)) + geom_smooth(color="blue",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Non-anginal Pain - age vs target")

asymptomatic <- heart[heart$cp==3,]
a <- ggplot(asymptomatic,aes(x=age,y=target)) + geom_smooth(color="purple",se=FALSE)
b <- a+scale_x_continuous(name="age") + scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Asymptomatic - age vs target")


# Rest Blood Pressure Analysis
summary(heart$trestbps)
cor(heart$trestbps,heart$age)
boxplot(heart$trestbps,
        col="orange",
        main="Analysis of RBP",
        horizontal=TRUE)

colfunc <- colorRampPalette(c("lightsalmon", "orangered"))
hist(heart$trestbps,col=colfunc(11),
     main="Histogram for RBP",
     xlab="Rest Blood Pressure Class",
     ylab="Frequency",
     labels=TRUE)

a <- ggplot(heart,aes(x=trestbps,y=target))+geom_point()+geom_smooth(color="orangered",se=FALSE)
b <- a+scale_x_continuous(name="rbp")+scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("rbp vs target")

# Cholestrol Analysis
summary(heart$chol)
boxplot(heart$chol~heart$sex,col=c("purple","lightblue"),
        main="Cholestrol Level Male vs Female",
        xlab="0 = Female, 1= Male",
        ylab="cholestrol")

colfunc <- colorRampPalette(c("seagreen2", "seagreen4"))
hist(heart$chol,
     main="Histogram of Cholestrol",
     xlab="Cholestrol Class",
     ylab="Frequency",
     col=colfunc(10),labels=TRUE)

a <- ggplot(heart,aes(x=chol,y=target))+geom_point()+geom_smooth(color="seagreen2",se=FALSE)
b <- a+scale_x_continuous(name="rbp")+scale_y_continuous(name="target",limit=c(0,1))
b + ggtitle("Cholestrol vs Target")

# Fasting Blood Sugar Analysis
total_non_target <- heart[heart$target==0,]$fbs
total_non_target_high_fbs <- heart[heart$target==0&heart$fbs==1,]$fbs
total_non_target_norm_fbs <- heart[heart$target==0&heart$fbs==0,]$fbs
length(total_non_target_high_fbs) / length(total_non_target) * 100
length(total_non_target_norm_fbs) / length(total_non_target) * 100

total_target <- heart[heart$target==1,]$fbs
total_target_high_fbs <- heart[heart$target==1&heart$fbs==1,]$fbs
total_target_norm_fbs <- heart[heart$target==1&heart$fbs==0,]$fbs
length(total_target_high_fbs) / length(total_target) * 100
length(total_target_norm_fbs) / length(total_target) * 100

cor(heart$fbs,heart$target)

fbs <- table(heart$fbs)
barplot(fbs,
        main="Fasting Blood Sugar",
        xlab="0 = No, 1 = Yes",
        ylab="Count",
        col=c("slateblue4","violetred1"),
        legend=rownames(fbs))

percentages <- round(fbs/sum(fbs)*100)
labls <- paste(c("FBS<120","FBS>120")," ",percentages,"%",sep="")
pie(fbs,labls,main="Fasting Blood Sugar",col=c("slateblue4","violetred1"))

# Resting Electrocardiographic Results Analysis
a <- heart[heart$target==0,]
b <- heart[heart$target==1,]

barplot(table(heart$restecg),
        main="Barplot of Resting ECG - All",
        xlab="ECG Category",
        ylab="Count",
        ylim=range(pretty(c(0, table(heart$restecg)))), #to adjust y-axis scale
        col=c("deepskyblue","dodgerblue4","lightslateblue"))

par(mfrow=c(1,2))
barplot(table(a$restecg),
        main="Barplot of Resting ECG - Heart Attacks",
        xlab="ECG Category",
        ylab="Count",
        ylim=range(pretty(c(0, table(a$restecg)))), #to adjust y-axis scale
        col=c("deepskyblue","dodgerblue4","lightslateblue"))

barplot(table(b$restecg),
        main="Barplot of Resting ECG - Non Heart Attacks",
        xlab="ECG Category",
        ylab="Count",
        ylim=range(pretty(c(0, table(b$restecg)))), #to adjust y-axis scale
        col=c("deepskyblue","dodgerblue4","lightslateblue"))

# Maximum Heart Rate Analysis
class(heart$thalach)
head(heart$thalach)
summary(heart$thalach)
cor(heart$age,heart$thalach)
chisq.test(heart$age,heart$thalach)

boxplot(heart$thalach~as.factor(heart$sex),
        main="Heart Rate vs Sex",
        xlab="Sex: 0 = Female, 1 = Male",
        ylab="Max Heart Rate",
        col=c("purple","lightblue"))

a <- ggplot(heart,aes(x=age,y=thalach))+geom_point()+geom_smooth(se=FALSE)
b <- a+scale_x_continuous(name="Age")+scale_y_continuous(name="Maximum Heart Rate Achieved")
b + ggtitle("Age vs Heart Rate")

a <- ggplot(heart,aes(x=thalach,y=target))+geom_point()+geom_smooth(se=FALSE)
b <- a+scale_x_continuous(name="Max Heart Rate")+scale_y_continuous(name="Prob. of Heart Attack")
b + ggtitle("Heart rate vs Prob. Heart attack")

# Exercise Induced Angina Analysis
class(heart$exang)
head(heart$exang)
barplot(table(heart$exang,heart$target),
        legend=c("Yes", "No"),
        col=c("orangered1","palegreen2"),
        main="Exercise Induced Angina vs Target",
        xlab="Target",
        ylab="Count")

a <- ggplot(heart,aes(x=exang,y=target))+geom_point()+geom_smooth(color="orangered1",se=FALSE)
b <- a + scale_x_continuous(name="Exercise Induced Angina")+scale_y_continuous(name="Target")
b + ggtitle("Relationship Between Exang and Target")

# ST Depression Induced by Exercise Relative to Rest Analysis
class(heart$oldpeak)
head(heart$oldpeak,20)
summary(heart$oldpeak)
range(heart$oldpeak)
sd(heart$oldpeak)
var(heart$oldpeak)

colfunc <- colorRampPalette(c("green3", "red3"))
hist(heart$oldpeak,
     main="Histogram ST Depression",
     xlab="ST Depression Class",
     ylab="Frequency",
     col=colfunc(14),
     labels=TRUE)

plot(density(heart$oldpeak),
     main="Density plot of ST Depression",
     xlab="ST Depression Class",
     ylab="Density")
polygon(density(heart$oldpeak),col="green3",border="green3")

boxplot(heart$oldpeak,
        main="ST Depression",
        ylab="ST Depression Class",
        col="green3")

boxplot(heart$oldpeak~heart$sex,
        main="ST Depression Female vs Male",
        col=c("purple","lightblue"),
        xlab="0: Feamle, 1:Male",
        ylab="ST Depression Class")

a <- ggplot(heart,aes(x=oldpeak,y=target))+geom_point()+geom_smooth(color="green3",se=FALSE)
b <- a+scale_x_continuous(name="ST Depression Class")+scale_y_continuous(name="Prob. of Heart Attack",limit=c(0,1))
b + ggtitle("Relation between oldpeak and heart attack")

# The Slope of the Peak Exercise ST Segment Analysis
class(heart$slope)
head(heart$slope)
unique(heart$slope)

barplot(table(heart$slope),
        main="ST Slope",
        xlab="ST Slope",
        ylab="Count",
        col=c("darksalmon","brown1","darkred"))

barplot(table(heart$slope,heart$target),
        main="ST slope vs target",
        col=c("darksalmon","brown1","darkred"),
        xlab="Target",
        ylab="Count")

a <- ggplot(heart,aes(x=slope,y=target))+geom_point()+geom_smooth(color="darksalmon",se=FALSE)
b <- a + scale_x_continuous(name="Slope")+scale_y_continuous(name="Target")
b + ggtitle("Relationship between Slope and Target")

# Number of Major Vessels Colored by Fluoroscopy Analysis
class(heart$ca)
unique(heart$ca)

barplot(table(heart$ca),
        main="Number of Major Vessels Colored by Fluoroscopy Analysis",
        xlab="Number of Major Vessels Colored",
        col=c("orangered2","orchid","palegreen3","paleturquoise3","peru"),
        ylim=range(pretty(c(0, 170))), #to adjust y-axis scale
        ylab="Count")

barplot(table(heart$target,heart$ca),
        main="Number of Major Vessels Colored vs Target",
        xlab="Number of Major Vessels Colored",
        col=c("palegreen3","orangered2"),
        legend=c(0,1),
        ylim=range(pretty(c(0, 170))), #to adjust y-axis scale
        ylab="Count")

a <- ggplot(heart,aes(x=ca,y=target))+geom_point()+geom_smooth(color="orangered2",se=FALSE)
b <- a+scale_x_continuous(name="Number of Major Vessels Colored")+scale_y_continuous(name="Target")
b + ggtitle("Relationship between No. of Major Vessels Colored and Target")

# Thalium Stress Test Result Analysis
class(heart$thal)
unique(heart$thal)
length(heart[heart$thal==0,]$thal)

a <- heart[heart$thal!=0,]
barplot(table(a$thal),
        main="Thalium Stress Test Result",
        xlab="Thalium Stress Test Result Class",
        col=c("orangered2","orchid","palegreen3","paleturquoise3","peru"),
        ylim=range(pretty(c(0, 170))), #to adjust y-axis scale
        ylab="Count")

barplot(table(a$target,a$thal),
        main="Thalium Stress Test Resul vs Target",
        xlab="Thalium Stress Test Result Class",
        col=c("palegreen3","orangered2"),
        legend=c(0,1),
        ylim=range(pretty(c(0, 170))), #to adjust y-axis scale
        ylab="Count")

a <- ggplot(a,aes(x=thal,y=target))+geom_point()+geom_smooth(color="orangered2",se=FALSE)
b <- a+scale_x_continuous(name="Thalium Stress Test Result Class")+scale_y_continuous(name="Target")
b + ggtitle("Relationship between Thalium Stress Test Result and Target")