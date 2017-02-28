library(caret)
head(iris)
x=iris[,-5]
y=iris[,5]
model=train(x,y,'nb',trControl = trainControl(method = 'cv',number = 10))
model
pred<-predict(model$finalModel,x)
confusion<-table(pred$class,y)
sum(diag(confusion))/sum(confusion)
naiveiris<-NaiveBayes(iris$Species~.,data = iris)
plot(naiveiris)

install.packages("e1071")
library(e1071)
head(BreastCancer)
install.packages("mlbench")
library(mlbench)
data("BreastCancer")
head(BreastCancer)
split=0.80
=================binary============
 binary <- read.csv("D:/ML/binary.csv")
 View(binary)
 split<-createDataPartition(y=binary$admit,p=0.8,list = FALSE)
 MydataTrain<-binary[split,]
 MydataTest<-binary[-split,]
 head(binary)
 MydataTrain$rank<-MydataTrain$rank
 MydataTrain$admit<-factor(MydataTrain$admit)
 levels(MydataTest)<-levels(MydataTrain)
 modelNb<-naiveBayes(admit~.,data = MydataTrain)
 abc<-predict(modelNb,MydataTest[,-1])
 confusion1<-table(abc,MydataTest$admit)
 sum(diag(confusion1))/sum(confusion1)
 plot(modelNb)
 library(e1071)
 library(klaR)
 modelNb<-NaiveBayes(admit~.,data = MydataTrain)
 plot(modelNb)
 
 