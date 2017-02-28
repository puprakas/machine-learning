install.packages("class")
library(class)
View(binary)
binary<-binary[,-1]
head(binary)


normalise<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}


binaryStd<-as.data.frame(lapply(binary[,c(2,3)], normalise))
summary(binaryStd)
dim(binaryStd)
library(caret)
names(Newdata)
Newdata<-cbind(binaryStd,binary$admit,binary$rank)
Index<-createDataPartition(y=Newdata$gre,p=0.8,list = FALSE)
Index
#Training<-binaryStd[Index,]
Training<-Newdata[Index,]
#Test<-binaryStd[-Index,]
Test<-Newdata[-Index,]
TrainingLabels<- Training[,3]
TestLabels<- Test[,3]
dim(TestLabels)
knnModel<-knn(train = Training,test =Test,cl= TrainingLabels,k=5)
#install.packages("gmodels")
#library(gmodels)
nrow(TestLabels)
dim(knnModel)
result1<-CrossTable(x=TestLabels,y=knnModel,prop.chisq = F)

