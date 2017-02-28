install.packages("randomForest")
library(randomForest)
datafile <- read.csv("D:/ML/datafile.csv")
output.forest<-randomForest(Total...Irrigation.potential.created~Irrigation.potential.created...Kharif+Irrigation.potential.created...Rabi+Irrigation.potential.created...Perennial,data = datafile,ntree=700)
print(output.forest)
plot(datafile$Culturable.Command.Area,datafile$Irrigation.potential.created...Rabi)

binary <- read.csv("D:/ML/binary.csv")
RandomForest1<-randomForest(binary$)
install.packages("shiny")
library(shiny)
install.packages("rpart")
library(rpart)
install.packages("caret")
library(caret)
split<-createDataPartition(y=binary$admit,p=0.8,list = FALSE)
train<-binary[split,]
x_test<-train[,-train$admit]
y_test<-train[,train$admit]
x<-cbind(x_train,y_train)
binary tree:
fit<-ctree(admit~.,data=train)
library(party)
summary(fit)
print(fit)
plot(fit)
predict1<-predict(fit,x_test,type="node")
predict1<-predict(fit,x_test,type="response")
predict1
Randomforest:
 OutputRandom<-randomForest(admit~.,data=train,ntree=600) 
predictRandom<-predict(OutputRandom,x_test)
predictRandom
library(randomForest)
print(OutputRandom)
plot(OutputRandom)
data.frame(predictRandom,predict1)
=========================
  Index<-createDataPartition(y=binary$admit,p=0.8,list = FALSE)
Training<-binary[Index,]
Test<-binary[-Index,]
summary(Training)
sapply(Training, sd)
attach(Training)
xtabs(~admit+rank,data = Training)
Training$rank<-factor(Training$rank)
levels(Test)<-levels(Training)
fit<-rpart(admit~.,method = "class",data = Training)

printcp(fit)
plotcp(fit)
plot(fit,uniform=TRUE,main="Classification tree for marks")
text(fit,use.n = TRUE,all=TRUE,cex=0.8)
post(fit,file="D://ML//tree.ps",title="Classification tree for marks")
Test$rank<-factor(Test$rank)
predicted<-predict(fit,Test,type="class")
confusion<-table(predicted,Test$admit)
sum(diag(confusion))/sum(confusion)
---------------------------------------------
#Random forest
set.seed(100)
library(randomForest)
fit<-randomForest(admit~.,data = Training,ntree=200)
print(fit)
  predictForest<-predict(fit,Test)
  PredForestRound<-round(predictForest)
  confusionForest<-table(round(predictForest),Test$admit)
  sum(diag(confusionForest))/sum(confusionForest)
  #install.packages("rattle")
  #library(rattle)
  -----------------------
    #Ctree
    set.seed(100)
  library(party)
  fitCtree<-ctree(admit~.,data=Training,controls = ctree_control(maxdepth = 5))
  Test$rank<-factor(Test$rank)
  PredictCtree<-predict(fitCtree,Test)
  
  ConfusionCtree<-table(round(PredictCtree),Test$admit)
  sum(diag(ConfusionCtree))/sum(ConfusionCtree)
  PredCtreeNew<-round(PredictCtree)
  
  --------compare
  compare<-data.frame(Ctree=round(PredictCtree),RandomForest=PredForestRound,Rpart=predicted,Test$admit)
  compare
  ------------Random Forest--------
    Prostate_Cancer <- read.csv("D:/ML/Prostate_Cancer.csv")
    str(Prostate_Cancer)
    View(Prostate_Cancer)
    head(Prostate_Cancer)
    Prostate_Cancer<-Prostate_Cancer[,-1]
    head(Prostate_Cancer)
    Index<-createDataPartition(y=Prostate_Cancer$diagnosis_result,p=0.8,list = FALSE)
    TrainingCancer<-Prostate_Cancer[Index,]
    TestCancer<-Prostate_Cancer[-Index,]
    fit<-randomForest(diagnosis_result~.,data = TrainingCancer,ntree=500)
    predictFor<-predict(fit,TestCancer)
    confusionNew<-table(predictFor,TestCancer$diagnosis_result)
    Accuracy<-sum(diag(confusionNew))/sum(confusionNew)
    varImpPlot(fit,type = 2,sort = TRUE)
    ----------------------------------knn
    Prostate_Cancer<-Prostate_Cancer[,-1]
    head(Prostate_Cancer)
      normalise<-function(x){
        return((x-min(x))/(max(x)-min(x)))
      }
    head(Prostate_Cancer)
    Prostate_Cancer1<-as.data.frame(lapply(Prostate_Cancer[,-1],normalise))
    ProstateNew<-cbind(Prostate_Cancer$diagnosis_result,Prostate_Cancer1)
    ProstateNew$`Prostate_Cancer$diagnosis_result`<-as.numeric(ProstateNew[,1])
    Index<-createDataPartition(y=ProstateNew$`Prostate_Cancer$diagnosis_result`,p=0.8,list = FALSE)
    Index
    
    Training<-ProstateNew[Index,]
    head(Training)
    Test<-ProstateNew[-Index,]
    TrainingLabel<-Training[,1]
    TestLabel<-Test[,1]
    knnModel<-knn(train = Training,test =Test,cl= TrainingLabel,k=5)
    CrossTable(x=TestLabel,y=knnModel,prop.chisq = F)
    str(Test)
    