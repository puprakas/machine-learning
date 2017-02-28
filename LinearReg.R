factbook[factbook==""]<-NA
factbook
factbook<-na.omit(factbook)
factbook<-factbook[]
trainingset<-sample(1:nrow(factbook),0.7*nrow(factbook))
trainingdata<-factbook[trainingset,]
testdata<-factbook[-trainingset,]
lmModel<-lm(as.numeric(factbook$Birth.rate.births.1000.population.)~as.numeric(factbook$Electricity...consumption.kWh.),data = trainingdata)
predictOutput<-predict(lmModel,testdata)
lmModel$residuals
plot(lmModel$residuals)
abline(lmModel)
plot(factbook$Electricity...consumption.kWh.,factbook$Birth.rate.births.1000.population.,type ="p")
co2
library(datasets)
CO2
install.packages("missForest")
library(missForest)
Co2Miss<-prodNA(CO2,noNA = 0.1)
Co2Miss
as.numeric(Co2Miss$Plant)
as.numeric(Co2Miss$Type)
as.numeric(Co2Miss$Treatment)
install.packages("mice")
library(mice)
md.pattern(Co2Miss)
cars
install.packages("VIM")
library(VIM)
imputation_plot<-aggr(Co2Miss,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(Co2Miss),gap=3,
                      ylab=c("Missing Data","Pattern"))
tempData <- mice(Co2Miss,m=5,maxit=50,meth='pmm',seed=500)

imputation_plot<-aggr(tempData,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(tempData),gap=3,
                      ylab=c("Missing Data","Pattern"))

completeData<-complete(tempData,4)

imputation_plot<-aggr(completeData,col=c('yellow','blue'),
                                             numbers=TRUE,sortVars=TRUE,
                                             labels=names(completeData),gap=3,
                                     
                              ylab=c("Missing Data","Pattern")
                       )

set.seed(100)
trainingRowIndex <- sample(1:nrow(datafile), 0.8*nrow(datafile))
trainingData <- datafile[trainingRowIndex, ]
testData <- datafile[-trainingRowIndex, ]
lmMod <- lm(Irrigation.potential.created...Kharif~Total...Irrigation.potential.created, data=trainingData)
distPred <- predict(lmMod, testData)
summary(lmMod)
plot(datafile$Irrigation.potential.created...Kharif~datafile$Total...Irrigation.potential.created)
plot(lmMod$residuals)
abline(lmMod)
abline(coef(lmMod))
layout(matrix(c(1,2,3,4),2,2))
plot(lmMod)
layout(matrix(c(1,1,2,2,3,4),nrow = 3,ncol = 2,byrow = TRUE))
plot(lmMod)
layout(matrix(c(1,2,3,4),nrow = 4,ncol = 1,byrow = TRUE))
plot(lmMod)
lmMod$coefficients
lmMod$residuals
----------------------------Multiple regression------------------
  
MulLM<-lm(datafile$Total...Irrigation.potential.created~datafile$Irrigation.potential.created...Rabi+datafile$Irrigation.potential.created...Perennial,data = datafile)
MulLM
InLM3<-lm(datafile$Total...Irrigation.potential.created~datafile$Irrigation.potential.created...Rabi+datafile$Irrigation.potential.created...Perennial+datafile$Irrigation.potential.created...Kharif,data = datafile)
InLM3
layout(matrix(c(1,2,3,4),nrow = 2,ncol = 2,byrow = TRUE))
plot(InLM3)
-------------------------------------------------------------------
  install.packages("DAAG")
library(DAAG)
fit<-lm(datafile$Total...Irrigation.potential.created~datafile$Irrigation.potential.created...Rabi)
predict(fit,datafile[1,])
cv.lm(datafile,form.lm=formula(Total...Irrigation.potential.created~Irrigation.potential.created...Rabi),m=10)
layout(1)
-------------------
  impution<-aggr(NHIS,col=c('Red','blue'),numbers=TRUE,sortVars=TRUE,labels=names(NHIS),gap=3,ylab=c("Missing Data","Pattern"))
  tempNHIS<-mice(NHIS,m=5,maxit = 50,meth='pmm',seed = 500)  
  
  completeData<-complete(tempNHIS,4)
  impution<-aggr(completeData,col=c('Red','blue'),numbers=TRUE,sortVars=TRUE,labels=names(completeData),gap=3,ylab=c("Missing Data","Pattern"))
  trainingNHIS<- sample(1:nrow(completeData),0.8*nrow(completeData))
  trainingData<-completeData[trainingNHIS,]
  testData<-completeData[-trainingNHIS,]
  linearM<-lm(completeData$weight~completeData$height,data = trainingData)
  plot(completeData$weight~completeData$height)
  abline(linearM)
  
  linearM<-lm(completeData$weight~completeData$height+completeData$BMI,data = trainingData)
  plot(completeData$weight~completeData$height+completeData$BMI)
  ==========================================================================
   Logistic regression
  
     mydata<-read.csv("binary.csv")
  mydata
head(mydata)
summary(mydata)
sapply(mydata,sd)
xtabs(~admit+rank,data = mydata)
mydata$rank<-factor(mydata$rank)
mylogit<-glm(admit~gre+gpa+rank,data = mydata,family = "binomial")
summary(mylogit)
trainingset<-sample(1:nrow(mydata),0.8*nrow(mydata))
trainingdata<-mydata[trainingset,]
testdata<-mydata[-trainingset,]
myglm<-glm(admit~gre+gpa+rank,data = trainingdata,family = "binomial")
summary(myglm)
prd<-predict(myglm,testdata,type = "response")  
prd<-ifelse(prd>=.5,1,0)
summary(prd)
confusion=table(prd,testdata$admit)
sum(diag(
  confusion))/sum((confusion))
confusion

====================visualization==================================
install.packages("ggplot2")
library(ggplot2)
barplot1<-ggplot(data = odi,aes(odi$Player)) +geom_bar()

BankCampaign<-read.csv(file.choose(),header = TRUE)
summary(BankCampaign)

ggplot(BankCampaign,aes(x=BankCampaign$Month,fill=BankCampaign$Month))+geom_bar()+xlab("Month")
================
  install.packages("dplyr")
library(dplyr)
BankCampaign1<-mutate(BankCampaign,Response.Rate=factor(BankCampaign$Response.Rate))
str(BankCampaign1)
ggplot(BankCampaign1,aes(x=BankCampaign1$Response.Rate,fill=BankCampaign1$Response.Rate))+geom_bar()+xlab("Response Rate")

----------------factbook-----------------------
factbook<-read.csv(file.choose(),header = TRUE)
View(factbook)
factbook<-factbook[-1,]
factbook[factbook==""]<-NA
View(factbook)
md.pattern(factbook)
str(factbook)
factbook12<-factbook
str(factbook12)
factbook12=sapply(factbook,as.numeric)
factbook12
str(factbook12)
View(factbook12)
fact123<-sapply(factbook[,-1],as.numeric)
View(fact123)
for(i in 2:ncol(factbook)){
factbook[,i]=as.numeric(factbook[,i])
}
str(factbook)
View(factbook)
options(scipen = 100)
imputation_plot<-aggr(factbook,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(factbook),gap=3,
                      ylab=c("Missing Data","Pattern"))

str(factbook)
View(factbook)
mutate(factbook,Area.sq.km.=as.numeric(factbook$Area.sq.km.),Birth.rate.births.1000.population.=as.numeric(factbook$Birth.rate.births.1000.population.)                                                                                                                                                                                                                                                                                                                     
imputeMice<-mice(factbook,m=5,maxit=20,meth='cart',seed=500)
completeSet<-complete(imputeMice,3)
completeSet<-completeSet[,-8]
View(completeSet)
imputation_plot<-aggr(completeSet,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(completeSet),gap=3,
                      ylab=c("Missing Data","Pattern"))
View(completeSet)
subset(completeSet$Country,nrow(completeSet$Country)=="")

80-20
TrainingSet<-sample(1:nrow(completeSet),0.8*nrow(completeSet))
trainingdata<-completeSet[TrainingSet,]
testdata<-completeSet[-TrainingSet,]
lmModel<-lm(completeSet$Electricity...consumption.kWh.~completeSet$Area.sq.km.+completeSet$Birth.rate.births.1000.population.+completeSet$Death.rate.deaths.1000.population.+completeSet$Infant.mortality.rate.deaths.1000.live.births.,data = trainingdata)
summary(lmModel)
predictlm<-predict(lmModel,testdata)
layout(matrix(c(1,2,3,4),2,2))
plot(lmModel)

70-30
Training<-sample(1:nrow(completeSet),0.9*row(completeSet))
Training2<-completeSet[Training,]
testdata<-completeSet[-TrainingSet2,]
lmModel2<-lm(completeSet$Electricity...consumption.kWh.~completeSet$Area.sq.km.
             +completeSet$Birth.rate.births.1000.population.+completeSet$Death.rate.deaths.1000.population.
             +completeSet$Infant.mortality.rate.deaths.1000.live.births.,data = Training2)
layout(matrix(c(1,2,3,4),2,2))
plot(lmModel2)
summary(lmModel2)    
    
CVMODEL
cvmodel<-cv.lm(completeSet,form.lm=formula(Electricity...consumption.kWh.~Area.sq.km.+Birth.rate.births.1000.population.+Death.rate.deaths.1000.population.+Infant.mortality.rate.deaths.1000.live.births.),m=11)
factbookTest<-completeSet[1:20,]
predictlm1<-predict(lmModel,factbookTest)
predictlm2<-predict(lmModel2,factbookTest)
cvmodel$cvpred
ModelComp<-data.frame(completeSet$Electricity...consumption.kWh.,predictlm1,predictlm2,cvmodel$cvpred)
ModelComp
factCountry<-factbook[,-1]
View(factCountry)
imputppm<-mice(factCountry,m=5,maxit = 10,meth='ppm',seed = 500)
imputation_plot<-aggr(factCountry,col=c('yellow','blue'),
                      numbers=TRUE,sortVars=TRUE,
                      labels=names(factCountry),gap=3,
                      ylab=c("Missing Data","Pattern"))
head(completeSet)
--------
factbook <- read.csv("D:/ML/factbook.csv", sep=";", stringsAsFactors=FALSE)
View(factbook)
factbook[factbook==""]<- NA
for(i in 2:ncol(factbook)){
factbook[,i]<- as.numeric(factbook[,i])
}
===============================
install.packages("caret")
library(caret)
trainIndex<-createDataPartition(y=mydata$admit,p=0.75,list = FALSE,times = 1)


summary(mylogit)

trainingdata<-mydata[trainIndex,]
testdata<-mydata[-trainIndex,]
levels(trainingdata$rank)<-levels(testdata$rank)
trainingdata$rank=factor(mydata$rank)
testdata$rank=factor(mydata$rank)
myglm<-glm(admit~gre+gpa+rank,data = trainingdata,family = "binomial")


prd<-predict(myglm,testdata,type = "response")  
confusion=table(round(prd),testdata$admit)
confusion
accuracy<-sum(diag(confusion)/sum(confusion))
accuracy

confusion
prd<-ifelse(prd>=.5,1,0)
summary(prd)
------------------------------------------------
  ------------------------------------
install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(testdata$admit,round(prd))
layout(1)
plot(x=testdata$admit,y=prd,xlab="Original",ylab="Predicted")
curve(predict(myglm,type = "resp"),add = TRUE)
lines(x=testdata$admit,y=prd)
library(datasets)
datasets
data()
-----------------------Bank-----------------------------------------------

str(bank.full)
View(bank.full)
str(bank)
trainingset<-bank.full
testset<-bank
glmModel<-glm(trainingset$y~.,data = trainingset,family = "binomial")
summary(glmModel)
predglm<-predict(glmModel,testset,type = 'response')
ConfusionMat<-table(round(predglm),testset$y)
ConfusionMat
accuracy<-sum(diag(ConfusionMat))/sum(ConfusionMat)
accuracy
plot(x=testset$y,y=predglm,xlab="original",ylab="predicted")
lines(x=testset$y,y=predglm)
++++++++++++++++++++++++++++++++++++++
Kmeans clustering
library(datasets)
head(iris)
set.seed(20)
 irisCluster<-kmeans(iris[,3:4],3,nstart = 20)
irisCluster
install.packages("ggplot2")
library(ggplot2)
ggplot(iris,aes(iris$Petal.Length,iris$Petal.Width,color=iris$Species))+geom_point()
irisCluster$cluster<-as.factor(irisCluster$cluster)
ggplot(iris,aes(iris$Petal.Length,iris$Petal.Width,color=irisCluster$cluster))+geom_point()
COnfusion<-table(irisCluster$cluster,iris$Species)
COnfusion
-----------------CrimeData----------
set.seed(20)
View(crime_data)
CrimeCluster<-kmeans(crime_data[,3:6],4,nstart = 20)
crime_data<-na.omit(crime_data)
crime<-data.matrix(crime_data)
kmeanwss<-function(crime2,k){
km<-kmeans(crime2,k)
return(km$tot.withinss)
}
maxk=10

kmeansDis<-function(crime,maxk){
dis<-(nrow(crime)-1)*sum(apply(crime,2,var))
dis[2:maxk]=sapply(2:maxk,kmeanwss,crime2=crime)
return(dis)
}
dis<-kmeansDis(crime,maxk)
dis
plot(1:maxk,dis,type = 'b',xlab = "Number of Clusters",ylab = "Distortion",col="blue")


names(crime_data)
View(crime_data)
CrimeCluster
install.packages("ggplot2")
library(ggplot2)
ggplot(crime_data,aes(crime_data$UrbanPop,crime_data$Assault,color=crime_data$Murder))+geom_point()
CrimeCluster$cluster<-as.factor(CrimeCluster$cluster)
ConfusionCrime<-table(CrimeCluster$cluster,crime_data$crime.cluster)
ConfusionCrime
----------------------Laptop Battery----------------------------
trainingdata<-read.table(file.choose(),sep = ',')
View(trainingdata)
trainingset<-sample(1:nrow(trainingdata),0.8*nrow(trainingdata))


--------------------------LAPTOP BATTERY------------

