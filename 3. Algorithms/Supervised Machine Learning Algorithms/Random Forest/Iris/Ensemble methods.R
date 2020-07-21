##Bagging method

acc<-c()
for (i in 1:500) 
  {
  print(i)
  
  #data partition
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list = F)
  training1<-iris[inTraininglocal,]
  testing1<-iris[-inTraininglocal,]
  
  #model Building
  fittree<- C5.0(training1$Species~.,data = training1)
  
  #predicting
  
  pred<-predict.C5.0(fittree,testing1[,-5])
  a<-table(testing1$Species,pred)
  #Accuracy
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
boxplot(acc)
plot(acc)


############Boosting in Bagging

acc<-c()
for (i in 1:500) 
{
  print(i)
  
  #data partition
  inTraininglocal<-createDataPartition(iris$Species,p=.85,list = F)
  training1<-iris[inTraininglocal,]
  testing1<-iris[-inTraininglocal,]
  
  #model Building
  fittree<- C5.0(training1$Species~.,data = training1,trials=25)
  
  #predicting
  
  pred<-predict.C5.0(fittree,testing1[,-5])
  a<-table(testing1$Species,pred)
  #Accuracy
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

####To show the trials in this use the model building code seperately and the summary of that
##summary(fittree)


summary(acc)

boxplot(acc)