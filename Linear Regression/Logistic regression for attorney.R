claimants<-read.csv(file.choose()) #To choose to load dataset
sum(is.na(claimants))  
claimants<-na.omit(claimants)  #ominting NA values of the data
#na.omit<-will omit the rows which has atleast 1 NA value
dim(claimants)
colnames(claimants)
claimants<-claimants[,-1] #removing the first column which is an Index

#Preparing a linear Regression
mod_lm<-lm(ATTORNEY~.,data = claimants)
pred1<-predict(mod_lm,claimants)
pred1
plot(claimants$CLMINSUR,pred1)
#we can no way use the linear regression technique to classify the data
plot(pred1)

#GLM function use sigmoid curve to produce desirabe results
#the o/p of the sigmoid function lies in between 0-1

model <- glm(ATTORNEY~.,data = claimants,family = "binomial")
summary(model)

#Confusion matrix
prod<-predict(model,claimants,type = "response")
prod

#confusion matrix and considering the threshold value as 0.5
confusion<-table(prod>0.5,claimants$ATTORNEY)
confusion

#Model Accuracy
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #70.52
