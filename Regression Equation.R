#load data

Nd<-read.csv("C:/Users/Admin/Downloads/NewspaperData.csv")
#visualization
library("lattice")
dotplot(Nd$sunday,main="Dot plot of Sunday Circulations",col="dodgerblue4")
dotplot(Nd$daily,main="Dot plot of Sunday Circulations",col="dodgerblue4")
boxplot(Nd$sunday,col = "dodgerblue4")
boxplot(Nd$daily,col = "dodgerblue4")

#Regression Equation
#syntax model<-(y~x,data=data set name)
model<-lm(sunday~daily,data=Nd)
summary(model)
new<-data.frame(daily=c(200,250,300))
pred<-predict(model,newdata = new) #to predict the error
pred
pred<-predict(model)
finaldata<-data.frame(Nd,pred,"Error"=Nd$sunday-pred) 
finaldata
