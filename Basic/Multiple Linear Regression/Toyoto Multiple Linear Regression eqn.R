Toy<-read.csv("C:/Users/Admin/Downloads/Toyoto_Corrola.csv")
Toyoto<-Toy[,-c(1,2,9)]

#scatter plot Matrix:
pairs(Toyoto)
#correlation Matrix:
cor(Toyoto)

##Regression model and summary###
model.Toyoto<-lm(Price~.,data = Toyoto)
summary(model.Toyoto)

car::vif(model.Toyoto)

#Model Validation/Diagnosis Plot:

#Residual plot,QQplot,std. Residual vs Fitted
plot(model.Toyoto)

#Residual vs Regressors
residualPlots(model.Toyoto)

#Added Variable plots
#avPlots(model.car.id.n=2,id.cex=0.7)
#QQ plots of Studentized residuals
qqPlot(model.Toyoto)

#Deletion Diagnotics
influenceIndexPlot(model.Toyoto)


############Iteration 1####
Toyoto1<-Toyoto[-222,]
model.Toyoto1<-lm(Toyoto1$Price~.,data = Toyoto1)
car::vif(model.Toyoto1)
plot(model.Toyoto1)
residualPlots(model.Toyoto1)
qqPlot(model.Toyoto1)
influenceIndexPlot(model.Toyoto1)
summary(model.Toyoto1)

############Iteration 2####
Toyoto2<-Toyoto[-c(222,602),]
model1<-lm(Toyoto2$Price~.,data = Toyoto2)
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)

########Iteration3########
Toyoto3<-Toyoto[-c(222,602,81),]
model2<-lm(Toyoto3$Price~.,data = Toyoto3)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
summary(model2)



############Iteration4#############
Toyoto4<-Toyoto[-c(222,602,81,961),]
model3<-lm(Toyoto4$Price~.,data = Toyoto4)
car::vif(model3)
plot(model3)
residualPlots(model3)
qqPlot(model3)
influenceIndexPlot(model3)
summary(model3)

##############Iteration5############
Toyoto5<-Toyoto[-c(222,602,81,961,992,655),]
model4<-lm(Toyoto5$Price~.,data = Toyoto5)
car::vif(model4)
plot(model4)
residualPlots(model4)
qqPlot(model4)
influenceIndexPlot(model4)
summary(model4)

