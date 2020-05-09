#Multiple Linear Regression for cars dataset
Cars<-read.csv("C:/Users/Admin/Downloads/Cars.csv")

#scatter plot Matrix:
pairs(Cars)
#correlation Matrix:
 cor(Cars)
 
 #regression Model and Summary
 model.car<-lm(MPG~VOL+SP+HP+WT,data = Cars) #oR model.car<-lm(MPG~.,data = Cars)
 summary(model.car)
 
 ##################################
 
 reg_vol<-lm(MPG~VOL,data = Cars)
 summary(reg_vol)
 
 reg_wt<-lm(MPG~WT,data = Cars)
 summary(reg_wt)
 
 
 #when 2 independent variable are highly corelated the tend to have collinearity
 reg_wtvol<-lm(VOL~WT,data = Cars)
summary(reg_wtvol) 



#Regressionmodel and summary
model.car<-lm(MPG~.,data = Cars)
summary(model.car)

#MULTICOLLINEARITY
install.packages("car")
library(car)
car::vif(model.car)

#DIAGNOSIS PLOTS:

#Residual plot,QQplot,std. Residual vs Fitted
plot(model.car)

#Residual vs Regressors
residualPlots(model.car)

#Added Variable plots
#avPlots(model.car.id.n=2,id.cex=0.7)
#QQ plots of Studentized residuals
qqPlot(model.car)

#Deletion Diagnotics
influenceIndexPlot(model.car) #Index Plots of the Influence measures


##########Iteration1###########
#remove 77th observation
Cars1<-Cars[-77,]
model1<-lm(Cars1$MPG~.,data = Cars1)
car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)


#######Iteration2#########
Cars2<-Cars[-c(77,79),]
model2<-lm(Cars2$MPG~.,data = Cars2)
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)

###Do this until we dont have any Outliers
