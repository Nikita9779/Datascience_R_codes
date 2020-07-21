#Consider only the below columns and prepare a prediction model for predicting Price.
#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

#loading the packages

library(ggplot2)
library(moments)
library(carData)
library(car)

##Loading the data

toyota <- read.csv("E:/Data science Excelr/Assigments/Multi linear Regression/Toyota_Corolla/ToyotaCorolla.csv")
summary(toyota)
attach(toyota)
str(toyota)

toyota<-data.frame(cbind(Price,Age_08_04,KM,HP,cc,Doors,Gears,Quarterly_Tax,Weight))
colnames(toyota) <- c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")
names(toyota)

#lets build the model 
model <- lm(Price~.,data = toyota)
summary(model)
#here the r Square value is 0.8638 and ERROR 1342

#Lets Predict the Values for the Data of the Dataset

pred <- predict(model)
pred

final_data<- data.frame(Price, pred, Error = model$residuals)
final_data

model_rmse <- sqrt(mean((Price - model$fit)^2, na.rm = T))
model_rmse ##1338.25

##LEts build by removing outliers
car::vif(model)
residualPlot(model)
influenceIndexPlot(model)

toyota1 <- toyota[-c(81,222),]
#Iteration 1
model1 <- lm(Price~.,data = toyota1)
summary(model1) ##0.877 and error 1270
car::vif(model1)
influenceIndexPlot(model1)

#Iteration 2
toyota2 <- toyota1[-c(602,961,992),]
model2 <- lm(Price~.,data = toyota2)
summary(model2) ###0.8779 error 1270 ......WHICH SAYS NOT MUCH DIFFERENCE IS SEEN

####log Transformatiin
expo_model <- lm(log(Price)~.,data = toyota)
summary(expo_model) #0.8507

expo_rmse <- sqrt(mean((Price - exp(expo_model$fit))^2, na.rm = T))
expo_rmse ##1175.03

####Sqrt model
sqrt_model <- lm(sqrt(Price)~., data = toyota)
summary(sqrt_model) #0.8665  and error 5.9
sqrt_model_rmse <- sqrt(mean((Price - (sqrt_model$fitted.values)^2)))
sqrt_model_rmse #5.881

###we could say the the sqrt model is best of all with rmse value of 5.88 and and R Square value 0.866
#lets Predict

predict <- predict(sqrt_model)
predict

final <- data.frame(Price,predict,error=sqrt_model$residuals)
final
