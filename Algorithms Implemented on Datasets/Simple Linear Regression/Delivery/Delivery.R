#####Simple Linear Equation For Delivery Time$#############
#Delivery_time -> Predict delivery time using sorting time 
#Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

delivery<- read.csv("E:/Data science Excelr/Assigments/Simple Linear Regression/Delivery/delivery_time.csv")
summary(delivery)
head(delivery)
tail(delivery)
plot(delivery)

####EDA

#standard Deviation
sd(delivery$Delivery.Time) #Standard Deviation of Delivery time column
sd(delivery$Sorting.Time) #Standard Deviation of sorting time column

#Variance
var(delivery$Delivery.Time) #Variance of Delivery time column
var(delivery$Sorting.Time) #Variance Of Sorting

#Correlation matrix
cor(delivery)  #Shows the correlation matrix of dataset

#PLOTS

boxplot(delivery)  #Boxplot shows if the dataset's columns has any outliers or not so here their are none
hist(delivery$Delivery.Time) #Histogram
hist(delivery$Sorting.Time) 
plot(delivery) #A simple dot plot of the dataset

####Simple LInear Model##
#syntax model<-lm(y~x,data=data set name)
model <- lm(Delivery.Time~.,data = delivery)  #A simple linear model for dataset= delivery
summary(model) #Gives the overview of your model 
plot(model) #Gives the QQplot,residualplot etc
#Here we can see the R-Squared value is 0.6823  which means 68% it will predict right value and p value is <0.05
#But here the residual error value is 2.93 which is not good 


##log transformation
#X= log(x) and Y=y ----lm(y~log(x))
delivery_log_model <-lm(Delivery.Time~log(Sorting.Time),data=delivery) # A logarithmic model 
summary(delivery_log_model)
plot(delivery_log_model)
##Here we can see the R-Squared value is 0.695  which means 69% it will predict right value and p value is <0.05
#But here the residual error value is 2.873 which is not good 

#Exponential Model
#X= x and Y=log(y) ----lm(log(y)~x)
delivery_expo_model<-lm(log(Delivery.Time)~.,data = delivery) #A exponential model 
summary(delivery_expo_model)
plot(delivery_expo_model)

#Here we can see the R-Squared value is 0.710  which means 71% it will predict right value and p value is <0.05
#and here the residual error value is 0.1755 which is good but we can see the % value is very low we can try making a better model by eliminating the outliers

library(carData)
library(car)
car::vif(delivery_expo_model)
influenceIndexPlot(delivery_expo_model1)

delivery1 <-delivery[-c(21,9),]
delivery_expo_model1<-lm(log(Delivery.Time)~.,data = delivery1) #A exponential model 
summary(delivery_expo_model1)
plot(delivery_expo_model1)
#so we can see by eliminating those point i got R-Squared Value as 0.843 
#and Residual Error 0.133 so we can say it is a good model to predict 

predict <- predict(delivery_expo_model1) #Predicts for the given model
predict

delivery_expo_model1$residuals #gives the residual values


sqrt(mean(delivery_expo_model1$residuals^2))

ac <- exp(predict)
ac


confint(delivery_expo_model1,level=0.95)
predict(delivery_expo_model1, interval="confidence")

final<-data.frame(delivery1,predict,"Error"= delivery1$Delivery.Time-ac)
final

