###Simple Linear Regression for Emp data#####
###Emp_data -> Build a prediction model for Churn_out_rate
##Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Load the data
emp<-read.csv("E:/Data science Excelr/Assigments/Simple Linear Regression/Employee/emp_data.csv")
summary(emp)
head(emp)
tail(emp)


####EDA

#standard Deviation
sd(emp$Salary_hike) #Standard Deviation of salary hike column
sd(emp$Churn_out_rate) #Standard Deviation of churn out column

#Variance
var(emp$Salary_hike) #Variance of salary hike column
var(emp$Churn_out_rate) #Variance Of churn out column

#Correlation matrix
cor(emp)  #Shows the correlation matrix of dataset

#PLOTS
library(lattice)
boxplot(emp)  #Boxplot shows if the dataset's columns has any outliers or not so here their are none
hist(emp$Salary_hike) #Histogram
hist(emp$Churn_out_rate) 
plot(emp) #A simple point plot of the dataset
dotplot(emp$Salary_hike,main="Dot plot of salary hike",col="dodgerblue4")
dotplot(emp$Churn_out_rate, main="Dot plot of churn out" , col="purple")

####Simple linear model
#syntax model<-lm(y~x,data=data set name)
model<- lm(Churn_out_rate~.,data = emp) # a simple model for emp data
summary(model) ##Gives A overview of your model 
plot(model) #gives plot as qqplot residualvsfitted..
###Here we can see the R-Squared value is 0.831  which means 83% it will predict right value and p value is <0.05
#But here the residual error value is 4.46 which is not good 


####A log model
#X= log(x) and Y=y ----lm(y~log(x))
emp_log_model<- lm(Churn_out_rate~log(Salary_hike), data = emp)  # a logarithmic model
summary(emp_log_model)
plot(emp_log_model)
###Here we can see the R-Squared value is 0.848  which means 84% it will predict right value and p value is <0.05
#But here the residual error value is 4.233 which is not good but better that simple model

###A exponential model
#X= x and Y=log(y) ----lm(log(y)~x) ##
emp_expo_model <- lm(log(Churn_out_rate)~., data = emp)
summary(emp_expo_model)
plot(emp_expo_model)
#####Here we can see the R-Squared value is 0.873  which means 87% it will predict right value and p value is <0.05
#But here the residual error value is 0.05 which is good but can be better if we remove those points


library(carData)
library(car)
car::vif(emp_expo_model)
influenceIndexPlot(emp_expo_model)

emp1<-emp[-c(1,10),]

#after removing those points lets again build the exponential model
emp_expo_model1 <- lm(log(Churn_out_rate)~., data = emp1)
summary(emp_expo_model1)
plot(emp_expo_model1)

##we can see the R-Squared value is increased to 0.929 
#and Residual Standard Error 0.03 which is very less compared to others
#so we can use this model to predict
predict <- predict(emp_expo_model1) #Predicts for the given model
predict

emp_expo_model1$residuals #gives the residual values


sqrt(mean(emp_expo_model1$residuals^2))

vc <- exp(predict)
vc

confint(emp_expo_model1,level=0.95)
predict(emp_expo_model1, interval="confidence")

final<-data.frame(emp1,predict,"Error"= emp1$Churn_out_rate-vc)
final
