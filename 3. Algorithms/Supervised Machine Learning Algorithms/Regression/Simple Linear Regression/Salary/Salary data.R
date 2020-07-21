###Simple Linear Regression for salary data#####
###Salary_hike -> Build a prediction model for Salary_hike
##Do the necessary transformations for input variables for getting better R^2 value for the model prepared.

#Load the data
salary<-read.csv("E:/Data science Excelr/Assigments/Simple Linear Regression/Salary/Salary_Data.csv")
summary(salary)
head(salary)
tail(salary)

##EDA
#standard Deviation
sd(salary$Salary) #Standard Deviation of salary hike column
sd(salary$YearsExperience) #Standard Deviation of Years experience column

#Variance
var(salary$Salary) #Variance of salary hike column
var(salary$YearsExperience) #Variance Of Years Experience column

#Correlation matrix
cor(salary)  #Shows the correlation matrix of dataset

#PLOTS
library(lattice)
boxplot(salary)  #Boxplot shows if the dataset's columns has any outliers or not so here their are none
hist(salary$Salary) #Histogram
hist(salary$YearsExperience) 
plot(salary) #A simple point plot of the dataset
dotplot(salary$Salary,main="Dot plot of salary hike",col="dodgerblue4")
dotplot(salary$YearsExperience, main="Dot plot of years experience" , col="purple")


####Simple linear model
#syntax model<-lm(y~x,data=data set name)
model<- lm(Salary~.,data = salary) # a simple model for emp data
summary(model) ##Gives A overview of your model 
plot(model)   #gives plot as qqplot residualvsfitted..
###Here we can see the R-Squared value is 0.957  which means 95% it will predict right value and p value is <0.05
#and here the residual error value is 5788 which is not good at all


####A log model
#X= log(x) and Y=y ----lm(y~log(x))
salary_log_model<- lm(YearsExperience~log(Salary), data = salary)  # a logarithmic model
summary(salary_log_model)
plot(salary_log_model)
###Here we can see the R-Squared value is 0.932  which means 93% it will predict right value and p value is <0.05
#and here the residual error value is 0.7 which is good and better than simple model

###A exponential model
#X= x and Y=log(y) ----lm(log(y)~x) ##
salary_expo_model <- lm(log(Salary)~., data = salary)
summary(salary_expo_model)
plot(salary_expo_model)
###Here we can see the R-Squared value is 0.932  which means 93% it will predict right value and p value is <0.05
#But here the residual error value is 0.09 which is  good and better than all other model so we can consider this model for prediction

predict <- predict(salary_expo_model) #Predicts for the given model
predict

salary_expo_model$residuals #gives the residual values


sqrt(mean(salary_expo_model$residuals^2))

abc <- exp(predict)
abc

confint(salary_expo_model,level=0.95)

predict(salary_expo_model, interval="confidence")


final<-data.frame(salary,predict,"Error"= salary$Salary-abc)
final
