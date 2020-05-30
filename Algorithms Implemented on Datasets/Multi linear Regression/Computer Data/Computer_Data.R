#Predict Price of the computer

#A dataframe containing :
#price : price in US dollars of 486 PCs
#speed : clock speed in MHz
#hd : size of hard drive in MB
#ram : size of Ram in in MB
#screen : size of screen in inches
#cd : is a CD-ROM present ?
#multi : is a multimedia kit (speakers, sound card) included ?
#premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
#ads : number of 486 price listings for each month
#trend : time trend indicating month starting from January of 1993 to November of 1995.

####loading packages
library(ggplot2)
library(moments)
library(carData)
library(car)


###Loading the dataset
computer<-read.csv("E:/Data science Excelr/Assigments/Multi linear Regression/Computer Data/Computer_Data.csv")
attach(computer)
summary(computer)
str(computer)
computer<-computer[,-1] #as we can see column 1 was only id number

###EDA

##Standard Deviation
sd(price)
sd(speed)
sd(hd)
sd(ram)
sd(screen)

#Variance
var(price)
var(speed)
var(hd)
var(ram)
var(screen)

skewness(price)
skewness(speed)
skewness(hd)
skewness(ram)
skewness(screen)

kurtosis(price)
kurtosis(speed)
kurtosis(hd)
kurtosis(ram)
kurtosis(screen)

cor(computer[,-c(6,7,8,10)])


#lets convert the non numeric columns
cd <- as.factor(cd)
multi<- as.factor(multi)
premium <- as.factor(premium)

#Plots
ggplot(computer) + geom_histogram(aes(price), fill = "brown", binwidth = 50) + xlab("Price") # the data is right skewed

ggplot(computer) + geom_bar(aes(trend),binwidth = 0.5 ,fill = "cyan") + xlab("Trend") #This is Normally Distributed

ggplot(computer) +geom_histogram(aes(hd), binwidth =50,  fill = "purple") #The data is right skewed

ggplot(computer) + geom_bar(aes(ram),binwidth = 0.9 ,fill = "pink") + xlab("ram")


###Building the model
model <- lm(price~.,data = computer)
summary(model)
###we found the R square value 0.775 and Residual error 275.3
model_rmse <- sqrt(mean((price - model$fit)^2, na.rm = T))
model_rmse #275.1


##LEts build by removing outliers
car::vif(model)
residualPlot(model)
influenceIndexPlot(model)

computer1 <- computer[-c(1441,1701),]
#Iteration 1
model1 <- lm(price~.,data = computer1)
summary(model1) ##0.777
car::vif(model1)
influenceIndexPlot(model1)

#Iteration 2
computer2 <- computer1[-c(994,5961,3784,4478)]
model2 <- lm(price~.,data = computer2)
summary(model2) ###0.777

###Data Transformation

###Exponential model
expo_model <- lm(log(price)~.,data = computer)
summary(expo_model)
##R square Value is 0.782 and residual error 0.1202
expo_rmse <- sqrt(mean((price- exp(expo_model$fitted.values))^2, na.rm = T))
expo_rmse     #273.0

###sqrt model
sqrt_model <- lm(sqrt(price)~.,data = computer)
summary(sqrt_model)
##R square Value is 0.7853 and residual error 2.807
sqrt_rmse <- sqrt(mean(price-(sqrt_model$fitted.values)^2))
sqrt_rmse ##2.804

###Preparing a table for every model's rmse values
rmse_table <- data.frame("model names" = c("Multilinear model","Exponential model","sqrt model"), " RMSE value" = c(model_rmse,expo_rmse,sqrt_rmse))
rmse_table

##We can see Sqrt model has lowest rmse value we will use it for prediction

predict <- predict(sqrt_model)
predict


final <- data.frame(price,predict, Error = sqrt_model$residuals)
final
