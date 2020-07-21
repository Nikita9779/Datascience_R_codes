#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and
#make a table containing R^2 value for each prepared model.

#R&D Spend -- Research and devolop spend in the past few years
#Administration -- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years
#State -- states from which data is collected
#Profit  -- profit of each state in the past few years
install.packages("corpcor")

library(ggplot2)
library(Amelia)
library(corpcor)
library(DataExplorer)
library(moments)

##Loading the data
startups <- read.csv("E:/Data science Excelr/Assigments/Multi linear Regression/50_startup/50_Startups.csv")
attach(startups)
summary(startups)
str(startups)
missmap(startups, col = c('yellow','black'), main = 'check') ##To see if their are any missing values and we had a full black plot so it indicate no missing values



###EDA###
###measure of dispersion
sd(R.D.Spend)
sd(Administration)
sd(Marketing.Spend)
sd(Profit)

var(R.D.Spend)
var(Administration)
var(Marketing.Spend)
var(Profit)

range(R.D.Spend)
range(Administration)
range(Marketing.Spend)
range(Profit)

skewness(R.D.Spend)
skewness(Administration)
skewness(Marketing.Spend)
skewness(Profit)

kurtosis(R.D.Spend)
kurtosis(Administration)
kurtosis(Marketing.Spend)
kurtosis(Profit)


###Plots
boxplot(startups[,-4])
pairs(startups[,-4])
cor2pcor(cor(startups[,-4]))
ggplot(startups) +geom_histogram(aes(R.D.Spend), binwidth = 4000, fill = "pink")
ggplot(startups) + geom_bar(aes(Administration),binwidth = 5000 ,fill = "cyan") + xlab("Administration")
plot(Marketing.Spend, xlab = "Marketing Expense", type = "l")
plot(Profit, type = "s")
avPlots(model,id.n=2,id.cex=0.7)


#Lets Build the Model
model <- lm(Profit~. , data = startups)
summary(model)  #

#The adjusted R-Square value for the above model is 0.945 which 94 % will give right asnwer
pred1 <- predict(model)
model_rmse <- sqrt(mean((startups$Profit - model$fit)^2, na.rm = T))
model_rmse  #8854.76

plot(model)
library(carData)
library(car)

#Lets Find Outliers and remove them
car::vif(model) #variation influence factor
influenceIndexPlot(model)
influencePlot(model)
#Here we got  49 and 50 as outliers so we will remove them

#Iteration 1
startups <- startups[-c(49,50),]
#Now lets make another model

model1 <- lm(Profit~. , data = startups)
summary(model1)
#The R-square value is 0.9584

car::vif(model1)
plot(model1)
residualPlots(model1)
qqPlot(model1)
influenceIndexPlot(model1)
influencePlot(model1)
#Iteration 2

startups <- startups[-c(15,16),]
model2 <- lm(Profit~. , data = startups)
summary(model2)
#The R-square value is 0.9676
car::vif(model2)
plot(model2)
residualPlots(model2)
qqPlot(model2)
influenceIndexPlot(model2)
influencePlot(model2)
#Iteration 3

startups <- startups[-c(46,47,37,20),]
model3 <- lm(Profit~. , data = startups)
summary(model3)

#The R-square value is 0.9672

r_square <- data.frame("model"=c("First","Iteration 1", "Iteration 2","Iteration 3"),"R-square" = c(0.945,0.9584, 0.9676, 0.9672))
r_square

##Log Transformation

#######Exponential Model


expo_model <- lm(log(Profit)~., data = startups)
summary(expo_model)  ##0.7385
expo_model_rmse <- sqrt(mean((Profit - exp(expo_model$fit))^2, na.rm = T))
expo_model_rmse  #15254.94

#######Sqrt model
sqrt_model <- lm(sqrt(Profit)~., data = startups)
summary(sqrt_model) #0.8844
sqrt_model_rmse <- sqrt(mean((Profit - (sqrt_model$fitted.values)^2)))
sqrt_model_rmse #20.696

#Lets Build the RMSE table

rmse_table <- data.frame("Model Name" = c("Multi_Linear_Model", "Expo_Model","sqrt_model"), "RMSE" = c(model_rmse, expo_model_rmse,sqrt_model_rmse))
rmse_table

#sqrt model has lowest rmse value so we can say it is best above all