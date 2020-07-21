#Neural Networks

#Prepare a model for strength of concrete data using Neural Networks

library(neuralnet)
library(ggplot2)

concrete <- read.csv("E:/Data science Excelr/Assigments/Supervised Machine Learning/Neural Networks/Concrete/concrete.csv")
attach(concrete)
summary(concrete)

names(concrete)

cor(concrete)

#Standard deviation
sd(cement)
sd(slag)
sd(ash)
sd(water)
sd(superplastic)
sd(coarseagg)
sd(fineagg)
sd(age)
sd(strength)

#Variance
var(cement)
var(slag)
var(ash)
var(water)
var(superplastic)
var(coarseagg)
var(fineagg)
var(age)
var(strength)

#Plots
boxplot(concrete)

pairs(concrete)

hist(strength, col = "purple3", title = "Strength")

hist(age, main = "Age of Cement", col = "pink")

#Lets Normalise the data
normalise <- function(x)
{
  return((x - min(x))/(max(x) - min(x)))
}

conc_norm <- as.data.frame(lapply(concrete, normalise))


#PLOTS

ggplot(conc_norm) + geom_histogram(aes(cement), binwidth = 0.1,fill = 'pink' , col = 'black') + ggtitle("cement")

ggplot(conc_norm) + geom_point(aes(slag, ash))

#lets Split the Data Randomly in test and train set
split <- sort(sample(nrow(conc_norm), nrow(conc_norm)*0.8))
train <- conc_norm[split,]
test <- conc_norm[-split,]

#Lets build the Neural Network Model
n1 <- neuralnet(formula = strength~. , data = train)
plot(n1) #Error= 5.5
n1_result <- compute(n1, test[1:8])
str1 <- n1_result$net.result
round(cor(test[,9],str1)*100, digits = 2)
#The Accuracy of the Model is 84.99%


#Lets Improve the Accuracy of the Model by Building complex Neural Network
n2 <- neuralnet(formula = strength~. , data = train, hidden = c(5,3))
plot(n2) #Error = 1.85
n2_result <- compute(n2, test[1:8])
str2 <- n2_result$net.result
round(cor(test[,9],str2)*100, digits = 2)
#The Accuracy of the Model is 95.31%


n3 <- neuralnet(formula = strength~. , data = train, hidden = c(5,5,3))
plot(n3) #Error = 1.67
n3_result <- compute(n3, test[1:8])
str3 <- n3_result$net.result
round(cor(test[,9],str3)*100, digits = 2)
#The Accuracy of the Model is 95.66%


n4 <- neuralnet(formula = strength~. , data = train, hidden = c(5,4,3,2))
plot(n4) #error = 1.3477
n4_result <- compute(n4, test[1:8])
str4 <- n4_result$net.result
round(cor(test[,9],str4)*100, digits = 2)
#The Accuracy of the Model is 94.63%


n5 <- neuralnet(formula = strength~. , data = train, hidden = c(6,5,4,3,2))
plot(n5) #Error = 1.33
n5_result <- compute(n5, test[1:8])
str5 <- n5_result$net.result
round(cor(test[,9],str5)*100, digits = 2)
#The Accuracy of the Model is 95.05%
#I could say n5 is best of all with 95% accuracy and minimum error rate