#Decision Tree

#Lets Build a Decision Tree for the Iris Data using ctree() of "party" package



datasets::iris
attach(iris)

library(caret)
library(party)
library(descr)
library(moments)

summary(iris)
#The column Species is already in the form of a categorical Variable
str(iris) #Gives the Entire Structure of the Data

names(iris) #Gives the Column Names

dim(iris) #Returns the Number of Rows and Columns


#Standard Deviation
sd(Sepal.Length)
sd(Sepal.Width)
sd(Petal.Length)
sd(Petal.Width)

#Variance
var(Sepal.Length)
var(Sepal.Width)
var(Petal.Length)
var(Petal.Width)

#Skewness
skewness(Sepal.Length)
skewness(Sepal.Width)
skewness(Petal.Length)
skewness(Petal.Width)

#Kurtosis
kurtosis(Sepal.Length)
kurtosis(Sepal.Width)
kurtosis(Petal.Length)
kurtosis(Petal.Width)


boxplot(iris) #Displays the Boxplot for every column in the Dataset

pairs(iris) #Using Pairs Function we get plot from which we can see if any columns are collinear

#Lets Divide the Data for building the model
inTraininglocal <- createDataPartition(iris$Species,p=.60,list = F) #.60 means 60% Partition
training<- iris[inTraininglocal,]
testing<- iris[-inTraininglocal,]
attach(iris)

#Lets build the model
model <- ctree(Species~. , data = training) #ctree() is in "party" library
summary(model)

#Lets Predict the data for the test Dataset
pred <- predict(model,testing[,-5])
pred
iris_tab <- table(testing$Species, pred)

acc <- sum(diag(iris_tab))/sum(iris_tab) #Gives the Accuracy of the Model
acc  #95%

CrossTable(testing$Species, pred)

plot(model) #This Plots the Decision Tree
#The Decision Tree says that the left group has 30 flowers whose Petal.Length is less than equal to 1.9
#The second group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width less than equal to 1.7 and Petal.Length less than equal to 4.6 with 23 numbers
#The Third group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width less than equal to 1.7 and Petal.Length greater than 4.6 which are 8
#The Fourth group consists of the flowers with Petal.Length greater than 1.9 and Petal.Width Greater than 1.7 which are 29 flowers