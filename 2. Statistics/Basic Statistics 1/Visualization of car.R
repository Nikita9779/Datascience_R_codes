###Perform Basic Visualizations for all the columns(numerical data only) on any 
#data set from data set folder make sure it has more data. 
#So we can make better inferences for the visualizations(boxplot,histogram)


cars<- read.csv("E:/Data science Excelr/Assigments/BAsic Statistics 1/Cars.csv")
summary(cars)
boxplot(cars)
#In boxplot Hp has lots of Outliers to its Upperextreme
##MPG has no outliers
#Vol has only 2 outliers one to upper extreme and one in lower
#Sp has lot of outliers
#Wt also has only 2 outliers one to upper extreme and one in lower

library(moments)

#For HP
mean(cars$HP)  
median(cars$HP)    #Mean> median
skewness(cars$HP)  ##skewness is positive that means it is right skewed
kurtosis(cars$HP)  ##and value here is greater than 3 so it is leptokurtic
hist(cars$HP,xlab = "HP",ylab = "Frequency") #shows skewness nature to right and a sharp peak & wide tail

#For MPG
mean(cars$MPG)
median(cars$MPG)    #mean<median
skewness(cars$MPG)  ###skewness is negative that means it is left skewed
kurtosis(cars$MPG)  ##and value here is less than 3 so it is platykurtic
hist(cars$MPG,xlab = "mpg",ylab = "Frequency") ##shows skewness nature to left and a broad peak & thin tail


#For VOl
mean(cars$VOL)
median(cars$VOL)   #Mean< median
skewness(cars$VOL)  ##skewness is negative that means it is left skewed
kurtosis(cars$VOL)  ##and value here is greater than 3 so it is leptokurtic
hist(cars$VOL,xlab = "vol",ylab = "Frequency") #shows skewness nature to left and a sharp peak& wide tail


#For SP
mean(cars$SP)   
median(cars$SP)     #Mean> median
skewness(cars$SP)   ##skewness is positive that means it is right skewed
kurtosis(cars$SP)   ##and value here is greater than 3 so it is leptokurtic
hist(cars$SP,xlab = "sp",ylab = "Frequency")
#shows skewness nature to right and a sharp peak & wide tail

#For WT
mean(cars$WT)      
median(cars$WT)     #Mean< median
skewness(cars$WT)   ##skewness is negative that means it is left skewed
kurtosis(cars$WT)   ##and value here is greater than 3 so it is leptokurtic
hist(cars$WT,xlab = "wt",ylab = "Frequency") #shows skewness nature to left and a sharp peak & wide tail



library(lattice)

#DOtplot
dotplot(cars$HP,main="Dot plot",col="dodgerblue4")
dotplot(cars$MPG,col="red")
dotplot(cars$VOL,col="pink")
dotplot(cars$SP,col="purple")
dotplot(cars$MPG,col="orange")


#scatterplot
plot(cars$HP,main="Dot plot",col="dodgerblue4")
plot(cars$MPG,col="red")
plot(cars$VOL,col="green")
plot(cars$SP,col="purple")
plot(cars$MPG,col="orange")
