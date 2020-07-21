install.packages("plyr")
library(plyr)
x<- runif(50) #generating 50 random numbers from uniform distribution(0,1)
y<- runif(50) #generating 50 random numbers from uniform distribution(0,1)
data<-cbind(x,y)
plot(data)

#get the refrence value of k for
#the experiment in elbow plt-4 clusers;
#use the above k value as reference and run the below for loop to find the optimum k value

wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "No of Clusters",ylab = "avg distance")


#CLuster algorithm building
km<-kmeans(data,10)
km$centers
km$cluster

install.packages("animation")
library(animation)

windows() #to pop up new window to show o/p use it with below code always
km<-kmeans.ani(data,10)
