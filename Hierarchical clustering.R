#Hierarchical Clustering

#LOAD DATA
mydata1<-read.csv("C:/Users/Admin/Downloads/Universities.csv")


########################AVERAGE###############
mydata <-scale(mydata1[,2:7])
d<-dist(mydata,method = "euclidean")  #compute the distance matrix
fit<-hclust(d,method = "average")  #Building the algorithm #try with 'centroid'
plot(fit)   #display dendogram
groups<-cutree(fit,k=4)    #cut tree into 4 clusters

#draw dendogram with red borders around the 4 clusters
rect.hclust(fit,k=4,border = "blue")

#attach the cluster number to uni
cluster=data.frame('uni'=mydata1[,1],'cluster'=groups)

#################CENTROID#################
fit1<-hclust(d,method = "centroid")
plot(fit1)   #display dendogram
groups<-cutree(fit,k=4)
rect.hclust(fit1,k=4,border = "red")
cluster1=data.frame('uni'=mydata1[,1],'cluster'=groups)
