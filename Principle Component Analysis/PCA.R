install.packages("gdata")
library(gdata)



PCA<-read.csv("C:/Users/Admin/Downloads/Universities.csv")

pca<-princomp(PCA[,2:7], cor = TRUE,scores = TRUE, covmat = NULL)
summary(pca)
pca$scores
pca$loadings

plot(pca$scores[,1:2],col="Red",pch=18,cex = 0.3, lwd = 3)

text(pca$scores[,1:2], labels=c(1:25), cex= 1,col = "Purple")
