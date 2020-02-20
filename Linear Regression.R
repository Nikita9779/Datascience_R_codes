d<-c(101,110,103,93,99,104)
sd(d) #to calculate standard deviation


2*(1-pt(5.57,5))  #to calculate p value 

#to computer two sample test
control<-c(91,87,99,77,88,91)
Treat<-c(101,110,103,93,99,104)
t.test(control,Treat,alternative = "two.sided")

#to calculate the linear regression
model<-lm(sunday~daily,data = NewspaperData)
summary(NewspaperData)
#to predict for 300 daily circulations
pred<-predict(model,newdata = data.frame(daily=300))
pred
