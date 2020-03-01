d<-c(101,110,103,93,99,104)
sd(d) #to calculate standard deviation of smthng


2*(1-pt(5.57,5))  #to calculate p value 

#to computer two sample test
control<-c(91,87,99,77,88,91)
Treat<-c(101,110,103,93,99,104)
t.test(control,Treat,alternative = "two.sided")


model<-lm(sunday~daily,data = NewspaperData)
summary(NewspaperData)

pred<-predict(model,newdata = data.frame(daily=300))
pred

