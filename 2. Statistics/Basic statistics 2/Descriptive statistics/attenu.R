attenu<-datasets::attenu #show the data set
attenu
head(attenu) #to show first few rows
tail(attenu)  #to show last few rows
attenu[,c(2,3)] #to show the first 2,3 columns
attenu$event  #to show entries in a particular column
attenu$mag
attenu$station

summary(attenu$event) #gives the summaries data.i.e mean,median,mode for column
summary(attenu$mag)
plot(attenu$event)
plot(attenu$event,attenu$station) #used to plot graph with 2 columns
plot(attenu)
summary(attenu)   #give the summary data for all columns

#plot is used to plot or display a graph of a particular dataframe
plot(attenu$event,type="l") #used to give the plotting type .i.e points,lines etc
plot(attenu$dist,type="h")
plot(attenu$dist,type="s")
plot(attenu$station, xlab = "station",ylab = "No of Instances", main = "", col="lightgreen") #xlab is used for giving name to x axis and ylab for y axis and main for heading

#used to plot a barplot
barplot(attenu$accel,main = 'bargraph of accel',xlab = 'ACCEL')

#Used to plot histogram
hist(attenu$mag,col = 'purple')
hist(attenu$dist,xlab = 'distribution',ylab = 'no of frequency',col = 'yellow')

#to box plot

boxplot(attenu$dist)
boxplot(attenu[,1:5],main='Multiple',col = 'pink')

#to plot various graphs in one

par(mfrow=c(4,4),mar=c(2,5,2,2),las=FALSE,bty='n') #mfrow is used to partition and mar is used for margin length
plot(attenu$event)
plot(attenu$event,attenu$station)
plot(attenu$event,type="l")
plot(attenu$dist,type="h")
plot(attenu$dist,type="s")
plot(attenu$station, xlab = "station",ylab = "No of Instances", main = "", col="lightgreen")
barplot(attenu$accel,main = 'bargraph of accel',xlab = 'ACCEL')
hist(attenu$mag,col = 'purple')
hist(attenu$dist,xlab = 'distribution',ylab = 'no of frequency',col = 'yellow')
boxplot(attenu$dist)
boxplot(attenu[,1:5],main='Multiple Plots',col = 'pink')

