yield.data<-read.csv(file.choose(),header=T)
head(yield.data)
//to skip first colum ie date
xx1<-yield.data[,2:12]
head(xx1)

// to check stationarity - acf plot
thumb Rule : shud lie within stand errors
dickey fuller test



adf.test(xx1[,3])
ylag<-diff(xx1[,3],lag=1)
adf.test(ylag)
eck p value , null hypo is rejected - data is stationary
//crictical value of -3.5
// do first diff or one lag - to convert non stationary to staionary

p<-c(0,ylag)
// principal component analysis
ylag1<-xx1[,1]
ylag2<-xx1[,2]
ytable<-cbind(ylag1,ylag2,p)
pc<-princomp(xx1[,1:2],p)
pc<-princomp(xx1)
pc<-princomp(ytable)
summary(pc)
plot(pc)

// 3 factor structre is good enough to explain the huge portion if varaince in data
// eigen values either thru cov or cor matrix

eig<-eigen(cov(xx1))
eig$values
eig$vectors