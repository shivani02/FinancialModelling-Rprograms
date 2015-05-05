yield.data<-read.csv(file.choose(),header=T)
xx1<-yield.data[,2:12]  	# In order to remove date column because we do not want to check stationarity for it	

#Now Load Package -- tseries

adf.test(xx1[,1])
adf.test(xx1[,2])
adf.test(xx1[,3])
adf.test(xx1[,4])
adf.test(xx1[,5])
adf.test(xx1[,6])
adf.test(xx1[,7])
adf.test(xx1[,8])
adf.test(xx1[,9])
adf.test(xx1[,10])
adf.test(xx1[,11])

#In Our case p-value is less than 0.05 only for column 1 & 2.

#If p value less than 0.05, then Alternate hypothesis is true, and reject null hypothesis and hence the returns are stationary.
#Also if Dickey Fuller value is less than  -3.5, then Alternate is true. This is true for column1 & column 2. 

#Pca cannot be done if any of the columns are NON STATIONARY.
#we need to chk all the collumns for its stationarity


ylag<-diff(xx1[,3],lag=1)
adf.test(ylag)
ylag3<-c(0,ylag)

ylag<-diff(xx1[,4],lag=1)
adf.test(ylag)
ylag4<-c(0,ylag)

ylag<-diff(xx1[,5],lag=1)
adf.test(ylag)
ylag5<-c(0,ylag)

ylag<-diff(xx1[,6],lag=1)
adf.test(ylag)
ylag6<-c(0,ylag)

ylag<-diff(xx1[,7],lag=1)
adf.test(ylag)
ylag7<-c(0,ylag)

ylag<-diff(xx1[,8],lag=1)
adf.test(ylag)
ylag8<-c(0,ylag)

ylag<-diff(xx1[,9],lag=1)
adf.test(ylag)
ylag9<-c(0,ylag)

ylag<-diff(xx1[,10],lag=1)
adf.test(ylag)
ylag10<-c(0,ylag)

ylag<-diff(xx1[,11],lag=1)
adf.test(ylag)
ylag11<-c(0,ylag)

# principal component analysis
ylag1<-xx1[,1]
ylag2<-xx1[,2]

ytable<-cbind(ylag1,ylag2,ylag3,ylag4,ylag5,ylag6,ylag7,ylag8,ylag9,ylag10,ylag11)
pc<-princomp(ytable[,1:11])
pc1<-princomp(xx1)
pc2<-princomp(ytable)
summary(pc1)
plot(pc1)
#Result for only 2 columns

Importance of components:
  Comp.1    Comp.2     Comp.3     Comp.4      Comp.5
Standard deviation     0.8740602 0.2114276 0.12576898 0.05742168 0.052774290
Proportion of Variance 0.9160754 0.0536009 0.01896687 0.00395367 0.003339592
Cumulative Proportion  0.9160754 0.9696763 0.98864318 0.99259685 0.995936442
Comp.6       Comp.7       Comp.8      Comp.9
Standard deviation     0.042713872 0.0224272176 0.0203366643 0.015681223
Proportion of Variance 0.002187693 0.0006031139 0.0004959159 0.000294855
Cumulative Proportion  0.998124135 0.9987272490 0.9992231648 0.999518020
Comp.10      Comp.11
Standard deviation     0.015073923 0.0132187292
Proportion of Variance 0.000272459 0.0002095212
Cumulative Proportion  0.999790479 1.0000000000
summary(pc2)
plot(pc2)
# Result
Importance of components:
  Comp.1     Comp.2     Comp.3    Comp.4     Comp.5
Standard deviation     0.1937699 0.08851488 0.05495142 0.0269792 0.02425733
Proportion of Variance 0.7401832 0.15445394 0.05952841 0.0143491 0.01159985
Cumulative Proportion  0.7401832 0.89463709 0.95416549 0.9685146 0.98011444
Comp.6      Comp.7      Comp.8      Comp.9
Standard deviation     0.017566367 0.015874422 0.012426050 0.010945260
Proportion of Variance 0.006083175 0.004967779 0.003043916 0.002361667
Cumulative Proportion  0.986197619 0.991165398 0.994209314 0.996570981
Comp.10     Comp.11
Standard deviation     0.009980534 0.008621516
Proportion of Variance 0.001963695 0.001465324
Cumulative Proportion  0.998534676 1.000000000


From PCA we can infer that Component 1,2 & 3 are significant and rest of the components have a very minimal contribution.