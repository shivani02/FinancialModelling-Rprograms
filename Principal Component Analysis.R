7th Feb 2013 Class

fEcfon

yield.data<- read.csv(file.choose(), header=T)
head(yield.data)            \\ for seeing the data
xx1<- yield.data[,2:12]     \\ for get columns 2 to 12 into object xx1

load : tseries
asf.test(xx1[,1])           \\ look at p values to reject (or not reject) the null hypothesis

pc<-princomp(xx1)           \\ pricipal component analysis of the data set
summary(pc)                 \\ summary of PCA
plot(pc)

eig<-eigen(cov(xx1))        \\ eigen values and eigen vectors using a covariance matrix
eig$values
eig$vectors

[critical value of -3.50]
null hypothesis: characteristic equation's 
null hypothesis of a unit root will be rejected

A linear stochastic process has a unit root if 1 is a root of the process's characteristic equation.
Such a process is non-stationary.

How to convert a non-stationary time-series into a stationary time-series?
1. Take the first difference
2. lag method