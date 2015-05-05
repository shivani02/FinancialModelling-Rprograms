//18 Jan 2013

//Objective: A gentle introduction to GARCH(1,1) with R

//Installed a package: quadprog and tseries
//Load package: tseries

z<-read.csv(file.choose(), header=t)
acf(z)
acf(z$Open) //Auto correlation function plot

z.ts<- ts(z[,2]) //creates a time-series out of the z object
z.tsdiff<- diff(z.ts)
z.tsdiff
ret<-z.tsdiff/z.ts // calculates the returns
zz<-cbind(z.ts,z.tsdiff,ret) //displays in tabular form
acf(ret)

garch.ts<- garch(ret)
coef(gfit.ts)
		a0		a1		b1

#plot in-sample 
