#VOLATILITY SURFACE IF INPUT FILE CONSISTS OF ONLY ONE STRIKE PRICE

########## Code for B-S Option Price for Call Option
callprice<-function(x,t,T,r,sigma,K)
{
  p<-(T-t)/252
  p<-as.numeric(p)
  d2<- (log(x/K) +(r-0.5*sigma^2)*(p))/(sigma*sqrt(p))
  d1<-d2+sigma*sqrt(p)
  x*pnorm(d1)-K*exp(-r*(p))*pnorm(d2)
}

########## Code for Implied Volatility
ImpliedVolatility<-function(x,t,T,r,K,ObsPrice)
{
  sigma<-seq(0.001,10,by=0.0001)
  length(sigma)
  C<-callprice(x,t,T,r,sigma,K)
  error<-abs(ObsPrice-C)
  leasterr<-0.1
  countn<-0
  for (i in 1:length(sigma))
  { 
    if(error[i] <0.1) 
    {
      if(leasterr>error[i])
      {
        leasterr<-error[i]
        countn<-(i)
      }
      ImpSigma<-sigma[countn]
    }
  }
  ImpSigma
}

##### Use file Option Prices - Final.csv or any file other file
z02<-read.csv(file.choose(),header=TRUE)   #read file 
r02<-0.04
T02<-z02$Expiry
t02<-z02$Date
## Check Link: http://www.statmethods.net/input/dates.html
T02<-as.Date(T02,"%d-%b-%y")
t02<-as.Date(t02,"%d-%b-%y")
x02<-z02$Underlying.Value
zFirstRow02<-z02[1,]
K_Start<-zFirstRow02$Strike.Price
K_interval<-((zFirstRow02$Strike.Price*2)-(zFirstRow02$Strike.Price))/(length(x02))
K_End<-(K_Start*2)-K_interval
K02<-seq(K_Start,K_End,by=K_interval)
ObsPrice02<-z02$Settle.Price
ImpSigma<-0

CalcSigma02<-mat.or.vec(length(x02),1)
for(j in 1:length(x02))
{
  CalcSigma02[j]<-ImpliedVolatility(x02[j],t02[j],T02[j],r02,K02[j],ObsPrice02[j])
} 

plot(K02, CalcSigma02, type="p")

cbind(K02,CalcSigma02)

require('rgl')
open3d()

P02_Num<-as.numeric(T02-t02)
plot3d(K02,P02_Num,CalcSigma02,col=rainbow(1000),type="s",xlab="Strike Price",
       ylab="Time to Maturity", zlab="Implied Volatility")


setInternet2(TRUE)
install.packages("scatterplot3d")
#load package
scatterplot3d(K02,P02_Num,CalcSigma02, color="red", type="l")
