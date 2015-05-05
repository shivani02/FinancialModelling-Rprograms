#VOLATILITY SURFACE IF THE INPUT FILE CONSISTS OF VARYING STRIKE PRICE

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


#### Use file GGPlot2.csv
z<-read.csv(file.choose(),header=TRUE)   #read file 
r<-0.04
T<-z$Expiry
t<-z$Date
## Check Link: http://www.statmethods.net/input/dates.html
T<-as.Date(T,"%d-%b-%y")
t<-as.Date(t,"%d-%b-%y")
x<-z$Underlying.Value
K<-z$Strike.Price
ObsPrice<-z$Settle.Price
ImpSigma<-0

CalcSigma<-mat.or.vec(length(x),1)
for(j in 1:length(x))
{
  CalcSigma[j]<-ImpliedVolatility(x[j],t[j],T[j],r,K[j],ObsPrice[j])
} 

plot(x, CalcSigma, type="l")

require('rgl')
open3d()

P_Num<-as.numeric(T-t)
plot3d(K,P_Num,CalcSigma,col=rainbow(1000),type="l",xlab="Strike Price",
       ylab="Time to Maturity", zlab="Implied Volatility")
