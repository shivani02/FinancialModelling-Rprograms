#COMPARE PRICES of Options (Actual and Calculated)


###### Use file S&P CNX NIFTY01-01-2012-31-12-2012.csv to  determine Historical Sigma Value
z<-read.csv(file.choose(),header=TRUE)   #read file to determine historical sigma
y1<-z$Open #select Open column
y1<-ts(data=y1,frequency=252) #convert into timeframe data
ylag<-lag(y1,k=-1) #create St-1
ytable<-cbind(y1,ylag) #create a table with y and ylag
daily.return<-y1/ylag 
var.dr<-var(daily.return)
samplesd<-sqrt(var.dr)
ylen<-length(y1)+1

hist_sigma<-samplesd*sqrt(ylen)


######### Use file Option Prices - Final.csv now
z<-read.csv(file.choose(),header=TRUE)  # to obtain parameters of B-S Model

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

#### Use  CallPrice and Implied Volatility Function from Below and then proceed to the following code.
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

CalcSigma<-mat.or.vec(length(x),1)
for(j in 1:length(x))
{
  CalcSigma[j]<-ImpliedVolatility(x[j],t[j],T[j],r,K[j],ObsPrice[j])
}
Theo_BS_Call_Hist_Sigma<-0
Theo_BS_Call_Imp_Vol<-0
for(i in 1:length(x))
{
  Theo_BS_Call_Hist_Sigma[i]<-callprice(x[i],t[i],T[i],r,hist_sigma,K[i])
  Theo_BS_Call_Imp_Vol[i]<-callprice(x[i],t[i],T[i],r,CalcSigma[i],K[i])
}

cbind(Theo_BS_Call_Hist_Sigma,Theo_BS_Call_Imp_Vol,ObsPrice)


plot (x, Theo_BS_Call_Hist_Sigma,type = "l",  col = "red")
par(new=TRUE)
plot (x, Theo_BS_Call_Imp_Vol,type = "l",  col = "green")
par(new=TRUE)
plot (x, ObsPrice,type = "l",  col = "blue")
