Option NCDX - Input File

callprice<-function(x,t,T,r,sigma,K)
{
  #x<-ts(data=x,frequency=length(x))
  p<-(T-t)/365
  p<-as.numeric(p)
  #p<-(T-t)
  d2<- (log(x/K) +(r-0.5*sigma^2)*(p))/(sigma*sqrt(p))
  d1<-d2+sigma*sqrt(p)
  x*pnorm(d1)-K*exp(-r*(p))*pnorm(d2)
}

z<-read.csv(file.choose(),header=TRUE)   #read file 
#head(z)
zFirstRow<-z[1,]

r<-0.04
#T<-1/4
#t<-1/12

T<-z$Expiry
t<-z$Date
## Check Link: http://www.statmethods.net/input/dates.html
T<-as.Date(T,"%d-%b-%y")
t<-as.Date(t,"%d-%b-%y")

x<-z$Underlying.Value
K<-zFirstRow$Strike.Price
ObsPrice<-z$Settle.Price

C<-callprice(x,t,T,r,sigma,K)
C
