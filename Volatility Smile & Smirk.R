
#         VOLATILITY SMILE
#--------volatility smirk --------------

callprice<-function(x,t,T,r,sigma,K){
  d2<- (log(x/K) + (r-0.5*sigma^2)*(T-t))/(sigma*sqrt(T-t))
  d1<- d2+sigma*sqrt(T-t)
  price<- x*pnorm(d1)-K*exp(-r*(T-t))*pnorm(d2)
  price
}

# K Implied Price Data file
price.optiondata <- read.csv(choose.files(),header=T)

sigma<- seq(0,1, by=0.00001)
S<-420
T<-1
k<-price.optiondata [,4]

r<-0.05
MP<-price.optiondata [,10]
l<-length(sigma)
limit<-length(k)

sig<-0
j<-1
t<-1/12
C<-0

for(j in 1:limit)
{
  z<-0
  y<-0
  
  C<- callprice(S,t,T,r,sigma,k[j])
  y= MP[j]- C
  z<- abs(y)
  
  temp=min(z)
  x<-0
  for(i in 1:l)
  {
    # if z[i] < 0.01
    if(z[i]==temp)
    {
      count<-i
      x=sigma[i]
      i=l
    }
  }
  sig[j]<-x
}
sig

cbind(k,sig)
plot(k,sig,col="red",type='l')
require('rgl')
open3d()
plot3d(k,sig,1,type="l",col=rainbow(1000))