# call price function
callprice<-function(x,t,T,r,sigma,K)
{
d2<- (log(x/K) +(r-0.5*sigma^2)*(T-t))/(sigma*sqrt(T-t))
d1<-d2+sigma*sqrt(T-t)
x*pnorm(d1)-K*exp(-r*(T-t))*pnorm(d2)
}

#implied volatility function
ImpliedVolatility<-function(x,t,T,r,K,ObsPrice)
{
	sigma<-seq(0.001,10,by=0.0001)
	length(sigma)
	C<-callprice(x,t,T,r,sigma,K)
	error<-abs(ObsPrice-C)
	ImpliedSigma <- 0
	for (i in 1:length(sigma))
	{ 
		if(error[i] <1) 
			{
				ImpliedSigma<-sigma[i]
			}
	}
	ImpliedSigma
}

z<-read.csv(file.choose(),header=TRUE)   
head(z)
zFirstRow<-z[1,]

r<-0.04
#T<-1/4
t<- 0

x<-z$Underlying.Value
K<-z$Strike.Price
ObsPrice<-z$Settle.Price
T<-z$TTM


CalcSigmaGG<-mat.or.vec(length(x),1)
for(j in 1:length(x))
{
	CalcSigmaGG[j]<-ImpliedVolatility(x[j],t,T[j]/252,r,K[j],ObsPrice[j])
} 


require('rgl')
open3d()

plot3d(K,T,CalcSigmaGG,col=rainbow(1000),type="p",xlab="Strike Price",
ylab="Time to Maturity", zlab="Implied Volatility")