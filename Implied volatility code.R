sigma<- seq(0.001,10, by= 0.0002)
length(sigma)
BS<- sigma^2 # (to keep things simple, we're keeping it sigma squared)
Price.ob<- 25   #say price is 25
y = (BS - Price.ob)
z<- abs(y)
z
for (i in 1:49996) {if(z[i]< 0.01) {print (sigma[i])}} install.package
 #CHECK SYNTAX
=====================

sort(x)
or<- order(x)
y.sortedbyX<- y[or]
x<- c(4,7,2,1,20,12,5,9,3,10)
y<- seq(2,20,by=2)

==============================


BS <-
function(S, K, T, r, sigma){
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
return(list(value=S*pnorm(d1)-K*pnorm(d2)*exp(-r*T),delta=pnorm(d1)))
}

BS(5000, 5500, 90,0.09, sigma)



a <- BS( 5000,5500,0:90,.09, volatility)
ts.plot(rev(a$value),main="Black-Scholes call option",
xlab="day",ylab="value")

=================================


