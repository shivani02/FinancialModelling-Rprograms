#14th Feb 2013, class

#code for Implied Volatility
#code for Stochastic Binomial

bondvalue = function(c,T,r,par)
{
# Computes bv = bond values (current prices) corresponding
# to all values of yield to maturity in the input vector r
#
# INPUT
# c = coupon payment (semiannual)
# T = time to maturity (in years)
# r = vector of yields to maturity (semiannual rates)
# par = par value
#
bv = c/r + (par - c/r) * (1+r)^(-2*T)
# return value to the function
bv
}

#Uses the function "bondvalue"
#
price = 1200 # current price of the bond
c = 40 # coupon payment
T = 30 # time to maturity
par = 1000 # par value of the bond
r = seq(.02,.05,length=300)
value = bondvalue(c,T,r,par)
table.price<-cbind(r,value) # combine r and value in a table
yield2M = spline(value,r,xout=price) # spline interpolation

plot(r,value,xlab='yield to maturity',ylab='price of bond',
type="l",main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M)

Class Exercise: keep r as constant and vary T

price = 1200 # current price of the bond
c = 40 # coupon payment
T = seq(0,30,length=360) # time to maturity
par = 1000 # par value of the bond
r = .05
value = bondvalue(c,T,r,par)
table.price<-cbind(T,value) # combine r and value in a table
yield2M = spline(value,T,xout=price) # spline interpolation

plot(T,value,xlab='time',ylab='price of bond',
type="l",main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M)

Class Exercise: vary r and vary T

price = 1200 # current price of the bond
c = 40 # coupon payment
T = seq(0,30,length=3) # time to maturity
par = 1000 # par value of the bond
r = seq(.02,.05,length=3)
value = bondvalue(c,T,r,par)
table.price<-cbind(r,T,value) # combine r and value in a table
yield2M = spline(value,T,xout=price) # spline interpolation
#Yield to maturity is always w.r.t. to a particular time

plot(T,value,xlab='time',ylab='price of bond',
type="l",main="par = 1000, coupon payment = 40, T = 30",lwd=2)
abline(h=1200)
abline(v=yield2M)

#?rep