bondvalue = function(c, T,r,par)
{
bv= c/r + (par-c/r)*(1+r)^(-2*T)
bv
}

price = 1200 
C = 40 
T = 30 
par = 1000
r = seq(0.02,0.05, length=300)

value = bondvalue(C, T, r, par)
cbind(r,value)
plot(r, value)

yield2M = spline(value, r, xout=price)

plot(r, value, xlab='Yield to Maturity', ylab='Price of Bond',
type="l",main="Par=1000, Coupon Payment = 40, T=30", lwd=2)
abline(h=price)
abline(v=yield2M)

---------------------------------------------------------------------------

price = 1200  # current price of the bond
C = 40 # Coupon Payment
T = seq(0.5,40, length=39) # Time to maturity
par = 1000  # par value of the bond
r = 0.03   # Interest Rate

value = bondvalue(C, T, r, par)
cbind(T,value)
plot(T, value, xlab='Time to Maturity', ylab='Price of Bond',
type="l",main="Par=1000, Coupon Payment = 40, r=0.03", lwd=2)


