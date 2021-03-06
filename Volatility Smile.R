===============================================

z<-read.csv(file.choose(), header=T)
y<-z[,4]
y<-ts(data=y,frequency=63)
ylag<-lag(y,k=-1)
ytable<-cbind(y,ylag)
daily.return<-y/ylag

##volatility , sigma = s/ t^0.5

var.dr<-var(daily.return)
s<-sqrt(var.dr)
t<-length(daily.return)
volatility<-s*sqrt(t)
volatility
 

===============================================

BS <-
function(S, K, T, r, volatility){
d1 <- (log(S/K) + (r + volatility^2/2)*T) / (volatility*sqrt(T))
d2 <- d1 - volatility*sqrt(T)
return(list(value=S*pnorm(d1)-K*pnorm(d2)*exp(-r*T),delta=pnorm(d1)))
}
bs<- BS(z[,4], 3000,63,0.05,volatility)


================================================

sigma<- seq(0.001,10, by= 0.0002)
length(sigma)
bs.price<- BS(y[1], 3000,63,0.05,sigma)
Price.ob<- bs$value[1]   
var = (bs.price$value - Price.ob)
z1<- abs(var)
z1
for (i in 1:49996) 
{  if(z1[i]< 0.01) 
   { 
     
     print(sigma[i])
     print(bs.price$value[i])
   }
} 

install.package
 #CHECK SYNTAX
============================
x<- 
plot(x,y,type='l')

=====================
implied = 0
bs.price<- BS(y[1], 3000,63,0.05,sigma)
Price.ob<- bs$value[1]   #say price is 2500
var = (bs.price$value - Price.ob)
z1<- abs(var)
for (i in 1:49996) 
 {if(z1[i]< 0.01) 
      { 
        implied<-sigma[i] 
        print(sigma[i])}
}

=====================

for(K in 2996:3177)
{
  for(T in 1:63)
  {
  bs.price<- BS(y[1], K,T,0.05,sigma)
  Price.ob<- bs$value[1]   #say price is 2500
  var = (bs.price$value - Price.ob)
  z1<- abs(var)
  for (i in 1:49996) 
    {  if(z1[i]< 0.01) 
      { 
        print(sigma[i])
      }
    }
  }
}


}



=====================
x<- sigma
y<- seq(2990,3178,by=5)
sort(x)
plot(x,y)

=====================
sort(x)
or<- order(x)
y.sortedbyX<- y[or]
x<- c(4,7,2,1,20,12,5,9,3,10)
y<- seq(2,20,by=2)

=========================

