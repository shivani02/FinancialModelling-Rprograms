x<-read.csv(file.choose(),header=T)    
y<-x$Open 			
y<-x[,2]
y<-ts(data=y,frequency=243)		
ylag<-lag(y,k=-1)			
ytable<-cbind(y,ylag)
daily.return<-y/ylag-1			
ytable<-cbind(y,ylag,daily.return)
ytable
acf(daily.return)

#Our acf plot states that the series is stationary because all the lags are within the range.
#The blue lines are the limits and all the values lie between these lines. Hence returns are stationary

# The statistical Test => Dickey Fuller Test
#First we Load Package -- tseries

adf.test(daily.return)

# The solution that I get is Augmented Dickey-Fuller Test

data:  daily.return 
Dickey-Fuller = -5.5423, Lag order = 6, p-value = 0.01
alternative hypothesis: stationary 


If p value less than 0.05, then Alternate hypothesis is true, and reject null hypothesis and hence the returns are stationary.
Also if Dickey Fuller value is less than  -3.5, then Alternate is true. In this case, that is also true.
