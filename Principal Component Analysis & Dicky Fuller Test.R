#Input file ---> yield data.csv

# Read data

yield.data<-read.csv(file.choose(),header=T)  #//open yield data.csv

head(yield.data) 

#We do not need the "Date" column, so we create a variable which contains the columns from 2 to 12, ie we remove date

xx1<-yield.data[,2:12]
head(xx1)

acf(xx1[,1])  # It is not stationary but it is correlated
acf(xx1[,2])
acf(xx1[,3])
acf(xx1[,4])


#Load Package -- tseries

adf.test(xx1[,1])
adf.test(xx1[,2])
adf.test(xx1[,3])
adf.test(xx1[,4])
adf.test(xx1[,5])

#If p value less than 0.05, then Alternate is true, and reject null hyp

#and if Dickey Fuller >  -3.5, then Alternate is true
#Pca cannot be done if any of the columns are NON STATIONARY

#we need to chk all the collumns for its stationarity

pc<-princomp(xx1)
summary(pc)
plot(pc)

#look at the factors, the first 4 comp affects ,more than 99%.
#see the drop in variance b/w comp 1 & 2 and comp 3 & 4
#but we also need to have a look at the eigen values also


cov(xx1)
eig<-eigen(cov(xx1))
eig
eig$values
eig$vectors

#generally the Thumb rule is "Eigen value of a Factor must be greater than 1 for the Factor to be considered"
#but we have seen that there are no factors with Eigen value greater than 1
#thus it is highly subjective to 
