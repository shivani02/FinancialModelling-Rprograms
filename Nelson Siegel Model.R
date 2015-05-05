#Submitted by : Shivani
#Subject : Fixed Income 
==================
  # Nelson Siegel Model
  
  setInternet2(TRUE)
install.packages("termstrc")
library("termstrc") # To access the library present in termstrc for govbonds coupon data
data("govbonds")
govbonds
head(govbonds)
class(govbonds) # To check whether the class is couponbonds
str(govbonds$GERMANY)

#  Nelson Siegel Model
ns_res<-estim_nss(govbonds,c("GERMANY","AUSTRIA","FRANCE"),matrange=c(0,30),method="ns",
                  tauconst = list(c(0.2,5,0.1),c(0.2,5,0.1),c(0.2,5,0.1)))
#Function for estimating the term structure of coupon bonds and yields, with the spot rate function  Nelson/Siegel
ns_res<-estim_nss(govbonds,c("GERMANY"),matrange=c(0,30),method="ns",tauconst = list(c(0.2,5,0.1)))
ns_res
class(ns_res$spsearch$GERMANY)#vector created by estim_nss
plot(ns_res$spsearch$GERMANY,main="GERMANY")
summary(ns_res)
plot(ns_res,multiple=TRUE)

#===========================================================================================
#Plot  errors in the specification search

setInternet2(TRUE)
install.packages("termstrc")
library("termstrc") # To access the library present in termstrc for govbonds coupon data
data("govbonds")
govbonds
head(govbonds)
class(govbonds) # To check whether the class is couponbonds
str(govbonds$GERMANY)
str(govbonds$FRANCE)
str(govbonds$AUSTRIA)

cs_res1<-estim_cs(govbonds,"FRANCE",matrange=c(0,30))
cs_res1
cs_res2<-estim_cs(govbonds,"GERMANY",matrange=c(0,30))
cs_res2
cs_res3<-estim_cs(govbonds,"AUSTRIA",matrange=c(0,30))
cs_res3

summary(cs_res1)
plot(cs_res1)
plot(cs_res1,errors="price")

# Calculations have been done for FRANCE.. In a similar way these could be done for others "GERMANY or AUSTRIA
cs_res_france<-estim_cs(rm_bond(govbonds,"FRANCE", c("FR0000571044","FR0000571085")),"FRANCE",
                        matrange=c(0,30))
plot(cs_res_france)
summary(cs_res_france)
plot(cs_res_france,errors="price") # errors in the specification search
#=============================================================================


#Generate a binomail lattice
gen_bin_lattice<- function(X0=x, u=u,d=d,N=N)
{
  X<-c()
  X[1] <- X0
  count<- 2
  for(i in 1:N){
    for(j in 0:i){
      X[count] <- X0*u^j*d^(i-j)
      count<- count+1
    }
  }
  return(X)
}
Binomial_lattice_Q4<-gen_bin_lattice(X0 = 100, u=1.1,d=0.75, N= 10)
plot(Binomial_lattice_Q4)
plot(Binomial_lattice_Q4, type="l")
#================================================================