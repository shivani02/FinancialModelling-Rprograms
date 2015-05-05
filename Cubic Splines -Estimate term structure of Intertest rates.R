setInternet2(TRUE)
install.packages("termstrc")
library("termstrc")
data("govbonds")
govbonds
head(govbonds)
class(govbonds)
str(govbonds$GERMANY)
str(govbonds$FRANCE)
str(govbonds$AUSTRIA)

# 1. NS execution
ns_res<-estim_nss(govbonds,c("GERMANY","AUSTRIA","FRANCE"),matrange=c(0,30),method="ns",
                  tauconst = list(c(0.2,5,0.1),c(0.2,5,0.1),c(0.2,5,0.1)))
#2.  plot 
class(ns_res$spsearch$GERMANY)#vector created by estim_nss
par(mfrow=c(1,3))#partitioning into 3 parts
plot(ns_res$spsearch$GERMANY,main="GERMANY")
plot(ns_res$spsearch$AUSTRIA,main="AUSTRIA")
plot(ns_res$spsearch$FRANCE,main="FRANCE")

# 3. Ns parameters
ns_res
# 4. Summary
summary(ns_res)

plot(ns_res,multiple=TRUE)

--------#Cubic Splines---------
cs_res<-estim_cs(govbonds,"FRANCE",matrange=c(0,30))
cs_res
# Summary
summary(cs_res)
plot(cs_res)
plot(cs_res,errors="price")


cs_res2<-estim_cs(rm_bond(govbonds,"FRANCE", c("FR0000571044","FR0000571085")),"FRANCE",
                  matrange=c(0,30))
plot(cs_res2)
summary(cs_res2)
plot(cs_res2,errors="price") # errors in the specification search