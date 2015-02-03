
# Segmentation lecture ----------------------------------------------------
## ---- segCode1
#
set.seed(6)
N <- 200
change.point <- round(cumsum(rexp(N, rate = 0.05)))
change.point <- change.point[which(change.point<N)]

## state sequence 
l1 <- Reduce("+", lapply(c(1,change.point), function(d){
 return( 1*(d<=(1:N) ))}))

Nchange <- length(change.point)
mu <-rnorm(Nchange+1, mean=5, sd=2)
sigma <- 1/rgamma(Nchange+1, shape = 20,rate = 10)                                  
signal <- rnorm(N, mean=mu[l1], sd=sigma[l1])
plot(signal)
lines(1:length(signal), mu[l1],'s', col=2, lwd=2)
Profil.seg <- signal

l1
save("Profil.seg", file="../Data/dataSegmentation.Rd")


pdf("rawSignal.pdf")
plot(signal)
dev.off()

## ---- segCode2
load("../Data/dataSegmentation.Rd")
summary(Profil.seg)
plot(Profil.seg)


## ---- segCode3
library('cghseg')
## format data into CGHdata
signalCGH    <- new("CGHdata",Y=Profil.seg)
CGHo         <- new("CGHoptions")
calling(CGHo)<- FALSE ## no classification 
segSignal   <- uniseg(.Object=signalCGH,CGHo=CGHo)
segSignalProf <- getsegprofiles(segSignal)  
plot(Profil.seg)
lines(1:length(segSignalProf),
      segSignalProf, type="s", col=2, lwd=2)




## ---- segCode4
## format data into CGHdata
calling(CGHo)<- TRUE ## no classification 
CGHo@nblevels=2
segSignal   <- uniseg(.Object=signalCGH,CGHo=CGHo)
segSignalProf <- getsegprofiles(segSignal)  
plot(Profil.seg)
lines(1:length(segSignalProf),
      segSignalProf, type="s", col=2, lwd=2)


