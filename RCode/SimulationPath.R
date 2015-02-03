## ---- pathPractical1
library('CircStats')
library('adehabitatLT') ## dealing with ltraj object
library('cghseg') ## segemntation and classification
N <- 1e5

# Parameters Section ------------------------------------------------------
gamma.wc <- c(0.92, 0.99)
mu.wc <- c(0, 0)  

mu.ln <- c(-4, -3)
sigma.ln <- c(0.3, 0.3)

trans.mat <- matrix(c(0.999, 0.005, 0.001, 0.995), ncol=2)
State <- rep(1,N)
for(i in 1:(N-1))
{
  State[i+1]<- sample(1:2, size=1, prob = trans.mat[State[i],])
}

# simulation Section ------------------------------------------------------

phi <- sapply(1:N, function(d){rwrpcauchy(1, location=mu.wc[State[d]], rho=gamma.wc[State[d]])})
hist(phi)
V <- exp(rnorm(N, mean=mu.ln[State]-sigma.ln[State]^2/2, sigma.ln[State]))
hist(V)
plot(phi,V, col=State)



## ---- pathPractical2
# Plot section ------------------------------------------------------------
date=  as.POSIXlt(Sys.time()-12*24*3600 + cumsum(c(0,rep(10, N))), origin = "GMT")
path <- data.frame(x=x, y=y, date=date)
par(mfrow=c(1,1))
plot(path$x,path$y, "l", asp=1,   cex=0.05)

## ---- pathPractical3
par(mfrow=c(1,1))
plot(path$x,path$y, "l", asp=1,   cex=0.05)
points(path$x,path$y, cex=0.05, pch=20, col=State+1)


# Sampling section --------------------------------------------------------
sample.path <- function( initial.path, acquisition.step=1)
{
  observed.path <- initial.path[seq(1,N, acquisition.step),]
  return(observed.path)
}

## ---- pathPractical4
 for(s in 1:20)
 {
   acquisition.step <- round(10*exp(s/5)/10)*10
   sampled.path <- sample.path(initial.path = path, acquisition.step=acquisition.step)
   plot(sampled.path$x, sampled.path$y,"l",lty=1, cex=0.1)
 }
  
## ---- pathPractical5
s=10
acquisition.step <- round(10*exp(s/5)/10)*10
sampled.path <- sample.path(initial.path = path, acquisition.step=acquisition.step)
plot(sampled.path$x, sampled.path$y,type = "p" ,lty=1, cex=0.4)


## ---- pathPractical6

## ---- pathPractical7
s=10
acquisition.step <- round(10*exp(s/5)/10)*10
sampled.path <- sample.path(initial.path = path, acquisition.step=acquisition.step)
plot(sampled.path$x, sampled.path$y,type = "p" ,lty=1, cex=0.4)

# Observed Data -----------------------------------------------------------
s=60
observed.path<- sample.path(initial.path = path, acquisition.step=s)
traj.ex <-as.ltraj(xy = observed.path[,1:2], date = observed.path$date, id=rep(1,nrow(observed.path)) ) 
state.ex <-State[seq(1, length(State), s)]
par(mfrow=c(1,1))
plot(traj.ex, addpoints = F)
points(traj.ex[[1]]$x, traj.ex[[1]]$y,   col =state.ex , pch=19, cex=0.5)
legend("bottomleft",pch=c(2, 0), col=c(4,2), legend=c("Start", "End"), bty = "n", pt.lwd = c(1.5,1.5), pt.cex = c(1.5,1.5))

save(list = c("traj.ex", "state.ex"), file = "trajEx.Rd")

