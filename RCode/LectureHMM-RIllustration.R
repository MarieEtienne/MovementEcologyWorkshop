
# HMM lecture ----------------------------------------------------
## ---- hmmCode1
### Hidden State simulation
set.seed(6)
N <- 200
pi11 <- 0.8
pi22 <- 0.9
## initial distribution
mu1 <- c(0.5, 0.5)
##transition matrix
PI <- matrix(c(pi11, 1-pi11, 1-pi22, pi22), ncol=2, byrow = T)

##initialisation of Z
Z <- rep(NA, N)
Z[1] <- sample(1:2, size=1, prob = mu1)
for( i in 1:(N-1))
{
  Z[i+1] <- sample(1:2, size=1, prob = PI[Z[i],])
}
plot(1:N, Z, "s")
points(1:N, Z, col=Z+1, pch=19)

## ---- hmmCode2
### observation simulation
mu <- c(3, 7)
sigma <- c(1,1.5)
Y.hmm <- rnorm(N, mean=mu[Z], sd=sigma[Z])
plot(Y.hmm, pch=19)
plot(Y.hmm, pch=19, col=Z+1)


## ---- hmmCode3
### geometric sojourn time
N <- 4000
Z <- rep(NA, N)
Z[1] <- sample(1:2, size=1, prob = mu1)
for( i in 1:(N-1))
{
  Z[i+1] <- sample(1:2, size=1, prob = PI[Z[i],])
}
plot(1:N, Z, "s")
points(1:N, Z, col=Z+1, pch=19)
#switching time
switchTime <- which(diff(Z)!=0)
sojournTime <- diff(switchTime)
sojournState <- rep(c(3-Z[1], Z[1]), 
                    length.out = length(sojournTime))
br <- unique(quantile(sojournTime, 
                      p<- seq(1/N, 1, length.out = 6)))

abc <- seq(1, max(sojournTime)+10)
lapply(1:2, function(i){
  var <- sojournTime[sojournState==i]
  br <- sort(unique(var))
  hist(var, col=i+1, freq=F,
       xlim=range(sojournTime),
       ylim=c(0,0.4),
       main=paste0("Sojourn Time, State ", i),
       breaks=br)
  lines(abc, dgeom(abc-1, prob = 1-PI[i,i]),
        col=1, lwd=2, lty = 1+i)
})
  


## ---- hmmCode4
### Hidden State simulation and estimation
set.seed(6)
N <- 200
Z <- rep(NA, N)
Z[1] <- sample(1:2, size=1, prob = mu1)
for( i in 1:(N-1))
{
  Z[i+1] <- sample(1:2, size=1, prob = PI[Z[i],])
}
plot(1:N, Z, "s")
points(1:N, Z, col=Z+1, pch=19)
Y.hmm <- rnorm(N, mean=mu[Z], sd=sigma[Z])
Y <- Y.hmm
library('depmixS4')

df <- data.frame(Y=Y)
K=2
m1 <- depmix(Y~1,data=df, nstates=2, family=gaussian())
fit.model <- fit(m1)
summary(fit.model)
Z.hat <- viterbi(fit.model)[,1]
table(Z, Z.hat)
