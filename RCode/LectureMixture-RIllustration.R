
# Segmentation lecture ----------------------------------------------------
## ---- mixCode1
#
set.seed(6)
N <- 200
Z <- sample(1:2, size = N, replace=T, prob=c(0.3, 0.7))
mu <- c(3, 7)
sigma <- c(1,1.5)

Y.mixture <- rnorm(N, mean=mu[Z], sd=sigma[Z])
plot(Y.mixture, pch=19)
plot(Y.mixture, pch=19, col=Z+1)

s=4
plot(Y.mixture, pch=19)
abline(h=s, col=4, lwd=2)

plot(Y.mixture, pch=19)
abline(h=s, col=4, lwd=2)
points(1:length(Y.mixture), Y.mixture, pch=19, col=1*(Y.mixture>s)+2)

hist(Y.mixture[Z==1],, col=2, xlim=range(Y.mixture), freq=F, breaks = 10, main="Two distributions", xlab="Y")
hist(Y.mixture[Z==2],, col=3, xlim=range(Y.mixture), add=TRUE, freq=F, breaks = 20)

save("Y.mixture", file="../Data/dataMixture.Rd")

## ---- mixCode2
K <- 2; N <- 100; mu <- c(3, 7); sigma <- c(1,1.5)
Z <- sample(1:2, size = N, replace=T, prob=c(0.3, 0.7))
plot(Z, col=Z+1, pch=15, cex=0.8)

Y.mixture <- rnorm(N, mean=mu[Z], sd=sigma[Z])
plot(Y.mixture, col=Z+1, pch=19)
## ---- mixCode3
set.seed(25)

## ---- mixCode4
#library('mclust')
library('mixtools')
Y.clustering <- normalmixEM (Y.mixture, lambda = NULL, mu = NULL, sigma = c(1, 1.5), k = 2,  maxrestarts=20)


summary(Y.clustering)
Y.clustering$posterior
map <-  apply(Y.clustering$posterior,1,which.max)

## ---- mixCode5
library('Rmixmod')
Y.clustering.2 <- mixmodCluster(Y.mixture,nbCluster = 2:6)
hist(Y.clustering.2)
summary(Y.clustering.2)
pk <- Y.clustering.2[9][8]
map <- apply(pk,1,which.max)
plot(df, col=map+1)

## ---- mixCode6
init = kmeans(Y.mixture, centers = 2)
mu.init <- init$centers
sd.init <- unlist(by(Y.mixture, init$cluster, sd))
p1 <- sum(init$cluster==1)/length(init$cluster)

Y.clustering <- normalmixEM (Y.mixture, lambda = p1, mu = mu.init, sigma = sd.init,  
                             mean.constr = NULL, sd.constr = NULL,
                             epsilon = 1e-07, maxit = 1000, maxrestarts=20)
map <-  apply(Y.clustering$posterior,1,which.max)
table(Z, map)
