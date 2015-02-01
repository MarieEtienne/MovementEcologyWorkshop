rm(list=ls())

## ---- MixtPractical1
library('mixtools')

load(file="../Data/trajEx.Rd")
load(file="../Data/traj.Rd")
Y <- traj.ex[[1]]$dP
hist(Y)
Y <- Y[!is.na(Y)]
init = kmeans(Y, centers = 2)
mu.init <- init$centers
sd.init <- unlist(by(Y, init$cluster, sd))
p1 <- sum(init$cluster==1)/length(init$cluster)
Y.clustering <- normalmixEM (Y, lambda = p1, mu = mu.init, sigma = sd.init,  
                             mean.constr = NULL, sd.constr = NULL,
                             epsilon = 1e-07, maxit = 1000, maxrestarts=20)
summary(Y.clustering)
map1 <-  apply(Y.clustering$posterior,1,which.max)
plot(Y, col=map1+1)
load(file="../Data/traj.Rd")
N <- length(state.ex)
table(map1+1, state.ex[2:(N-1)])


library('Rmixmod')
Y.clustering.2 <- mixmodCluster(Y,nbCluster = 1:5)
hist(Y.clustering.2)
summary(Y.clustering.2)
pk <- Y.clustering.2[9][8]
map2 <- apply(pk,1,which.max)

Y.clustering.2 <- mixmodCluster(Y,nbCluster = 1:5)
hist(Y.clustering.2)
summary(Y.clustering.2)
pk <- Y.clustering.2["bestResult"][8]
map2 <- apply(pk,1,which.max)


## ---- MixtPractical3
Y1 <- Y
Y2 <- traj.ex[[1]]$dN[!is.na(traj.ex[[1]]$dN)]
Y <- data.frame(Y1=Y1,Y2=Y2)
Y.clustering.3 <- mixmodCluster(Y, nbCluster=2:4, model=mixmodGaussianModel(family = "all"))
plot(Y.clustering.3)
plotCluster(Y.clustering.3["bestResult"], data=Y)
bRes <- Y.clustering.3["bestResult"]

table(bRes@partition, state.ex[2:(N-1)])

